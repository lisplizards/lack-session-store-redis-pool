(in-package #:lack/middleware/session/store/redis-pool/tests)

(setup
 (redis:with-connection (:host "localhost" :port 6379)
   (red:del "lack-session-store-redis-pool-test:*")))

(deftest session-middleware-with-redis-pool-store
  (testing "adds a session hash-table and session options to the ENV, and writes to the Redis store"
           (flet ((app (env)
                    (ok (listp (getf env :lack.session.options)))
                    (ok (eq t (getf (getf env :lack.session.options) :new-session)))
                    (ok (hash-table-p (getf env :lack.session)))
                    (setf (gethash "foo" (getf env :lack.session))
                          "bar")
                    (setf (gethash :quux (getf env :lack.session))
                          "baaz")
                    `(200
                      (:content-type "text/plain"
                       :content-length 13
                       :x-session-id ,(getf (getf env :lack.session.options) :id)
                       :headers ,(make-hash-table :test #'equal))
                      ("Hello, World."))))
             (let* ((store (lack/middleware/session/store/redis-pool:make-redis-store
                            :host "localhost"
                            :namespace "lack-session-store-redis-pool-test"
                            :max-open-count 10
                            :max-idle-count 2))
                    (app (funcall lack/middleware/session:*lack-middleware-session*
                                 #'app
                                 :state (lack/session/state/cookie:make-cookie-state)
                                 :store store))
                    (response (funcall app (list :headers (make-hash-table :test #'equal)))))
               (let* ((session-id (getf (second response) :x-session-id))
                      (session (lack/middleware/session/store:fetch-session store session-id)))
                 (ok (equal "bar" (gethash "foo" session)))
                 (ok (equal "baaz" (gethash :quux session)))))))

  (testing "signals REDIS-POOL-TIMEOUT-ERROR when the maximum number of connections has been checked out within the timeout limit (timeout 0), and calls ON-TOO-MANY-CONNECTIONS-ERROR if provided"
           (skip "FIXME: subject to race conditions")
           #+nil
           (flet ((app (env)
                    (sleep 5)
                    (setf (gethash "quux" (getf env :lack.session))
                          "baar")
                    `(200
                      (:content-type "text/plain"
                       :content-length 13
                       :x-session-id ,(getf (getf env :lack.session.options) :id)
                       :headers ,(make-hash-table :test #'equal))
                      ("Hello, World."))))
             (let* ((on-too-many-open-connections-called)
                    (store (lack/middleware/session/store/redis-pool:make-redis-store
                            :host "localhost"
                            :namespace "lack-session-store-redis-pool-test"
                            :max-open-count 3
                            :max-idle-count 4
                            :timeout 0
                            :on-too-many-open-connections (lambda (condition)
                                                            (declare (ignore condition))
                                                            (setq on-too-many-open-connections-called
                                                                  t))))
                    (app (funcall lack/middleware/session:*lack-middleware-session*
                                 #'app
                                 :state (lack/session/state/cookie:make-cookie-state)
                                 :store store))
                    (timeout-lock (bt2:make-lock))
                    (timeout-count 0))
               (flet ((make-thread ()
                        (bt2:make-thread
                         (lambda ()
                           (handler-case (funcall app (list :headers (make-hash-table :test #'equal)))
                             (lack/middleware/session/store/redis-pool:redis-pool-timeout-error (e)
                              (declare (ignore e))
                               (bt2:with-lock-held (timeout-lock)
                                 (incf timeout-count))))))))
                 (let ((threads (list (make-thread)
                                      (make-thread)
                                      (make-thread)
                                      (make-thread)
                                      (make-thread))))
                   (dolist (thread threads)
                     (bt2:join-thread thread))
                  ))
               (ok (= 2 timeout-count))
               (ok (eq t on-too-many-open-connections-called)))))

  (testing
   "does NOT error when maximum open connection count has been reached but pool has availability within the timeout limit"
   (flet ((app (env)
                    (sleep 5)
                    (setf (gethash "quux" (getf env :lack.session))
                          "baar")
                    `(200
                      (:content-type "text/plain"
                       :content-length 13
                       :x-session-id ,(getf (getf env :lack.session.options) :id)
                       :headers ,(make-hash-table :test #'equal))
                      ("Hello, World."))))
             (let* ((on-too-many-open-connections-called)
                    (store (lack/middleware/session/store/redis-pool:make-redis-store
                            :host "localhost"
                            :namespace "lack-session-store-redis-pool-test"
                            :max-open-count 3
                            :max-idle-count 5
                            :timeout 10000
                            :on-too-many-open-connections (lambda (condition)
                                                            (declare (ignore condition))
                                                            (setq on-too-many-open-connections-called
                                                                  t))))
                    (app (funcall lack/middleware/session:*lack-middleware-session*
                                 #'app
                                 :state (lack/session/state/cookie:make-cookie-state)
                                 :store store))
                    (timeout-lock (bt2:make-lock))
                    (timeout-count 0))
               (flet ((make-thread ()
                        (bt2:make-thread
                         (lambda ()
                           (handler-case (funcall app (list :headers (make-hash-table :test #'equal)))
                             (lack/middleware/session/store/redis-pool:redis-pool-timeout-error (e)
                              (declare (ignore e))
                               (bt2:with-lock-held (timeout-lock)
                                 (incf timeout-count))))))))
                 (let ((threads (list (make-thread)
                                      (make-thread)
                                      (make-thread)
                                      (make-thread)
                                      (make-thread))))
                   (dolist (thread threads)
                     (bt2:join-thread thread))))
               (ok (= timeout-count 0))
               (ok (null on-too-many-open-connections-called))))))

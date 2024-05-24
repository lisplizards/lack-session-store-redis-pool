;; Copyright (c) 2015 Eitaro Fukamachi
;; Copyright (c) 2024 John Newton (pooling support adaptations)
;; SPDX-License-Identifier: MIT

(in-package #:lack/middleware/session/store/redis-pool)

(defmacro with-connection (store &body body)
  `(anypool:with-connection (redis:*connection* (redis-store-pool ,store))
     ,@body))

(define-condition redis-pool-timeout-error (simple-error)
  ((too-many-open-connection :initarg :too-many-open-connection
                             :type anypool:too-many-open-connection))
  (:report (lambda (condition stream)
             (with-slots (too-many-open-connection)
                 condition
               (format stream "Failed to checkout a Redis connection  within the time limit: ~A"
                       too-many-open-connection)))))

(defstruct (redis-store (:include store)
                        (:constructor %make-redis-store))
  (namespace "session" :type string)
  (expires nil :type (or null integer))
  (serializer (lambda (data)
                (usb8-array-to-base64-string
                 (string-to-utf-8-bytes (prin1-to-string (marshal data))))))
  (deserializer (lambda (data)
                  (unmarshal (read-from-string
                              (utf-8-bytes-to-string (base64-string-to-usb8-array data))))))
  (on-too-many-open-connections nil :type (or null function))
  pool)

(defun make-redis-store (&rest kwargs &key
                                        (host "127.0.0.1") (port 6379) auth namespace expires
                                        serializer deserializer pool pool-name (max-open-count 8)
                                        (max-idle-count 4) (timeout 2000) (idle-timeout 10000)
                                        on-too-many-open-connections)
  (declare (ignore namespace expires serializer deserializer on-too-many-open-connections))
  (when pool
    (check-type pool anypool:pool))
  (let ((optional-initargs (loop for (key value) on kwargs by #'cddr
                                 when (and (member key '(:namespace
                                                         :expires
                                                         :serializer
                                                         :deserializer
                                                         :on-too-many-open-connections))
                                           value)
                                   append (list key value))))
    (apply #'%make-redis-store
           (append
            (list
             :pool (or pool (anypool:make-pool
                             :name (or pool-name "lack-session-store-redis")
                             :connector (lambda ()
                                          (make-instance 'redis:redis-connection
                                                         :host host
                                                         :port port
                                                         :auth auth))
                             :disconnector (lambda (connection)
                                             (declare (optimize (speed 3) (safety 0) (debug 0))
                                                      (type redis:redis-connection connection))
                                             (let ((redis:*connection* connection))
                                               (declare (type redis:redis-connection redis:*connection*))
                                               (redis:disconnect)))
                             :ping (lambda (connection)
                                     (declare (optimize (speed 3) (safety 0) (debug 0))
                                              (type redis:redis-connection connection))
                                     (let ((redis:*connection* connection))
                                       (declare (type redis:redis-connection redis:*connection*))
                                       (red:ping)))
                             :max-open-count max-open-count
                             :max-idle-count max-idle-count
                             :timeout timeout
                             :idle-timeout idle-timeout)))
            optional-initargs))))

(defmethod fetch-session ((store redis-store) sid)
  (let ((data (handler-bind ((anypool:too-many-open-connection
                               (lambda (condition)
                                 (and (redis-store-on-too-many-open-connections store)
                                      (funcall (redis-store-on-too-many-open-connections store)
                                               condition))
                                 (error 'redis-pool-timeout-error
                                        :too-many-open-connection condition))))
                (with-connection store
                  (red:get (format nil "~A:~A"
                                   (redis-store-namespace store)
                                   sid))))))
    (if data
        (handler-case (funcall (redis-store-deserializer store) data)
          (error (e)
            (warn "Error (~A) occured while deserializing a session. Ignoring.~2%    Data:~%        ~A~2%    Error:~%        ~A"
                  (class-name (class-of e))
                  data
                  e)
            nil))
        nil)))

(defmethod store-session ((store redis-store) sid session)
  (let ((data (funcall (redis-store-serializer store) session))
        (key  (format nil "~A:~A" (redis-store-namespace store) sid)))
    (handler-bind ((anypool:too-many-open-connection
                     (lambda (condition)
                       (and (redis-store-on-too-many-open-connections store)
                            (funcall (redis-store-on-too-many-open-connections store)
                                     condition))
                       (error 'redis-pool-timeout-error
                              :too-many-open-connection condition))))
      (with-connection store
        (red:set key data)
        (when (redis-store-expires store)
          (red:expire key (redis-store-expires store)))))))

(defmethod remove-session ((store redis-store) sid)
  (handler-bind ((anypool:too-many-open-connection
                   (lambda (condition)
                     (and (redis-store-on-too-many-open-connections store)
                          (funcall (redis-store-on-too-many-open-connections store)
                                   condition))
                     (error 'redis-pool-timeout-error
                            :too-many-open-connection condition))))
    (with-connection store
      (red:del (format nil "~A:~A"
                       (redis-store-namespace store)
                       sid)))))

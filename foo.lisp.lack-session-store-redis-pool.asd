;; Copyright (c) 2015 Eitaro Fukamachi
;; Copyright (c) 2024 John Newton (adaptations)
;; SPDX-License-Identifier: MIT

(defsystem "foo.lisp.lack-session-store-redis-pool"
  :version "1.0.0"
  :author "John Newton"
  :maintainer "John Newton"
  :license "MIT"
  :depends-on ("anypool"
               "cl-base64"
               "cl-redis"
               "lack-middleware-session"
               "marshal"
               "trivial-utf-8")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("package"))
                 (:file "package"))))
  :description "Redis-backed Lack session storage with connection pooling"
  :in-order-to ((test-op (test-op "foo.lisp.lack-session-store-redis-pool/tests"))))

(defsystem "foo.lisp.lack-session-store-redis-pool/tests"
  :author "John Newton"
  :license "MIT"
  :depends-on ("bordeaux-threads"
               "foo.lisp.lack-session-store-redis-pool"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main" :depends-on ("package"))
                 (:file "package"))))
  :description "Test system for foo.lisp.lack-session-store-redis-pool"
  :perform (test-op (op c) (symbol-call :rove :run c)))

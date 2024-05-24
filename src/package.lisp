;; Copyright (c) 2015 Eitaro Fukamachi
;; Copyright (c) 2024 John Newton (adaptations)
;; SPDX-License-Identifier: MIT

(in-package #:cl-user)

(defpackage #:lack/middleware/session/store/redis-pool
  (:use :cl)
  (:import-from #:lack/middleware/session/store
                #:store
                #:fetch-session
                #:store-session
                #:remove-session)
  (:import-from #:marshal
                #:marshal
                #:unmarshal)
  (:import-from #:cl-base64
                #:base64-string-to-usb8-array
                #:usb8-array-to-base64-string)
  (:import-from #:trivial-utf-8
                #:string-to-utf-8-bytes
                #:utf-8-bytes-to-string)
  (:export #:redis-store
           #:make-redis-store
           #:redis-pool-timeout-error
           #:fetch-session
           #:store-session
           :remove-session)
  (:documentation "Fork of the official Lack session store LACK/MIDDLEWARE/SESSION/STORE/REDIS to add pooling"))

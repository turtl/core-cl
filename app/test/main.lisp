(defpackage :turtl-core-test
  (:use :cl :fiveam :event-glue :turtl-core)
  (:import-from :turtl-core
                ;; crypto
                :encrypt
                :decrypt
                :keygen
                :to-base64
                :from-base64
                :to-hex
                :from-hex
                :key-to-string
                :key-from-string
                :random-bytes
                :make-iv
                :random-key
                :thash
                :random-number
                :uuid
                
                ;; mvc
                :dispatch
                :model
                :collection
                :models
                :model-type
                :sort-function
                :mserialize
                :mid
                :mget
                :mset
                :munset
                :mclear
                :mdestroy
                :create-model
                :msort
                :madd
                :mrem
                :mclear
                :mreset
                :mfind
                :create-collection
                
                ;; protected
                :mdeserialize)
  (:shadowing-import-from :event-glue
                          :trigger
                          :bind)
  (:export #:run-tests))
(in-package :turtl-core-test)

(def-suite turtl-core :description "The main turtl-core test suite.")
(def-suite turtl-core-crypto :description "Crypto tests" :in turtl-core)
(def-suite turtl-core-mvc :description "MVC tests" :in turtl-core)
(def-suite turtl-core-protected :description "MVC protected tests" :in turtl-core)

(defun run-tests ()
  "Run all turtl-core tests."
  (run! 'turtl-core))


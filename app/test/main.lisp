(defpackage :turtl-core-test
  (:use :cl :cl-async-future :fiveam :event-glue :turtl-core)
  (:import-from :turtl-core
                ;; main bs
                :do-start
                
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
                :minit
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
                :protected
                :key
                :body-key
                :public-fields
                :private-fields
                :raw-data
                :mdeserialize
                
                ;; models
                :user
                :generate-key
                :generate-auth)
  (:shadowing-import-from :event-glue
                          :trigger
                          :bind)
  (:export #:run-tests))
(in-package :turtl-core-test)

(def-suite turtl-core :description "The main turtl-core test suite.")
(def-suite turtl-core-crypto :description "Crypto tests" :in turtl-core)
(def-suite turtl-core-mvc :description "MVC tests" :in turtl-core)
(def-suite turtl-core-protected :description "MVC protected tests" :in turtl-core)
;; models
(def-suite turtl-core-models :description "Model tests" :in turtl-core)
(def-suite turtl-core-model-sync :description "Sync model" :in turtl-core-models)
(def-suite turtl-core-model-user :description "User model" :in turtl-core-models)
(def-suite turtl-core-model-keychain :description "Keychain model" :in turtl-core-models)


(defmacro with-running-test (&body body)
  "Runs body in the context of a running Turtl core. Designed to quit after
   executing the events in the body."
  (let ((loglevel (gensym "loglevel")))
    `(let ((,loglevel (getf vom::*config* :turtl-core)))
       (vom:config :turtl-core :error)
       (do-start :start-fn (lambda () ,@body) :no-signal-handler t)
       (vom:config :turtl-core ,loglevel))))

(defun run-tests ()
  "Run all turtl-core tests."
  (run! 'turtl-core))


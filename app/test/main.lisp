(defpackage :turtl-core-test
  (:use :cl :fiveam :turtl-core)
  (:import-from :turtl-core
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
                :uuid)
  (:export #:run-tests))
(in-package :turtl-core-test)

(def-suite turtl-core :description "The main turtl-core test suite.")
(def-suite turtl-core-crypto :description "Crypto tests" :in turtl-core)


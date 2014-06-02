(asdf:defsystem turtl-core-test
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.6.0"
  :description "(tests for) The core logic for the Turtl clients"
  :depends-on (#:fiveam #:turtl-core)
  :components
  ((:module test
    :serial t
    :components
    ((:file "main")))))


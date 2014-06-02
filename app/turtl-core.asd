(asdf:defsystem turtl-core
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.6.0"
  :description "The core logic for the Turtl clients"
  :depends-on (#-ecl :turtl-pkg)
  :components
  (#+ecl (:compiled-file "turtl-pkg" :pathname #p"./turtl-pkg.fas")
   (:file "package")
   (:file "config" :depends-on ("package"))
   (:file "util" :depends-on ("package" "config"))
   (:file "event" :depends-on ("package" "util" "config"))
   (:module comm
    :depends-on ("package" "util" "config" "event")
    :serial t
    :components
    ((:file "native")
     (:file "remote")))
   (:module library
    :depends-on ("package" "util" "event")
    :serial t
    :components
    ((:file "crypto")
     (:file "api")
     (:file "model")
     (:file "storage")))
   (:module models
    :depends-on ("package" "util" "event" library)
    :serial t
    :components
    ((:file "board")
     (:file "file")
     (:file "invite")
     (:file "keychain")
     (:file "message")
     (:file "note")
     (:file "persona")
     (:file "profile")
     (:file "sync")
     (:file "user")))
   (:file "main" :depends-on ("package" "comm" "util" "event"))))


(asdf:defsystem turtl-core
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.6.0"
  :description "The core logic for the Turtl clients"
  :depends-on (#-ecl :turtl-pkg)
  :components
  (#+ecl (:compiled-file "turtl-pkg" :pathname #p"./turtl-pkg.fas")
   ;#+ecl (:compiled-file "turtl-core" :pathname #p"./turtl-core.fas")
   (:file "package")
   (:file "config" :depends-on ("package"))
   (:file "util" :depends-on ("package" "config"))
   (:file "event" :depends-on ("package" "util" "config"))
   (:module comm
    :depends-on ("package" "util" "config" "event")
    :serial t
    :components
    ((:file "remote")))
   (:module library
    :depends-on ("package" "util" "event")
    :serial t
    :components
    ((:file "crypto")
     (:file "api")
     (:file "worker")
     (:module mvc
      :serial t
      :components
      ((:file "common")
       (:file "model")
       (:file "collection")))
     (:file "protected")
     (:file "mvc-api-adapter")
     (:file "mvc-db-adapter")
     (:file "turtl-model")
     (:file "search")))
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
   (:module controllers
    :depends-on ("package" "util" "event" library models)
    :serial t
    :components
    ((:file "sync")
     (:file "user")
     (:file "board")
     (:file "note")
     (:file "profile")))
   (:file "main" :depends-on ("package" comm "util" "event"))
   (:file "turtl" :depends-on ("package" "event" controllers models))))


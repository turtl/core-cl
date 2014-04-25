(asdf:defsystem turtl-core
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :description "The core logic for the Turtl clients"
  :depends-on ()
  :components
  ((:compiled-file "turtl-pkg" :pathname #p"./turtl-pkg.fas")
   (:file "package")
   (:file "util" :depends-on ("package"))
   (:file "event" :depends-on ("package" "util"))
   (:file "native" :depends-on ("package" "event"))
   (:file "comm" :depends-on ("package" "util" "native" "event"))
   (:file "main" :depends-on ("package" "comm" "util" "event"))))


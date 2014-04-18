(asdf:defsystem turtl-core
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :description "The core logic for the Turtl clients"
  :depends-on (#:alexandria
               #:cl-async-future
               #:cl-async 
               #:bordeaux-threads
               #:lparallel
               #:cl-ppcre
               #:babel
               #:drakma-async
               #:flexi-streams)
  :components
  ((:file "package")
   (:file "comm" :depends-on ("package"))
   (:file "main" :depends-on ("package" "comm"))))


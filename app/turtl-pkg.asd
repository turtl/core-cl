(asdf:defsystem turtl-pkg
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :description "An ASDF system to load all Turtl core dependencies (mainly for easy compiling)."
  :depends-on (#:alexandria
               #:cl-async-future
               #:cl-async 
               #:cl-hash-util
               #:static-vectors
               #:bordeaux-threads
               #:cl-nettle
               #:lparallel
               #:event-glue
               #:sqlite-obj
               #:vom
               #:cl-ppcre
               #:babel
               #:sqlite
               #:drakma-async
               #:flexi-streams
               #:yason
               #:simple-search))


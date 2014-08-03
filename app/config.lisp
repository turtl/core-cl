(in-package :turtl-core)

;; client self-awareness
(defparameter *client* "core")
(defparameter *version* (asdf/component:component-version (asdf/system:find-system :turtl-core)))
(defparameter *root* (asdf:system-relative-pathname :turtl-core #P"")
  "Defines the directory we're loading from.")

;; data directory
(defvar *data-directory* "~/.turtl"
  "Holds our Turtl database and any other files associated with storage.")

(defparameter *comm-url* "inproc://turtl"
  "The nanomsg URL we use to talk to Turtl.")

;; api stuff
(defparameter *api-url* "http://turtl.dev:8181/api")
(defparameter *api-key* "")
(defparameter *api-auth*
  '((:post . "/users")
    (:post . "/log/error"))
  "API resources that *don't* need auth.")

(defparameter *db-schema*
  '(("kv")
    ("users")
    ("keychain"
     :indexes (("item_id" . :id)))
    ("personas")
    ("boards"
     :indexes (("user_id" . :id)))
    ("notes"
     :indexes (("user_id" . :id)
               ("board_id" . :id)
               ("has_file" . :bool)))
    ("files"
     :binary-data t
     :indexes (("note_id" . :id)
               ("synced" . :bool)
               ("has_data" . :bool))))
  "Holds the local DB schema. This is really a table name and a set of indexes
   for that table. Anything else for each table is implemented as a JSON blob,
   effectively giving us an object store without th hassle of codifying every
   last possible field. So in general the setup is:

     ID | [indexed_field1] | [indexed_field2] | ... | data
   
   where `data` is a JSON blob, or possibly a binary blob (if :binary-data t is
   given in the table's metadata).")

(vom:config :turtl-core :debug)


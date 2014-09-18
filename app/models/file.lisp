(in-package :turtl-core)

(deftobject file "files"
            ("id"
             "note_id"
             "synced"
             "has_data")
            ("data"))

(defclass files (sync-collection)
  ((model-type :accessor model-type :initform 'file)))


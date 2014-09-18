(in-package :turtl-core)

(deftobject note "notes"
            ("id"
             "user_id"
             "board_id"
             "file"
             "has_file"
             "keys"
             "meta"
             "sort"
             "mod")
            ("type"
             "title"
             "tags"
             "url"
             "text"
             "embed"
             "color"))

(defclass notes (sync-collection)
  ((model-type :accessor model-type :initform 'note)))

(defmethod find-key ((model note) keys &optional search)
  (let* ((board-id (mget model "board_id"))
         (board-key (ignore-errors (key (mfind (mget *profile* "boards") board-id)))))
    (when (and board-id board-key)
      (setf (getf search :b) `((:id ,board-id :k ,board-key))))
    (call-next-method model keys search)))


(in-package :turtl-core)

(deftobject keychain-entry "keychain"
            ("id"
             "type"
             "item_id"
             "user_id")
            ("k"))

(defclass keychain (sync-collection)
  ((model-type :accessor model-type :initform 'keychain-entry)))

(defmethod minit ((model keychain-entry))
  (setf (key model) (key *user*))
  (call-next-method))

(defclass keychain (collection)
  ((model-type :accessor model-type :initform 'keychain-entry)))

(defgeneric find-keychain-entry (keychain id)
  (:documentation "Find an entry for an item in the keychain by ID."))

(defmethod find-keychain-entry ((keychain keychain) id)
  (let ((entry (mfind keychain id :field "item_id")))
    (vom:debug1 "keychain: find entry: ~a: ~a" id (if entry (mid entry) nil))
    (when entry
      (let ((key (mget entry "k")))
        (when key
          (from-base64 key))))))


(in-package :turtl-core)

(deftobject persona "personas"
            ("id"
             "user_id"
             "pubkey"
             "email"
             "name"
             "settings")
            ("privkey"))

(defclass personas (sync-collection)
  ((model-type :accessor model-type :initform 'persona)))

(defmethod minit ((model persona))
  (setf (key model) (key *user*))
  (call-next-method))


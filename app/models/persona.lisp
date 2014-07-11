(in-package :turtl-core)

(deftobject persona "personas"
            ("id"
             "user_id"
             "pubkey"
             "email"
             "name"
             "settings")
            ("privkey"))

(defmethod minit ((model persona))
  (setf (key model) (key *user*))
  (call-next-method))


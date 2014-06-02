(in-package :turtl-core)

(define-db-object file
  (("id"        :public t :type :pkey)
   ("note_id"   :public t :type :id)
   ("synced"    :public t :type :bool)
   ("has_data"  :public t :type :bool)
   ("data"))
  (:indexes
     (("node_id.v1" . ("note_id"))
      ("synced.v1" . ("synced"))
      ("has_data.v1" . ("has_data")))))


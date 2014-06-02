(in-package :turtl-core)

(define-db-object note
  (("id"         :public t :type :pkey)
   ("user_id"    :public t :type :id)
   ("board_id"   :public t :type :id)
   ("file"       :public t :type :object)
   ("has_file"   :public t :type :integer)
   ("keys"       :public t :type :object)
   ("meta"       :public t :type :object)
   ("sort"       :public t :type :integer)
   ("mod"        :public t :type :integer)
   ("type")
   ("title")
   ("tags")
   ("url")
   ("text")
   ("embed")
   ("color"))
  (:indexes
    (("user_id.v1" . ("user_id"))
     ("board_id.v1" . ("board_id"))
     ("has_file.v1" . ("has_file")))))


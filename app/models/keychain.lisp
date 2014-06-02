(in-package :turtl-core)

(define-db-object keychain
  (("id"        :public t :type :pkey)
   ("type"      :public t :type :string)
   ("item_id"   :public t :type :id)
   ("user_id"   :public t :type :id)
   ("k"))
  (:indexes
     (("item_id.v1" . ("item_id")))))


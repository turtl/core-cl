(in-package :turtl-core)

(define-db-object board
  (("id"        :public t :type :pkey)
   ("user_id"   :public t :type :id)
   ("keys"      :public t :type :object)
   ("privs"     :public t :type :object)
   ("personas"  :public t :type :object)
   ("meta"      :public t :type :object)
   ("shared"    :public t :type :bool)
   ("title"))
  (:indexes
     (("user_id.01" . ("user_id")))))


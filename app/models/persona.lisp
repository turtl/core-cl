(in-package :turtl-core)

(define-db-object persona
  (("id"        :public t :type :pkey)
   ("user_id"   :public t :type :id)
   ("pubkey"    :public t :type :text)
   ("email"     :public t :type :string)
   ("name"      :public t :type :string)
   ("settings"  :public t :type :object)
   ("privkey")))


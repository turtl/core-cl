(in-package :turtl-core)

(define-db-object sync
  (("id"      :public t :type :pkey)
   ("value"   :public t :type :string)))


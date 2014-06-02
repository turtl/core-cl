(in-package :turtl-core)

(define-db-object user
  (("id"        :public t :type :pkey)
   ("value"     :type :string)
   ("settings"  :type :object)))

(defmethod generate-key ((user user))
  ""
  (let ((username (mget user "username"))
        (password (mget user "password")))
    (unless (and username password)
      (return-from generate-key))
    (let* ((username (babel:string-to-octets username))
           (password (babel:string-to-octets password))
           (key (keygen password
                        (concatenate 'nec:octet-array
                                     username
                                     (babel:string-to-octets ":a_pinch_of_salt"))
                        400
                        32)))
      (mset user `(:key ,key))
      key)))

(defmethod generate-auth ((user user))
  ""
  (let ((username (mget user "username"))
        (password (mget user "password")))
    (unless (and username password)
      (return-from generate-auth))
    (let* ((username (babel:string-to-octets username))
           (password (babel:string-to-octets password))
           (intermediary (concatenate 'nec:octet-array
                                      (nec:sha256 password)
                                      username))
           (key (or (mget user "key") (generate-key user)))
           (iv (make-iv (concatenate 'nec:octet-array
                                     username
                                     (babel:string-to-octets "4c281987249be78a"))))
           (auth (encrypt key intermediary :version 0 :iv iv)))
      (mset user `(:auth ,auth))
      auth)))


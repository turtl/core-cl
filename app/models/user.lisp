(in-package :turtl-core)

(define-db-object user
  (("id"        :public t :type :pkey)
   ("settings"  :type :object))
  (:extra-fields
    ((logged-in :accessor logged-in :initform nil))))

(defun login (user)
  "Log the passed user in."
  (trigger (event "login") :dispatch (dispatch user)))

(defun logout (user)
  "Log the passed user out."
  (setf (logged-in user) nil)
  (mclear user)
  (trigger (event "logout") :dispatch (dispatch user)))

(defmethod generate-key ((user user))
  "Generate a user's key from the username/password."
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
  "Generate a user's auth token from username/password."
  (let ((username (mget user "username"))
        (password (mget user "password")))
    (unless (and username password)
      (return-from generate-auth))
    (let* ((username (babel:string-to-octets username))
           (password (babel:string-to-octets password))
           (intermediary (concatenate 'nec:octet-array
                                      (babel:string-to-octets (to-hex (nec:sha256 password)))
                                      (babel:string-to-octets ":")
                                      username))
           (key (or (mget user "key") (generate-key user)))
           (iv (make-iv (concatenate 'nec:octet-array
                                     username
                                     (babel:string-to-octets "4c281987249be78a"))))
           (auth (encrypt key intermediary :version 0 :iv iv)))
      (mset user `(:auth ,auth))
      auth)))

(defafun test-login (future) (user)
  "Test a user's login."
  (set-api-auth (generate-auth user))
  (future-handler-case
    (alet ((user-id (api :post "/auth" nil)))
      (finish future user-id))
    (api-error ()
      (finish future nil))))


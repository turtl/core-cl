(in-package :turtl-core)

(with-bind ("do-login" ev :response res)
  (let* ((data (data ev))
         (username (gethash "username" data))
         (password (gethash "password" data))
         (user *user*))
    (when (logged-in user)
      (error "Already logged in"))
    (mset user (list :username username
                     :password password))
    (alet ((success (test-login user :grab-data t)))
      (if success
          (let ((data (mdata user)))
            (res data)
            (vom:info "user successful login: ~a" (mid user))
            (login user)
            (setf *user* user))
          (error 'turtl-error :msg "Login failed.")))))

(with-bind ("do-logout" ev)
  (logout *user*))


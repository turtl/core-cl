(in-package :turtl-core)

(with-bind ("do-login" ev)
  (let* ((data (data ev))
         (username (gethash "username" data))
         (password (gethash "password" data))
         (user *user*))
    (mset user (list :username username
                     :password password))
    (alet ((success (test-login user)))
      (if success
          (wait-for (mserialize user)
            (trigger-remote (event "success:do-login" :data user :uuid (id ev)))
            (vom:info "user successful login: ~a" (mid user))
            (login user)
            (setf *user* user))
          (error 'turtl-error :msg "Login failed.")))))

(with-bind ("do-logout" ev)
  (logout *user*))


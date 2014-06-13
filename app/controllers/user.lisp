(in-package :turtl-core)

(with-bind ("do-login" ev respond)
  (let* ((username (gethash "username" ev))
         (password (gethash "password" ev))
         (user (create-model 'user (list :username username
                                         :password password))))
    (alet ((success (test-login user)))
      (if success
          (wait-for (mserialize user)
            (login user)
            (respond (event "do-login" :data user)))
          (error 'turtl-error :msg "Login failed.")))))

(with-bind ("do-logout" ev)
  (logout *user*))


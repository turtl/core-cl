(in-package :turtl-core)

(defvar *user* (make-instance 'user)
  "Holds the current user.")

(defvar *profile* nil
  "Holds the current user's profile.")

(with-bind ("logout" ev)
  (clear-api-auth))

(with-bind ("ping" event respond)
  (vom:debug "ping!")
  (respond (event "pong")))

(with-bind ("cmd" event respond)
  (case (intern (string-upcase (hget (data event) '("name"))) :keyword)
    (:reload
      (asdf:operate 'asdf:load-op :turtl-core)
      (respond (event "success")))))


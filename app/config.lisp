(in-package :turtl-core)

;; client self-awareness
(defparameter *client* "core")
(defparameter *version* (asdf/component:component-version (asdf/system:find-system :turtl-core)))
(defparameter *root* (asdf:system-relative-pathname :turtl-core #P"")
  "Defines the directory we're loading from.")

;; api stuff
(defparameter *api-url* "http://turtl.dev:8181/api")
(defparameter *api-key* "")
(defparameter *api-auth*
  '((:post . "/users")
    (:post . "/log/error"))
  "API resources that *don't* need auth.")

(vom:config :turtl-core :debug)


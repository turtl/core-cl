;;; This file holds the top-level bindings and wiring for the turtl core app.
;;; it can be likened to turtl.js in the js project

(in-package :turtl-core)

(defvar *user* (make-instance 'user-db)
  "Holds the current user.")

(defvar *profile* nil
  "Holds the current user's profile.")

(defvar *keychain* nil
  "Holds the current user's keychain.")

(defvar *sync* nil
  "Holds the sync model.")

(defvar *messages* nil
  "Holds incoming messages.")

(defvar *files* nil
  "Holds the files handler/model.")

(with-bind ("login" ev :dispatch (dispatch *user*))
  (trigger-remote (event "profile-loading"))
  (wait-for (setup-db)
    (setf *profile* (create-model 'profile-db))
    (setf *sync* (create-model 'sync-db))
    (trigger-remote (event "profile-loading-progress" :data "db"))
    (wait-for (populate-profile *profile*)
      (trigger-remote (event "profile-loading-progress" :data (list "index")))
      (wait-for (index-notes (mget *profile* "notes"))
        (trigger-remote (event "profile-loaded"))))))

(with-bind ("logout" ev :dispatch (dispatch *user*))
  (close-db)
  (clear-api-auth)
  (setf *profile* nil)
  (setf *sync* nil)
  (setf *messages* nil)
  (setf *files* nil))

(with-bind ("profile-get" ev)
  "Get an item in the profile by id."
  (let* ((data (data ev))
         (what (hget data "type"))
         (id (hget data "id")))
    (let ((model (mfind (mget *profile* what) id)))
      (trigger-remote (event "profile-get" :data model :uuid (id ev))))))

(with-bind ("profile-list" ev)
  "Get all items of s single type from a profile."
  (let* ((data (data ev))
         (what (hget data "type")))
    (let ((models (models (mget *profile* what))))
      (trigger-remote (event "profile-list" :data models :uuid (id ev))))))

(with-bind ("ping" ev)
  (trigger-remote (event "pong" :uuid (id event))))

(with-bind ("cmd" ev)
  (case (intern (string-upcase (hget (data ev) "name")) :keyword)
    (:reload
      (asdf:operate 'asdf:load-op :turtl-core)
      (trigger-remote (event "success:cmd:reload" :uuid (id ev))))
    (:set-data-directory
      ;; convert windows paths
      (let ((pruned (cl-ppcre:regex-replace-all "\\" (hget (data ev) "path") "/")))
        (setf *data-directory* pruned)))))

(defafun setup-db (future) ()
  (let ((db-name (format nil "~a/core-~a.sqlite" *data-directory* (mid *user*))))
    (when *db* (db-close *db*))
    (setf *db* (db-open db-name))
    (apply-schema *db* *db-schema*)
    (finish future t)))

(defafun close-db (future) ()
  (when *db* (db-close *db*))
  (finish future t))
  


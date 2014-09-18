;;; This file holds the top-level bindings and wiring for the turtl core app.
;;; it can be likened to turtl.js in the js project

(in-package :turtl-core)

(defvar *do-sync* t
  "Whether or not to sync local data to/from the API.")

(defvar *user* (make-instance 'user-db)
  "Holds the current user.")

(defvar *profile* nil
  "Holds the current user's profile.")

(defvar *keychain* nil
  "Holds the current user's keychain.")

(defvar *sync* (make-instance 'sync-db)
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

(with-bind ("ping" ev :response res)
  (trigger-remote (event "pong" :uuid (id ev))))

(with-bind ("cmd" ev :response res)
  (case (intern (string-upcase (hget (data ev) "name")) :keyword)
    ;; reload the turtl-core app
    (:reload
      (asdf:operate 'asdf:load-op :turtl-core)
      (res))

    ;; tells the core where we are going to store data files
    (:set-data-directory
      ;; convert windows paths
      (let ((pruned (cl-ppcre:regex-replace-all "\\" (hget (data ev) "path") "/")))
        (setf *data-directory* pruned)
        (res)))

    ;; wipe the local database file(s)
    (:wipe-local-db
      (unless (logged-in *user*)
        (error "User not logged in."))
      (let ((db-name (format nil "~a/core-~a.sqlite" *data-directory* (mid *user*))))
        (logout *user*)
        (when (probe-file db-name)
          (delete-file db-name))
        (res)))

    ;; test the work queue by blasting some jobs at it
    (:test-work
      (dotimes (i 99)
        (alet* ((x (random 100))
                (y (random 100))
                (res (work (sleep .01) (+ x y))))
          (format t "~a + ~a = ~a~%" x y res))))

    ;; stop the turtl-core thread
    (:stop
      (stop))))

(defafun setup-db (future) ()
  (let ((db-name (format nil "~a/core-~a.sqlite" *data-directory* (mid *user*))))
    (when *db* (db-close *db*))
    (setf *db* (db-open db-name))
    (apply-schema *db* *db-schema*)
    (finish future t)))

(defun setup-syncing ()
  "Setup the syncing API <--> DB <--> UI"
  (sync-register-local "users" (create-collection 'users) 'user-db)
  (sync-register-local "keychain" (create-collection 'keychain) 'keychain-entry-db)
  (sync-register-local "personas" (create-collection 'personas) 'persona-db)
  (sync-register-local "boards" (create-collection 'boards) 'board-db)
  (sync-register-local "notes" (create-collection 'notes) 'note-db)
  (sync-register-local "files" (create-collection 'files) 'file-db)

  (sync-register-remote "users" (create-collection 'users) 'user-api)
  (sync-register-remote "keychain" (create-collection 'keychain) 'keychain-entry-api)
  (sync-register-remote "personas" (create-collection 'personas) 'persona-api)
  (sync-register-remote "boards" (create-collection 'boards) 'board-api)
  (sync-register-remote "notes" (create-collection 'notes) 'note-api)
  (sync-register-remote "files" (create-collection 'files) 'file-api)

  (start-sync))

(defafun close-db (future) ()
  (when *db* (db-close *db*))
  (setf *db* nil)
  (finish future t))


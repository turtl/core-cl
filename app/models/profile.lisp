(in-package :turtl-core)

(deftobject profile nil)

(defmethod mget ((model profile) field &optional default)
  "Defines an mget that always returns a collection, even if empty."
  (declare (ignorable model field default))
  (let ((sub (call-next-method)))
    (if sub
        sub
        (create-collection 'collection))))
  
(defafun persist-profile (future) (profile-data)
  "Given a set of profile data, persist it to the local DB."
  (trigger-remote (event "profile-loading-progress" :data "persist"))
  (let ((profile-data (sync-process-data profile-data))
        (items '(("user" . "users") "keychain" "personas" "boards" "notes" "files")))
    (dolist (key items)
      (vom:debug "profile: persist: ~a" key)
      (let ((table (if (consp key) (cdr key) key))
            (key (if (consp key) (car key) key))
            (records (gethash key profile-data)))
        (dolist (record records)
          (vom:debug1 "persist-profile: save: ~a" key (gethash "id" record))
          (db-save table record))))
    (mset *sync* (list :sync_id (gethash "sync_id" profile-data)))
    (msave *sync*)
    (finish future t)))

(defafun download-profile (future) (&key persist)
  "Download and optionally persist the database."
  (trigger-remote (event "profile-loading-progress" :data "download"))
  (future-handler-case
    (alet ((profile-data (api :get (format nil "/profiles/users/~a" (mid *user*)) nil)))
      (wait-for (when persist
                  (persist-profile profile-data))
        (finish future profile-data)))
    (api-error (e)
      (trigger-remote
        (event "error"
               :data (hash ("str" (format nil "Problem loading profile: ~a" (turtl-error-msg e)))
                           ("code" (api-error-status e))))))))

(defafun populate-profile (future) (profile)
  "Grab the current user's profile and load the contained data into the local
   DB."
  (let ((sync-id (db-get "kv" "sync-id")))
    (wait-for (unless sync-id
                (download-profile :persist t))
      (trigger-remote (event "profile-loading-progress" :data "populate"))
      (wait-for (mfetch *user*)
        (setf *keychain* (create-collection 'keychain))
        (wait-for
            (adolist (type `((keychain-entry-db ,*keychain*)
                             persona-db
                             board-db
                             note-db) dfuture)
              (let* ((collection (if (listp type)
                                     (cadr type)
                                     (create-collection 'collection)))
                     (type (if (listp type) (car type) type))
                     (table (table (create-model type)))
                     (key table)
                     (items (db-all table)))
                (trigger-remote (event "profile-loading-progress" :data (list "populate" table)))
                (setf (model-type collection) type)
                (wait-for (mreset-async collection items)
                  (mset profile (hash (key collection)))
                  (finish dfuture))))
          (finish future))))))


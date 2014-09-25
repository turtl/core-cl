(in-package :turtl-core)

(deftobject sync "kv" ("id" "sync_id") nil
            ((enabled :accessor enabled :initform nil)
             (local-registry :accessor local-registry :initform (make-hash-table :test 'equal))
             (remote-registry :accessor remote-registry :initform (make-hash-table :test 'equal))))

(defclass sync-collection (collection) ())

(defmethod minit ((model sync-db))
  (setf (mid model) "sync-id")
  (setf (key model) (random-key))
  (call-next-method))

(defun sync-register-local (name collection model-type)
  "Register a collection as a DB <--> UI sync collection."
  (setf (model-type collection) model-type)
  (setf (hget (local-registry *sync*) name) collection))

(defun sync-register-remote (name collection model-type)
  "Register a collection as an API <--> DB sync collection."
  (setf (model-type collection) model-type)
  (setf (hget (remote-registry *sync*) name) collection))

(defun queue-remote (table action data)
  "Queue a remote sync job."
  (let ((msg (hash ("type" table)
                   ("action" action)
                   ("data" data))))
    (enqueue msg)))

(defun queue-local (table action data)
  "Notify of a local DB change."
  (trigger (event "sync-local"
                  :data (hash ("table" table)
                              ("action" action)
                              ("data" data)))
           :dispatch (dispatch *sync*)))

(defun sync-from-db ()
  "Starts polling changes from DB -> UI (created with queue-local)"
  (unless (and (logged-in *user*) (enabled *sync*))
    (return-from sync-from-db))
  (bind "sync-local"
        (lambda (ev)
          (let* ((msg (data ev))
                 (type (hget msg "table"))
                 (tracker (hget (local-registry *sync*) type)))
            (sync-record-from-db tracker (hget msg "data") msg)))
        :name "local-sync"
        :dispatch (dispatch *sync*)))

(defun sync-to-api ()
  "Starts polling for DB -> API sync jobs (created with queue-remote)."
  (unless (and (logged-in *user*) (enabled *sync*))
    (return-from sync-to-api))
  (labels ((consumer (item)
             (handler-case
               (let* ((type (hget item "type"))
                      (tracker (hget (remote-registry *sync*) type)))
                 (when tracker
                   (vom:debug "sync: queue remote: recv: " item)
                   (sync-record-to-api tracker (hget item "data") item)))
               (t (e)
                 (vom:error "sync-to-api: sync-record: ~a" e)
                 (requeue item))))
           (poller ()
             (handler-case
               (let ((job (dequeue)))
                 (when job (consumer job)))
               (t (e)
                 (vom:error "sync-to-api: dequeue: ~a" e)))
             (when (and (enabled *sync*)
                        (logged-in *user*))
               (as:delay #'poller :time .2))))
    (poller)))

(defun sync-from-api ()
  "Polls the API for changes to the current user's profile (and syncs them to
   the local DB)."
  (unless (and (logged-in *user*) (enabled *sync*))
    (return-from sync-to-api))
  (labels ((do-sync ()
             (let ((sync-id (mget *sync* "sync_id")))
               )))))

(format t "~%~%- sync model: FINISH ME!~%~%")

(defgeneric sync-record-from-db (collection data msg)
  (:documentation
    "Responsible for making sure a record of a specific type is posted to the
     UI."))

(defgeneric sync-record-to-api (collection data job)
  (:documentation
    "Responsible for making sure a record of a specific type is posted to the
     API."))

(defmethod sync-record-from-db ((collection sync-collection) data msg)
  (when (should-ignore-sync (hget msg "sync_id") :local)
    (return-from sync-record-from-db))
  (let ((model (mfind collection (hget data "id"))))
    (when (and (not model) (hget data "cid"))
      (setf model (mfind collection (hget data "cid")))
      (when model
        (mset model (hash ("id" (hget data "id"))))))
    ))

(defmethod sync-record-to-api ((collection sync-collection) data job)
  )

(defun start-sync ()
  "Start syncing data to/from the API."
  (setf (enabled *sync*) t)
  (sync-from-db)
  (sync-from-api)
  (sync-to-api))

(defun stop-sync ()
  "Signal sync system to stop."
  (setf (enabled *sync*) nil))

(defun sync-process-data (data)
  "Pre-process data we get from various sources to normalize it and/or add/infer
   useful bits of information."
  data)


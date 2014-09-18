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

(defun sync-to-api ()
  "Starts polling for DB -> API sync jobs (created with queue-remote)."
  (unless (and (logged-in *user*) *do-sync*)
    (return-from sync-to-api))
  (labels ((consumer (item)
             (let* ((type (hget item "type"))
                    (tracker (hget (remote-registry *sync*) type)))
               (when tracker
                 (vom:debug "sync: queue remote: recv: " item)
                 (sync-record-to-api tracker (hget item "data") item))))
           (poller ()
             (let ((job (dequeue)))
               (when job (consumer job)))
             (when (and (enabled *sync*)
                        (logged-in *user*))
               (as:delay #'poller :time .2))))
    (poller)))

(defgeneric sync-record-to-api (collection data job)
  (:documentation
    "Responsible for making sure a record of a specific type is posted to the
     API."))

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


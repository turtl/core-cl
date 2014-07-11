(in-package :turtl-core)

(deftobject sync "kv" ("id" "sync_id"))

(defmethod minit ((model sync-db))
  (setf (mid model) "sync-id")
  (setf (key model) (random-key))
  (call-next-method))

(defun sync-process-data (data)
  data)


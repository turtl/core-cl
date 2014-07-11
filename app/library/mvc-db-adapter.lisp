(in-package :turtl-core)

(defclass db-model (protected)
  ((table :accessor table :initform "my-model"))
  (:documentation
    "Describes a model that syncs with the local DB using some basic commands."))

(defamethod msync (future) ((method keyword) (model db-model) &key)
  (alet* ((id (mid model)))
    (when (and (eq method :delete)
               (not id))
      (finish future nil)
      (return-from msync future))
    (alet* ((table (table model))
            (data (when (find method '(:save :insert :update))
                    (mserialize model))))
      (future-handler-case
        (alet ((res (case method
                      (:get (db-get table id))
                      (:insert (db-insert table data))
                      (:update (db-update table data))
                      (:save (db-save table data))
                      (:delete (db-delete table id))
                      (t (error (format nil "bad method passed to msync: ~a" method))))))
          (finish future res))
        (t (e)
          (signal-error future e))))))

(defamethod msave (future) ((model db-model))
  (finish future (msync :save model)))

(defamethod mdestroy (future) ((model db-model))
  (finish future (msync :delete model)))

(defamethod mfetch (future) ((model db-model))
  (alet* ((data (msync :get model))
          (nil (mset model data)))
    (finish future model)))


;;; Mimicks the ProtectedModel in Turtl's JS app...provides tie-ins to standard
;;; encryption/decryption when serializing/deserializing models.

(in-package :turtl-core)

(define-condition protected-error (turtl-error) ()
  (:documentation "General protected model error"))

(define-condition protected-key-missing-error (protected-error) ()
  (:documentation "Fired when (de)serializing a model that doesn't have a key."))

(defclass protected (model)
  ((key :accessor key :initform nil)
   (body-key :accessor body-key :initform "body")
   (public-fields :accessor public-fields :initform '("id"))
   (private-fields :accessor private-fields :initform nil)
   (raw-data :accessor raw-data :initform nil)
   (cached-serialization :accessor cached-serialization :initform nil))
  (:documentation
    "Wraps an MVC model and provides easy tie-ins to encryption/decryption via
     the model's serialization methods."))

(defgeneric mdeserialize (model body data)
  (:documentation
    "Deserializes a model from a hash object (async, returns a future)."))

(defgeneric process-body (model data)
  (:documentation
    "Processes a model's data's body key, turning an encrypted block into mset-
     able data (a hash, for example)."))

(defgeneric find-key (model keydata)
  (:documentation
    "Find the appropriate key for this model."))

(defgeneric ensure-key-exists (model data)
  (:documentation
    "Make this the given model has a key avialable."))

(defun format-data (data)
  "Format data before passing it into crypto functions. Detects the version 0
   serialization format and acts accordingly."
  (if (cl-ppcre:scan ":i[0-9a-f]{32}$" data)
      (babel:string-to-octets data)
      (from-base64 data)))

(defmethod yason:encode ((model protected) &optional (stream *standard-output*))
  ;; cached-serialization holds the result of our last serialization
  (yason:encode (cached-serialization model) stream))

(defmethod mdeserialize ((model protected) body data)
  (declare (ignorable data))
  (let ((future (make-future))
        (raw (format-data body))
        (id (or (mid model)
                (gethash "id" data)))
        (key (key model)))
    (unless key
      (error 'protected-key-missing-error
             :msg (format nil "protected: deserialize: missing model key: ~a" id)))
    ;; queue the decryption to happen in the background, finishing our returned
    ;; future once done. if we happen to catch any errors, signal them on the
    ;; future.
    (vom:debug1 "protected: deserialize: ~a" id)
    (alet ((decrypted (work (decrypt key raw))))
      (if (typep decrypted 'error)
          (signal-error future decrypted)
          (finish future decrypted)))
    future))

(defmethod mserialize ((model protected) &key &allow-other-keys)
  (let ((future (make-future))
        (data (call-next-method))
        (public-fields (public-fields model))
        (private-fields (private-fields model))
        (public (make-hash-table :test #'equal))
        (private (make-hash-table :test #'equal))
        (key (key model)))
    (loop for k being the hash-keys of data
          for v being the hash-values of data do
      (cond ((find k public-fields :test 'string=)
             (setf (gethash k public) v))
            ((find k private-fields :test 'string=)
             (setf (gethash k private) v))))
    (when (raw-data model)
      (setf (gethash (body-key model) public) (mget model (body-key model)))
      (return-from mserialize public))
    (unless key
      (error 'protected-key-missing-error
             :msg (format nil "protected: serialize: missing model key: ~a" (mid model))))
    (vom:debug1 "protected: serialize: ~a bytes" (mid model))
    (alet* ((json (with-output-to-string (s) (yason:encode private s)))
            (enc (work (encrypt key (babel:string-to-octets json))))
            (base64 (to-base64 enc)))
      (setf (gethash (body-key model) public) base64)
      (setf (cached-serialization model) public)
      (finish future public))
    future))

(defmethod mclear ((model protected))
  ;; make sure we wipe out any cached data this model has
  (setf (cached-serialization model) nil)
  (call-next-method))

(defmethod find-key ((model protected) keydata)
  )

(defmethod ensure-key-exists ((model protected) data)
  (let ((key (key model)))
    (when key (return-from ensure-key-exists key))
    (setf key (find-key model (gethash "keys" data)))
    (when key
      (setf (key model) key)
      key)))

(defmethod process-body ((model protected) data)
  "Process a model's body key, deserializing it into usable data."
  (vom:debug2 "protected: process-body (has body: ~a): ~a" (nth-value 1 (gethash (body-key model) data)) data)
  (let ((future (make-future))
        (body (gethash (body-key model) data)))
    (when body
      (unless (ensure-key-exists model data)
        (return-from process-body))
      (when (stringp body)
        (future-handler-case
          (alet* ((deserialized (mdeserialize model body data))
                  (object (yason:parse (babel:octets-to-string deserialized))))
            (mset model object)
            (finish future object model))
          (t (e) (signal-error future e))))
      future)))

(defmethod mset ((model protected) data)
  (if (raw-data model)
      (call-next-method)
      (let* ((body-key (body-key model))
             (data (if (hash-table-p data)
                       ;; already a hash table
                       data
                       ;; convert from plist to hash
                       (let ((hash (make-hash-table :test #'equal)))
                         (loop for (k v) on data by #'cddr do
                           (setf (gethash k hash) v))
                         hash)))
             (body (gethash body-key data)))
        ;; make sure the default mset doesn't bother with the body
        (remhash body-key data)
        (call-next-method)
        (when body
          (setf (gethash body-key data) body)
          (process-body model data)))))


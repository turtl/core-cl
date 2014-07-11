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

(defgeneric find-key (model keydata &optional search)
  (:documentation
    "Find the appropriate key for this model."))

(defgeneric ensure-key-exists (model data)
  (:documentation
    "Make this the given model has a key avialable."))

(defgeneric madd-async (collection item)
  (:documentation "Asynchronously add an item to a collection."))

(defgeneric mreset-async (collection data &key append)
  (:documentation
    "Instantiate a number of models into the collection (async)."))

(defun format-data (data)
  "Format data before passing it into crypto functions. Detects the version 0
   serialization format and acts accordingly."
  (if (cl-ppcre:scan ":i[0-9a-f]{32}$" data)
      (babel:string-to-octets data)
      (from-base64 data)))

(defmethod yason:encode ((model protected) &optional (stream *standard-output*))
  ;; cached-serialization holds the result of our last serialization
  (yason:encode (cached-serialization model) stream))

(defamethod mdeserialize (future) ((model protected) body data)
  (declare (ignorable data))
  (let ((raw (format-data body))
        (id (or (mid model)
                (gethash "id" data)))
        (key (key model)))
    (unless key
      (error 'protected-key-missing-error
             :msg (format nil "protected: deserialize: missing model key (~a): ~a" (type-of model) id)))
    ;; queue the decryption to happen in the background, finishing our returned
    ;; future once done. if we happen to catch any errors, signal them on the
    ;; future.
    (vom:debug1 "protected: deserialize: ~a" id)
    (alet ((decrypted (work (decrypt key raw))))
      (if (typep decrypted 'error)
          (signal-error future decrypted)
          (finish future decrypted)))))

(defamethod mserialize (future) ((model protected) &key &allow-other-keys)
  (let ((data (call-next-method))
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
      (finish future public)
      (return-from mserialize future))
    (unless key
      (error 'protected-key-missing-error
             :msg (format nil "protected: serialize: missing model key (~a): ~a" (type-of model) (mid model))))
    (vom:debug1 "protected: serialize: ~a" (mid model))
    (alet* ((json (with-output-to-string (s) (yason:encode private s)))
            (enc (work (encrypt key (babel:string-to-octets json))))
            (base64 (to-base64 enc)))
      (setf (gethash (body-key model) public) base64)
      (setf (cached-serialization model) public)
      (finish future public))))

(defmethod mclear ((model protected))
  ;; make sure we wipe out any cached data this model has
  (setf (cached-serialization model) nil)
  (call-next-method))

(defun decrypt-key (model-id decrypting-key encrypted-key)
  "Convenience function to decrypt a key."
  (let ((raw (format-data encrypted-key)))
    (handler-case
      (decrypt decrypting-key raw)
      (t (e)
        (vom:warn "decrypt-key: fail: (~a): ~a" model-id e)
        nil))))

(defmethod find-key ((model protected) keys &optional search)
  "Given a set of keys for an object and a search pattern, find the matching
	 key and decrypt it using one of the decrypting keys provided by the
	 search object. This in turn allows the object to be decrypted.
	 
	 Keys are in the format
	   {b: <id>, k: <encrypted key>}
	   {u: <id>, k: <encrypted key>}
	 `b` `u` and `p` correspond to board, user, persona restecpfully.
	 
	 Search is in the format:
	 {
	   u: {id: <user id>, k: <user's key>}
	   b: {id: <board id>, k: <board's key>}
	   ...
	 }
	 
	 Search keys can also be arrays, if you are looking for multiple items
	 under that key:
	 {
	   u: [
	     {id: <user1 id>, k: <user1's key>},
	     {id: <user2 id>, k: <user2's key>}
	   ],
	   b: {id: <board id>, k: <board's key>}
	 }"
  (let* ((keys (clone-object keys))
         (search-user (getf search :u))
         (search-user (if search-user
                          search-user
                          (list :id (mid *user*) :k (key *user*))))
         (encrypted-key nil)
         (decrypting-key nil))
    (setf (getf search :u) search-user)
    (block have-key
      (loop for key in keys
            for enckey = (hget key '("k")) do
        ;; we only want b => id, u => id, etc here (so we remove the key which
        ;; we already saved to enckey)
        (remhash "k" key)
        (loop for type-str being the hash-keys of key
              for type = (intern (string-upcase (string type-str)) :keyword)
              for id being the hash-values of key
              for search-entries = (getf search type) do
          ;; if our search entries aren't a list, turn wrap the entry in a list
          (unless (listp (car search-entries))
            (setf search-entries (list search-entries)))
          (dolist (entry search-entries)
            (let ((entry-key (getf entry :k))
                  (entry-id (getf entry :id)))
              (when (and entry-key
                         entry-id
                         (string= id entry-id))
                (setf encrypted-key enckey)
                (setf decrypting-key entry-key)
                (return-from have-key)))))))
    (let ((key nil))
      (when (and decrypting-key encrypted-key)
        (setf key (decrypt-key (mid model) decrypting-key encrypted-key)))
      (unless key
        (setf key (find-keychain-entry *keychain* (mid model))))
      key)))

(defmethod ensure-key-exists ((model protected) data)
  (let ((key (key model)))
    (when key (return-from ensure-key-exists key))
    (setf key (find-key model (gethash "keys" data)))
    (when key
      (setf (key model) key)
      key)))

(defamethod process-body (future) ((model protected) data)
  (vom:debug2 "protected: process-body (has body: ~a): ~a" (nth-value 1 (gethash (body-key model) data)) data)
  (let ((body (gethash (body-key model) data)))
    (cond ((and body (stringp body))
            (unless (ensure-key-exists model data)
              (error 'protected-key-missing-error
                     :msg (format nil "protected: process-body: missing model key (~a): ~a" (type-of model) (mid model))))
              (future-handler-case
                (alet* ((deserialized (mdeserialize model body data))
                        (object (yason:parse (babel:octets-to-string deserialized))))
                  (mset model object)
                  (finish future object model))
                (t (e) (signal-error future e))))
          (t
           (mset model data)
           (finish future data model)))))

(defamethod mset (future) ((model protected) data)
  (cond ((raw-data model)
         (finish future (call-next-method)))
        ((null data)
         (finish future model))
        (t
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
            (cond (body
                    (setf (gethash body-key data) body)
                    (wait-for (process-body model data)
                      (finish future model)))
                  (t (finish future model)))))))

(defamethod madd-async (future) ((collection collection) item)
  (alet* ((model (create-model (model-type collection)))
          (nil (mset model item)))
    (finish future (madd collection model))))

(defamethod mreset-async (future) ((collection collection) items &key append)
  (unless append
    (mclear collection))
  (let* ((num-finished 0)
         (num-items (length items))
         (finish-fn (lambda (&rest _)
                      (declare (ignore _))
                      (incf num-finished)
                      (when (<= num-items num-finished)
                        (finish future t)))))
    (if (zerop (length items))
        (finish future t)
        (dolist (item items)
          (attach (madd-async collection item) finish-fn)))))


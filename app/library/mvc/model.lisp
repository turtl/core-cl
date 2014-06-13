;;; This file creates a class which somewhat mimicks how a Model functions in
;;; the Composer.js javascript MVC framework: it is an object that contains
;;; arbitrary data (no schema enforcements) and is able to be serialized and
;;; deserialized to JSON easily. It also provides a number of methods for
;;; accessing its data which, when changed, fire events using the model's event
;;; dispatch capabilities (provided by event-glue).

(in-package :turtl-core)

(defclass model (mvc-base)
  ((data :accessor data :initform (make-hash-table :test #'equal))
   (changed :accessor changed :initform nil)
   (collections :accessor collections :initform nil))
  (:documentation "Defines a generic model."))

(defgeneric mid (model &optional strict)
  (:documentation "Get a model's ID."))

(defgeneric mget (model field &optional default)
  (:documentation "Get an item from a model's data."))

(defgeneric mset (model data)
  (:documentation "Set data into a model."))

(defgeneric munset (model field)
  (:documentation "Unset a field in a model."))

(defgeneric mdestroy (model)
  (:documentation "Destroy a model."))

(defmethod yason:encode ((model model) &optional (stream *standard-output*))
  (yason:encode (mserialize model)))

(defun create-model (type &optional data)
  "Create a model of type type with the given data."
  (let ((model (make-instance type)))
    (minit model)
    (when data
      (mset model data))
    model))

(defmethod mserialize ((model model) &key &allow-other-keys)
  (data model))

(defmethod mid ((model model) &optional strict)
  (let ((id (gethash "id" (data model))))
    (cond (id id)
          (strict nil)
          (t (cid model)))))

(defmethod mget ((model model) field &optional default)
  (multiple-value-bind (value exists)
      (gethash field (data model))
    (if exists
        value
        default)))

(defmethod mset ((model model) data)
  (let ((model-data (data model))
        (changed nil))
    (flet ((do-set (key val)
             (let ((cur (gethash key model-data)))
               (unless (equalp cur val)
                 (setf (gethash key model-data) val)
                 (setf changed t)
                 (trigger (event (concatenate 'string "change:" key) :data val)
                          :dispatch (dispatch model))))))
      (cond ((listp data)
             (loop for (k v) on data by #'cddr do
               (do-set (string-downcase (string k)) v)))
            ((hash-table-p data)
             (loop for k being the hash-keys of data
                   for v being the hash-values of data do
               (do-set k v)))))
    (when changed
      (trigger (event "change") :dispatch (dispatch model)))
    model))

(defmethod munset ((model model) field)
  (multiple-value-bind (_ exists)
      (gethash field (data model))
    (when exists
      (remhash field (data model))
      (trigger (event (concatenate 'string "change:" field)) :dispatch (dispatch model))
      (trigger (event "change") :dispatch (dispatch model)))
    model))

(defmethod mclear ((model model))
  (unless (zerop (hash-table-count (data model)))
    (clrhash (data model))
    (trigger (event "change") :dispatch (dispatch model)))
  model)

(defmethod mdestroy ((model model))
  (mclear model)
  (trigger (event "destroy") :dispatch (dispatch model))
  (wipe :dispatch (dispatch model)))


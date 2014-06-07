;;; This file creates a class which somewhat mimicks how a Collection functions
;;; the Composer.js javascript MVC framework: it is an object that contains
;;; a number of models an provides easy ways to filter and (de)serialize those
;;; models to/from JSON.

(in-package :turtl-core)

(defclass collection (mvc-base)
  ((model-type :accessor model-type :initform 'model)
   (models :accessor models :initform nil)
   (sort-function :accessor sort-function :initform nil)))

(defgeneric msort (collection)
  (:documentation "Sort the collection's models using its sort-function."))

(defgeneric madd (collection model-or-data)
  (:documentation "Add a model to a collection."))

(defgeneric mrem (collection model)
  (:documentation "Remove a model from a collection."))

(defgeneric mclear (collection)
  (:documentation "Remove all models from a collection."))

(defgeneric mreset (collection data &key append)
  (:documentation "Reset the collection with new data (or append it)."))

(defgeneric mfind (collection id)
  (:documentation "Find a model in this collection with the given ID."))

(defun create-collection (type &optional data)
  "Create a collection of type type with the given data."
  (let* ((collection (make-instance type))
         (model-type (model-type collection)))
    (when data
      (setf (models collection) (mapcar (lambda (d)
                                          (create-model model-type d))
                                        data)))
    collection))

(defun forward-event (collection model event)
  "Forward an event from a model's dispatch to the containing collection's."
  (let ((ev (ev event)))
    (when (and (or (string= ev "add")
                   (string= ev "remove"))
               (not (find model (models collection) :test 'eq)))
      ;; firing an event on a model that isn't in this collection.
      (return-from forward-event))
    (when (string= ev "destroy")
      (mrem collection model))
    ;; forward it.
    (trigger event :dispatch (dispatch collection))))

(defmethod mserialize ((collection collection) &key &allow-other-keys)
  (mapcar 'mserialize (models collection)))

(defmethod msort ((collection collection))
  (let ((sort-function (sort-function collection)))
    (when sort-function
      ;; a bit inefficient perhaps, but premature optimization and blah blah
      (setf (models collection) (sort (models collection) sort-function)))))

(defmethod madd ((collection collection) model-or-data)
  (let ((model (if (typep model-or-data 'model)
                   model-or-data
                   (create-model (model-type collection) model-or-data))))
    (unless (find collection (collections model) :test 'eq)
      (push collection (collections model)))
    (push model (models collection))
    (msort collection)    ; brute force sort
    (bind :* (lambda (event) (forward-event collection model event))
          :name (format nil "collection:~a:listen:model:all" (cid collection))
          :dispatch (dispatch model))
    (trigger (event "add" :data model) :dispatch (dispatch collection))))

(defmethod mrem ((collection collection) (model model))
  (let ((models (find-if (lambda (m) (eq m model)) (models collection))))
    (when models
      ;; unreference the model from the collection
      (setf (collections model) (remove collection (collections model) :test 'eq))
      (setf (models collection) (remove model (models collection) :test 'eq))
      (unbind :* (format nil "collection:~a:listen:model:all" (cid collection))
              :dispatch (dispatch model))
      (trigger (event "remove") :dispatch (dispatch collection)))))

(defmethod mclear ((collection collection))
  (when (models collection)
    (dolist (model (models collection))
      (mrem collection model))
    (trigger (event "clear") :dispatch (dispatch collection))))

(defmethod mreset ((collection collection) data &key append)
  (unless append
    (mclear collection))
  (dolist (item data)
    (madd collection item))
  (trigger (event "reset") :dispatch (dispatch collection)))

(defmethod mfind ((collection collection) id)
  (find-if (lambda (m) (equal (mid m) id))
           (models collection)))


(in-package :turtl-core)

(define-condition db-error (turtl-error) ()
  (:documentation "General database error"))

(define-condition db-already-open (db-error) ()
  (:documentation "Thrown when a DB is being opened while another is open."))

(define-condition db-not-open (db-error) ()
  (:documentation "Thrown when operating on a closed database."))

(defparameter *db* nil
  "Holds the current database connection.")

(defparameter *schema* nil
  "Holds the local DB schema. Added to incrementally by define-db-object.")

(defclass db-object (protected) ()
  (:documentation "Base Turtl DB object class."))

(defgeneric db-table (object)
  (:documentation "Grab an object's DB table name."))

(defgeneric db-fields (object)
  (:documentation "Grab all of an object's field definitions."))

(defgeneric db-get-by-id (object)
  (:documentation "Grab a Turtl object by its ID."))

(defgeneric db-insert (object)
  (:documentation "Insert a new Turtl DB object into its table."))

(defgeneric db-update (object)
  (:documentation "Update a Turtl object."))

(defgeneric db-destroy (object)
  (:documentation "Remove a Turtl DB object."))

(defgeneric make-object (type data &key strip)
  (:documentation
    "Makes creating serializable DB objects from hash tables easy."))

(defgeneric strip-object (object)
  (:documentation
    "Strip any non-schema-codified information from a copy of a model's data."))

(defmethod strip-model ((model db-object))
  (let ((fields (fields model))
        (data (hash-copy (data model))))
    (loop for k being the hash-keys of data do
      (let ((field (assoc k fields :test #'equal)))
        (unless field
          (remhash k data))))
    (setf (data model) data)
    model))

(defun transform-type-to-sql (type)
  "Given a type (string or keyword) transofrm it into its SQL equivalent."
  (let ((type (if (stringp type)
                  (intern (string-downcase (string type)) :keyword)
                  type)))
    (case type
      (:pkey "nvarchar(32) primary key")
      (:id "nvarchar(32)")
      (:integer "integer")
      (:string "nvarchar(255)")
      ;; 0 = false, 1 = true
      (:bool "tinyint")
      ;; serialize objects to JSON and store as text
      (:object "text")
      (:text "text")
      (:binary "blob"))))

(defun generate-schema-entry (name fields indexes)
  "Create a schema entry for a given set of information."
  (list name
        (loop for f in fields
              ;; if it's public, we include it in he schema, otherwise it's just
              ;; encrypted in the body field
              when (getf (cdr f) :public)
              collect (list (car f) (transform-type-to-sql (getf (cdr f) :type))))
        indexes))

(defmacro define-db-object (name fields &optional meta)
  "Define a database object and also provide a set of functions for interacting
   with that object in the local DB."
  (let ((table-name (string-downcase (string name))))
    `(progn
       ;; create the schema entry for this object
       (push (generate-schema-entry ',table-name ',fields ',(getf meta :indexes)) *schema*)

       ;; create an honest to god CLOS class for this object
       (defclass ,name (db-object) ())

       ;; create a method that returns this object's DB table.
       (defmethod table ((obj ,name))
         ,table-name)

       ;; create a method that returns this object's fields.
       (defmethod fields ((obj ,name))
         ',fields))))

(defun check-db-open ()
  "Whines if the DB isn't open."
  (unless *db*
    (error 'db-not-open :msg "There is no currently open database.")))

(defun read-schema ()
  "Talked to Drew about reading the current DB schema. Sounds good."
  (check-db-open)
  (flet ((get-table-info (table-name)
           (sqlite:execute-to-list *db* (format nil "PRAGMA table_info(~a)" table-name)))
         (get-table-indexes (table-name)
           (sqlite:execute-to-list *db* (format nil "PRAGMA index_list(~a)" table-name)))
         (get-db-tables ()
           (sqlite:execute-to-list *db* "SELECT * FROM sqlite_master WHERE type='table'")))
    (let ((info nil))
      (dolist (table (get-db-tables))
        (let ((table-name (cadr table)))
          (push (cons table-name (list :schema (get-table-info table-name)
                                       :indexes (get-table-indexes table-name))) info)))
      info)))

(defun db-open (user-id)
  "Open a Turtl user database."
  (when *db*
    (error 'db-already-open :msg "A database is already open. Close it first."))
  (ensure-directories-exist #P"~/.turtl/")
  (let ((db-name (format nil "~~/.turtl/user-~a.db" user-id)))
    (vom:info "opening user db (~a)" db-name)
    (setf *db* (sqlite:connect db-name))))

(defun db-close ()
  "Close a Turtl user database."
  (when *db*
    (vom:info "closing user db")
    (sqlite:disconnect *db*)
    (setf *db* nil)))


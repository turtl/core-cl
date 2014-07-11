(in-package :turtl-core)

(defvar *next-mvc-cid*
  (let ((cid 0))
    (lambda () (format nil "z.~a000.c~a" (timestamp) (incf cid))))
  "Function that generates TMP ids")

(defclass mvc-base ()
  ((cid :accessor mcid :initform (funcall *next-mvc-cid*))
   (dispatch :accessor dispatch :initform (make-dispatch)))
  (:documentation "The base class for all MVC objects."))

(defgeneric minit (mvc-object)
  (:documentation
    "Generic initialization function for MVC objects."))

(defgeneric mserialize (mvc-object &key &allow-other-keys)
  (:documentation
    "Serialize an MVC object (model/collection) into its basic types (hashes,
     lists, atoms)"))

(defgeneric mclear (mvc-object)
  (:documentation "Clear out a model/collection's data."))

(defmethod minit ((object mvc-base)) 'override-me)


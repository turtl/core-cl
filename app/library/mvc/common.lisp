(in-package :turtl-core)

(defvar *next-mvc-cid*
  (let ((cid 0))
    (lambda () (format nil "z.~a000.c~a" (timestamp) (incf cid))))
  "Function that generates TMP ids")

(defclass mvc-base ()
  ((cid :accessor cid :initform (funcall *next-mvc-cid*))
   (dispatch :accessor dispatch :initform (make-dispatch)))
  (:documentation "The base class for all MVC objects."))

(defgeneric mserialize (model &key &allow-other-keys)
  (:documentation
    "Serialize an MVC object (model/collection) into its basic types (hashes,
     lists, atoms)"))




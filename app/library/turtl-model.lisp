(in-package :turtl-core)

(defmacro deftobject (classname url/table &optional (public-fields '("id")) private-fields extra-fields)
  "Define three classes: a generic protected model, an API-backed model, and a
   local DB-backed model as such:

     (deftobject user \"user\")

   Creates the classes `user`, `user-db`, and `user-api`, the last two can be
   synced via msave/mdestroy/mfetch."
  (let ((name-db (intern (format nil "~a-DB" classname)))
        (name-api (intern (format nil "~a-API" classname))))
    `(progn
       (defclass ,classname (protected)
         ,(append `((public-fields :accessor public-fields :initform ',public-fields)
                    (private-fields :accessor private-fields :initform ',private-fields))
                  extra-fields))
       (defclass ,name-db (db-model ,classname)
         ((table :accessor table :initform ,url/table)))
       (defclass ,name-api (api-model ,classname)
         ((url :accessor url :initform ,(format nil "/~a" url/table))
          (raw-data :accessor raw-data :initform t))))))


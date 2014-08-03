(in-package :turtl-core)

(define-condition api-error (turtl-error)
  ((status :initarg :status :accessor api-error-status :initform nil))
  (:documentation "Describes an API error."))

(defclass api-data ()
  ((url :accessor api-url :initarg :url :initform nil)
   (auth :accessor api-auth :initarg :auth :initform nil))
  (:documentation "Holds API state."))
   
(defvar *api-data* (make-instance 'api-data :url *api-url*)
  "Global API data container.")

(defun api-auth-needed (method resource)
  "Test if auth is needed for the given call."
  (let ((auth-default t))
    (block auth-main-jump
      (dolist (entry *api-auth*)
        (when (and (eq method (car entry))
                   (eq resource (cdr entry)))
          (setf auth-default nil)
          (return-from auth-main-jump))))
    auth-default))

(defafun api (future) (method resource &key data headers progress-fn upload-progress-fn)
  "Make an API call, finishes a future with the results."
  (declare (ignore progress-fn upload-progress-fn))
  (let* ((need-auth (api-auth-needed method resource))
         (url (concatenate 'string (api-url *api-data*) resource))
         (client-header `("X-Turtl-Client" . ,(format nil "~a-~a" *client* *version*)))
         (data (cond ((hash-table-p data)
                      (hash-to-alist data))
                     ((and (listp data)
                           (typep (car data) 'keyword))
                      (let ((res nil))
                        (loop for (k v) on data by #'cddr do
                          (push (cons (string-downcase (string k)) v) res))
                        res))
                     (t
                      data))))
    (push client-header headers)
    (when need-auth
      (let* ((auth-str (format nil "user:~a" (api-auth *api-data*)))
             (encoded (cl-base64:string-to-base64-string auth-str))
             (auth-header `("Authorization" . ,(format nil "Basic ~a" encoded))))
        (push auth-header headers)))
    (vom:debug "api: ~s ~a (~a)" method resource url)
    (multiple-future-bind (res status headers)
        (drakma-async:http-request
          url
          :method method
          :parameters data
          :additional-headers headers)
      (vom:debug "api: res: (~a) ~s ~a" status method resource)
      (let* ((res-str (if (stringp res)
                          res
                          (babel:octets-to-string res :encoding :utf-8)))
             (obj (ignore-errors (yason:parse res-str))))
        (cond ((and (<= 200 status 299)
                    obj)
               (finish future obj status headers))
              (t
               (signal-error future (make-instance 'api-error :status status :msg res-str))))))))

(defun set-api-auth (user-auth)
  "Register a user's auth with the API."
  (setf (api-auth *api-data*) user-auth))

(defun clear-api-auth ()
  "Clear out user auth"
  (setf (api-auth *api-data*) nil))


(in-package :turtl-core)

(define-condition turtl-error (simple-error)
  ((msg :initarg :msg :accessor turtl-error-msg :initform nil))
  (:report (lambda (c s) (format s "turtl error: ~a" (turtl-error-msg c))))
  (:documentation "Base Turtl error condition."))

(define-condition turtl-error-wrapper (simple-error)
  ((error :accessor error-error :initarg :error :initform nil)
   (function :accessor error-function :initarg :function :initform nil))
  (:report (lambda (c s) (format s "Turtl wrapped error: ~a: ~a" (error-function c) (error-error c))))
  (:documentation "Used to wrap caught errors and provide more debugging information about them."))

(defun timestamp ()
  "Get unix timestamp."
  (let ((diff 2208988800))  ; (encode-universal-time 0 0 0 1 1 1970 0)
    (- (get-universal-time) diff)))

(defun to-keyword (str)
  "Convert a string into a keyword."
  (intern (string-upcase str) :keyword))

(defmacro def-async-function (type name (future-var &key (forward-errors t)) args &body body)
  "Define an asynchronous function with a returned future that will be finished
   when the function completes. Also has the option to forward all async errors
   encountered during excution (in this lexical scope) to the returned future."
  (let* ((docstring (car body))
         (declare nil))
    (when (stringp docstring)
      (setf body (cdr body)))
    (when (and (listp (car body))
               (eq (caar body) 'declare))
      (setf declare (car body))
      (setf body (cdr body)))
    `(,type ,name ,args
       ,(if (stringp docstring) docstring "")
       ,declare
       (let ((,future-var (make-future)))
         ,(if forward-errors
              `(future-handler-case
                 (progn ,@body)
                 (t (e)
                   ;; wrap the caught error in the error wrapper, which when
                   ;; printed out gives us the name of the function the error
                   ;; occurred in. makes debugging, oh, about 6000x easier.
                   ;; also, log the error
                   (vom:error "wrapping (~a): ~a" ',name e)
                   (signal-error ,future-var
                                 (make-instance 'turtl-error-wrapper
                                                :error e
                                                :function ',name))))
              `(progn ,@body))
         ,future-var))))

(defmacro defafun (name (future-var &key (forward-errors t)) args &body body)
  `(def-async-function defun ,name (,future-var :forward-errors ,forward-errors)
                       ,args ,@body))

(defmacro defamethod (name (future-var &key (forward-errors t)) args &body body)
  `(def-async-function defmethod ,name (,future-var :forward-errors ,forward-errors)
                       ,args ,@body))

(defun jprint (db-result &key (pretty t) (stream *standard-output*))
  "Pretty printer for JSON (mainly for database results)."
  (let ((res (with-output-to-string (s)
               (yason:encode db-result (yason:make-json-output-stream s :indent (if pretty 2 nil))))))
    (format stream res)))

(defmacro with-test (&body body)
  "Makes testing async functions easier by abstracting an extremely common
   pattern."
  `(as:with-event-loop (:catch-app-errors t)
     (future-handler-case
       (progn ,@body)
       (t (e) (format t "err: ~a~%" e)))))

(defun get-num-cores ()
  "Gets the number of CPUs/cores on the current device."
  4)

(defun hash-to-alist (hash-table)
  "Recursive conversion from a hash table to an alist."
  (loop for k being the hash-keys of hash-table
        for v being the hash-values of hash-table
        collect (cons k (if (hash-table-p v)
                            (hash-to-alist v)
                            v))))

(defun clone-object (object)
  "Perform a deep clone of an object. Probably not the most efficient way, but
   most certainly the easiest on my fingers."
  (yason:parse
    (with-output-to-string (s)
      (yason:encode object s))))


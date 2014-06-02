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

(defmacro defafun (name (future-var &key (forward-errors t)) args &body body)
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
    `(defun ,name ,args
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

(defun jprint (db-result &key (stream *standard-output*))
  "Pretty printer for JSON (mainly for database results)."
  (yason:encode db-result (yason:make-json-output-stream stream :indent 2)))

(defmacro with-test (&body body)
  "Makes testing async functions easier by abstracting an extremely common
   pattern."
  `(as:with-event-loop (:catch-app-errors t)
     (future-handler-case
       (progn ,@body)
       (t (e) (format t "err: ~a~%" e)))))

(defun to-string (bytes)
  "Converts bytes to a string (no encoding or anything, just raw byte string)."
  (with-output-to-string (s)
    (loop for x across bytes do
      (format s "~c" (code-char x)))))


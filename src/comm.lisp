(in-package :turtl-core)

(defvar *events* nil
  "Holds all pending events.")
(defvar *events-lock* (bt:make-lock "event-lock"))

(defparameter *handlers* (make-hash-table :test #'eq)
  "Holds all event handlers.")

(defun normalize-event-name (event)
  "Turn an event name into a keyword."
  (cond ((keywordp event)
         event)
        ((symbolp event)
         (intern (string event) :keyword))
        ((stringp event)
         (intern (string-upcase (cl-ppcre:regex-replace-all "_" event "-")) :keyword))
        (t
         (error (format nil "Bad event name: ~a" event)))))

(defun dispatch-event (name args)
  "Blast out an event triggered to any listening handlers."
  (let ((handlers (gethash (normalize-event-name name) *handlers*)))
    (dolist (handler handlers)
      (apply handler args))))

(defun event-handler ()
  "Do a threadsafe copy of all pending events, wipe pending events, and process
   what we copied event-by-event."
  (declare (optimize (speed 3) (safety 1)))
  (format t "hai~%")
  (let ((events nil))
    (bt:with-lock-held (*events-lock*)
      (when *events*
        (setf events (copy-tree *events*)
              *events* nil)))
    (dolist (event (nreverse events))
      (apply 'trigger (append (list (getf event :name))
                              (getf event :args))))))

(defun trigger (event &rest args)
  "Trigger a local event."
  (dispatch-event event args))

(defun trigger-remote (event &rest args)
  "Trigger an event in the remote process (ie, non-lisp). Used to communicate
   between lisp and whatever GUI system is wrapping it (Webkit, Android, iOS)."
  ;; TODO will probably depend on GUI thread and what's running there.
  )

(defun bind (event function)
  "Locally bind a function to an event. The function will be called whenever the
   event is triggered."
  (push function (gethash (normalize-event-name event) *handlers*)))

;; TODO
;(defun bind-remote ())


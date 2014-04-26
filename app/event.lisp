(in-package :turtl-core)

(defvar *events* nil
  "Holds all pending events.")
(defvar *events-lock* (bt:make-lock "event-lock")
  "Locks the pending events container.")

(defparameter *handlers* (make-hash-table :test #'equal)
  "Holds all event handlers.")

(defclass event ()
  ((id :accessor id :initarg :id :initform nil
     :documentation "Holds the event's UUID.")
   (ev :accessor ev :initarg :ev :initform nil
     :documentation "The event's name.")
   (data :accessor data :initarg :data :initform nil
     :documentation "Arbitrary data attached to the event. Usually a set of args.")
   (meta :accessor meta :initarg :meta :initform (make-hash-table :test #'equal)
     :documentation "Any top-level meta associated with the event, used to describe it."))
  (:documentation
    "Describes an event and any data it holds."))

(defun make-event (name &key data meta uuid)
  "Easy wrapper for creating a standard event object. Meta is a plist of
   optional data to set (top-level) into the event object. Also allows
   specifying the event's UUID."
  (let ((event (make-instance 'event :id uuid :ev name :data data)))
    (when meta
      (loop for (k v) on meta by #'cddr do
        (setf (gethash (string-downcase (string k)) (meta event)) v)))
    event))

(defmethod yason:encode ((event event) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "ev" (ev event))
      (when (id event)
        (yason:encode-object-element "id" (id event)))
      (when (data event)
        (yason:encode-object-element "data" (data event)))
      (let ((meta (meta event)))
        (loop for k being the hash-keys of meta
              for v being the hash-values of meta do
          (yason:encode-object-element k v))))))

(defun event-from-hash (hash)
  "Converts a hash table (presumably from a JSON decode) into an event object."
  (let ((event (make-instance 'event)))
    (loop for k being the hash-keys of hash
          for v being the hash-values of hash do
      (cond ((string= k "id")
             (setf (id event) v))
            ((string= k "ev")
             (setf (ev event) v))
            ((string= k "data")
             (setf (data event) v))
            (t
             (setf (gethash k (meta event)) v))))
    event))

(defun dispatch-event (event)
  "Blast out an event, triggering any corresponding listening handlers."
  (let* ((name (ev event))
         (handlers (gethash name *handlers*))
         ;; if they want to send a response
         (response-fn (lambda (revent)
                        ;; overwrite the response event's ID with the original
                        (setf (id revent) (id event))
                        (trigger-remote revent))))
    (when handlers
      (dolist (handler handlers)
        (funcall handler event response-fn)))))

(defun trigger (event)
  "Trigger a local event."
  (dispatch-event event))

(defun bind (event function)
  "Locally bind a function to an event. The function will be called whenever the
   event is triggered."
  (push function (gethash event *handlers*)))

(defmacro with-bind ((event event-bind &optional (send-response-bind (gensym "send-response-bind"))) &body body)
  "Nicer syntax for bind function."
  (let ((response-fn-bind (gensym "response-fn-bind")))
    `(bind ,event
       (lambda (,event-bind ,response-fn-bind)
         (declare (ignorable ,event-bind))
         (flet ((,send-response-bind (event)
                   (funcall ,response-fn-bind event)))
           ,@body)))))

(defun event-handler ()
  "Do a threadsafe copy of all pending events, wipe pending events, and process
   what we copied event-by-event."
  (declare (optimize (speed 3) (safety 1)))
  (let ((events nil))
    (bt:with-lock-held (*events-lock*)
      (when *events*
        (setf events (copy-list *events*)
              *events* nil)))
    (when events
      (dolist (event (nreverse events))
        (trigger event)))))


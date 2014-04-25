(in-package :turtl-core)

(defvar *events* nil
  "Holds all pending events.")
(defvar *events-lock* (bt:make-lock "event-lock")
  "Locks the pending events container.")

(defparameter *handlers* (make-hash-table :test #'equal)
  "Holds all event handlers.")

(defclass event ()
  ((ev :accessor ev :initarg :ev :initform nil
     :documentation "The event's name.")
   (data :accessor data :initarg :data :initform nil
     :documentation "Arbitrary data attached to the event. Usually a set of args.")
   (meta :accessor meta :initarg :meta :initform (make-hash-table :test #'equal)
     :documentation "Any top-level meta associated with the event, used to describe it."))
  (:documentation
    "Describes an event and any data it holds."))

(defun make-event (name &key data meta)
  "Easy wrapper for creating a standard event object. Meta is a plist of
   optional data to set (top-level) into the event object."
  (let ((event (make-instance 'event :ev name :data data)))
    (when meta
      (loop for (k v) on meta by #'cddr do
        (setf (gethash (string-downcase (string k)) (meta event)) v)))
    event))

(defmethod yason:encode ((event event) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "ev" (ev event))
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
      (cond ((string= k "ev")
             (setf (ev event) v))
            ((string= k "data")
             (setf (data event) v))
            (t
             (setf (gethash k (meta event)) v))))
    event))

(defun dispatch-event (event)
  "Blast out an event triggered to any listening handlers."
  (let* ((name (ev event))
         (handlers (gethash name *handlers*)))
    (when handlers
      (dolist (handler handlers)
        (funcall handler event)))))

(defun trigger (event)
  "Trigger a local event."
  (dispatch-event event))

(defun bind (event function)
  "Locally bind a function to an event. The function will be called whenever the
   event is triggered."
  (push function (gethash event *handlers*)))

(defmacro with-bind ((event event-bind) &body body)
  "Nicer syntax for bind function."
  `(bind ,event
     (lambda (,event-bind)
       (declare (ignorable ,event-bind))
       ,@body)))

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


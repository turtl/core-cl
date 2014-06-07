(in-package :turtl-core)

(defclass turtl-dispatch (dispatch)
  ((queue :accessor dispatch-queue :initform nil)
   (queue-lock :accessor dispatch-queue-lock :initform (bt:make-lock)))
  (:documentation
    "Extends event-glue:dispatch to add a synchronized event queue."))

(defvar *remote-dispatch* (make-instance 'turtl-dispatch)
  "Define a dispatcher for handling remote events. Generally, these will be
   forwarded to the main event-glue:*dispatch* dispatch object.")

(defvar *remote-forward-fn*
  (lambda (event)
    (vom:debug1 "forwarding from remote to global: ~a" event)
    ;; mark the event as a "remote" event before sending it off to *dispatch*
    (setf (gethash "remote" (meta event)) t)
    *dispatch*) 
  "Function responsible for forwarding events from the remote dispatch to the
   global dispatch.")

(defun setup-remote-forwarding ()
  "Send all events from our remote dispatch to the default (global) one."
  (unless (forwardsp *remote-dispatch* *remote-forward-fn*)
    (forward *remote-dispatch* *remote-forward-fn*)))

(defclass turtl-event (event-glue:event)
  ((id :accessor id :initarg :id :initform nil
     :documentation "Holds the event's UUID."))
  (:documentation
    "Extends event-glue:event to give it an ID, used for tracking event
     responses."))

(defun event (name &key data meta uuid)
  "Easy wrapper for creating a standard event object. Meta is a plist of
   optional data to set (top-level) into the event object. Also allows
   specifying the event's UUID."
  (let ((event (event-glue:event name :data data :meta meta :type 'turtl-event)))
    (when uuid
      (setf (id event) uuid))
    event))

(defmethod yason:encode ((event turtl-event) &optional (stream *standard-output*))
  "JSON encode a turtl-core event."
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
  (let ((event (make-instance 'turtl-event)))
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

(defun trigger (event &key (dispatch *dispatch*))
  "Trigger a local event."
  (vom:debug1 "trigger: ~a" (ev event))
  (event-glue:trigger event :dispatch dispatch))

(defmacro with-bind ((event event-bind &optional (send-response-bind (gensym "send-response-bind"))) &body body)
  "Nicer syntax for bind function."
  (let ((event-sym (gensym "bind")))
    `(let ((,event-sym ,event))
       (bind ,event-sym
         (lambda (,event-bind)
           (vom:debug "event fired: ~a" (ev ,event-bind))
           (flet ((,send-response-bind (revent)
                    ;; overwrite the response event's ID with the original
                    (setf (id revent) (id ,event-bind))
                    (trigger-remote revent)))
             (future-handler-case
               (progn ,@body)
               (t (e)
                 (vom:error "with-bind: ~a: ~a" ',event-sym e)
                 (,send-response-bind (event "error" :data (format nil "~a" e)))))))))))

(defun push-event (event &key (dispatch *remote-dispatch*))
  "Safely push an event into the queue (from any thread). Note that we use our
   remote dispatch by default here."
  (declare (optimize (speed 3) (safety 1))
           (type turtl-event event)
           (type turtl-dispatch dispatch))
  (bt:with-lock-held ((dispatch-queue-lock dispatch))
    (push event (dispatch-queue dispatch)))
  (let ((cl-async-base:*event-base* *turtl-event-loop*))
    (as:delay (lambda () (event-handler)) :time 0))
  nil)

(defun event-handler (&key (dispatch *remote-dispatch*))
  "Do a threadsafe copy of all pending events, wipe pending events, and process
   what we copied event-by-event. Note that by default we use the remote
   dispatch object."
  (declare (optimize (speed 3) (safety 1))
           (type turtl-dispatch dispatch))
  (vom:debug4 "running event-handler")
  (let ((events nil))
    (bt:with-lock-held ((dispatch-queue-lock dispatch))
      (when (dispatch-queue dispatch)
        (setf events (copy-list (dispatch-queue dispatch)))
        (setf (dispatch-queue dispatch) nil)))
    (when events
      (dolist (event (nreverse events))
        (trigger event :dispatch dispatch)))))


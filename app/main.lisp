(in-package :turtl-core)

(defvar *run* t
  "Whether or not we should be running.")
(defvar *run-lock* (bt:make-lock "run")
  "Run lock.")

(defparameter *turtl-thread* nil
  "Holds Turtl's main event loop thread.")

(defun main-event-loop ()
  "Our main idle loop function. Checks our pending events and listens for quit
   conditions."
  (let ((run (bt:with-lock-held (*run-lock*) *run*)))
    (cond (run
            (event-handler)
            (as:delay 'main-event-loop :time 0.0001))
          (t
            (as:clear-signal-handlers)))))

(defun do-start ()
  "Inits the Turtl event loop."
  (unwind-protect
    (as:with-event-loop (:catch-app-errors t)
      (main-event-loop)
      (with-bind ("ping" event)
        (as:with-delay (.4)
          (trigger-remote (make-event "pong"))))
      (as:signal-handler 2
        (lambda (sig)
          (declare (ignore sig))
          (as:clear-signal-handlers))))
    (setf *turtl-thread* nil)))

(defun start ()
  "Starts a thread with Turtl's event loop listener."
  (unless *turtl-thread*
    (setf *run* t)
    ;; listen for remote messages
    (bind-remote-message-handler)
    (let ((thread (bt:make-thread
                    (lambda () (do-start))
                    :name "turtl-main")))
      (setf *turtl-thread* thread))))

(defun stop ()
  (bt:with-lock-held (*run-lock*)
    (setf *run* nil)))


(in-package :turtl-core)

(setf *random-state* (make-random-state t))

(defvar *run* t
  "Whether or not we should be running.")
(defvar *run-lock* (bt:make-lock "run")
  "Run lock.")

(defvar *turtl-thread* nil
  "Holds Turtl's main event loop thread.")

(defun main-event-dispatch ()
  "Sets up any top-level events we need to listen for."
  (with-bind ("ping" event respond)
    (vom:debug "ping!")
    (respond (event "pong")))
  (with-bind ("http" event respond)
    (as:with-delay (.01)
      (vom:debug "http: ~s~%" (data event))
      (multiple-future-bind (res status)
          (drakma-async:http-request (data event))
        (let ((res (if (stringp res)
                       res
                       (babel:octets-to-string res))))
          (respond (event "http" :data (list res status)))))))
  (with-bind ("cmd" event respond)
    (case (intern (string-upcase (hget (data event) '("name"))) :keyword)
      (:reload
        (asdf:operate 'asdf:load-op :turtl-core)
        (respond (event "success"))))))

(defun main-event-loop ()
  "Our main idle loop function. Checks our pending events and listens for quit
   conditions."
  (let ((run (bt:with-lock-held (*run-lock*) *run*)))
    (cond (run
            (event-handler)
            (poll-jobs)
            (as:delay 'main-event-loop :time 0.0001))
          (t
            (as:clear-signal-handlers)))))

(defun do-start (&key start-fn)
  "Inits the Turtl event loop."
  (unwind-protect
    (as:with-event-loop (:catch-app-errors nil
                         :default-event-cb
                           (lambda (ev)
                             (vom:error "top-level error: ~a" ev)))
      (vom:info "event loop started")
      (setf lparallel:*kernel* (lparallel:make-kernel (get-num-cores)))
      (ignore-errors (nec:random-init))
      (main-event-loop)
      (main-event-dispatch)
      (as:signal-handler 2
        (lambda (sig)
          (declare (ignore sig))
          (as:clear-signal-handlers)))
      (when start-fn
        (as:with-delay (0) (funcall start-fn))))
    (vom:info "event loop ended (turtl shutting down)")
    (lparallel:end-kernel :wait t)
    (nec:random-close)
    (setf *turtl-thread* nil)))

(defun start (&key single-thread start-fn)
  "Starts a thread with Turtl's event loop listener."
  (unless *turtl-thread*
    (vom:info "starting turtl-core")
    (setf *run* t)
    ;; listen for remote messages
    (bind-remote-message-handler)
    (if single-thread
        (do-start :start-fn start-fn)
        (let ((thread (bt:make-thread (lambda () (do-start :start-fn start-fn)) :name "turtl-main")))
          (setf *turtl-thread* thread)))))

(defun stop ()
  (vom:info "stopping turtl-core")
  (bt:with-lock-held (*run-lock*)
    (setf *run* nil)))

;(push-event (event "http" :data "http://api.beeets.com/"))

(defun test2 ()
  (push-event (event "http" :data "http://api.beeets.com/"))
  (event-handler))

(defun test ()
  (wipe)
  (wipe :dispatch *remote-dispatch* :preserve-forwards t)
  (with-bind ("http" event respond)
    (alet ((res (drakma-async:http-request (data event))))
      (respond (event "http" :data (babel:octets-to-string res)))))
  (with-bind ("ping" event respond)
    (respond (event "pong")))
  (as:with-event-loop (:catch-app-errors t)
    (test2)))


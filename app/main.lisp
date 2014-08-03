(in-package :turtl-core)

(setf *random-state* (make-random-state t))

(defvar *turtl-thread* nil
  "Holds Turtl's main event loop thread.")

(defvar *stop-event* nil
  "Holds the event used to stop Turtl.")

(defvar *msg-sock* nil
  "Holds our messaging socket.")

(defun do-start (&key start-fn no-signal-handler)
  "Inits the Turtl event loop."
  (unwind-protect
    (let* ((*msg-sock* (nn-socket nn:+nn-pair+))
           (conn (nn-bind *msg-sock* *comm-url*))
           (fd (nn-get-sock-fd *msg-sock*)))
      (vom:debug "start: sock/conn: ~a/~a/~a" *msg-sock* conn fd)
      (setup-remote-forwarding)
      (ignore-errors (nec:random-init))
      (setf *main-queue* (make-queue (get-num-cores)))
      (as:enable-threading-support)
      (as:with-event-loop (:catch-app-errors nil
                           :default-event-cb
                             (lambda (ev)
                               (vom:error "top-level error: ~a" ev)))
        (let ((watch (as:watch-fd fd :read-cb 'recv-remote)))
          (vom:info "event loop started")
          (unless no-signal-handler
            (setf *stop-event* (as:make-event (lambda ()
                                                (as:free-event watch)
                                                (as:clear-signal-handlers))))
            (as:signal-handler 2
              (lambda (sig)
                (declare (ignore sig))
                (as:clear-signal-handlers)))))
        (trigger-remote (event "turtl-loaded"))
        ;; if we have a start function, run it after everything has been set up
        (when start-fn
          (as:delay start-fn :time 0)))
      (nn:shutdown *msg-sock* conn)
      (nn:close *msg-sock*))
    (vom:info "event loop ended (turtl shutting down)")
    (stop-queue *main-queue*)
    (nec:random-close)
    (setf *turtl-thread* nil)))

(defun start (&key single-thread start-fn)
  "Starts a thread with Turtl's event loop listener."
  (unless *turtl-thread*
    (vom:info "starting turtl-core")
    (if single-thread
        (do-start :start-fn start-fn)
        ;; create some thread-locals and start our turtl thread
        (let* ((*stop-event* nil)
               (*turtl-event-loop* nil)
               (thread (bt:make-thread (lambda () (do-start :start-fn start-fn)) :name "turtl-main")))
          (setf *turtl-thread* thread)))))

(defun stop ()
  (vom:info "stopping turtl-core")
  (when *stop-event*
    (as:add-event *stop-event* :activate t)
    (setf *stop-event* nil)))


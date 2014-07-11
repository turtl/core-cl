(in-package :turtl-core)

(setf *random-state* (make-random-state t))

(defvar *turtl-thread* nil
  "Holds Turtl's main event loop thread.")

(defvar *stop-event* nil
  "Holds the event used to stop Turtl.")

(defvar *push-outgoing-messages* nil
  "Whether or not we push messages to UI (not always available) or save outgoing
   messages locally and let UI pluck them out.")

(defun do-start (&key start-fn no-signal-handler)
  "Inits the Turtl event loop."
  (unwind-protect
    (progn
      (setup-remote-forwarding)
      (ignore-errors (nec:random-init))
      (setf lparallel:*kernel* (lparallel:make-kernel (get-num-cores)))
      ;; some windowz BS (for testing standalone executables mainly)
      (when (cffi:foreign-symbol-pointer "WSAStartup")
        (cffi:with-foreign-object (data :char 400)
          (cffi:foreign-funcall "WSAStartup" :uint16 #x0202 :pointer data)))
      (as:enable-threading-support)
      (as:with-event-loop (:catch-app-errors nil
                           :default-event-cb
                             (lambda (ev)
                               (vom:error "top-level error: ~a" ev)))
        (vom:info "event loop started")
        (unless no-signal-handler
          (setf *stop-event* (as:make-event (lambda () (as:clear-signal-handlers))))
          (as:signal-handler 2
            (lambda (sig)
              (declare (ignore sig))
              (as:clear-signal-handlers))))
        (trigger-remote (event "turtl-loaded"))
        ;; if we have a start function, run it after everything has been set up
        (when start-fn
          (as:delay start-fn :time 0))))
    (vom:info "event loop ended (turtl shutting down)")
    (lparallel:end-kernel :wait t)
    (nec:random-close)
    (setf *turtl-thread* nil)))

(defun start (&key single-thread start-fn push-messages)
  "Starts a thread with Turtl's event loop listener."
  (when push-messages
    (setf *push-outgoing-messages* t))
  (unless *turtl-thread*
    (vom:info "starting turtl-core")
    ;; listen for remote messages
    (bind-remote-message-handler)
    ;; listen for UI polling
    (bind-ui-poll-handler)
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


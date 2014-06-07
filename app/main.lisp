(in-package :turtl-core)

(setf *random-state* (make-random-state t))

(defvar *turtl-thread* nil
  "Holds Turtl's main event loop thread.")

(defvar *stop-event* nil
  "Holds the event used to stop Turtl.")

(defvar *turtl-event-loop* nil
  "Holds the main event loop.")

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

(defun do-start (&key start-fn no-signal-handler)
  "Inits the Turtl event loop."
  (unwind-protect
    (progn
      (wipe)
      (wipe :dispatch *remote-dispatch*)
      (setup-remote-forwarding)
      (ignore-errors (nec:random-init))
      (setf lparallel:*kernel* (lparallel:make-kernel (get-num-cores)))
      (as:enable-threading-support)
      (as:with-event-loop (:catch-app-errors nil
                           :default-event-cb
                             (lambda (ev)
                               (vom:error "top-level error: ~a" ev)))
        ;(vom:info "event loop started")
        (setf *turtl-event-loop* cl-async-base:*event-base*)
        (main-event-dispatch)
        (unless no-signal-handler
          (setf *stop-event* (as:make-event (lambda () (as:clear-signal-handlers))))
          (as:signal-handler 2
            (lambda (sig)
              (declare (ignore sig))
              (as:clear-signal-handlers))))
        (when start-fn
          (as:delay start-fn :time 0))
        ))
    (vom:info "event loop ended (turtl shutting down)")
    (lparallel:end-kernel :wait t)
    (nec:random-close)
    (setf *turtl-thread* nil)))

(defun start (&key single-thread start-fn)
  "Starts a thread with Turtl's event loop listener."
  (unless *turtl-thread*
    (vom:info "starting turtl-core")
    ;; listen for remote messages
    (bind-remote-message-handler)
    (if single-thread
        (do-start :start-fn start-fn)
        ;; create some thread-locals and start our turtl thread
        (let* ((*stop-event* nil)
               (*turtl-event-loop* nil)
               (thread (bt:make-thread (lambda () (do-start :start-fn start-fn)) :name "turtl-main")))
          (setf *turtl-thread* thread)))))

(defun stop ()
  (vom:info "stopping turtl-core")
  (as:add-event *stop-event* :activate t)
  (setf *stop-event* nil))

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


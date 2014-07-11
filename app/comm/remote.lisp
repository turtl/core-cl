(in-package :turtl-core)

(defvar *outgoing-messages* nil
  "Holds messages we're going to send to UI.")
(defvar *outgoing-messages-lock* (bt:make-lock)
  "Locks our outgoing messages container.")

(defun trigger-remote (event)
  "Send an event to our UI thread."
  (declare (optimize (speed 3) (safety 0))
           (type turtl-event event))
  (vom:debug "trigger-remote: ~a" (ev event))
  (let ((str (with-output-to-string (s) (yason:encode event s))))
    (send-remote str)))

(defun send-remote (str)
  "Trigger an event in the remote process (ie, non-lisp). Used to communicate
   between lisp and whatever GUI system is wrapping it (Webkit, Android, iOS)."
  (declare (optimize (speed 3) (safety 0))
           (type string str))
  (vom:debug2 "send-remote (~a)" (length str))
  (handler-case
    (if *push-outgoing-messages*
        (let* ((bytes (babel:string-to-octets str :encoding :utf-8))
               (size (length bytes)))
          (static-vectors:with-static-vector (c-msg size)
            (replace c-msg bytes)
            (msg-to-ui size (static-vectors:static-vector-pointer c-msg))))
        (let* ((bytes (babel:string-to-octets str :encoding :utf-8)))
          (bt:with-lock-held (*outgoing-messages-lock*)
            (push bytes *outgoing-messages*))))
    (t (e) (vom:error "send-remote: ~a~%" e))))

(defun bind-remote-message-handler ()
  "Registers our lisp message handler."
  (vom:info "binding remote message handler")
  (set-lisp-msg-handler (cffi:callback msg-from-ui)))

(defun bind-ui-poll-handler ()
  (vom:info "binding ui poll handler")
  (set-ui-msg-poll-handler (cffi:callback ui-msg-poll)))


(in-package :turtl-core)

(defun trigger-remote (event)
  "Send an event to our UI thread."
  (let ((str (with-output-to-string (s) (yason:encode event s))))
    (send-remote str)))

(defun send-remote (str)
  "Trigger an event in the remote process (ie, non-lisp). Used to communicate
   between lisp and whatever GUI system is wrapping it (Webkit, Android, iOS)."
  (handler-case
    (let* ((bytes (babel:string-to-octets str :encoding :utf-8))
           (size (length bytes)))
      (vom:debug "send-remote (~a): ~a" size (subseq str 0 (min (length str) 100)))
      (static-vectors:with-static-vector (c-msg size)
        (replace c-msg bytes)
        (msg-to-ui size (static-vectors:static-vector-pointer c-msg))))
    (t (e) (vom:error "send-remote: ~a~%" e))))

(defun bind-remote-message-handler ()
  "Registers our lisp message handler."
  (vom:info "binding remote message handler")
  (set-lisp-msg-handler (cffi:callback msg-from-ui)))


(in-package :turtl-core)

(defun trigger-remote (event)
  "Send an event to our UI thread."
  (let ((str (with-output-to-string (s) (yason:encode event s))))
    (send-remote str)))

(defun send-remote (str)
  "Trigger an event in the remote process (ie, non-lisp). Used to communicate
   between lisp and whatever GUI system is wrapping it (Webkit, Android, iOS)."
  (format t "lisp: send-remote (~a): ~a~%" (length str) str)
  (handler-case
    (let* ((bytes (babel:string-to-octets str :encoding :utf-8))
           (size (length bytes)))
      (cffi:with-foreign-object (c-msg :char size)
        (dotimes (i size)
          (setf (cffi:mem-aref c-msg :char i) (aref bytes i)))
        (msg-to-ui size c-msg)))
    (t (e) (format t "send-remote: err: ~a~%" e))))

(defun bind-remote-message-handler ()
  "Registers our lisp message handler."
  (set-lisp-msg-handler (cffi:callback msg-from-ui)))


(in-package :turtl-core)

(cffi:defcfun ("turtl_set_lisp_msg_handler" set-lisp-msg-handler% :convention :cdecl) :void
  (fn :pointer))

(cffi:defcfun ("turtl_set_ui_msg_poll_handler" set-ui-msg-poll-handler% :convention :cdecl) :void
  (fn :pointer))

(defun set-lisp-msg-handler (fn)
  "Let C land know what function to call when we have messages."
  (when (cffi:foreign-symbol-pointer "turtl_set_lisp_msg_handler")
    (set-lisp-msg-handler% fn)))

(defun set-ui-msg-poll-handler (fn)
  "Let C land know what function to call when the UI wants to poll messages."
  (when (cffi:foreign-symbol-pointer "turtl_set_ui_msg_poll_handler")
    (set-ui-msg-poll-handler% fn)))

(cffi:defcfun ("turtl_msg_to_ui" msg-to-ui% :convention :cdecl) :void
  "Send a message to our UI layer (whoever that may be)."
  (message-length :unsigned-long)
  (c-str :pointer))

(defun msg-to-ui (message-length c-str)
  "Send a message to our UI layer (whoever that may be)."
  (when (cffi:foreign-symbol-pointer "turtl_msg_to_ui")
    (msg-to-ui% message-length c-str)))

(cffi:defcallback (msg-from-ui :convention :cdecl) :void ((length :unsigned-long) (data :pointer))
  "Called from our UI system when it sends us a message."
  (handler-case
    (let* ((vec (make-array length :element-type '(unsigned-byte 8)))
           (str (progn
                  (dotimes (i length)
                    (setf (aref vec i) (cffi:mem-aref data :unsigned-char i)))
                  (babel:octets-to-string vec :encoding :utf-8))))
      (vom:debug "recv-remote (~a): ~a" length (subseq str 0 (min (length str) 100)))
      (push-event str))
    (t (e) (format t "msg_from_ui: err: ~a~%" e))))

(cffi:defcallback (ui-msg-poll :convention :cdecl) :int ()
  "Called when the UI thread polls for messages. It must have set up a ui msg
   handler before calling this. So what happens when using the push method is
   lisp calls the ui msg handler directly which notifies the UI thread of the
   message (when libuv is present). When polling, the UI thread tells us to
   check for a message, and we push the response through the same ui msg handler
   (but it happens in the same thread the UI calls from)."
  (declare (optimize (speed 3) (safety 0)))
  (handler-case
    (bt:with-lock-held (*outgoing-messages-lock*)
      (dolist (bytes (nreverse *outgoing-messages*))
        (let ((size (length bytes)))
          (static-vectors:with-static-vector (c-msg size :element-type '(unsigned-byte 8))
            (replace c-msg bytes)
            (msg-to-ui size (static-vectors:static-vector-pointer c-msg)))))
      (setf *outgoing-messages* nil)
      0)
    (t (e)
      (vom:error "ui-msg-poll: ~a" e))))


(in-package :turtl-core)

(cffi:defcfun ("turtl_set_lisp_msg_handler" set-lisp-msg-handler :convention :cdecl) :void
  "Let C land know what function to call when we have messages."
  (fn :pointer))

(cffi:defcfun ("turtl_msg_to_ui" msg-to-ui :convention :cdecl) :void
  "Send a message to our UI layer (whoever that may be)."
  (message-length :unsigned-long)
  (c-str :pointer))

(cffi:defcallback (msg-from-ui :convention :cdecl) :void ((length :unsigned-long) (data :pointer))
  "Called from our UI system when it sends us a message."
  (handler-case
    (let* ((str (cffi:foreign-string-to-lisp data :count length))
           (event-hash (yason:parse str))
           (event (event-from-hash event-hash)))
      (setf (gethash "remote" (meta event)) t)
      (format t "lisp: recv-remote (~a): ~a~%" length str)
      (bt:with-lock-held (*events-lock*)
        (push event *events*)))
    (t (e) (format t "msg_from_ui: err: ~a~%" e))))


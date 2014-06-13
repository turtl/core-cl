(in-package :turtl-core)

(cffi:defcfun ("turtl_set_lisp_msg_handler" set-lisp-msg-handler% :convention :cdecl) :void
  "Let C land know what function to call when we have messages."
  (fn :pointer))

(defun set-lisp-msg-handler (fn)
  "Let C land know what function to call when we have messages."
  (when (cffi:foreign-symbol-pointer "turtl_set_lisp_msg_handler")
    (set-lisp-msg-handler% fn)))

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
  ;; TODO: handle creation of event object in core thread. this way the UI can
  ;; push the event as a string then return immediately. responsive UI is the
  ;; primary goal.
  (handler-case
    (let* ((str (cffi:foreign-string-to-lisp data :count length))
           (event-hash (yason:parse str))
           (event (event-from-hash event-hash)))
      (vom:debug "recv-remote (~a): ~a" length (subseq str 0 (min (length str) 100)))
      (push-event event))
    (t (e) (format t "msg_from_ui: err: ~a~%" e))))


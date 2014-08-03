(in-package :turtl-core)

(define-condition nn-sock-error (turtl-error) ()
  (:documentation "Fired when there's an error making/using a nanomsg socket."))

;;; -----------------------------------------------------------------------------
;;; nanomsg wrappers
;;; -----------------------------------------------------------------------------
(defun %nn-create (create-fn args)
  "Generic function to verify a newly created nanomsg sock/connection/binding."
  (let ((val (apply create-fn args)))
    (if (<= 0 val)
        val
        (let* ((errno (nn:errno))
               (msg (format nil "nanomsg: ~s -- ~a (~a)"
                            (append (list create-fn) args)
                            (nn:strerror errno)
                            errno)))
          (error 'nn-sock-error :msg msg)))))

(defun nn-socket (type)
  "Create and verify a new nanomsg socket."
  (%nn-create 'nn:socket (list nn:+af-sp+ type)))

(defun nn-connect (sock url)
  "Create and verify a nanomsg connection."
  (%nn-create 'nn:connect (list sock url)))

(defun nn-bind (sock url)
  "Create and verify a nanomsg binding."
  (%nn-create 'nn:bind (list sock url)))

(defun nn-recv (sock &key wait)
  "Receive a message from a nanomsg socket."
  (declare (optimize (speed 3) (safety 0))
           (type integer sock)
           (type boolean wait))
  (handler-case
    (cffi:with-foreign-object (buf :pointer)
      (let* ((len (cffi:foreign-funcall "nn_recv"
                                        :int sock
                                        :pointer buf
                                        :int -1
                                        :int (if wait 0 nn:+nn-dontwait+)
                                        :int))
             (pt (cffi:mem-aref buf :pointer))
             (arr (make-array len :element-type '(unsigned-byte 8))))
        (dotimes (i len)
          (setf (aref arr i) (cffi:mem-aref pt :unsigned-char i)))
        (nn:freemsg pt)
        (babel:octets-to-string arr :encoding :utf-8)))
    (t (e) (vom:error "comm-recv: ~a" e))))

(defun nn-send (sock msg &key wait)
  "Send a message to a nanomsg socket."
  (declare (optimize (speed 3) (safety 0))
           (type integer sock)
           (type vector wait))
  (handler-case
    (let ((msg (if (stringp msg)
                   (babel:string-to-octets msg :encoding :utf-8)
                   msg)))
      (static-vectors:with-static-vector (msg-c (length msg))
        (replace msg-c msg)
        (nn:send sock
                 (static-vectors:static-vector-pointer msg-c)
                 (length msg)
                 (if wait 0 nn:+nn-dontwait+))))
    (t (e) (vom:error "comm-send: ~a" e))))

(defun nn-get-sock-fd (sock)
  "Get the underlying file descriptor for a messaging socket."
  (cffi:with-foreign-objects ((pt :pointer)
                              (size :int))
    (setf (cffi:mem-aref size :int) (cffi:foreign-type-size :long))
    (nn:getsockopt sock nn:+nn-sol-socket+ nn:+nn-rcvfd+ pt size)
    (cffi:mem-aref pt :long)))

;;; -----------------------------------------------------------------------------
;;; core-specific stuff
;;; -----------------------------------------------------------------------------
(defun trigger-remote (event)
  "Send an event to our UI thread."
  (declare (optimize (speed 3) (safety 0))
           (type turtl-event event))
  (vom:debug "trigger-remote: ~a" (ev event))
  (let ((str (with-output-to-string (s) (yason:encode event s))))
    (send-remote str)))

(defun recv-remote ()
  "Receive a message from the remote message socket."
  (let ((msg (nn-recv *msg-sock*)))
    (push-event msg)))

(defun send-remote (str)
  "Trigger an event in the remote process (ie, non-lisp). Used to communicate
   between lisp and whatever GUI system is wrapping it (Webkit, Android, iOS)."
  (vom:debug2 "send-remote (~a)" (length str))
  (nn-send *msg-sock* str :wait t))


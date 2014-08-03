(in-package :turtl-core)

(define-condition stop-worker (simple-condition) ())
  
(defclass work-queue ()
  ((threads :accessor work-queue-threads :initform nil)
   (sock :accessor work-queue-sock :initarg :sock :initform nil)
   (conn :accessor work-queue-conn :initarg :conn :initform nil)
   (jobs :accessor work-queue-jobs :initform (make-hash-table :test 'equal))
   (jobs-lock :accessor work-queue-jobs-lock :initform (bt:make-lock))))

(defvar *main-queue* nil
  "Holds the default work queue object.")

(defun make-queue (num-threads)
  "Creates a worker queue object and threads."
  (let* ((url "inproc://core-work-queue")
         (sock (nn-socket nn:+nn-push+))
         (conn (nn-bind sock url))
         (queue (make-instance 'work-queue :sock sock :conn conn)))
    (dotimes (i num-threads)
      (let* ((num i)
             (name (format nil "~a" num))
             (thread (bt:make-thread (lambda ()
                                       (start-worker queue url
                                                     :name name)))))
        (push thread (work-queue-threads queue))))
    queue))

(defun start-worker (queue url &key name)
  "Starts out a new worker, paid to sit quietly and monitor the queue for jobs
   to do."
  (let* ((sock (nn-socket nn:+nn-pull+))
         (conn (nn-connect sock url))
         (mainloop (lambda ()
                     (vom:debug1 "worker ~a: (re)starting main loop" name)
                     (loop for uuid = (nn-recv sock :wait t) do
                       (let* ((jobs (work-queue-jobs queue))
                              (work-op nil))
                         (bt:with-lock-held ((work-queue-jobs-lock queue))
                           (setf work-op (gethash uuid jobs))
                           (remhash uuid jobs))
                         (when work-op
                           (funcall work-op)))))))
    (block end-loop
      (loop do
        (handler-case
          (funcall mainloop)
          (stop-worker ()
            (vom:debug "worker ~a: ending (stop-worker fired)" name)
            (nn:shutdown sock conn)
            (nn:close sock)
            (return-from end-loop))
          (t (e)
            (vom:error "worker ~a: error: ~a" name e)))))))

(defun stop-queue (queue)
  "Stop all workers in a queue."
  (dotimes (i (length (work-queue-threads queue)))
    (push-work queue (lambda () (error 'stop-worker))))
  (let ((sock (work-queue-sock queue))
        (conn (work-queue-conn queue)))
    (nn:shutdown sock conn)
    (nn:close sock)))

(defun push-work (queue work-op)
  "Push a job onto the work queue."
  (let ((uuid (uuid))
        (jobs (work-queue-jobs queue))
        (sock (work-queue-sock queue)))
    (bt:with-lock-held ((work-queue-jobs-lock queue))
      (setf (gethash uuid jobs) work-op))
    (nn-send sock uuid :wait t)))

(defun do-work (operation)
  "Run operation in background thread, finishing the returned future with the
   value(s) of the operation once complete. The future is finished in the same
   thread as the event loop, offering seamless integration with futures for
   background jobs."
  (let* ((result nil)
         (err nil)
         (future (make-future))
         (event (as:make-event (lambda ()
                                  (vom:debug2 "do-work: completed")
                                  (if err
                                      (signal-error future err)
                                      (apply 'finish (append (list future)
                                                             result)))))))
    (push-work *main-queue*
      (lambda ()
        (unwind-protect
          (handler-case
            (progn
              (vom:debug2 "do-work: processing")
              (setf result (multiple-value-list (funcall operation))))
            (t (e)
              (setf err e)))
          (as:add-event event :activate t))))
    (vom:debug2 "do-work: queuing")
    future))

(defmacro work (&body body)
  "Thin wrapper around do-work to remove the HORRIBLE need to wrap things in
   (lambda () ...)"
  `(do-work (lambda () ,@body)))


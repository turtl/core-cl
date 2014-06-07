(in-package :turtl-core)

(defvar *jobs* (make-hash-table :test #'eq)
  "Holds current jobs being worked on.")

(defmacro work ((binding operation) &body body)
  "Run operation in parallel, calling body with binding bound to the result of
   operation when complete. Normally we could just use lparallel's plet, but it
   blocks, and we instead nee to poll for the completion of our futures from
   the top-level.
   
   For now, this only supports one return value."
  (let ((future-bind (gensym "future")))
    `(let ((,future-bind (lparallel:future (progn
                                             (vom:debug1 "work: processing ~a" ',operation)
                                             ,operation))))
       (vom:debug1 "work: queuing ~s" ',operation)
       (setf (gethash ,future-bind *jobs*) (lambda (,binding) ,@body))
       ,future-bind)))

(defun poll-jobs ()
  "Poll any jobs that are active for results."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (handler-case
    (loop for future being the hash-keys of *jobs* do
      (when (lparallel:fulfilledp future)
        (unwind-protect
          (let ((fn (gethash future *jobs*))
                (value (lparallel:force future)))
            (vom:debug1 "work: finishing ~a" value)
            (funcall fn value))
          ;; cleanup
          (remhash future *jobs*))))
    (t (e)
      (vom:error "poll-jobs: ~a" e))))


(in-package :turtl-core)

(defun do-work (operation)
  "Run operation in background thread, finishing the returned future with the
   value(s) of the operation once complete. The future is finished in the same
   thread as the event loop, offering seamless integration with futures for
   background jobs.
   
   Uses lparallel in the background for the thread pool."
  (let* ((result nil)
         (err nil)
         (future (make-future))
         (event (as:make-event (lambda ()
                                  (vom:debug2 "do-work: completed")
                                  (if err
                                      (signal-error future err)
                                      (apply 'finish (append (list future)
                                                             result)))))))
    (lparallel:future
      (unwind-protect
        (handler-case
          (progn
            (vom:debug1 "do-work: processing")
            (setf result (multiple-value-list (funcall operation))))
          (t (e)
            (setf err e)))
        (as:add-event event :activate t)))
    (vom:debug2 "do-work: queuing")
    future))

(defmacro work (&body body)
  "Thin wrapper around do-work to remove the HORRIBLE need to wrap things in
   (lambda () ...)"
  `(do-work (lambda () ,@body)))


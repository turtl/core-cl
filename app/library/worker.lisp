(in-package :turtl-core)

(defmacro work (operation)
  "Run operation in parallel, calling body with binding bound to the result of
   operation when complete. Normally we could just use lparallel's plet, but it
   blocks, and we instead nee to poll for the completion of our futures from
   the top-level.
   
   For now, this only supports one return value."
  (let ((result (gensym "result"))
        (err (gensym "err"))
        (future (gensym "future"))
        (event (gensym "event")))
    `(let* ((,result nil)
            (,err nil)
            (,future (make-future))
            (,event (as:make-event (lambda ()
                                     (vom:debug1 "work: completed: ~a" ',operation)
                                     (if ,err
                                         (signal-error ,future ,err)
                                         (apply 'finish (append (list ,future)
                                                                ,result)))))))
       (lparallel:future
         (unwind-protect
           (handler-case
             (progn
               (vom:debug1 "work: processing ~a~%" ',operation)
               (setf ,result (multiple-value-list ,operation)))
             (t (e)
               (setf ,err e)))
           (as:add-event ,event :activate t)))
       (vom:debug1 "work: queuing ~s" ',operation)
       ,future)))


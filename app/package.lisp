(defpackage :turtl-core
  (:use :cl :alexandria :cl-async-future :cl-hash-util :event-glue :sqlite-obj)
  (:shadow :event
           :trigger)
  (:export :start
           :stop
           :trigger
           :bind))


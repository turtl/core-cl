(defpackage :turtl-core
  (:use :cl :alexandria :cl-async-future :cl-hash-util :event-glue)
  (:shadow :event
           :trigger)
  (:export :start
           :stop
           :trigger
           :bind))


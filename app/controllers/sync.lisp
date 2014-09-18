(in-package :turtl-core)

(with-bind ("sync:ui" ev :response res)
  (alet* ((data (data ev))
          (method (to-keyword (hget data "method")))
          (table (to-keyword (hget data "table")))
          (id (hget data "id"))
          (modeldata (hget data "data"))
          (class (model-from-table table))
          (model (create-model class (hash ("id" id)))))
    (wait-for
        (case method
          (:read
            (wait-for (mfetch model)
              (res (data model))))
          (:create
            (wait-for (msave model)
              (res (data model))))
          (:update
            (wait-for (msave model)
              (res (data model))))
          (:delete
            (wait-for (mdestroy model)
              (res t))))
      (queue-remote table method (data model)))))


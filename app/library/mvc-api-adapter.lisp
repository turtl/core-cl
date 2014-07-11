(in-package :turtl-core)

(defclass api-model (protected)
  ((url :accessor url :initform "/my-model"))
  (:documentation
    "Describes a model that syncs with the Turtl API using some basic commands."))

(defamethod msync (future) ((method keyword) (model api-model) &key)
  (alet* ((id (mid model)))
    (when (and (eq method :delete)
               (not id))
      (finish future nil)
      (return-from msync future))
    (alet* ((url (url model))
            (url (if id
                     (format nil "~a/~a" url id)
                     url))
            (data (when (find method '(:post :put))
                    (mserialize model)))
            (data (when data (hash ("data" data)))))
      ;; do some API data massaging.
      (let ((keys (hget data '("data" "keys"))))
        (when (and keys
                   (zerop (length keys)))
          (setf (hget data '("data" "keys")) "")))
      (future-handler-case
        (attach (api method url data)
          (lambda (&rest vals)
            (apply 'finish (append (list future) vals))))
        (api-error (e)
          (if (and (eq method :delete)
                   (eq (api-error-status e) 404))
              (finish future t)
              (signal-error future e)))))))

(defamethod msave (future) ((model api-model))
  (let* ((id (mid model))
         (method (if id :put :post)))
    (finish future (msync method model))))

(defamethod mdestroy (future) ((model api-model))
  (alet ((id (mid model)))
    (unless id
      (call-next-method)
      (finish future nil)
      (return-from mdestroy future))
    (finish future (msync :delete model))))

(defamethod mfetch (future) ((model api-model))
  (alet ((id (mid model)))
    (unless id
      (finish future nil)
      (return-from mfetch future))
    (alet* ((data (msync :get model))
            (nil (mset model data)))
      (finish future model))))


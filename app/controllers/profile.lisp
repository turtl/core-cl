(in-package :turtl-core)

(with-bind ("grab-profile" ev :response res)
  "Grab the entire profile (sans notes/files)."
  (let ((data (mdata *profile*)))
    (res data)))

(with-bind ("profile-get" ev :response res)
  "Get an item in the profile by id."
  (let* ((data (data ev))
         (what (hget data "type"))
         (id (hget data "id")))
    (let ((model (mfind (mget *profile* what) id)))
      (res model))))

(with-bind ("profile-list" ev :response res)
  "Get all items of s single type from a profile."
  (let* ((data (data ev))
         (what (hget data "type")))
    (let ((models (models (mget *profile* what))))
      (res models))))


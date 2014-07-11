(in-package :turtl-core)

(with-bind ("note-search" ev)
  "Search items in the profile."
  (let* ((data (data ev))
         (what (hget data "type"))
         (sort (intern (string-upcase (hget data "sort")) :keyword))
         (tags (hget data "tags"))
         (search-string (hget data "search"))
         (offset (or (hget data "offset") 0))
         (limit (min (or (hget data "limit") 100) 400)))
    ;; ok, do our search
    (let ((result (search-notes (mget *profile* "notes")
                                :tags tags
                                :sort sort
                                :search-string search-string
                                :start start
                                :limit limit)))
      (trigger-remote (event "search" :data result :uuid (id ev))))))


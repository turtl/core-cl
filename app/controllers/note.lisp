(in-package :turtl-core)

(with-bind ("search-notes" ev :response res)
  "Search notes (full-text or tag search)"
  (let* ((data (data ev))
         (board-id (hget data "board_id"))
         (sort (intern (string-upcase (hget data "sort")) :keyword))
         (tags (hget data "tags"))
         (search-string (hget data "search"))
         (offset (or (hget data "offset") 0))
         (limit (min (or (hget data "limit") 100) 400)))
    ;; ok, do our search
    (let* ((result (search-notes (mget *profile* "notes")
                                 :board-id board-id
                                 :tags tags
                                 :sort sort
                                 :search-string search-string
                                 :limit limit
                                 :offset offset))
                                 
           (tag-gray (get-gray-tags board-id result)))
      (res (hash ("notes" (mapcar 'mdata result))
                 ("tags" tag-gray))))))


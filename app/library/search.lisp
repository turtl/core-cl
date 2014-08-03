(in-package :turtl-core)

(defvar *note-index* nil
  "Holds our full-text index for notes.")

(defun make-note-search-document (note)
  "Convert a note into a document that our search system understands."
  (let ((ndata (mdata note)))
    (setf (hget ndata "tag") (hget ndata "tags"))
    (simple-search:make-document
      '(("id")
        ;; TODO: support multiple boards
        ("board_id")
        ("title" :tokenize t :stem t)
        ("url" :tokenize t)
        ("text" :tokenize t :stem t)
        ("tags" :tokenize t :stem t)
        ("tag")
        ("mod" :sort t))
      ndata)))

(defun index-notes (notes)
  "Index each note in our search system."
  (setf *note-index* (simple-search:make-index))
  (dolist (note (models notes))
    (simple-search:index *note-index* (make-note-search-document note)))
  t)

(defun search-notes (notes &key board-id tags search-string sort (offset 0) (limit 100))
  "Search our notes."
  (let* ((sort (case sort
                 (:id-asc '("id"))
                 (:id-desc '("id" . :desc))
                 (:mod-asc '("mod"))
                 (:mod-desc '("mod" . :desc))
                 (t '("id"))))
         (query nil)
         (query (if board-id
                    (list (concatenate 'string "board_id:" board-id))
                    query))
         (query (if search-string
                    (append query (cdr (simple-search:process-search-string search-string)))
                    query))
         (query (if tags
                    (append query
                            (mapcar (lambda (x)
                                      (if (eq (aref x 0) #\-)
                                          (list :not (concatenate 'string
                                                                  "tag:"
                                                                  (subseq x 1)))
                                          (concatenate 'string "tag:" x)))
                                    tags))
                    query))
         (query (when query
                  (append (list :and) query)))
         (res-query (when query (simple-search:query *note-index* query :sort sort)))
         (res (cond (query res-query)
                    (t (simple-search:query *note-index*
                                            :all
                                            :sort sort
                                            :offset offset
                                            :limit limit))))
         (end-offset (min (+ offset (or limit 100)) (length res)))
         (res-limit (subseq res offset end-offset)))
    (remove-if 'null
               (mapcar (lambda (note-id)
                         (mfind notes note-id))
                       res-limit))))


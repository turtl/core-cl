(in-package :turtl-core)

(defvar *note-index* nil
  "Holds our full-text index for notes.")
(defvar *note-tag-index* nil
  "Holds our index for note tags.")

(defun make-note-search-document (note)
  "Convert a note into a document that our search system understands."
  (simple-search:make-document
    '(("id")
      ("boards")
      ("title" :tokenize t)
      ("url" :tokenize t)
      ("text" :tokenize t)
      ("tags" :tokenize t)
      ("mod" :sort t))
    (data note)))

(defun make-note-tag-document (note)
  "Convert a note's tag into a searchable doc."
  (simple-search:make-document
    '(("id")
      ("tag")   ; no tokenizing!
      ("mod" :sort t))
    note))

(defun make-note-tag-documents (note)
  "Convert a note index a set of searchable tag docs."
  (mapcar (lambda (tag)
            (make-note-tag-document (hash ("id" (mid note)) ("tag" tag) ("mod" (mget note "mod")))))
          (mget note "tags")))

(defafun index-notes (future) (notes)
  "Index each note in our search system."
  (setf *note-index* (simple-search:make-index :stemming t))
  (setf *note-tag-index* (simple-search:make-index))
  (dolist (note (models notes))
    (simple-search:index *note-index* (make-note-search-document note))
    (dolist (doc (make-note-tag-documents note))
      (simple-search:index *note-tag-index* doc)))
  (finish future t))

(defun note-search (notes &key tags search-string sort (offset 0) (limit 100))
  "Search our notes."
  (let* ((sort (case sort
                 (:id-asc '("id"))
                 (:id-desc '("id" . :desc))
                 (:mod-asc '("mod"))
                 (:mod-desc '("mod" . :desc))))
         (query (simple-search:process-search-string search-string))
         (tag-query (when tags
                      (list :and (mapcar (lambda (x)
                                           (if (eq (aref x 0) #\-)
                                               (list :not (subseq x 1))
                                               x))
                                         tags))))
         (res-query (when query (simple-search:query *note-index* query :sort sort)))
         (res-tags (when tag-query (simple-search:query *note-tag-index* tag-query :sort sort)))
         (res (cond ((and query tag-query)
                     (intersection res-query res-tags))
                    (query res-query)
                    (tag-query res-tags)
                    (t (simple-search:query *note-index* :all :sort sort :offset offset :limit limit))))
         (end-offset (min (+ offset (or limit 100)) (length res)))
         (res-limit (subseq res offset end-offset)))
    (remove-if 'null
               (mapcar (lambda (note-id)
                         (let ((note (mfind notes note-id)))
                           (when note (data note))))
                       res-limit))))


(in-package :turtl-core)

(deftobject board "boards"
            ("id"
             "user_id"
             "keys"
             "privs"
             "personas"
             "meta"
             "shared")
            ("title"))

(defun get-board-tags (board-id &key zero)
  (let* ((notes (mget *profile* "notes"))
         (tags (make-hash-table :test 'equal)))
    (dolist (note (mfilter notes "board_id" board-id))
      (dolist (tag (mget note "tags"))
        (if zero
            (setf (gethash tag tags) 0)
            (progn
              (unless (gethash tag tags)
                (setf (gethash tag tags) 0))
              (incf (gethash tag tags))))))
    tags))

(defun get-gray-tags (board-id notes)
  "Given a board ID and a set of notes, return the status of all the tags in the
   board (whether they are grayed out or not)."
  (let ((tags (get-board-tags board-id :zero t)))
    (dolist (note notes)
      (dolist (tag (mget note "tags"))
        (unless (gethash tag tags)
          (setf (gethash tag tags) 0))
        (incf (gethash tag tags))))
    tags))


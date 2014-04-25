;; -----------------------------------------------------------------------------
;; main setup
;; -----------------------------------------------------------------------------
(require :sockets)
;; setup ASDF
(require :asdf)
(let ((cur-dir (truename *default-pathname-defaults*)))
  (setf asdf:*central-registry* (list cur-dir (truename "./app/asdf/registry/") (truename "./app/")))
  (setf asdf::*user-cache* (list (merge-pathnames #p"app/asdf/" cur-dir) "cache" :IMPLEMENTATION)))
;; setup compiler
;(ext:install-c-compiler)
;(setf c:*cc* "gcc.exe")
;(setf c:*user-cc-flags* "")

;; load main libraries/initialize
(asdf:operate 'asdf:load-op :turtl-core)

(unless (fboundp 'cffi::%close-foreign-library)
  (defun cffi::%close-foreign-library (&rest args) (declare (ignore args))))

;; -----------------------------------------------------------------------------
;; start the app
;; -----------------------------------------------------------------------------
(format t "~%~%")
(format t "Turtl loaded.~%")
(turtl-core:start)


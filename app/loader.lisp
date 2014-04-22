;; -----------------------------------------------------------------------------
;; main setup
;; -----------------------------------------------------------------------------
;; load our main libs
(load "require/asdf")
(load "require/sockets")
(load "require/sb-bsd-sockets")
;; setup ASDF
(setf asdf:*central-registry* (list (truename *default-pathname-defaults*) "./app/asdf/registry/"))
(setf asdf::*user-cache* (list (merge-pathnames #p"app/asdf/" (truename *default-pathname-defaults*)) "cache" :IMPLEMENTATION))
;; setup compiler
;(load "require/cmp")
;(ext:install-c-compiler)
;(setf c:*cc* "gcc.exe")
;(setf c:*user-cc-flags* "")

;; load main libraries/initialize
(asdf:operate 'asdf:load-op :turtl-core)

;(unless (fboundp 'cffi::%close-foreign-library)
;  (defun cffi::%close-foreign-library (&rest args) (declare (ignore args))))

;; -----------------------------------------------------------------------------
;; start the app
;; -----------------------------------------------------------------------------
(format t "~%~%")
(format t "Turtl loaded.~%")
;(turtl-core:start)


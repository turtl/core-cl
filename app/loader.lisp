;; -----------------------------------------------------------------------------
;; main setup
;; -----------------------------------------------------------------------------
;; load our main libs
(load "require/asdf")
(load "require/sockets")
(load "require/sb-bsd-sockets")
;; setup compiler
;(load "require/cmp")
;(ext:install-c-compiler)
;(setf c:*cc* "d:/MinGW/bin/gcc.exe")
;(setf c:*user-cc-flags* "-Id:/usr/local/ecl/ -Ld:/usr/local/ecl/")
;; setup ASDF
(setf asdf:*central-registry* (list (truename *default-pathname-defaults*) "./asdf/registry/"))
(setf asdf::*user-cache* (list (merge-pathnames #p"asdf/" (truename *default-pathname-defaults*)) "cache" :IMPLEMENTATION))

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


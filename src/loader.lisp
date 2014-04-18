;; -----------------------------------------------------------------------------
;; main setup
;; -----------------------------------------------------------------------------
(load "require/asdf")
(load "require/sockets")
(load "require/sb-bsd-sockets")
;; setup compiler
;(load "require/cmp")
;(ext:install-c-compiler)
;(setf c:*cc* "d:/MinGW/bin/gcc.exe")
;(setf c:*user-cc-flags* "-Id:/usr/local/ecl/ -Ld:/usr/local/ecl/")
;; setup ASDF
(setf asdf:*central-registry* '(*default-pathname-defaults* "./asdf/registry/"))
(setf asdf::*user-cache* (list (merge-pathnames #p"asdf/" (truename *default-pathname-defaults*)) "cache" :IMPLEMENTATION))
(load "sockets/sb-bsd-sockets.asd")

;; load main libraries/initialize
(push :future-debug *features*)
(asdf:operate 'asdf:load-op :turtl-core)

(format t "~%~%")
(format t "Turtl loaded.~%")

;; -----------------------------------------------------------------------------
;; start the app
;; -----------------------------------------------------------------------------
(turtl-core:start)


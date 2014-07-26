;; -----------------------------------------------------------------------------
;; main setup
;; -----------------------------------------------------------------------------
(defpackage :turtl-bootstrap
  (:use :cl))
(in-package :turtl-bootstrap)

;; in an ideal world, all our packages would already be compiled and we'd just
;; load them. we can do this, but running an external compile for every change
;; ain't my bag, baby. so instead, we specify our compiler here and let it run
;; our changes on the fly.
;; TODO: investigate removing all compilers from ECL for smaller footprint and
;; just running pre-compiled fas files.
(ext:install-bytecodes-compiler)
;(ext:install-c-compiler)
(require :asdf)
(defparameter *turtl-root* "./core/app")
(let ((cur-dir (truename *default-pathname-defaults*))
      (registry (truename (format nil "~a/../asdf/registry/" *turtl-root*)))
      (app-dir (truename (format nil "~a/" *turtl-root*))))
  (setf asdf:*central-registry* (list cur-dir registry app-dir)))
;; setup sockets. we really don't need the full socket lib though since all comm
;; goes through cl-async, but we do need to fool ECL into thinking the
;; sb-bsd-sockets package exists.
;(require :sockets)
(defpackage sb-bsd-sockets)

;; load main libraries/initialize
(handler-case
  (asdf:operate 'asdf:load-op :turtl-core)
  (error (e)
    (format t "turtl: boostrap: ASDF error: ~a~%" e)
    (error e)))

;; -----------------------------------------------------------------------------
;; start the app
;; -----------------------------------------------------------------------------

(format t "~%~%")
(format t "Turtl loaded (~a)~%" (ext:getpid))
(let ((single-threaded (when (boundp 'cl-user::*turtl-single-threaded*)
                         cl-user::*turtl-single-threaded*)))
  (handler-case
    (turtl-core:start :single-thread single-threaded)
    (t (e)
      (vom:error "turtl: bootstrap: ~a" e)
      (error e))))


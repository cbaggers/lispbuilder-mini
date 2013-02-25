;;;; lbm-sdl-binaries

(in-package #:cl-user)

(defpackage #:lbm-sdl-binaries
  (:use #:cl)
  (:nicknames #:sdl-bin)
  (:documentation "The main package of `lbm-sdl'.")
  (:export
   ;; globals.lisp
   #:*dll-path*))

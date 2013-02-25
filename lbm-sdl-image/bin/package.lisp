;;;; lbm-sdl-image-binaries

(in-package #:cl-user)

(defpackage #:lbm-sdl-image-binaries
  (:use #:cl)
  (:nicknames #:sdl-image-bin)
  (:documentation "The main package of `lbm-sdl-image'.")
  
  (:export

   ;; globals.lisp
   #:*dll-path*))
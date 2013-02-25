;;;; lbm-sdl-ttf-binaries

(in-package #:cl-user)

(defpackage #:lbm-sdl-ttf-binaries
  (:use #:cl)
  (:nicknames #:sdl-ttf-bin)
  (:documentation "The main package of `lbm-sdl-ttf'.")
  
  (:export

   ;; globals.lisp
   #:*dll-path*))
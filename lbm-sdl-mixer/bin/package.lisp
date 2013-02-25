;;;; lbm-sdl-mixer-binaries

(in-package #:cl-user)

(defpackage #:lbm-sdl-mixer-binaries
  (:use #:cl)
  (:nicknames #:sdl-mixer-bin)
  (:documentation "The main package of `lbm-sdl-mixer'.")
  
  (:export

   ;; globals.lisp
   #:*dll-path*))

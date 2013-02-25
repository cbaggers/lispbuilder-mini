;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lbm-sdl-ttf
  (:use #:cl #:cffi)
  (:nicknames #:sdl-ttf)
  (:documentation "The main package of `lbm-sdl-ttf'.")
  (:export

   ;; globals.lisp
   #:*generation*
     
   ;; sdl-util-ttf.lisp
   #:ttf-init-p
   #:init-ttf
   #:quit-ttf

   #:load-library))

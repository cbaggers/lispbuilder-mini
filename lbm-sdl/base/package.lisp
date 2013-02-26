;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lbm-sdl-base
  (:use #:cl #:cffi)
  (:nicknames #:sdl-base #:sbl)
  (:documentation "The basic wrapper package of `lbm-sdl'.")
  (:export
   ;; util.lisp
   #:check-bounds
   #:clamp
   #:is-valid-ptr
   #:to-int

   ;; rectangle.lisp
   #:with-rectangle
   #:rect-x
   #:rect-y
   #:rect-w
   #:rect-h
   #:rect-x2
   #:rect-y2
   #:copy-rectangle
   #:clone-rectangle
   #:rectangle

   ;; rwops.lisp
   #:create-RWops-from-file

   ;; surfaces.lisp
   #:with-surface
   #:with-surfaces
   #:with-surface-free
   #:get-surface-rect

   ;; video.lisp
   #:with-display
   #:set-screen
   #:set-window
   #:update-display
   #:clear-display))

;;;; lbm-sdl
;;;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:cl-user)

(defpackage #:lbm-sdl-cffi
  (:use #:cl #:cffi)
  (:nicknames #:sdl-cffi #:scl)
  (:documentation "The basic wrapper package of `lbm-sdl'.")
  (:export
   #:SDL-Enable-UNICODE
   #:SDL-Enable-Key-Repeat
   #:SDL-Get-Key-Repeat
   #:SDL-Get-Key-State
   #:SDL-Get-Mod-State
   #:SDL-Set-Mod-State
   #:SDL-Get-Key-Name))




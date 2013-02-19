
;; SDL library using CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>
;; see COPYING for license

(in-package #:lispbuilder-sdl-cffi)

(defctype sdl-surface :pointer)
(defctype sdl-rectangle :pointer)
(defctype sdl-string (:wrapper :string :to-c to-sdl-string))

(defun to-sdl-string (value)
  (unless value
    (setf value ""))
  (values (cffi:foreign-string-alloc value) t))

(defmethod free-translated-object (ptr (name (eql 'sdl-string)) free-p)
  (if free-p
      (cffi:foreign-string-free ptr)))

(defctype return->=0-as-t (:wrapper :int :from-c return-val->=0-as-t))

(defun return-val->=0-as-t (value)
  (if (>= value 0) t nil))

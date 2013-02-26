;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lbm-sdl)

(defclass sdl-surface (foreign-object)
  ((position-rect
    :reader position-rect
    :initform (rectangle)
    :initarg :position)
   (display-surface-p
    :accessor display-surface-p
    :initform nil
    :initarg :display-surface-p))
  (:documentation
   "A wrapper for the foreign SDL_Surface object."))

(defclass display-surface (sdl-surface) ()
  (:default-initargs
   :display-surface-p t
    :gc nil)
  (:documentation
   "The current display surface. Can be accessed using `SDL:*DEFAULT-DISPLAY*`."))

(defmethod initialize-instance :before ((surface sdl-surface) &key)
  (unless (initialized-subsystems-p)
    (error "ERROR: The SDL library must be initialized prior to use.")))

(defmethod width ((surface sdl-surface))
  "Returns the width of `SURFACE` as an `INTEGER`."
  (sdl-base::surf-w (fp surface)))

(defmethod height ((surface sdl-surface))
  "Returns the height of `SURFACE` as an `INTEGER`."
  (sdl-base::surf-h (fp surface)))

(defmethod x ((surface sdl-surface))
  "Returns the `X` position coordinate of `SURFACE` as an `INTEGER`."
  (x (position-rect surface)))
(defmethod (setf x) (x-val (surface sdl-surface))
  "Sets the integer `X` position coordinate of `SURFACE`."
  (setf (x (position-rect surface)) x-val))

(defmethod y ((surface sdl-surface))
  "Returns the `Y` position coordinate of `SURFACE` as an `INTEGER`."
  (y (position-rect surface)))
(defmethod (setf y) (y-val (surface sdl-surface))
  "Sets the integer `Y` position coordinate of `SURFACE`."  
  (setf (y (position-rect surface)) y-val))

(defmethod rectangle-* ((surface sdl-surface))
  "Returns the fields `X`, `Y`, `WIDTH` and `HEIGHT` from `SURFACE` as a spread."
  (values (x surface) (y surface) (width surface) (height surface)))

(defmethod get-rectangle-* ((surface sdl-surface))
  "Returns a new `RECTANGLE` containing the `X`, `Y`, `WIDTH` and `HEIGHT` values of `SURFACE`."
  (rectangle :x (x surface)
             :y (y surface)
             :w (width surface)
             :h (height surface)))

(defun get-surface-attribute (surface attribute)
  "Returns `T` if the attribute is set for the surface surface, or returns `NIL` otherwise."
  (unless (cffi:null-pointer-p surface)
    (/= 0 (logand (cffi:foreign-slot-value surface 'sdl-cffi::sdl-surface 'sdl-cffi::flags)
                  attribute))))

(defun display-info ()
  "Returns information about the display surface."
  (labels ((check-feature (info description)                            
             (unless (eq (logand (cffi:foreign-slot-value (fp surface) 
                                                          'sdl-cffi::sdl-surface
                                                          'sdl-cffi::flags)
                                 info) 0) description))))
  (let ((surface *default-display*))
    (check-type surface sdl-surface)
    (remove nil (mapcar #'check-feature `((sdl-any-format 'sdl-any-format)
                                          (sdl-fullscreen 'sdl-fullscreen)
                                          (sdl-resizable 'sdl-resizable)
                                          (sdl-pre-alloc 'sdl-pre-alloc))))))

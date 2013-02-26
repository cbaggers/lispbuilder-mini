;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lbm-sdl-base)

;; [TODO] Candidate for deletiong if surfaces are not useful within opengl
(defmacro with-surface ((var &optional surface (free t))
                        &body body)
  "Don't use this for managing the display surface."
  (let ((body-value (gensym "body-value-"))
        (free-value (gensym "free-value-")))
    `(let ,(when surface
                 `((,var ,surface)))
       (let ((,body-value nil)
             (,free-value ,free))
         (setf ,body-value (progn ,@body))
         (when ,free-value
           (sdl-cffi::sdl-Free-Surface ,var))
         ,body-value))))

(defmacro with-surface-slots ((var &optional surface)
                              &body body)
  `(with-surface (,var ,surface nil)
     ,@body))

(defmacro with-surfaces (bindings &rest body)
  (if bindings
      (return-with-surface bindings body)))

(defun return-with-surface (bindings body)
  (if bindings
      `(with-surface (,@(car bindings))
         ,(return-with-surface (cdr bindings) body))
      `(progn ,@body)))

(defun get-surface-rect (surface rectangle)
  (setf (rect-x rectangle) 0
        (rect-y rectangle) 0
        (rect-w rectangle) (surf-w surface)
        (rect-h rectangle) (surf-h surface))
  rectangle)

;; cl-sdl "sdl-ext.lisp"
;; #define SDL_MUSTLOCK(surface) (surface->offset ||    ((surface->flags & (SDL_HWSURFACE|SDL_ASYNCBLIT|SDL_RLEACCEL)) != 0))
(defun must-lock? (surface)
  "Checks if a surface can be locked.
   Re-implementation of the SDL_MUSTLOCK macro.
   Returns
    T if the surface can be locked.
    NIL if the surface cannot be locked."
  (or (/= 0 (cffi:foreign-slot-value surface 'sdl-cffi::sdl-surface 'sdl-cffi::offset))
      (/= 0 (logand (cffi:foreign-slot-value surface 'sdl-cffi::sdl-surface 'sdl-cffi::flags)
                    (logior sdl-cffi::SDL-HW-SURFACE
                            sdl-cffi::SDL-ASYNC-BLIT
                            sdl-cffi::SDL-RLE-ACCEL)))))

(defun surf-w (surface)
  "return the width of the SDL_surface."
  (cffi:foreign-slot-value surface 'sdl-cffi::Sdl-Surface 'sdl-cffi::w))

(defun surf-h (surface)
  "return the height of the Sdl-Surface." 
  (cffi:foreign-slot-value surface 'sdl-cffi::Sdl-Surface 'sdl-cffi::h))

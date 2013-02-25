;; This file contains some useful functions for using SDL_image from Common lisp
;; 2006 (c) Luke Crook, see LICENCE.

(in-package #:lbm-sdl-image)

(defun load-library ()
  (sdl-cffi::load-image-library))

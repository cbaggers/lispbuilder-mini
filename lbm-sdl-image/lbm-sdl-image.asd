;;; -*- lisp -*-

(defpackage #:lbm-sdl-image-system
  (:use #:cl #:asdf))

(in-package #:lbm-sdl-image-system)

(defsystem lbm-sdl-image
  :description "lbm-sdl-image: SDL_image 1.2.10 library wrapper and tools"
  :long-description
  "lbm-sdl-image is a wrapper for the SDL_image 1.2.10 library."
  :version "0.6.0"
  :author "Luke Crook <luke@balooga.com>"
  :maintainer "Application Builder <application-builder@lispniks.com>"
  :licence "MIT"
  :depends-on (cffi lbm-sdl lbm-sdl-image-cffi)
  :perform (load-op :after (op lbm-sdl-image)
		    (pushnew :lbm-sdl-image *features*))
  :components
  ((:module "sdl-image"
	    :components
	    ((:file "package")
	     (:file "generics" :depends-on ("package"))
	     (:file "sdl-image-util" :depends-on ("package" "generics"))))))

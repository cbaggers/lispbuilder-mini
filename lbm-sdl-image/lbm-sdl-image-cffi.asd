;;; -*- lisp -*-

(defpackage #:lbm-sdl-image-cffi-system
  (:use #:cl #:asdf))

(in-package #:lbm-sdl-image-cffi-system)

(defsystem lbm-sdl-image-cffi
    :description "lbm-sdl-image-cffi: SDL_image 1.2.10 library wrapper and tools"
    :long-description
    "lbm-sdl-image is a wrapper for the SDL_image 1.2.10 library."
    :version "0.5.0"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "MIT"
    :depends-on (cffi lbm-sdl lbm-sdl-image-binaries)
    :components
    ((:module "cffi"
	      :components
	      ((:file "package")
	       (:file "library" :depends-on ("package"))
	       (:file "image" :depends-on ("package" "library"))
	       (:file "translate" :depends-on ("image"))
	       ))))

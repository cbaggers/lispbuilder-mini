;;; -*- lisp -*-

(defpackage #:lbm-sdl-mixer-cffi-system
  (:use #:cl #:asdf))
(in-package #:lbm-sdl-mixer-cffi-system)

(defsystem lbm-sdl-mixer-cffi
    :description "lbm-sdl-mixer-cffi: SDL_mixer 1.2.11 library wrapper and tools"
    :long-description
    "lbm-sdl-mixer is a wrapper for the SDL_mixer 1.2.11 library."
    :version "0.4"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "MIT"
    :depends-on (cffi lbm-sdl lbm-sdl-mixer-binaries)
    :components
    ((:module "cffi"
	      :components
	      ((:file "package")
	       (:file "library" :depends-on ("package"))
 	       (:file "mixer" :depends-on ("package"))
	       (:file "sdl-mixer" :depends-on ("package" "library"))
	       (:file "translate" :depends-on ("sdl-mixer" "mixer"))))))

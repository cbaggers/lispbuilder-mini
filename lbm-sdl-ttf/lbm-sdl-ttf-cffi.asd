;;; -*- lisp -*-

(defpackage #:lbm-sdl-ttf-cffi-system
  (:use #:cl #:asdf))

(in-package #:lbm-sdl-ttf-cffi-system)

(defsystem lbm-sdl-ttf-cffi
    :description "lbm-sdl-ttf: SDL_ttf 2.0.9 library wrapper and tools"
    :long-description
    "lbm-sdl-ttf is a wrapper for the SDL_ttf 2.0.9 library."
    :version "0.2.2"
    :author "Luke J Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>, Luke Crook <luke@balooga.com>"
    :licence "BSD"
    :depends-on (cffi lbm-sdl lbm-sdl-ttf-binaries)
    :components
    ((:module "cffi"
	      :components
	      ((:file "package")
	       (:file "library" :depends-on ("package"))
	       (:file "ttf" :depends-on ("package" "library"))
	       (:file "translate" :depends-on ("ttf"))))
     (:module "glue"
	      :components
	      ((:static-file "Makefile")
               (:static-file "lbm-sdl-ttf-glue.c")
               (:static-file "lbm-sdl-ttf-glue.h")))))

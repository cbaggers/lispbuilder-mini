;;; -*- lisp -*-

(defpackage #:lbm-sdl-mixer-system
  (:use #:cl #:asdf))
(in-package #:lbm-sdl-mixer-system)

(defsystem lbm-sdl-mixer
  :description "lbm-sdl-mixer: SDL_mixer v1.2.11 library wrapper and tools"
  :long-description
  "lbm-sdl-mixer uses CFFI to be highly compatible across lisp 
    implementations. It includes a selection of utilities to assist  
    game programming in Common Lisp."
  :version "0.5.0"
  :author "Luke Crook <luke@balooga.com>"
  :maintainer "Application Builder <application-builder@lispniks.com>"
  :licence "MIT"
  :depends-on (cffi lbm-sdl lbm-sdl-mixer-cffi)
  :perform (load-op :after (op lbm-sdl-mixer)
		    (pushnew :lbm-sdl-mixer *features*))
  :components
  ((:module "sdl-mixer"
	    :components
	    ((:file "package")
	     (:file "globals" :depends-on ("package"))
	     (:file "mixer" :depends-on ("globals"))))
   (:module "documentation"
	    :components
	    ((:doc-file "README")
	     (:doc-file "COPYING")
	     (:doc-file "CONTRIBUTORS")
	     (:html-file "lbm-sdl-mixer")
	     (:html-file "footer")
	     (:html-file "header")))
   (:module "build"
	    :components
	    ((:static-file "sdlmixerswig.i")))))

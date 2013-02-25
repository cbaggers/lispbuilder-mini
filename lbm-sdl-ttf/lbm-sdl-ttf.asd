;;; -*- lisp -*-

(defpackage #:lbm-sdl-ttf-system
  (:use #:cl #:asdf))

(in-package #:lbm-sdl-ttf-system)

(defsystem lbm-sdl-ttf
  :description "lbm-sdl-ttf: SDL_ttf 2.0.9 library wrapper and tools"
  :long-description
  "lbm-sdl-ttf is a wrapper for the SDL_ttf 2.0.9 library."
  :version "0.3.0"
  :author "Luke J Crook <luke@balooga.com>"
  :maintainer "Application Builder <application-builder@lispniks.com>, Luke J Crook <luke@balooga.com>"
  :licence "BSD"
  :depends-on (cffi lbm-sdl lbm-sdl-ttf-cffi)
  :perform (load-op :after (op lbm-sdl-ttf)
		    (pushnew :lbm-sdl-ttf *features*))
  :components
  ((:module "sdl-ttf"
	    :components
	    ((:file "package")
	     (:file "generics")
	     (:file "globals")
             (:file "ttf-font-definition")
             (:file "ttf-font-data")
             (:file "sdl-util-ttf")
	     (:file "ttf-font")
;;	     (:file "font")
             (:file "string-solid")
	     (:file "string-shaded")
	     (:file "string-blended"))
            :serial t)
   (:module "documentation"
	    :components
	    ((:html-file "header")
	     (:html-file "footer")
	     (:html-file "lbm-sdl-ttf")))))

;;; -*- lisp -*-

(defpackage #:lbm-sdl-ttf-binaries-system
  (:use #:cl #:asdf))
(in-package #:lbm-sdl-ttf-binaries-system)

(defsystem lbm-sdl-ttf-binaries
  :description "lbm-sdl-ttf-binaries: The windows binary for the SDL_ttf v2.0.9 library"
  :version "2.0.9"
  :author "Sam Lantinga <slouken@libsdl.org>"
  :maintainer "Application Builder <application-builder@lispniks.com>"
  :licence "LGPL"
  :components
  ((:module "bin"
    :components
    ((:file "package")
     (:file "globals")
     ;;(:static-file "SDL_ttf.dll"))
     ;;(:static-file "libfreetype-6.dll")
     ;;(:static-file "zlib1.dll")
     ;;(:static-file "lbm-sdl-ttf-glue.dll")
     ))
   ;;(:module "documentation"
   ;;    :components
   ;;    ((:doc-file "bin_README.txt")))
   ))

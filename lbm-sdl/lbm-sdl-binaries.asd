;;; -*- lisp -*-

(defpackage #:lbm-sdl-binaries-system
  (:use #:cl #:asdf))
(in-package #:lbm-sdl-binaries-system)

(defsystem lbm-sdl-binaries
  :description "lbm-sdl-binaries: The windows binary for the SDL v1.2.14 library"
  :version "1.2.14"
  :author "Sam Lantinga <slouken@libsdl.org>"
  :maintainer "Application Builder <application-builder@lispniks.com>"
  :licence "LGPL"
  :components
  ((:module "bin"
            :components
            ((:file "package")
             (:file "globals"))
            :serial t)))

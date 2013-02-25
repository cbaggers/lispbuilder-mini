
;;; -*- lisp -*-

(defpackage #:lbm-sdl-cocoahelper-system
  (:use #:cl #:asdf))
(in-package #:lbm-sdl-cocoahelper-system)

(defsystem cocoahelper;; lbm-sdl-cocoahelper
  :description "cocoahelper system for SDL"
  :version "0.1"
  :author "Brad Beveridge"
  :maintainer "Application Builder <application-builder@lispniks.com>"
  :licence "BSD"
  :depends-on (cffi lbm-sdl-binaries)
  :components ((:module "cocoahelper"
                :components ((:file "cocoahelper")))))

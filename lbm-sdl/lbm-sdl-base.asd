;;; -*- lisp -*-
;; [TODO] Look into pixel package. What is it for?
;; [TODO] Look into surfaces, if we can't combine them with
;;        opengl graphics then it doesnt really have a place
;;        here and we should limit the functionality to the 
;;        window-surface

(defpackage #:lbm-sdl-base-system
  (:use #:cl #:asdf))
(in-package #:lbm-sdl-base-system)

(defsystem lbm-sdl-base
  :description "lbm-sdl-base: SDL library wrapper providing a base set of functionality."
  :long-description
  "The lbm-sdl-base prackage provides a base set of functionality on top of the CFFI bndings of lbm-sdl-wrapper."
  :version "0.9.8"
  :author "Justin Heyes-Jones <justinhj@gmail.com>, Luke J Crook <luke@balooga.com>"
  :maintainer "Application Builder <application-builder@lispniks.com>"
  :licence "MIT"
  :depends-on (cffi lbm-sdl-cffi)
  :components
  ((:module "base"
            :components
            ((:file "package")
             (:file "pixel")
             (:file "util")
             (:file "rectangle")
             (:file "surfaces")
             (:file "rwops")
             (:file "sdl-util"))
            :serial t)))

;;; -*- lisp -*-

;; [TODO] Add version back in?

(defpackage #:lbm-sdl-system
  (:use #:cl #:asdf))
(in-package #:lbm-sdl-system)

(defsystem lbm-sdl
  :description "lbm-sdl: Wrapper and tools for SDL 1.2.14"
  :long-description "lbm-sdl uses CFFI to be highly compatible across lisp 
    implementations. It includes a selection of utilities to assist  
    game programming in Common Lisp."
  :version "0.9.8.2"
  :author "Justin Heyes-Jones <justinhj@gmail.com>, Luke J Crook <luke@balooga.com>"
  :maintainer "Application Builder <application-builder@lispniks.com>, Luke J Crook <luke@balooga.com>"
  :licence "MIT"
  :depends-on (#:cffi 
               #:trivial-garbage
               lbm-sdl-base
               #:cl-opengl)
  :perform (load-op :after (op lbm-sdl)
                    (pushnew :lbm-sdl *features*))
  :components
  ((:module "sdl"
            :components
            ((:file "package")
             (:file "globals")
             (:file "generics")
             (:file "base")
             (:file "init")
             (:file "mouse")
             (:file "events")
             (:file "rectangle")
             (:file "surfaces")
             (:file "video")
             (:file "rwops")
             (:file "image")
             (:file "keys")
             (:file "audio")
             (:file "mixer")
             (:file "active"))
            :serial t)))

;;;; -*- lisp -*-
;;;; Examples for lbm sdl mixer
;;;; By Justin Heyes-Jones
;;;; Thanks to Luke Crook for package help, testing and feedback

(defpackage #:lbm-sdl-mixer-examples-system
  (:use #:cl #:asdf))
(in-package #:lbm-sdl-mixer-examples-system)

(defsystem lbm-sdl-mixer-examples
    :description "Examples for the lbm-sdl-mixer package."
    :version "0.4"
    :depends-on (cffi lbm-sdl lbm-sdl-mixer)
    :components
    ((:module "examples"
	      :components
	      ((:file "package")
	       (:file "globals" :depends-on ("package"))
	       (:file "mixer" :depends-on ("globals" "package"))
	       (:static-file "music.mp3")
	       (:static-file "music.ogg")
	       (:static-file "phaser.wav")))))
	       



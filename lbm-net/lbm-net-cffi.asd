(defpackage #:lbm-net-cffi-system
  (:use #:cl #:asdf))
(in-package #:lbm-net-cffi-system)

(defsystem lbm-net-cffi
  :description "lbm-net-cffi: Basic Lisp wrapper for the net library."
  :author "Frank Buss <fb@frank-buss.de>"
  :maintainer "Application Builder <application-builder@lispniks.com>"
  :licence "MIT"
  :depends-on (cffi)
  :components
  ((:module "cffi"
    :components
    ((:file "package")
     (:file "library")
     (:file "net"))
    :serial t)))

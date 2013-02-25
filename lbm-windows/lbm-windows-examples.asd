;;; -*-Lisp-*-

(defpackage :lbm-windows-examples
  (:use :common-lisp :asdf :cffi :lbm-windows))
(in-package :lbm-windows-examples)

(defsystem lbm-windows-examples
  :description "Examples for the lbm-windows package."
  :depends-on (cffi lbm-windows)
  :components
  ((:module "examples"
    :components
    ((:file "gui-eval")))))

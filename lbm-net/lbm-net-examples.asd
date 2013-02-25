(defpackage #:lbm-net-examples-system
  (:use #:cl #:asdf))
(in-package #:lbm-net-examples-system)

(defsystem lbm-net-examples
  :description "Examples for the lbm-net package."
  :depends-on (cffi lbm-net)
  :components
  ((:module "examples"
    :components
    ((:file "package")
     (:file "wget" :depends-on ("package"))
     (:file "webserver" :depends-on ("package"))))))

(defpackage #:lbm-net-system
  (:use #:cl #:asdf))
(in-package #:lbm-net-system)

(defsystem lbm-net
  :description "lbm-net: asynchronous sockets library"
  :author "Frank Buss <fb@frank-buss.de>"
  :maintainer "Application Builder <application-builder@lispniks.com>"
  :licence "MIT"
  :depends-on (cffi lbm-net-cffi)
  :components
  ((:module "net"
    :components
    ((:file "package")))))

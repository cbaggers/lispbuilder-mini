;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lbm-sdl-base)

(defun create-RWops-from-file (filename)
  (let ((file-path (namestring filename)))
    (when (and (stringp file-path) (probe-file file-path))
      (let ((rwops (sdl-cffi::sdl-RW-From-File file-path "rb")))
        (when (is-valid-ptr rwops)
          ;;(format t "create-RWops-from-file: ~A~%" rwops)
          rwops)))))


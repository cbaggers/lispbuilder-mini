

(in-package :lbm-windows) 

(cffi:defcfun ("GetDC" get-dc) :pointer
  (hwnd :pointer))

(cffi:defcfun ("DestroyWindow" Destroy-Window) :int
  (hwnd :pointer))

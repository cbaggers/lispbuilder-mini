
(in-package #:lbm-sdl)

(export '*ttf-font-vera* :lbm-sdl)

(defparameter *ttf-font-vera*
  (make-instance 'ttf-font-definition
                 :size 32
                 :filename (merge-pathnames "Vera.ttf" *default-font-path*)))


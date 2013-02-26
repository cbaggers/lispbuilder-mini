;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lbm-sdl)

(defclass sdl-surface (foreign-object)
  ((position-rect
    :reader position-rect
    :initform (rectangle)
    :initarg :position)
   (cell-index
    :accessor cell-index
    :initform 0
    :initarg :cell-index)
   (cells
    :reader cells
    :initform nil
    :initarg :cells)
   (display-surface-p
    :accessor display-surface-p
    :initform nil
    :initarg :display-surface-p))
  (:documentation
   "A wrapper for the foreign SDL_Surface object."))

(defclass display-surface (sdl-surface) ()
  (:default-initargs
   :display-surface-p t
    :gc nil
    :free #'(lambda (fp) fp))
  (:documentation
   "The current display surface. Can be accessed using `SDL:*DEFAULT-DISPLAY*`."))

(defmethod free ((self display-surface))
  "Freeing the display surface is not a valid operation."
  nil)

(defmethod initialize-instance :before ((surface sdl-surface) &key)
  (unless (initialized-subsystems-p)
    (error "ERROR: The SDL library must be initialized prior to use.")))

(defclass surface (sdl-surface) ()
  (:default-initargs
   :display-surface-p nil
    :gc t
    :free (simple-free 'sdl-cffi::sdl-free-surface 'surface))
  (:documentation
   "This object is garbage collected and the `SDL_Surface` object freed when out of scope.
Free using [FREE](#free)."))

(defclass rectangle-array ()
  ((foreign-pointer-to-rectangle :accessor fp :initform nil :initarg :rectangle)
   (length :reader len :initform nil :initarg :length)))

(defmethod initialize-instance :after ((surface surface)
                                       &key using-surface
                                         width height x y bpp
                                         (type :sw)
                                         rle-accel
                                         (cells 1)
                                         (cell-index 0)
                                         mask
                                         &allow-other-keys)
  ;; A surface can be created any of four ways:
  ;; 1) Using SDL_Surface in :SURFACE only. No surface parameters may be set.
  ;; 2) Creating a new SDL_Surface from the SDL_Surface when :SURFACE and :WIDTH & :HEIGHT are set. Surface parameters may be set.
  ;; 3) Using SDL_Surface when :USING-SURFACE is set. Surface parameters may be set.
  ;; 4) Creating a new SDL_Surface when :WIDTH & :HEIGHT are set. Surface parameters may be set.

  ;; Takes care of (2) and (4)
  (if (and width height)
      (setf (slot-value surface 'foreign-pointer-to-object)
            (sdl-base::create-surface width
                                      height
                                      :surface (or (fp surface) using-surface)
                                      :bpp bpp
                                      :type type
                                      :rle-accel rle-accel
                                      :mask mask))
      ;; Takes care of (3)
      (if using-surface
          (setf (slot-value surface 'foreign-pointer-to-object)
                using-surface)))

  (unless (cells surface)
    (setf (cells surface) cells))
  (if (> (length (cells surface)) cell-index)
      (setf (cell-index surface) cell-index)
      (setf (cell-index surface) 0))

  (when x (setf (x surface) x))
  (when y (setf (y surface) y)))

(defmacro with-surface ((var &optional surface (free t))
                        &body body)
  (let ((body-value (gensym "body-value-"))
        (free-value (gensym "free-value-")))
    `(let ,(when surface
                 `((,var ,surface)))
       (let ((*default-surface* ,var)
             (,body-value nil)
             (,free-value ,free))
         (symbol-macrolet ((,(intern (string-upcase (format nil "~A.width" var))) (width ,var))
                           (,(intern (string-upcase (format nil "~A.height" var))) (height ,var))
                           (,(intern (string-upcase (format nil "~A.x" var))) (x ,var))
                           (,(intern (string-upcase (format nil "~A.y" var))) (y ,var)))
           (declare (ignorable ,(intern (string-upcase (format nil "~A.width" var)))
                               ,(intern (string-upcase (format nil "~A.height" var)))
                               ,(intern (string-upcase (format nil "~A.x" var)))
                               ,(intern (string-upcase (format nil "~A.y" var)))))
           (setf ,body-value (progn ,@body)))
         (when ,free-value
           (free ,var))
         ,body-value))))

(defmethod (setf cells) ((num integer) (self sdl-surface))
  (setf (slot-value self 'cells) (make-array num :initial-element (get-rectangle-* self)))
  (setf (cell-index self) 0))

(defmethod (setf cells) ((rects list) (self sdl-surface))
  (setf (slot-value self 'cells) (make-array (length rects) :initial-contents (loop for rect in rects
                                                                                 collect (copy-rectangle rect))))
  (setf (cell-index self) 0))

(defmethod (setf cells) ((rects vector) (self sdl-surface))
  (setf (slot-value self 'cells) (make-array (length rects) :initial-contents (loop for rect across rects
                                                                                 collect (copy-rectangle rect))))
  (setf (cell-index self) 0))

(defmethod (setf cells) ((rect rectangle) (self sdl-surface))
  (setf (slot-value self 'cells) (make-array 1 :initial-element (copy-rectangle rect)))
  (setf (cell-index self) 0))

(defmethod copy-cells ((self sdl-surface) &optional index)
  (copy-cells (cells self) index))

(defmethod copy-cells ((self vector) &optional index)
  (if index
      (make-array 1 :initial-element (copy-rectangle (aref self index)))
      (make-array (length self) :initial-contents (loop for cell across self
                                                     collect (copy-rectangle cell)))))

(defmethod copy-cells ((self rectangle) &optional index)
  (declare (ignorable index))
  (make-array 1 :initial-element (copy-rectangle self)))

(defmethod reset-cells ((self sdl-surface))
  (setf (slot-value self 'cells) (make-array 1 :initial-element (get-rectangle-* self)))
  (setf (cell-index self) 0))

(defmacro with-surface-slots ((var &optional surface)
                              &body body)
  `(with-surface (,var ,surface nil)
     ,@body))

(defmacro with-surfaces (bindings &rest body)
  (if bindings
      (return-with-surface bindings body)))

(defun return-with-surface (bindings body)
  (if bindings
      `(with-surface (,@(car bindings))
         ,(return-with-surface (cdr bindings) body))
      `(progn ,@body)))

(defmacro with-locked-surface ((var &optional surface)
                               &body body)
  (let ((body-value (gensym "body-value-")))
    `(let ,(when surface
                 `((,var ,surface)))
       (let ((*default-surface* ,var)
             (,body-value nil))
         (with-surface (,var ,surface nil)
           (unwind-protect 
                (progn (when (must-lock? (fp ,var))
                         (when (/= (sdl-cffi::sdl-Lock-Surface (fp ,var)) 0)
                           (error "Cannot lock surface")))
                       (setf ,body-value (progn ,@body)))
             (when (must-lock? (fp ,var))
               (Sdl-Cffi::Sdl-Unlock-Surface (fp ,var)))))
         ,body-value))))

(defmacro with-locked-surfaces (bindings &rest body)
  (if bindings
      (return-with-locked-surfaces bindings body)))

(defun return-with-locked-surfaces (bindings body)
  (if bindings
      `(with-locked-surface (,@(car bindings))
         ,(return-with-locked-surfaces (cdr bindings) body))
      `(progn ,@body)))

(defmethod width ((surface sdl-surface))
  "Returns the width of `SURFACE` as an `INTEGER`."
  (sdl-base::surf-w (fp surface)))

(defmethod height ((surface sdl-surface))
  "Returns the height of `SURFACE` as an `INTEGER`."
  (sdl-base::surf-h (fp surface)))

(defmethod x ((surface sdl-surface))
  "Returns the `X` position coordinate of `SURFACE` as an `INTEGER`."
  (x (position-rect surface)))
(defmethod (setf x) (x-val (surface sdl-surface))
  "Sets the integer `X` position coordinate of `SURFACE`."
  (setf (x (position-rect surface)) x-val))

(defmethod y ((surface sdl-surface))
  "Returns the `Y` position coordinate of `SURFACE` as an `INTEGER`."
  (y (position-rect surface)))
(defmethod (setf y) (y-val (surface sdl-surface))
  "Sets the integer `Y` position coordinate of `SURFACE`."  
  (setf (y (position-rect surface)) y-val))

(defmethod set-surface ((surface sdl-surface) (position vector))
  "See [SET-POINT](#set-point)."
  (set-surface-* surface :x (x position) :y (y position))
  surface)

(defmethod set-surface-* ((surface sdl-surface) &key x y)
  "See [SET-POINT-*](#set-point-*)."
  (when x (setf (x surface) x))
  (when y (setf (y surface) y))
  surface)

(defmethod rectangle-* ((surface sdl-surface))
  "Returns the fields `X`, `Y`, `WIDTH` and `HEIGHT` from `SURFACE` as a spread."
  (values (x surface) (y surface) (width surface) (height surface)))

(defmethod get-rectangle-* ((surface sdl-surface))
  "Returns a new `RECTANGLE` containing the `X`, `Y`, `WIDTH` and `HEIGHT` values of `SURFACE`."
  (rectangle :x (x surface)
             :y (y surface)
             :w (width surface)
             :h (height surface)))

(defun get-surface-attribute (surface attribute)
  "Returns `T` if the attribute is set for the surface surface, or returns `NIL` otherwise."
  (unless (cffi:null-pointer-p surface)
    (/= 0 (logand (cffi:foreign-slot-value surface 'sdl-cffi::sdl-surface 'sdl-cffi::flags)
                  attribute))))

(defun surface-info (surface &optional (info nil))
  "Returns information about the SDL surface `SURFACE`.

##### Parameters

* `SURFACE` is an SDL surface of type [SDL-SURFACE](#sdl-surface).
* `INFO` must be one of `NIL`, [SDL-SW-SURFACE](#sdl-sw-surface), 
[SDL-HW-SURFACE](#sdl-hw-surface), [SDL-ASYNC-BLIT](#sdl-async-blit),
[SDL-ANY-FORMAT](#sdl-any-format), [SDL-HW-PALETTE](#sdl-hw-palette), 
[SDL-DOUBLEBUF](#sdl-doublebuf), [SDL-FULLSCREEN](#sdl-fullscreen), 
[SDL-OPENGL](#sdl-opengl), [SDL-RESIZABLE](#sdl-resizable)
[SDL-HW-ACCEL](#sdl-hw-accel), [SDL-SRC-COLOR-KEY](#sdl-src-color-key),
[SDL-RLE-ACCEL](#sdl-rle-accel), [SDL-SRC-ALPHA](#sdl-src-alpha)
 or [SDL-PRE-ALLOC](#sdl-pre-alloc).

##### Returns

`INFO` when `NIL` will return a list of all enabled surface flags. Otherwise will
return `INFO` as `T` or `NIL` if supported by the surface.

##### Example

    \(SURFACE-INFO A-SURFACE '\(SDL-HW-SURFACE SDL-HW-PALETTE SDL-HW-ACCELL\)\)"
  (check-type surface sdl-surface)
  (if info
      (let ((attribute (find info (list SDL-HW-SURFACE SDL-SW-SURFACE SDL-ASYNC-BLIT SDL-ANY-FORMAT
                                        SDL-HW-PALETTE SDL-DOUBLEBUF SDL-FULLSCREEN
                                        SDL-OPENGL SDL-RESIZABLE SDL-HW-ACCEL
                                        SDL-SRC-COLOR-KEY SDL-RLE-ACCEL SDL-SRC-ALPHA
                                        SDL-PRE-ALLOC))))
        (if attribute
            (if (eq (logand attribute
                            (cffi:foreign-slot-value (fp surface) 'sdl-cffi::sdl-surface 'sdl-cffi::flags))
                    attribute)
                t
                nil)))
      (remove nil (mapcar #'(lambda (query)
                              (let ((info (first query))
                                    (description (second query)))
                                (let ((result (logand (cffi:foreign-slot-value (fp surface) 'sdl-cffi::sdl-surface 'sdl-cffi::flags)
                                                      info)))
                                  (unless (eq result 0)
                                    description))))
                          (list (list SDL-HW-SURFACE 'SDL-HW-SURFACE)
                                (list SDL-SW-SURFACE 'SDL-SW-SURFACE)
                                (list SDL-ASYNC-BLIT 'SDL-ASYNC-BLIT)
                                (list SDL-ANY-FORMAT 'SDL-ANY-FORMAT)
                                (list SDL-HW-PALETTE 'SDL-HW-PALETTE)
                                (list SDL-DOUBLEBUF 'SDL-DOUBLEBUF)
                                (list SDL-FULLSCREEN 'SDL-FULLSCREEN)
                                (list SDL-OPENGL 'SDL-OPENGL)
                                (list SDL-RESIZABLE 'SDL-RESIZABLE)
                                (list SDL-HW-ACCEL 'SDL-HW-ACCEL)
                                (list SDL-SRC-COLOR-KEY 'SDL-SRC-COLOR-KEY)
                                (list SDL-RLE-ACCEL 'SDL-RLE-ACCEL)
                                (list SDL-SRC-ALPHA 'SDL-SRC-ALPHA)
                                (list SDL-PRE-ALLOC 'SDL-PRE-ALLOC))))))

(defmethod hardware-surface-p ((surface sdl-surface))
  "Returns `T` if the surface is stored in video memory."
  (get-surface-attribute (fp surface) sdl-hw-surface))
(defmethod software-surface-p ((surface sdl-surface))
  "Returns `T` if the surface is stored in system memory."
  (get-surface-attribute (fp surface) sdl-sw-surface))


(defmethod clip-rect ((surface sdl-surface))
  (get-clip-rect :surface surface))
(defmethod (setf clip-rect) (value (surface sdl-surface))
  (set-clip-rect value :surface surface))

(defun clear-clip-rect (&optional (surface *default-surface*))
  "Removes the clipping [RECTANGLE](#rectangle)."
  (check-type surface sdl-surface)
  (set-clip-rect NIL :surface surface)
  t)

(defun get-clip-rect (&key (surface *default-surface*) (rectangle (rectangle)))
  "Returns the clipping [RECTANGLE](#rectangle)."
  (check-type surface sdl-surface)
  (check-type rectangle rectangle)
  (sdl-base::get-clip-rect (fp surface) (fp rectangle))
  rectangle)

(defun set-clip-rect (rectangle &key (surface *default-surface*))
  "See [CLIP-RECT](#clip-rect)."
  (check-type surface sdl-surface)
  (when rectangle (check-type rectangle rectangle))
  (if rectangle
      (sdl-base::set-clip-rect (fp surface) (fp rectangle))
      (sdl-base::set-clip-rect (fp surface) (cffi:null-pointer))))

(defun get-cell (&key (surface *default-surface*) (index 0))
  "Returns a [RECTANGLE](#rectangle) describing the bounds of `CELL` at the specified `INDEX`.
*Note:* When `SURFACE` is the source of a blit, only the area within the cell rectangle is drawn."
  (check-type surface sdl-surface)
  (aref (cells surface) index))

(defun current-cell (&key (surface *default-surface*))
  "Returns the [RECTANGLE](#rectangle) describing the bounds of the current `CELL`.
*Note:* When `SURFACE` is the source of a blit, only the area within the cell rectangle is drawn."
  (get-cell :surface surface :index (cell-index surface)))

(defun clear-cell (&key (surface *default-surface*) (index nil))
  "Sets the `CELL` at `INDEX` to the bounds of `SURFACE`."
  (check-type surface sdl-surface)
  (unless index
    (setf index (cell-index surface)))
  (with-rectangle (cell (get-cell :surface surface :index index) nil)
    (setf cell.x (x surface)
          cell.y (y surface)
          cell.w (width surface)
          cell.h (height surface))
    cell))

(defun set-cell (rectangle &key (surface *default-surface*) (index nil))
  "Sets the `CELL` at `INDEX` to the bounds of `RECTANGLE`.
*Note:* When `SURFACE` is the source of a blit, only the area within the cell rectangle is drawn."
  (check-type surface sdl-surface)
  (check-type rectangle rectangle)
  (unless index
    (setf index (cell-index surface)))
  (let ((rect (get-cell :surface surface :index index)))
    (sdl-base::copy-rectangle (fp rectangle) (fp rect))
    (get-cell :surface surface :index index)))

(defun set-cell-* (x y w h &key (surface *default-surface*) (index nil))
  "Sets the `CELL` at `INDEX` to a rectangle bounded by `X`, `Y`, `W` and `H`.
*Note:* When `SURFACE` is the source of a blit, only the area within the cell rectangle is drawn."
  (check-type surface sdl-surface)
  (unless index
    (setf index (cell-index surface)))
  (with-rectangle (cell (get-cell :surface surface :index index) nil)
    (setf cell.x x
          cell.y y
          cell.w w
          cell.h h)
    cell))

(defun get-surface-rect (&key (surface *default-surface*) (rectangle (rectangle)))
  (check-type surface sdl-surface)
  (check-type rectangle rectangle)
  (sdl-base::get-surface-rect (fp surface) (fp rectangle))
  rectangle)



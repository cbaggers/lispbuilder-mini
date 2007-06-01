
(in-package #:lispbuilder-sdl-base)

;; (defmacro with-pixel ((var &optional surface) &body body)
;;   (let ((fn-write-pixel (gensym "fn-write-pixel-"))
;; 	(fn-read-pixel (gensym "fn-read-pixel-")))
;;     (if (or surface (atom var))
;; 	`(let ((,@(if surface
;; 		     `(,var ,surface)
;; 		     `(,var ,var))))
;; 	   (with-locked-surface (,var)
;; 	     (let ((,fn-write-pixel (generate-write-pixel ,var))
;; 		   (,fn-read-pixel (generate-read-pixel ,var)))
;; 	       (labels ((,(intern (string-upcase (format nil "~A.write-pixel" var))) (x y color)
;; 			  (funcall ,fn-write-pixel x y color))
;; 			(,(intern (string-upcase (format nil "~A.read-pixel" var))) (x y)
;; 			  (funcall ,fn-read-pixel x y)))
;; 		 ,@body))))
;; 	(error "Var must be a symbol or variable, not a function."))))

(defmacro with-pixel ((var surface) &body body)
  (let ((surface-fp (gensym "surface-fp")))
    `(let ((,surface-fp ,surface))
       (with-locked-surface (,surface-fp)
	 (let ((,var (make-pixels)))
	   (setf (pixels-fp-writer ,var) (generate-write-pixel ,surface-fp)
		 (pixels-fp-reader ,var) (generate-read-pixel ,surface-fp))
	   (labels ((write-pixel (pixels x y color)
		      (funcall (pixels-fp-writer pixels) x y color))
		    (read-pixel (pixels x y)
		      (funcall (pixels-fp-reader pixels) x y)))
	     (declare (ignorable #'read-pixel #'write-pixel))
	     ,@body))))))

(defmacro with-pixels (bindings &rest body)
  (if bindings
      (return-with-pixels bindings body)))

(defun return-with-pixels (bindings body)
  (if bindings
      `(with-pixel (,@(car bindings))
	 ,(return-with-pixels (cdr bindings) body))
      `(progn ,@body)))

(defstruct pixels
  fp-reader fp-writer)

(defun read-pixel (pixels x y)
  (declare (ignore pixels x y))
  (error "READ-PIXEL only valid within WITH-PIXEL/S."))

(defun write-pixel (pixels x y color)
  (declare (ignore pixels x y color))
  (error "WRITE-PIXEL only valid within WITH-PIXEL/S."))

(defun generate-write-pixel (surface)
  (let* ((format (pixel-format surface))
	 (bpp (foreign-slot-value format 'sdl-cffi::SDL-Pixel-Format 'sdl-cffi::BytesPerPixel))
	 (pixel-address (foreign-slot-value surface 'sdl-cffi::SDL-Surface 'sdl-cffi::Pixels))
	 (pitch (foreign-slot-value surface 'sdl-cffi::SDL-Surface 'sdl-cffi::Pitch)))
    (labels ((offset (x y)
	       (+ (* y pitch)
		  (* x bpp)))
	     (generate-write-pixel-fn (bpp)
	       (case bpp
		 (1 #'(lambda (x y color)
			(setf (mem-aref pixel-address :unsigned-char (offset x y)) color)))
		 (2 #'(lambda (x y color)
			(setf (mem-aref pixel-address :unsigned-short (/ (offset x y) 2)) color)))
		 (3 #'(lambda (x y color)
			#-(or little-endian PC386 X86 I386)
			(let ((offset (offset x y)))
			  (setf (mem-aref pixel-address :unsigned-char offset) (logand (ash color -16) #xff)
				(mem-aref pixel-address :unsigned-char (1+ offset)) (logand (ash color -8) #xff)
				(mem-aref pixel-address :unsigned-char (+ 2 offset)) (logand color #xff)))
			#+(or little-endian PC386 X86 I386)
			(let ((offset (offset x y)))
			  (setf (mem-aref pixel-address :unsigned-char offset) (logand color #xff)
				(mem-aref pixel-address :unsigned-char (1+ offset)) (logand (ash color -8) #xff)
				(mem-aref pixel-address :unsigned-char (+ 2 offset)) (logand (ash color -16) #xff)))))
		 (4 #'(lambda (x y color)
			(setf (mem-aref pixel-address :unsigned-int (/ (offset x y) 4)) color)))
		 (otherwise (error "generate-write-pixel, bpp not 1, 2, 3 or 4")))))
      (let ((write-pixel-fn (generate-write-pixel-fn bpp)))      
	#'(lambda (x y color)
	    (funcall write-pixel-fn x y color))))))

(defun generate-read-pixel (surface)
  (let* ((format (pixel-format surface))
	 (bpp (foreign-slot-value format 'sdl-cffi::SDL-Pixel-Format 'sdl-cffi::BytesPerPixel))
	 (pixel-address (foreign-slot-value surface 'sdl-cffi::SDL-Surface 'sdl-cffi::Pixels))
	 (pitch (foreign-slot-value surface 'sdl-cffi::SDL-Surface 'sdl-cffi::Pitch)))
    (labels ((offset (x y)
	       (+ (* y pitch)
		  (* x bpp)))
	     (generate-read-pixel-fn (bpp)
	       (case bpp
		 (1 #'(lambda (x y)
			(mem-aref pixel-address :unsigned-char (offset x y))))
		 (2 #'(lambda (x y)
			(mem-aref pixel-address :unsigned-short (/ (offset x y) 2))))
		 (3 #'(lambda (x y)
			#-(or little-endian PC386 X86 I386)
			(let ((offset (offset x y)))
			  (logior (ash (mem-aref pixel-address :unsigned-char offset) 16)
				  (ash (mem-aref pixel-address :unsigned-char (1+ offset)) 8)
				  (mem-aref pixel-address :unsigned-char (+ 2 offset))))
			#+(or little-endian PC386 X86 I386)
			(let ((offset (offset x y)))
			  (logior (mem-aref pixel-address :unsigned-char offset)
				  (ash (mem-aref pixel-address :unsigned-char (1+ offset)) 8)
				  (ash (mem-aref pixel-address :unsigned-char (+ 2 offset)) 16)))))
		 (4 #'(lambda (x y)
			(mem-aref pixel-address :unsigned-int (/ (offset x y) 4))))
		 (otherwise (error "generate-write-pixel, bpp not 1, 2, 3 or 4")))))
      (let ((read-pixel-fn (generate-read-pixel-fn bpp)))
	#'(lambda (x y)
	    (cffi:with-foreign-objects ((r :unsigned-char) (g :unsigned-char) (b :unsigned-char)
					(a :unsigned-char))
	      (sdl-cffi::SDL-Get-RGBA (funcall read-pixel-fn x y) format r g b a)
	      (sdl-cffi::SDL-Map-RGBA format
				      (mem-aref r :unsigned-char)
				      (mem-aref g :unsigned-char)
				      (mem-aref b :unsigned-char)
				      (mem-aref a :unsigned-char))))))))

(defun draw-pixel (surface x y color)
  "Set the pixel at (x, y) to the given value 
   NOTE: The surface must be locked before calling this.
   Also NOTE: Have not tested 1,2,3 bpp surfaces, only 4 bpp"
  (let* ((format (foreign-slot-value surface 'sdl-cffi::SDL-Surface 'sdl-cffi::format))
	 (bpp (foreign-slot-value format 'sdl-cffi::SDL-Pixel-Format 'sdl-cffi::BytesPerPixel))
	 (offset (+ (* y (foreign-slot-value surface 'sdl-cffi::SDL-Surface 'sdl-cffi::Pitch))
		    (* x bpp)))
	 (pixel-address (foreign-slot-value surface 'sdl-cffi::SDL-Surface 'sdl-cffi::Pixels)))
    (cond
      ((= bpp 1) 
       (setf (mem-aref pixel-address :unsigned-char offset) color))
      ((= bpp 2) 
       (setf (mem-aref pixel-address :unsigned-short (/ offset 2)) color))
      ((= bpp 3) 
       #-(or little-endian PC386 X86 I386)
       (setf (mem-aref pixel-address :unsigned-char offset) (logand (ash color -16) #xff)
	     (mem-aref pixel-address :unsigned-char (1+ offset)) (logand (ash color -8) #xff)
	     (mem-aref pixel-address :unsigned-char (+ 2 offset)) (logand color #xff))
       #+(or little-endian PC386 X86 I386)
       (setf (mem-aref pixel-address :unsigned-char offset) (logand color #xff)
	     (mem-aref pixel-address :unsigned-char (1+ offset)) (logand (ash color -8) #xff)
	     (mem-aref pixel-address :unsigned-char (+ 2 offset)) (logand (ash color -16) #xff)))
      ((= bpp 4) 
       (setf (mem-aref pixel-address :unsigned-int (/ offset 4)) color))))
  (values x y))

(defun get-pixel (surface x y)
  "Get the pixel at (x, y) as a Uint32 color value
   NOTE: The surface must be locked before calling this.
   Also NOTE: Have not tested 1,2,3 bpp surfaces, only 4 bpp"
  (let* ((format (pixel-format surface))
	 (bpp (foreign-slot-value format 'sdl-cffi::SDL-Pixel-Format 'sdl-cffi::BytesPerPixel))
	 (offset (+ (* y (foreign-slot-value surface 'sdl-cffi::SDL-Surface 'sdl-cffi::Pitch))
		    (* x bpp)))
	 (pixel-address (foreign-slot-value surface 'sdl-cffi::SDL-Surface 'sdl-cffi::Pixels)))
    (cffi:with-foreign-objects ((r :unsigned-char) (g :unsigned-char) (b :unsigned-char) (a :unsigned-char))
      (sdl-cffi::SDL-Get-RGBA (cond
				((= bpp 1) 
				 (mem-aref pixel-address :unsigned-char offset))
				((= bpp 2) 
				 (mem-aref pixel-address :unsigned-short (/ offset 2)))
				((= bpp 3)
				 #-(or little-endian PC386 X86 I386)
				 (logior (ash (mem-aref pixel-address :unsigned-char offset) 16)
					 (ash (mem-aref pixel-address :unsigned-char (1+ offset)) 8)
					 (mem-aref pixel-address :unsigned-char (+ 2 offset)))
				 #+(or little-endian PC386 X86 I386)
				 (logior (mem-aref pixel-address :unsigned-char offset)
					 (ash (mem-aref pixel-address :unsigned-char (1+ offset)) 8)
					 (ash (mem-aref pixel-address :unsigned-char (+ 2 offset)) 16)))
				((= bpp 4)
				 (mem-aref pixel-address :unsigned-int (/ offset 4))))
			      format
			      r g b a)
      (sdl-cffi::SDL-Map-RGBA format 
			      (mem-aref r :unsigned-char)
			      (mem-aref g :unsigned-char)
			      (mem-aref b :unsigned-char)
			      (mem-aref a :unsigned-char)))))

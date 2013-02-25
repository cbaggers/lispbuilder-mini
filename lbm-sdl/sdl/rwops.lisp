(in-package #:lbm-sdl)

(defclass rwops (foreign-object) ()
  (:default-initargs
   :gc t
    :free #'(lambda (fp)
              (when (is-valid-ptr fp)
                (sdl-cffi::SDL-Free-RW fp))))
  (:documentation "A wrapper around a foreign SDL_RWops object.
Free using [FREE](#free)."))

(defun create-RWops-from-file (filename)
  "Creates and returns a new `RWOPS` object from the file at location `FILENAME`."
  (let ((rwops (sdl-base::create-RWops-from-file (namestring filename))))
    (when (sdl-base::is-valid-ptr rwops)
      (make-instance 'rwops :fp rwops))))

(defun create-RWops-from-byte-array (array)
  "Creates and returns a new `RWOPS` object from the Lisp array `ARRAY`."
  (let ((mem-array (cffi:foreign-alloc :unsigned-char :initial-contents array)))
    (make-instance 'rwops :fp (sdl-cffi::sdl-rw-from-mem mem-array (length array)))))

(defun file-to-byte-sequence (filepath)
  "Load a file into an Array of unsigned-byte 8"
  (with-open-file (str filepath :element-type '(unsigned-byte 8))
    (let* ((length (file-length str))
           (content (make-array (list length)
                                :element-type '(unsigned-byte 8)
                                :adjustable nil)))
      (read-sequence content str)
      content)))

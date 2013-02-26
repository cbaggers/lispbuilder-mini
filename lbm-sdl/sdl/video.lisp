;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lbm-sdl)

;;;; Functions

(defun any-format-p ()
  "Returns `T` if any pixel format is allowed."
  (get-surface-attribute (sdl-cffi::sdl-get-video-surface) sdl-any-format))

(defun opengl-context-p ()
  "Returns `T` if the video surface has an OpenGL context, or returns `NIL` otherwise."
  (get-surface-attribute (sdl-cffi::sdl-get-video-surface) sdl-opengl))

(defun double-buffered-p ()
  "Returns `T` if the video surface is double buffered, or returns `NIL` otherwise."
  (get-surface-attribute (sdl-cffi::sdl-get-video-surface) sdl-doublebuf))

(defun fullscreen-p ()
  "Returns `T` if the video surface is fullscreen. Returns `NIL` if the video surface is windowed."
  (get-surface-attribute (sdl-cffi::sdl-get-video-surface) sdl-fullscreen))

(defun resizable-p ()
  "Returns `T` if the video surface is resizable, returns `NIL` otherwise."
  (get-surface-attribute (sdl-cffi::sdl-get-video-surface) sdl-resizable))

(defun set-window-position (position)
  (let ((window-position (if (symbolp position)
                             "center"
                             (format nil "~A,~A" (elt position 0) (elt position 1)))))
    (if position
        (sdl-cffi::sdl-put-env (format nil "SDL_VIDEO_WINDOW_POS=~A" window-position))
        (sdl-cffi::sdl-put-env (format nil "SDL_VIDEO_WINDOW_POS=")))))

(defun set-video-driver (driver)
  (when driver
    (sdl-cffi::sdl-put-env (format nil "SDL_VIDEODRIVER=~A" driver))))

;; [TODO] What is this doing here?
(defun set-audio-driver (driver)
  (when driver
    (sdl-cffi::sdl-put-env (format nil "SDL_AUDIODRIVER=~A" driver))))

;;-----------------------------------------------------------------------------
;; DISPLAY
;;---------

(defclass display (display-surface) ()
  (:default-initargs))

(defmethod initialize-instance :before ((self display) &key position video-driver audio-driver)
  ;; Set the x/y window position
  (set-window-position position)
  ;; Set the video driver
  (set-video-driver video-driver)
  ;; Set the audio driver
  (set-audio-driver audio-driver)
  ;; Initialize the video subsytem, if not already initialized.
  (unless (init-video)
    (error "ERROR Cannot initialize the video subsystem. Cannot create the display surface~%")))

(defmethod initialize-instance :after 
    ((self display) &key (width 640) (height 480) (bpp 0) 
                      (title-caption "") (icon-caption "") fullscreen 
                      any-format (resizable t) no-frame (alpha-size 0) 
                      (depth-size 16) (stencil-size 8) (red-size 8)
                      (green-size 8) (blue-size 8) (buffer-size 32)
                      (double-buffer t) (swap-control t))
  (sdl:set-gl-attribute :sdl-gl-alpha-size alpha-size)
  (sdl:set-gl-attribute :sdl-gl-depth-size depth-size)
  (sdl:set-gl-attribute :sdl-gl-stencil-size stencil-size)
  (sdl:set-gl-attribute :sdl-gl-red-size red-size)
  (sdl:set-gl-attribute :sdl-gl-green-size green-size)
  (sdl:set-gl-attribute :sdl-gl-blue-size blue-size)
  (sdl:set-gl-attribute :sdl-gl-buffer-size buffer-size)
  (sdl:set-gl-attribute :sdl-gl-doublebuffer (if double-buffer 1 0))
  (sdl:set-gl-attribute :sdl-gl-swap-control (if swap-control 1 0))
  ;; Make sure the display surface is created
  (let ((surface (sdl-cffi::SDL-Set-Video-Mode 
                  (cast-to-int width)
                  (cast-to-int height)
                  bpp 
                  (sdl-base::set-flags
                   (remove nil (list (when fullscreen sdl-fullscreen)
                                     (when any-format sdl-any-format)
                                     sdl-opengl 
                                     (when resizable sdl-resizable)
                                     (when no-frame sdl-no-frame)))))))
    (if (is-valid-ptr surface)
        (setf (slot-value self 'foreign-pointer-to-object) surface
              *default-display* self)
        (setf *default-display* nil)))
  (setf (slot-value self 'position-rect) (rectangle))
  ;; Set the cells for the display
  (setf (cells self) 1)
  (setf (cell-index self) 0)
  ;; And set the captions
  (set-caption title-caption icon-caption))

(defmethod window (width height &rest rest)
  (let ((window (apply #'make-instance 'display (append rest (list :width width :height height)))))
    (when (fp window)
      window)))

(defmethod resize-window 
    (width height &key bpp 
                    (title-caption "") (icon-caption "") fullscreen 
                    any-format resizable no-frame (alpha-size 0) 
                    (depth-size 16) (stencil-size 8) (red-size 8)
                    (green-size 8) (blue-size 8) (double-buffer t)
                    (swap-control t))
  "Modifies the display, resets the input loop and clears all key events."
  (multiple-value-bind (title icon)
      (sdl:get-caption)
    (sdl:window width height
                :bpp (if bpp bpp (sdl:bit-depth sdl:*default-display*))
                :title-caption (if title-caption title-caption title)
                :icon-caption (if icon-caption icon-caption icon)
                :fullscreen fullscreen
                :any-format any-format
                :resizable resizable
                :no-frame no-frame
                :alpha-size alpha-size
                :depth-size depth-size
                :stencil-size stencil-size
                :red-size red-size
                :green-size green-size
                :blue-size blue-size
                :buffer-size buffer-size
                :double-buffer double-buffer
                :swap-control swap-control)))

(defun update-display (&optional (surface *default-display*))
  "`UPDATE-DISPLAY` will call sdl-gl-swap-buffers to update the OpenGL display."
  (sdl-cffi::sdl-gl-swap-buffers))

(defun show-cursor (state)
  "Disables the cursor when state is `NIL`, otherwise enables the cursor."
  (if state
      (sdl-cffi::SDL-Show-Cursor sdl-cffi::sdl-enable)
      (sdl-cffi::SDL-Show-Cursor sdl-cffi::sdl-disable)))

(defun get-native-window ()
  "Returns a foreign pointer to the native SDL display window."
  (let ((wm-info (cffi:foreign-alloc 'sdl-cffi::SDL-Sys-WM-info)))
    ;; Set the wm-info structure to the current SDL version.
    (sdl-cffi::set-sdl-version (cffi:foreign-slot-value wm-info 'sdl-cffi::SDL-Sys-WM-info 'sdl-cffi::version))
    (sdl-cffi::SDL-Get-WM-Info wm-info)
    ;; For Windows
    #+windows(cffi:foreign-slot-value wm-info 'sdl-cffi::SDL-Sys-WM-info 'sdl-cffi::window)
    ;; For X
    #-windows(cffi:foreign-slot-pointer (cffi:foreign-slot-pointer (cffi:foreign-slot-pointer wm-info
                                                                                              'sdl-cffi::SDL-Sys-WM-info
                                                                                              'sdl-cffi::info)
                                                                   'sdl-cffi::SDL-Sys-WM-info-info
                                                                   'sdl-cffi::x11)
                                        'sdl-cffi::SDL-Sys-WM-info-info-x11
                                        'sdl-cffi::window)))



(defun video-memory ()
  "Returns the amount of video memory of the graphics hardware. Must be called after SDL is initialized 
using [INIT-SDL](#init-sdl) or [WITH-INIT](#with-init)."
  (let ((video-info (sdl-cffi::SDL-Get-Video-Info)))
    (unless (cffi:null-pointer-p video-info)
      (cffi:foreign-slot-value video-info 'sdl-cffi::sdl-video-info 'sdl-cffi::video-mem))))

(defun video-dimensions ()
  "Returns the best video dimensions if called before a window is created, using [WINDOW](#window). 
Returns the current video dimensions if called after a window is created.
Must be called after SDL is initialized using [INIT-SDL](#init-sdl) or [WITH-INIT](#with-init)"
  (let ((video-info (sdl-cffi::SDL-Get-Video-Info)))
    (unless (cffi:null-pointer-p video-info)
      (vector (cffi:foreign-slot-value video-info 'sdl-cffi::sdl-video-info 'sdl-cffi::current-w)
              (cffi:foreign-slot-value video-info 'sdl-cffi::sdl-video-info 'sdl-cffi::current-h)))))

(defun list-modes (&rest flags)
  "Returns a LIST of vectors sorted largest to smallest containing window or screen dimensions
that will support the specified video `FLAGS`. `LIST-MODES` must be called after SDL is 
initialised using [INIT-SDL](#init-sdl) or [WITH-INIT](#with-init).

##### Parameters

* `FLAGS` is one or more of the following;
[SDL-ANY-FORMAT](#sdl-any-format), {SDL-DOUBLEBUF](#sdl-doublebuf), 
[SDL-FULLSCREEN](#sdl-fullscreen), [SDL-OPENGL](#sdl-opengl), 
[SDL-RESIZABLE](#sdl-resizable) and [SDL-NO-FRAME](#sdl-no-frame).

##### Returns

* Returns a list of `VECTOR`s of display dimensions, sorted largest to smallest, that will support 
the pixel format of surface `SURFACE`; for example `(#(1024 768) #(640 480) #(512 384) #(320 240))`.
Returns `NIL` if there are no dimensions available for a particular pixel format. 
Returns `T` if any dimension will support the pixel format and video flags.

##### Example

    \(LIST-MODES SDL-HW-SURFACE SDL-FULLSCREEN\)"
  (when (video-info)
    (let ((modes nil)
          (listmodes (sdl-cffi::SDL-List-Modes (cffi:null-pointer) (apply #'logior flags))))
      (cond
        ((cffi:null-pointer-p listmodes) nil)
        ((equal (cffi:pointer-address listmodes) 4294967295) t)
        (t
         (do ((i 0 (1+ i)))
             ((cffi:null-pointer-p (cffi:mem-aref listmodes :pointer i)) (reverse modes))
           (let ((rect (cffi:mem-ref (cffi:mem-aref listmodes :pointer i) 'sdl-cffi::sdl-rect)))
             (setf modes (cons (vector (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::w)
                                       (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::h))
                               modes)))))))))

(defun query-cursor ()
  "Queries the current state of the cursor. 
Returns `T` if the cursor is enabled and shown on the display. Returns `NIL` if the cursor 
is disabled and hidden."
  (case (sdl-cffi::SDL-Show-Cursor sdl-cffi::sdl-query)
    (sdl-cffi::sdl-disable nil)
    (sdl-cffi::sdl-enable t)))

(defun video-driver-name ()
  "Returns the driver name of the initialised video driver. The driver name is a `STRING` containing a 
one-word identifier like \"x11\" or \"windib\". Returns 'NIL' if the video driver 
is not already initialised with [INIT-SDL](#init-sdl) or [WITH-INIT](#with-init).

##### Example

    \(sdl:with-init \(\)
      \(sdl:video-driver-name\)\)
    >> \"windib\""
  (let ((string-return-val (cffi:with-foreign-pointer-as-string (str 100)
                             (sdl-cffi::sdl-video-driver-name str 100))))
    (if (equal string-return-val "")
        nil
        string-return-val)))

(defun set-gl-attribute (attribute value)
  (unless (video-init-p)
    (init-video))
  (sdl-cffi::sdl-gl-set-attribute attribute value))

;; [TODO] Fill this out?
(defun get-gl-attribute (attribute value)
  (declare (ignore attribute value))
  ;; TBD
  )

(defun set-caption (window-caption icon-caption)
  "Sets the caption text for window bar, and icon."
  (unless (cffi:null-pointer-p (sdl-cffi::SDL-Get-Video-Info))
    (sdl-cffi::sdl-wm-set-caption window-caption icon-caption)))

(defun get-caption ()
  "Returns the caption and icon text for the display surface as a spread; \(VALUES title-caption icon caption\)"
  (unless (cffi:null-pointer-p (sdl-cffi::SDL-Get-Video-Info))
    (cffi:with-foreign-objects ((title-handle :pointer)
                                (icon-handle :pointer))
      (sdl-cffi::SDL-WM-Get-Caption title-handle icon-handle)
      (let ((foreign-title-caption (cffi:mem-aref title-handle :pointer))
            (foreign-icon-caption (cffi:mem-aref icon-handle :pointer)))
        (values (cffi:foreign-string-to-lisp foreign-title-caption)
                (cffi:foreign-string-to-lisp foreign-icon-caption))))))

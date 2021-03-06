;;;; lbm-sdl

(in-package #:cl-user)

(defpackage #:lbm-sdl
  (:use #:cl #:cffi)
  (:nicknames #:sdl)
  (:documentation "The main package of `lbm-sdl'.")

  (:import-from #:lbm-sdl-base
   lbm-sdl-base::is-valid-ptr
   lbm-sdl-base::1/0->t/nil)

  (:import-from #:lbm-sdl-cffi
   lbm-sdl-cffi::sdl-get-ticks

   lbm-sdl-cffi::sdl-init-everything
   lbm-sdl-cffi::sdl-init-video
   lbm-sdl-cffi::sdl-init-cdrom
   lbm-sdl-cffi::sdl-init-audio 
   lbm-sdl-cffi::sdl-init-timer
   lbm-sdl-cffi::sdl-init-joystick
   lbm-sdl-cffi::sdl-init-eventthread
   lbm-sdl-cffi::sdl-init-noparachute

   lbm-sdl-cffi::sdl-any-format
   lbm-sdl-cffi::sdl-doublebuf
   lbm-sdl-cffi::sdl-fullscreen
   lbm-sdl-cffi::sdl-opengl
   lbm-sdl-cffi::sdl-resizable
   lbm-sdl-cffi::sdl-no-frame
   lbm-sdl-cffi::sdl-hw-accel
   lbm-sdl-cffi::sdl-rle-accel-ok
   lbm-sdl-cffi::sdl-rle-accel
   lbm-sdl-cffi::sdl-pre-alloc
   lbm-sdl-cffi::sdl-yv12-overlay
   lbm-sdl-cffi::sdl-iyuv-overlay
   lbm-sdl-cffi::sdl-yuy2-overlay
   lbm-sdl-cffi::sdl-uyvy-overlay
   lbm-sdl-cffi::sdl-yvyu-overlay

   lbm-sdl-cffi::Num-Joysticks
   lbm-sdl-cffi::SDL-Joystick-Name
   lbm-sdl-cffi::SDL-WM-GRAB-INPUT

   lbm-sdl-cffi::SDL-BUTTON-LEFT
   lbm-sdl-cffi::SDL-BUTTON-MIDDLE
   lbm-sdl-cffi::SDL-BUTTON-RIGHT
   lbm-sdl-cffi::SDL-BUTTON-WHEEL-UP
   lbm-sdl-cffi::SDL-BUTTON-WHEEL-DOWN
   lbm-sdl-cffi::SDL-BUTTON-X1
   lbm-sdl-cffi::SDL-BUTTON-X2
   ;; (cl:defconstant SDL-BUTTON-WHEEL-LEFT 6)
   ;; (cl:defconstant SDL-BUTTON-WHEEL-RIGHT 7)

   lbm-sdl-cffi::sdl-gl-get-proc-address

   #:sdl-library-version
   #:version-number
   #:get-library-version
   #:sdl-wrapper-version
   #:sdl-version-at-least
   #:image-library-version
   #:image-wrapper-version
   #:image-version-at-least

   #:*image-loaded-p*
   #:*glue-loaded-p*)

  (:export

   ;; globals.lisp
   #:*default-display*
   #:*default-rectangle*

   #:*default-audio-path*
   #:*default-image-path*
   #:*quit-on-exit*

   #:*mixer*
   #:*managed-audio*
   #:+DEFAULT-FORMAT+
   #:+CHANNELS+
   #:+DEFAULT-FREQUENCY+
   #:+DEFAULT-CHANNELS+
   #:+MAX-VOLUME+
   #:+DEFAULT-SAMPLE-BUFFER+
   #:+CHANNEL-POST+
   #:+MAX-AUDIO-16+
   #:+MIN-AUDIO-16+
   #:+MAX-AUDIO-8+
   #:+MIN-AUDIO-8+

   ;; init.lisp
   #:with-init
   #:initialize-subsystems-on-startup
   #:quit-subsystems-on-exit
   #:quit-on-exit-p
   #:quit-on-exit
   #:list-subsystems
   #:return-subsystems-of-status
   #:init-subsystems
   #:quit-subsystems
   #:initialized-subsystems-p

   #:sdl-init-p
   #:video-init-p
   #:audio-init-p
   #:joystick-init-p
   #:cdrom-init-p

   #:init-sdl
   #:init-video
   #:init-audio
   #:init-joysick
   #:init-cdrom

   #:quit
   #:quit-video
   #:quit-audio
   #:quit-joysick
   #:quit-cdrom

   #:load-library
   
   ;;generics.lisp
   #:fp
   #:fp-position
   #:width
   #:height
   #:x
   #:y
   #:x2
   #:y2
   #:cached-surface
   #:free
   #:w
   #:h

   #:rectangle-*
   #:get-rectangle
   #:set-rectangle
   #:set-rectangle-*

   #:set-surface
   #:set-surface-*

   #:free-cached-surface

   #:image-p
   #:image-type-of

   ;; base.lisp
   #:gc-p
   #:cast-to-int

   ;; events.lisp
   #:collect-event-types
   #:case-events
   #:get-event-type
   #:event-type
   #:event=
   #:active-event-p
   #:key-down-event-p
   #:key-up-event-p
   #:mouse-motion-event-p
   #:mouse-button-down-event-p
   #:mouse-button-up-event-p
   #:joy-axis-motion-event-p
   #:joy-button-down-event-p
   #:joy-button-up-event-p
   #:joy-hat-motion-event-p
   #:joy-ball-motion-event-p
   #:video-resize-event-p
   #:video-expose-event-p
   #:sys-wm-event-p
   #:quit-event-p
   #:user-event-p
   #:idle-event-p

   #:set-event-filter
   #:remove-event-filter
   #:remove-all-event-filters
   #:enable-event-filters
   #:disable-event-filters
   #:new-event
   #:free-event
   #:quit-requested-p
   #:push-quit-event
   #:push-user-event
   #:pump-events
   #:get-event
   #:with-active-event
   #:with-key-down-event
   #:with-key-up-event
   #:with-mouse-motion-event
   #:with-mouse-button-down-event
   #:with-mouse-button-up-event
   #:with-joy-axis-motion-event
   #:with-joy-button-down-event
   #:with-joy-button-up-event
   #:with-joy-hat-motion-event
   #:with-joy-ball-motion-event
   #:with-video-resize-event
   #:with-user-event
   
   #:active-gain
   #:active-state
   #:key-state
   #:key-scancode
   #:key-key
   #:key-mod
   #:key-mod-key
   #:key-unicode
   #:mouse-motion-button
   #:mouse-motion-x
   #:mouse-motion-y
   #:mouse-motion-x-rel
   #:mouse-motion-y-rel
   #:mouse-button-button
   #:mouse-button-state
   #:mouse-button-x
   #:mouse-button-y

   #:mouse-motion-event-state

   #:joy-axis-motion-which
   #:joy-axis-motion-axis
   #:joy-axis-motion-value
   #:joy-hat-motion-which
   #:joy-hat-motion-axis
   #:joy-hat-motion-value
   #:joy-ball-motion-which
   #:joy-ball-motion-ball
   #:joy-ball-motion-x-rel
   #:joy-ball-motion-y-rel
   #:joy-button-which
   #:joy-button-button
   #:joy-button-state
   #:user-type
   #:user-code
   #:user-data1
   #:user-data2
   #:video-resize-w
   #:video-resize-h

   ;; image.lisp
   #:supported-image-formats
   #:image-init-p
   #:init-image
   #:quit-image
   #:image-p
   #:image-type-of
   #:image-supported-p
   #:load-image
   #:save-image
   #:load-and-convert-image
   #:image-library-version

   ;; rectangle.lisp
   #:rectangle
   #:rectangle-from-edges-*
   #:rectangle-from-edges
   #:rectangle-from-midpoint-*
   #:with-rectangle
   #:with-rectangles
   #:copy-rectangle

   ;; surfaces.lisp
   #:sdl-surface
   #:display-surface

   ;; video.lisp
   #:window
   #:resize-window
   #:update-display
   #:set-video-mode
   #:display-cursor
   #:video-driver-name
   #:query-cursor
   #:show-cursor
   #:video-info
   #:get-native-window
   #:list-modes
   #:video-memory
   #:video-dimensions
   #:set-gl-attribute
   #:set-caption
   #:get-caption
   #:any-format-p
   #:opengl-context-p
   #:double-buffered-p
   #:fullscreen-p
   #:resizable-p
   #:set-video-driver
   #:set-audio-driver

   ;; rwops.lisp
   #:rwops
   #:create-RWops-from-file
   
   ;; audio.lisp
   #:audio-spec
   #:print-object
   #:sample-frequency
   #:audio-format
   #:output-channels
   #:audio-silence
   #:audio-buffer-size
   #:audio-buffer-size-calculated
   #:spec-callback
   #:spec-user-data
   #:audio-cvt
   #:audio-buffer
   #:audio
   #:audio-buffer-handle
   #:audio-remaining
   #:audio-position
   #:audio-paused-p
   #:volume
   #:callback-finished
   #:copy-audio
   #:audio-length
   #:register-audio-finished
   #:unregister-audio-finished
   #:audio-finished-callback
   #:resume-audio
   #:halt-sample
   #:rewind-audio
   #:audio-playing-p
   #:audio-playable-p
   #:audio-opened-p
   #:mixer
   #:requested-sample-frequency
   #:requested-audio-format
   #:requested-output-channels
   #:requested-audio-buffer-size
   #:audio-spec
   #:mixer-opened-p
   #:output-audio-buffer-size
   #:output-buffer
   #:audio-volume
   #:open-audio
   #:play-audio
   #:pause-audio
   #:close-audio
   #:load-sample
   #:build-audio-cvt
   #:load-audio
   #:default-fill-audio-buffer
   #:audio-driver-name

   ;; keys.lisp
   #:unicode-enabled-p
   #:enable-unicode
   #:disable-unicode
   #:enable-key-repeat
   #:disable-key-repeat
   #:key-repeat-enabled-p
   #:key-repeat-delay
   #:key-repeat-interval
   #:get-key-state
   #:get-keys-state
   #:key-state-p
   #:get-mod-state
   #:get-mods-state
   #:mod-state-p
   #:key=
   #:modifier=
   #:modifier-p
   #:key-down-p
   #:keys-down-p
   #:mod-down-p
   #:mods-down-p

   ;; mouse.lisp
   #:mouse-left
   #:mouse-middle
   #:mouse-right
   #:mouse-wheel-up
   #:mouse-wheel-down
   #:mouse-x1
   #:mouse-x2
   #:mouse-x
   #:mouse-y
   #:mouse-position
   #:mouse-relative-position
   #:mouse-buttons
   #:mouse-left-p
   #:mouse-right-p
   #:mouse-middle-p
   #:mouse-wheel-up-p
   #:mouse-wheel-down-p
   #:mouse-x1-p
   #:mouse-x2-p

   ;; active.lisp
   #:mouse-gain-focus-p
   #:input-gain-focus-p
   #:active-gain-p
   #:mouse-focus-p
   #:input-focus-p
   #:active-p
   #:app-mouse-focus
   #:app-input-focus
   #:app-active
   
   ;; Imports from lbm-sdl-cffi
   #:sdl-get-ticks
   #:sdl-opengl
   #:sdl-init-everything
   #:sdl-init-video
   #:sdl-init-cdrom
   #:sdl-init-audio 
   #:sdl-init-timer
   #:sdl-init-joystick
   #:sdl-init-eventthread
   #:sdl-init-noparachute
   
   #:sdl-any-format
   #:sdl-doublebuf
   #:sdl-fullscreen
   #:sdl-opengl
   #:sdl-resizable
   #:sdl-no-frame

   #:sdl-pre-alloc
   #:sdl-yv12-overlay
   #:sdl-iyuv-overlay
   #:sdl-yuy2-overlay
   #:sdl-uyvy-overlay
   #:sdl-yvyu-overlay

   #:num-joysticks
   #:sdl-joystick-name

   #:sdl-wm-grab-input

   #:sdl-button-left
   #:sdl-button-middle
   #:sdl-button-right
   #:sdl-button-wheel-up
   #:sdl-button-wheel-down
   #:sdl-button-x1
   #:sdl-button-x2

   #:sdl-gl-get-proc-address

   #:sdl-library-version
   #:version-number
   #:get-library-version
   #:sdl-wrapper-version
   #:sdl-version-at-least
   #:image-library-version
   #:image-wrapper-version
   #:image-version-at-least
   
   #:*image-loaded-p*
   #:*glue-loaded-p*
   
   ;; Imports from lbm-sdl-base  

   #:with-events
   #:push-quit-event
   #:key=
   #:is-valid-ptr
   #:push-user-event

   ;; I put this back for now
   #:bit-depth))



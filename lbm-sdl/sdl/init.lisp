(in-package #:lbm-sdl)

;;;; Functions

(defmacro with-init (flags &body body)
  `(block nil
     (unwind-protect
          (when (init-sdl :flags ',flags)
            ,@body)
       (close-audio)
       (quit :flags ',flags))))

(defun quit-on-exit-p ()
  "Returns `T` if the SDL library will be uninitialised in a call to [QUIT](#quit), or [WITH-INIT](#with-init). 
Returns `NIL` otherwise."
  *quit-on-exit*)
(defun set-quit-on-exit (val)
  (setf *quit-on-exit* val))
(defsetf quit-on-exit set-quit-on-exit)

(defun initialize-subsystems-on-startup (&rest flags)
  "Sets the SDL subsystems that must be initialized in calls to [INIT-SUBSYSTEMS](#init-subsystems).

##### Parameters

* `FLAGS` may be one or more of: `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-JOYSTICK` and `SDL-INIT-NOPARACHUTE`, or `NIL` to not initialize any
subsystems.

##### Returns

* Returns an INTEGER bitmask of the SDL subsystems in `FLAGS`.

##### Example

    \(INITIALIZE-SUBSYSTEMS-ON-STARTUP SDL:SDL-INIT-VIDEO SDL:SDL-INIT-CDROM\)"
  (setf *initialize-subsystems-on-startup* (if flags
                                               (apply #'logior flags)
                                               0)))

(defun quit-subsystems-on-exit (&rest flags)
  "Sets one or more SDL subsystems that must *not* be uninitialized in calls to
[QUIT-SUBSYSTEMS](#quit-subsystems).

##### Parameters

* `FLAGS` may be one or more of: `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-JOYSTICK` and `SDL-INIT-NOPARACHUTE` , or `NIL` to not uninitialize any
subsystems.

##### Returns

* Returns an INTEGER bitmask of the SDL subsystems in `FLAGS`.

##### Example

    \(QUIT-SUBSYSTEMS-ON-EXIT SDL:SDL-INIT-VIDEO SDL:SDL-INIT-CDROM\)"
  (setf *quit-subsystems-on-exit* (if flags
                                      (apply #'logior flags)
                                      0)))

(defun list-subsystems (flag)
  "Returns a list of SDL subsystems that are specified in `FLAGS`.

`FLAGS` is an `INTEGER` bitmask containing the logior of zero or more of: `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`,
`SDL-INIT-JOYSTICK` and `SDL-INIT-NOPARACHUTE`."
  (let ((subsystems nil))
    (when (/= 0 (logand flag sdl-cffi::sdl-init-video))
      (push (list 'sdl-cffi::sdl-init-video
                  sdl-cffi::sdl-init-video)
            subsystems))
    (when (/= 0 (logand flag sdl-cffi::sdl-init-cdrom))
      (push (list 'sdl-cffi::sdl-init-cdrom
                  sdl-cffi::sdl-init-cdrom)
            subsystems))
    (when (/= 0 (logand flag sdl-cffi::sdl-init-audio))
      (push (list 'sdl-cffi::sdl-init-audio
                  sdl-cffi::sdl-init-audio)
            subsystems))
    (when (/= 0 (logand flag sdl-cffi::sdl-init-timer))
      (push (list 'sdl-cffi::sdl-init-timer
                  sdl-cffi::sdl-init-timer)
            subsystems))
    (when (/= 0 (logand flag sdl-cffi::sdl-init-joystick))
      (push (list 'sdl-cffi::sdl-init-joystick
                  sdl-cffi::sdl-init-joystick)
            subsystems))
    (when (/= 0 (logand flag sdl-cffi::sdl-init-eventthread))
      (push (list 'sdl-cffi::sdl-init-eventthread
                  sdl-cffi::sdl-init-eventthread)
            subsystems))
    (when (/= 0 (logand flag sdl-cffi::sdl-init-noparachute))
      (push (list 'sdl-cffi::sdl-init-noparachute
                  sdl-cffi::sdl-init-noparachute)
            subsystems))
    subsystems))

(defun return-subsystems-of-status (flags status)
  "Returns the `STATUS` of the the specified SDL subsystems in `FLAGS`. 

##### Parameters
* `FLAGS` must contain one or more of: `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`,
`SDL-INIT-JOYSTICK`.
* `STATUS` when `T` returns the specified initialized subsystems.
`STATUS` when `NIL`, returns the specified uninitialised subsystems."
  (let ((initialized (list-subsystems (sdl-cffi::sdl-was-init sdl-cffi::sdl-init-everything)))
        (queried (list-subsystems (sdl-base::set-flags flags)))
        (to-initialize 0))
    (mapc #'(lambda (subsystem)
              (let ((value (second subsystem)))
                (setf to-initialize (logior to-initialize
                                            value))))
          (if status
              (intersection queried initialized :key #'car)
              (set-difference queried initialized :key #'car)))
    to-initialize))

(defun init-subsystems (subsystems &optional force)
  "Initializes only the SDL subsystems specified in `FLAGS` that are not
already initialized. 
`subsystems` is a list containing the one or more of:
`SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-JOYSTICK`, and
`SDL-INIT-NOPARACHUTE`.

`INIT-SUBSYSTEMS` can be called only after SDL is succesfully initialized by [INIT-SDL](#init-sdl)."
  (if (/= 0 (sdl-base::set-flags *initialize-subsystems-on-startup*))
      (setf subsystems (sdl-base::set-flags *initialize-subsystems-on-startup*)))
  (if (= -1 (if force
                (sdl-cffi::sdl-init-subsystem (sdl-base::set-flags subsystems))
                (sdl-cffi::sdl-init-subsystem (return-subsystems-of-status (sdl-base::set-flags subsystems) nil))))
      nil
      t))

(defun quit-subsystems (subsystems &optional force)
  "Uninitializes only the SDL subsystems specified in the `FLAGS` that are
already initialized.
`subsystems` is a list containing the one or more of:
`SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-JOYSTICK` and `SDL-INIT-NOPARACHUTE`.

`QUIT-SUBSYSTEMS` can be called only after SDL is successfully intialized using [INIT-SDL](#init-sdl)."
  (if (/= 0 (sdl-base::set-flags *quit-subsystems-on-exit*))
      ;; Close only specified subsystems, if specified
      (quit-subsystems (sdl-base::set-flags *quit-subsystems-on-exit*) force)
      (if force
          (sdl-cffi::sdl-init-subsystem (sdl-base::set-flags subsystems))
          (sdl-cffi::sdl-quit-subsystem (return-subsystems-of-status (sdl-base::set-flags subsystems) t)))))

(defun initialized-subsystems-p (&optional subsystem)
  "Returns a list of the initialized SDL subsystems."
  (if subsystem
      (if (and (/= 0 (logand (sdl-cffi::sdl-was-init sdl-cffi::sdl-init-everything)
                             (sdl-base::set-flags subsystem)))
               (>= (sdl-cffi::sdl-was-init sdl-cffi::sdl-init-everything) (sdl-base::set-flags subsystem)))
          (list-subsystems (sdl:return-subsystems-of-status (sdl-base::set-flags subsystem) t)))
      (list-subsystems (sdl:return-subsystems-of-status (sdl-base::set-flags sdl:sdl-init-everything) t))))

(defun init-sdl (&key force video cdrom audio joystick 
                   no-parachute)
  "Initalizes the SDL library."
  (let ((init? (when (sdl-base::init-sdl)
                 (init-subsystems 
                  (sdl-base::set-flags 
                   (remove nil (list (when video sdl-init-video)
                                     (when cdrom sdl-init-cdrom)
                                     (when audio sdl-init-audio)
                                     (when joystick 
                                       sdl-init-joystick)
                                     (when no-parachute 
                                       sdl-init-noparachute)))) 
                  force))))
    (setf cl-opengl-bindings:*gl-get-proc-address* 
          #'sdl-cffi::sdl-gl-get-proc-address)
    init?))

(defun quit (&key flags force
                   video cdrom audio joystick no-parachute)
  (if *quit-on-exit*
      ;; Quit SDL if *quit-on-exit*, otherwise quit each subsystem
      (sdl-cffi::sdl-quit)
      (let ((flags (sdl-base::set-flags flags)))
        ;; Close only specified subsystems
        (when (> 0 flags)
          (setf flags (sdl-base::set-flags (remove nil
                                                   (list (when video sdl-init-video)
                                                         (when cdrom sdl-init-cdrom)
                                                         (when audio sdl-init-audio)
                                                         (when joystick sdl-init-joystick)
                                                         (when no-parachute sdl-init-noparachute))))))
        ;; Or close all subsystems
        (when (= 0 flags)
          (setf flags (sdl-cffi::sdl-was-init sdl-cffi::sdl-init-everything)))
        (quit-subsystems flags force))))

(defun sdl-init-p ()
  (initialized-subsystems-p))
(defun video-init-p ()
  (when (/= 0 (logand (sdl-cffi::sdl-was-init sdl-init-everything)
                      sdl-init-video))
    t))
(defun audio-init-p ()
  (when (/= 0 (logand (sdl-cffi::sdl-was-init sdl-init-everything)
                      sdl-init-audio))
    t))
(defun joystick-init-p ()
  (when (/= 0 (logand (sdl-cffi::sdl-was-init sdl-init-everything)
                      sdl-init-joystick))
    t))
(defun cdrom-init-p ()
  (when (/= 0 (logand (sdl-cffi::sdl-was-init sdl-init-everything)
                      sdl-init-cdrom))
    t))

(defun init-video (&optional force)
  (init-subsystems sdl-init-video force))
(defun init-audio (&optional force)
  (init-subsystems sdl-init-audio force))
(defun init-joystick (&optional force)
  (init-subsystems sdl-init-joystick force))
(defun init-cdrom (&optional force)
  (init-subsystems sdl-init-cdrom force))

(defun quit-video (&optional force)
  (quit-subsystems sdl-init-video force))
(defun quit-audio (&optional force)
  (quit-subsystems sdl-init-audio force))
(defun quit-joystick (&optional force)
  (quit-subsystems sdl-init-joystick force))
(defun quit-cdrom (&optional force)
  (quit-subsystems sdl-init-cdrom force))

(defun load-library ()
  (sdl-cffi::load-sdl-library)
  (sdl-cffi::load-image-library))

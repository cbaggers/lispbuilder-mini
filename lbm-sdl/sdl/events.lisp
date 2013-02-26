;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lbm-sdl)

(defun collect-event-types ()
  (let* ((x (sdl:new-event))
        (event-types (loop until (= 0 (sdl-cffi::sdl-poll-event x))
                        collect (sdl:event-type x))))
    (sdl:free-event x)
    event-types))

(defmacro case-events ((event-var) &body cases)
  (if (symbolp event-var)
      `(let ((,event-var (sdl:new-event)))
         (loop :while (not (eq (sdl-cffi::SDL-Poll-Event 
                                ,event-var) 0))
            :do (case (sdl:event-type ,event-var)
                  ,@cases))
         (sdl:free-event ,event-var))
      (error "event-var must be a symbol")))

(defun create-event (slot-fn)
  (let ((hash-table (make-hash-table)))
    (loop for (slot fn) in slot-fn
       do (setf (gethash slot hash-table) fn))
    hash-table))

(defun create-events (event-list)
  (let ((hash-table (make-hash-table)))
    (loop for (event slots) in event-list
       do (setf (gethash event hash-table) slots))
    hash-table))

(defparameter *events*
  (create-events (list (list :active-event
                             (create-event (list (list :gain 'active-gain)
                                                 (list :state 'active-state))))
                       (list :key-down-event
                             (create-event (list (list :state 'key-state)
                                                 (list :scancode 'key-scancode)
                                                 (list :key 'key-key)
                                                 (list :mod 'key-mod)
                                                 (list :mod-key 'key-mod-key)
                                                 (list :unicode 'key-unicode))))
                       (list :key-up-event
                             (create-event (list (list :state 'key-state)
                                                 (list :scancode 'key-scancode)
                                                 (list :key 'key-key)
                                                 (list :mod 'key-mod)
                                                 (list :mod-key 'key-mod-key)
                                                 (list :unicode 'key-unicode))))
                       (list :mouse-motion-event
                             (create-event (list (list :state 'mouse-motion-button)
                                                 (list :x 'mouse-motion-x)
                                                 (list :y 'mouse-motion-y)
                                                 (list :x-rel 'mouse-motion-x-rel)
                                                 (list :y-rel 'mouse-motion-y-rel))))
                       (list :mouse-button-down-event
                             (create-event (list (list :button 'mouse-button-button)
                                                 (list :state 'mouse-button-state)
                                                 (list :x 'mouse-button-x)
                                                 (list :y 'mouse-button-y))))
                       (list :mouse-button-up-event
                             (create-event (list (list :button 'mouse-button-button)
                                                 (list :state 'mouse-button-state)
                                                 (list :x 'mouse-button-x)
                                                 (list :y 'mouse-button-y))))
                       (list :joy-axis-motion-event
                             (create-event (list (list :which 'joy-axis-motion-which)
                                                 (list :axis 'joy-axis-motion-axis)
                                                 (list :value 'joy-axis-motion-value))))
                       (list :joy-button-down-event
                             (create-event (list (list :which 'joy-button-which)
                                                 (list :button 'joy-button-button)
                                                 (list :state 'joy-button-state))))
                       (list :joy-button-up-event
                             (create-event (list (list :which 'joy-button-which)
                                                 (list :button 'joy-button-button)
                                                 (list :state 'joy-button-state))))
                       (list :joy-hat-motion-event
                             (create-event (list (list :which 'joy-hat-motion-which)
                                                 (list :axis 'joy-hat-motion-axis)
                                                 (list :value 'joy-hat-motion-value))))
                       (list :joy-ball-motion-event
                             (create-event (list (list :which 'joy-ball-motion-which)
                                                 (list :ball 'joy-ball-motion-ball)
                                                 (list :x-rel 'joy-ball-motion-x-rel)
                                                 (list :y-rel 'joy-ball-motion-y-rel))))
                       (list :video-resize-event
                             (create-event (list (list :w 'video-resize-w)
                                                 (list :h 'video-resize-h))))
                       (list :video-expose-event
                             (create-event nil))
                       (list :sys-wm-event
                             (create-event nil))
                       (list :user-event
                             (create-event (list (list :type 'user-type)
                                                 (list :code 'user-code)
                                                 (list :data1 'user-data1)
                                                 (list :data2 'user-data2)))))))


(defun get-event-type (sdl-event)
  (cffi:foreign-enum-keyword 'sdl-cffi::Event-Type (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type)))

(defun event-type (sdl-event)
  (if (keywordp sdl-event)
      nil
      (get-event-type sdl-event)))

(defun event= (sdl-event event-type &optional event-type-end)
  "Returns `T` if `SDL-EVENT` is of `EVENT-TYPE`.
`EVENT-TYPE` must be one of
:NO-EVENT, :ACTIVE-EVENT, :KEY-DOWN-EVENT, :KEY-UP-EVENT, :MOUSE-MOTION-EVENT, 
:MOUSE-BUTTON-DOWN-EVENT, :MOUSE-BUTTON-UP-EVENT, :JOY-AXIS-MOTION-EVENT, :JOY-BALL-MOTION-EVENT, 
:JOY-HAT-MOTION-EVENT, :JOY-BUTTON-DOWN-EVENT, :JOY-BUTTON-UP-EVENT, :QUIT-EVENT, 
:SYS-WM-EVENT, :VIDEO-RESIZE-EVENT, :VIDEO-EXPOSE-EVENT, :USER-EVENT."
  (unless (keywordp sdl-event)
    (if event-type-end
        (and (>= (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type)
                 (cffi:foreign-enum-value 'sdl-cffi::Event-Type event-type))
             (< (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type)
                (- (cffi:foreign-enum-value 'sdl-cffi::Event-Type event-type-end) 1)))
        (= (cffi:foreign-enum-value 'sdl-cffi::Event-Type event-type)
           (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type)))))

(defun active-event-p (event)
  (eq (event-type event) :ACTIVE-EVENT))
(defun key-down-event-p (event)
  (eq (event-type event) :KEY-DOWN-EVENT))
(defun key-up-event-p (event)
  (eq (event-type event) :KEY-UP-EVENT))
(defun mouse-motion-event-p (event)
  (eq (event-type event) :MOUSE-MOTION-EVENT))
(defun mouse-button-down-event-p (event)
  (eq (event-type event) :MOUSE-BUTTON-DOWN-EVENT))
(defun mouse-button-up-event-p (event)
  (eq (event-type event) :MOUSE-BUTTON-UP-EVENT))
(defun joy-axis-motion-event-p (event)
  (eq (event-type event) :JOY-AXIS-MOTION-EVENT))
(defun joy-button-down-event-p (event)
  (eq (event-type event) :JOY-BUTTON-DOWN-EVENT))
(defun joy-button-up-event-p (event)
  (eq (event-type event) :JOY-BUTTON-UP-EVENT))
(defun joy-hat-motion-event-p (event)
  (eq (event-type event) :JOY-HAT-MOTION-EVENT))
(defun joy-ball-motion-event-p (event)
  (eq (event-type event) :JOY-BALL-MOTION-EVENT))
(defun video-resize-event-p (event)
  (eq (event-type event) :VIDEO-RESIZE-EVENT))
(defun video-expose-event-p (event)
  (eq (event-type event) :VIDEO-EXPOSE-EVENT))
(defun sys-wm-event-p (event)
  (eq (event-type event) :SYS-WM-EVENT))
(defun quit-event-p (event)
  (eq (event-type event) :QUIT-EVENT))
(defun user-event-p (event)
  (event= event :USER-EVENT :NUM-EVENTS))
(defun idle-event-p (event)
  (eq (event-type event) :idle))

(defun active-gain (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Active-Event 'sdl-cffi::gain))

(defun active-state (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Active-Event 'sdl-cffi::state))

(defun key-state (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Keyboard-Event 'sdl-cffi::state))

(defun key-scancode (sdl-event)
  (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
                                                      'sdl-cffi::sdl-keyboard-event
                                                      'sdl-cffi::keysym)
                           'sdl-cffi::sdl-key-sym 'sdl-cffi::scancode))

(defun key-key (sdl-event)
  (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
                                                      'sdl-cffi::sdl-keyboard-event
                                                      'sdl-cffi::keysym)
                           'sdl-cffi::sdl-key-sym 'sdl-cffi::sym))

(defun key-mod (sdl-event)
  (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
                                                      'sdl-cffi::sdl-keyboard-event
                                                      'sdl-cffi::keysym)
                           'sdl-cffi::sdl-key-sym 'sdl-cffi::mod))

(defun key-mod-key (sdl-event)
  (let ((mod-state (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
                                                                       'sdl-cffi::sdl-keyboard-event
                                                                       'sdl-cffi::keysym)
                                            'sdl-cffi::sdl-key-sym 'sdl-cffi::mod)))
    (remove nil (loop for key in (cffi:foreign-enum-keyword-list 'sdl-cffi::sdl-mod)
                   collect (when (> (logand mod-state
                                            (foreign-enum-value 'sdl-cffi::sdl-mod key))
                                    0)
                             key)))))

(defun key-unicode (sdl-event)
  (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
                                                      'sdl-cffi::sdl-keyboard-event
                                                      'sdl-cffi::keysym)
                           'sdl-cffi::sdl-key-sym 'sdl-cffi::unicode))

(defun mouse-motion-button (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::state))
(defun mouse-motion-x (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::x))
(defun mouse-motion-y (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::y))
(defun mouse-motion-x-rel (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::xrel))
(defun mouse-motion-y-rel (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::yrel))

(defun mouse-button-button (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::button))
(defun mouse-button-state (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::state))
(defun mouse-button-x (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::x))
(defun mouse-button-y (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::y))

(defun joy-axis-motion-which (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Axis-Event 'sdl-cffi::which))
(defun joy-axis-motion-axis (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Axis-Event 'sdl-cffi::axis))
(defun joy-axis-motion-value (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Axis-Event 'sdl-cffi::value))

(defun joy-hat-motion-which (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-joy-hat-event 'sdl-cffi::which))
(defun joy-hat-motion-axis (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-joy-hat-event 'sdl-cffi::hat))
(defun joy-hat-motion-value (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-joy-hat-event 'sdl-cffi::value))

(defun joy-ball-motion-which (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Ball-Event 'sdl-cffi::which))
(defun joy-ball-motion-ball (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Ball-Event 'sdl-cffi::ball))
(defun joy-ball-motion-x-rel (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Ball-Event 'sdl-cffi::xrel))
(defun joy-ball-motion-y-rel (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Ball-Event 'sdl-cffi::yrel))

(defun joy-button-which (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::which))
(defun joy-button-button (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::button))
(defun joy-button-state (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::state))


(defun user-type (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-User-Event 'sdl-cffi::type))
(defun user-code (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-User-Event 'sdl-cffi::code))
(defun user-data1 (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-User-Event 'sdl-cffi::data1))
(defun user-data2 (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-User-Event 'sdl-cffi::data2))


(defun video-resize-w (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Resize-Event 'sdl-cffi::w))
(defun video-resize-h (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Resize-Event 'sdl-cffi::h))

(defun return-args (params event event-fp)
  (loop for (user-val arg) in params
     collect (cond
               ((gethash (if (keywordp arg) arg (intern (symbol-name arg) "KEYWORD")) event)
                `(,user-val (,(gethash (intern (symbol-name arg) "KEYWORD") event) ,event-fp)))
               (t (error "~A is not a valid parameter." arg)))))

(defmacro with-event ((&rest params) event-fp event &body body)
  `(let (,@(return-args params (gethash event *events*) event-fp))
     ,@body))

(defmacro with-active-event ((&rest params) event-fp &body body)
  `(let (,@(return-args params (gethash :active-event *events*) event-fp))
     ,@body))

(defmacro with-key-down-event ((&rest params) event-fp &body body)
  `(let (,@(return-args params (gethash :key-down-event *events*) event-fp))
     ,@body))

(defmacro with-key-up-event ((&rest params) event-fp &body body)
  `(let (,@(return-args params (gethash :key-up-event *events*) event-fp))
     ,@body))

(defmacro with-mouse-motion-event ((&rest params) event-fp &body body)
  `(let (,@(return-args params (gethash :mouse-motion-event *events*) event-fp))
     ,@body))

(defmacro with-mouse-button-down-event ((&rest params) event-fp &body body)
  `(let (,@(return-args params (gethash :mouse-button-down-event *events*) event-fp))
     ,@body))

(defmacro with-mouse-button-up-event ((&rest params) event-fp &body body)
  `(let (,@(return-args params (gethash :mouse-button-up-event *events*) event-fp))
     ,@body))

(defmacro with-joy-axis-motion-event ((&rest params) event-fp &body body)
  `(let (,@(return-args params (gethash :joy-axis-motion-event *events*) event-fp))
     ,@body))

(defmacro with-joy-button-down-event ((&rest params) event-fp &body body)
  `(let (,@(return-args params (gethash :joy-button-down-event *events*) event-fp))
     ,@body))

(defmacro with-joy-button-up-event ((&rest params) event-fp &body body)
  `(let (,@(return-args params (gethash :joy-button-up-event *events*) event-fp))
     ,@body))

(defmacro with-joy-hat-motion-event ((&rest params) event-fp &body body)
  `(let (,@(return-args params (gethash :joy-hat-motion-event *events*) event-fp))
     ,@body))

(defmacro with-joy-ball-motion-event ((&rest params) event-fp &body body)
  `(let (,@(return-args params (gethash :joy-ball-motion-event *events*) event-fp))
     ,@body))

(defmacro with-video-resize-event ((&rest params) event-fp &body body)
  `(let (,@(return-args params (gethash :video-resize-event *events*) event-fp))
     ,@body))

(defmacro with-user-event ((&rest params) event-fp &body body)
  `(let (,@(return-args params (gethash :user-event *events*) event-fp))
     ,@body))


(cffi:defcallback event-filter
    :int ((sdl-event sdl-cffi::sdl-event))
  (if *event-filters*
      (let ((hook (gethash (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type) *filter-event-hooks*)))
        (if hook
            (if (funcall hook sdl-event)
                1
                0)
            1))
      1))

(defun set-event-filter (event event-filter-hook)
  "Sets the callback function that handles the event filter. Return `NIL` if the event is to be removed
from the event queue. Return `T` if the event is to remain on the event queue.

`EVENT-FILTER-HOOK` must use the following template;

  \#'(LAMBDA (SDL-EVENT)
         \t\)

For example;
\(setf \(SDL:SET-EVENT-FILTER :QUIT-EVENT \#(LAMBDA \(EVENT\) t\))"
  (setf (gethash (cffi:foreign-enum-value 'sdl-cffi::Event-Type event)
                 *filter-event-hooks*) event-filter-hook))

(defun remove-event-filter (sdl-event)
  "Removes any existing event filters. that were set with `SET-EVENT-FILTERS`"
  (setf (gethash sdl-event *filter-event-hooks*) nil))

(defun remove-all-event-filters ()
  "Removes any existing event filters. that were set with `SET-EVENT-FILTERS`"
  (setf *filter-event-hooks* (make-hash-table)))

(defun enable-event-filters ()
  "Enables event filters."
  (setf *event-filters* t))

(defun disable-event-filters ()
  "Disables event filters."
  (setf *event-filters* nil))

(defun pump-events ()
  "Pumps the event loop, gathering events from the input devices. 
`PUMP-EVENTS` gathers all the pending input information from devices and places it on the event queue.
Without calls to SDL_PumpEvents no events would ever be placed on the queue.
Often the need for calls to SDL_PumpEvents is hidden from the user since
[SDL-POLL-EVENT](#sdl-poll-event) and [SDL-WAIT-EVENT](#sdl-wait-event) implicitly call `PUMP-EVENTS`.
However, if you are not polling or waiting for events (e.g. you are filtering them), then you must call
`PUMP-EVENTS` to force an event queue update.

Note: Only call this function in the thread that set the video mode."
  (lbm-sdl-cffi::sdl-pump-events))

(defun new-event (&optional (event-type :NO-EVENT))
  "Creates a new `SDL_Event` and of `EVENT-TYPE`.
 An event of type `:NO-EVENT` is created if 
the `OPTIONAL` event type `EVENT-TYPE` is unspecified.

##### Example

    \(NEW-EVENT :QUIT-EVENT\)"
  (unless (cffi:foreign-enum-value 'sdl-cffi::EVENT-TYPE event-type :errorp nil)
    (error "NEW-EVENT: EVENT-TYPE ~A is not a valid SDL event. Must be one of
:NO-EVENT, :ACTIVE-EVENT, :KEY-DOWN-EVENT, :KEY-UP-EVENT, :MOUSE-MOTION-EVENT, 
:MOUSE-BUTTON-DOWN-EVENT, :MOUSE-BUTTON-UP-EVENT, :JOY-AXIS-MOTION-EVENT, :JOY-BALL-MOTION-EVENT, 
:JOY-HAT-MOTION-EVENT, :JOY-BUTTON-DOWN-EVENT, :JOY-BUTTON-UP-EVENT, :QUIT-EVENT, 
:SYS-WM-EVENT, :VIDEO-RESIZE-EVENT, :VIDEO-EXPOSE-EVENT, :USER-EVENT."
           event-type))
  (let ((event (cffi:foreign-alloc 'sdl-cffi::sdl-event)))
    (setf (cffi:foreign-slot-value event 'sdl-cffi::SDL-event 'type) (cffi:foreign-enum-value 'sdl-cffi::EVENT-TYPE event-type))
    event))

(defun free-event (event*)
  (cffi:foreign-free event*))

(defun get-event (&optional event)
  (let ((event (or event (sdl:new-event))))
    (if (= (sdl-cffi::SDL-Poll-Event event) 0)
        (progn (sdl:free-event event)
               nil)
        event)))

(defun push-quit-event ()
  "Pushes a new `SDL_Event` of type `:QUIT-EVENT` onto the event queue."
  (let ((quit (new-event :quit-event)))
    (sdl-cffi::SDL-Push-Event quit)
    ;; The event can be safely freed, as SDL makes a copy
    ;; of the event when it is added to the event queue.
    (cffi:foreign-free quit)))

;;/* There are no functions directly affecting the quit event */
;;#define SDL_QuitRequested() \
;;        (SDL_PumpEvents(), SDL_PeepEvents(NULL,0,SDL_PEEKEVENT,SDL_QUITMASK))
(defun quit-requested-p ()
  "Returns the number of quit-events on the queue or `NIL` otherwise."
  (pump-events)
  (let ((events (sdl-cffi::sdl-peep-events (cffi:null-pointer) 0 :peek-event (sdl-cffi::sdl-quit-mask))))
    (if (= -1 events)
        nil
        events)))

(defun push-user-event (&key (code 0) (data1 nil) (data2 nil))
  "Pushes a new `SDL_Event` of type `:USER-EVENT` onto the event queue."
  (let ((event (new-event :USER-EVENT)))
    (setf (cffi:foreign-slot-value event 'sdl-cffi::SDL-user-event 'sdl-cffi::code) code
          (cffi:foreign-slot-value event 'sdl-cffi::SDL-user-event 'sdl-cffi::data1) (cffi:convert-to-foreign data1 :pointer)
          (cffi:foreign-slot-value event 'sdl-cffi::SDL-user-event 'sdl-cffi::data2) (cffi:convert-to-foreign data2 :pointer))
    (sdl-cffi::SDL-Push-Event event)
    (cffi:foreign-free event)))

;;; Event Handling from here   -----------------------

(defun pressed-p (state)
  (= state sdl-cffi::sdl-pressed))

(defun released-p (state)
  (= state sdl-cffi::sdl-released))


;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lbm-sdl)

;;;; Globals

(defvar *default-display* nil
  "The symbol `\*DEFAULT-DISPLAY\*` is bound to the current display surface 
[DISPLAY-SURFACE](#display-surface)) by the function [WINDOW](#WINDOW)).")
  
(defvar *default-color* nil
    "Functions that accept the `KEY`word parameter `COLOR` will most likely bind to the 
symbol `\*DEFAULT-COLOR*\` by default if `COLOR` is not specified. 

A color is bound to `\*DEFAULT-COLOR*\` by the following macro: [WITH-COLOR](#with-color).

##### Example

    \(DRAW-BOX A-BOX :SURFACE SDL:DEFAULT-DISPLAY* :COLOR SDL:*BLACK*\)
    \(DRAW-BOX B-BOX :SURFACE SDL:DEFAULT-DISPLAY* :COLOR SDL:*BLACK*\)
    \(DRAW-BOX C-BOX :SURFACE SDL:DEFAULT-DISPLAY* :COLOR SDL:*BLACK*\)

The above can be shortened by setting `\*DEFAULT-COLOR\*` to `\*BLACK\*`.

    \(WITH-SURFACE \(DISP SDL:*DEFAULT-DISPLAY*\)
      \(WITH-COLOR \(COL SDL:*BLACK*\)
        \(DRAW-BOX A-BOX\)
        \(DRAW-BOX B-BOX\)
        \(DRAW-BOX C-BOX\)\)\)")

(defvar *opengl-context* nil
  "The symbol `\*OPENGL-CONTEXT\*` is `T` when an OpenGL display context is created, and `NIL` otherwise.
[UPDATE-SURFACE](#update-surface) will swap the `OPENGL` buffers when `\*OPENGL-CONTEXT\*` is `T`, 
and swap the `SDL` video buffers otherwise.")

(defvar *default-position* nil)
(defvar *default-rectangle* nil)

(defvar *initialized* nil)

(defvar *external-init-subsystems-on-startup* nil
  "The list of functions that are called from [INIT-SDL](#init-sdl).")
(defvar *external-quit-subsystems-on-exit* nil
  "The list of functions that are called from [QUIT-SDL](#quit-sdl).")

(defparameter *initialize-subsystems-on-startup* nil)
(defparameter *quit-subsystems-on-exit* nil)
(defparameter *quit-on-exit* t)


(defvar *mixer* nil)
(defvar *managed-audio* nil)

(defconstant +DEFAULT-FORMAT+ sdl-cffi::AUDIO-S16SYS)
(setf (documentation '+DEFAULT-FORMAT+ 'variable)
      "Default SDL audio format; little-endian is `SDL:AUDIO-S16LSB`, big-endian is `SDL:AUDIO-S16MSB`.
Audio formats are defined in `SDL_audio.h`;

* `SDL:AUDIO-U8`     : Unsigned 8-bit samples
* `SDL:AUDIO-S8`     : Signed 8-bit samples
* `SDL:AUDIO-U16LSB` : Unsigned 16-bit samples, in little-endian byte order
* `SDL:AUDIO-S16LSB` : Signed 16-bit samples, in little-endian byte order
* `SDL:AUDIO-U16MSB` : Unsigned 16-bit samples, in big-endian byte order
* `SDL:AUDIO-S16MSB` : Signed 16-bit samples, in big-endian byte order
* `SDL:AUDIO-U16`    : same as `SDL:AUDIO-U16LSB` (for backwards compatability probably)
* `SDL:AUDIO-S16`    : same as `SDL:AUDIO-S16LSB` (for backwards compatability probably)
* `SDL:AUDIO-U16SYS` : Unsigned 16-bit samples, in system byte order
* `SDL:AUDIO-S16SYS` : Signed 16-bit samples, in system byte order")

(defconstant +CHANNELS+ 8)
(setf (documentation '+channels+ 'variable)
      "Default number of `8` mixer channels.")

(defconstant +DEFAULT-FREQUENCY+ 22050)
(setf (documentation '+default-frequency+ 'variable)
      "Default sampling frequency of `22,050hz`")

(defconstant +DEFAULT-CHANNELS+ 2)
(setf (documentation '+default-channels+ 'variable)
      "Default number of `2` sound channels for Stereo output.")

(defconstant +MAX-VOLUME+ 128)
(setf (documentation '+max-volume+ 'variable)
      "Default volume of `128` for [CHUNK](#chunk), output channels and mix channels.")

(defconstant +DEFAULT-SAMPLE-BUFFER+ 4096
  "Default size of the sample output buffer is `4096` bytes")

(defconstant +CHANNEL-POST+ -2)
(setf (documentation '+channel-post+ 'variable)
      "Default channel of `-2` used for post-processing.")

(defconstant +MAX-AUDIO-16+ (- (ash 1 (- 16 1)) 1))
(defconstant +MIN-AUDIO-16+ (* -1 (ash 1 (- 16 1))))

(defconstant +MAX-AUDIO-8+ (- (ash 1 (- 8 1)) 1))
(defconstant +MIN-AUDIO-8+ (* -1 (ash 1 (- 8 1))))

(defparameter *event-filters* t)

(defparameter *filter-active-event* nil)
(defparameter *filter-key-down-event* nil)
(defparameter *filter-key-up-event* nil)
(defparameter *filter-mouse-button-down-event* nil)
(defparameter *filter-mouse-button-up-event* nil)
(defparameter *filter-mouse-motion-event* nil)
(defparameter *filter-joy-axis-motion-event* nil)
(defparameter *filter-joy-button-down-event* nil)
(defparameter *filter-joy-button-up-event* nil)
(defparameter *filter-joy-hat-motion-event* nil)
(defparameter *filter-joy-ball-motion-event* nil)
(defparameter *filter-video-resize-event* nil)
(defparameter *filter-video-expose-event* nil)
(defparameter *filter-sys-wm-event* nil)
(defparameter *filter-user-event* nil)
(defparameter *filter-quit-event* nil)

(defparameter *filter-event-hooks* (make-hash-table))

(defvar *allow-convert-to-display-format* t
  "`CONVERT-TO-DISPLAY-FORMAT` will convert the input surface to the display format when `T`,
and copy to a new surface when `NIL`.")

(defvar *base-image-support* nil)
(defvar *additional-image-support* nil)
(defvar *current-image-support* *base-image-support*)

(defvar *sdl-event* nil)

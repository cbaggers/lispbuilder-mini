;;;; lbm-sdl
;;;; The OO wrapper for the lbm-sdl package
;;;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:lbm-sdl)

(defgeneric position-* (obj)
  (:documentation "See [POINT-*](#point-*)"))
(defgeneric set-position (dst src)
  (:documentation "See [SET-POINT](#set-point)"))
(defgeneric set-position-* (obj &key x y)
  (:documentation "See [SET-POINT-*](#set-point-*)"))

(defgeneric get-position (object)
  (:documentation "See [GET-POINT](#get-POINT)"))

(defgeneric rectangle-* (obj)
  (:documentation "Returns the `X`, `Y`, `WIDTH` and `HEIGHT` coordinates of the object as a spread. 
The `RESULT` is `\(VALUES X Y WIDTH HEIGHT\)`"))
(defgeneric set-rectangle (dst src)
  (:documentation "Copies the `X`, `Y`, `WIDTH` and `HEIGHT` coordinates to the destination rectangle `DST` from the source rectangle `SRC`."))
(defgeneric set-rectangle-* (rectangle &key x y w h)
  (:documentation "Sets the `X`, `Y`, `WIDTH` and `HEIGHT` coordinates of the rectangle `RECTANGLE`.
`X`, `Y`, `WIDTH` and `HEIGHT` are `KEY`word parameters having default values of `0` if unspecified."))

(defgeneric get-rectangle (obj)
  (:documentation "Returns the rectangle `RECTANGLE`."))
(defgeneric get-rectangle-* (obj)
  (:documentation "Creates and returns a `RECTANGLE` object from the `X`, `Y`, `WIDTH` and `HEIGHT` values in obj."))

(defgeneric set-surface (surface position)
  (:documentation "Sets the coordinates of the surface SURFACE to `POSITION`, 
where position is of type `POINT`."))
(defgeneric set-surface-* (surface &key x y)
  (:documentation "Sets the coordinates of the surface `SURFACE`.
`X` and `Y` are `KEY`word parameters having default values of `0` if unspecified."))


(defgeneric width (obj))
(defgeneric height (obj))
(defgeneric x (obj))
(defgeneric y (obj))
(defgeneric x2 (obj))
(defgeneric y2 (obj))

(defgeneric (setf width) (value obj)
  (:documentation "Sets/Returns the width of the object, as an `INTEGER`."))
(defgeneric (setf height) (value obj)
  (:documentation "Sets/Returns the height of the object, as an `INTEGER`."))

(defgeneric (setf x) (value obj)
  (:documentation "Sets/Returns the `X` coordinate of the object, as an `INTEGER`."))
(defgeneric (setf y) (value obj)
  (:documentation "Sets/Returns the `Y` coordinate of the object, as an `INTEGER`."))

(defgeneric (setf x2) (value obj)
  (:documentation "Returns `\(+ X WIDTH\)` of the object, as an `INTEGER`.
Sets the WIDTH of the object to `\(- X2 X\)`"))
(defgeneric (setf y2) (value obj)
  (:documentation "Returns `\(+ Y HEIGHT\)` of the object, as an `INTEGER`.
Sets the HEIGHT of the object to `\(- Y2 Y\)`"))


(defgeneric image-p (source image-type)
  (:documentation
   "Returns `T` when the image type in `SOURCE` is of `IMAGE-TYPE`. Returns `NIL` otherwise. 
Attempts to detect the image type using the *magic number* contained in the image if one is available.
 `NIL` is always returned for images of type `TGA` as a `TGA` image does not contain a *magic number*.
 `IMAGE-TYPE` must be one of `:BMP`, `:GIF`, `:JPG`, `:LBM`, `:PCX`, `:PNG`, 
`:PNM`, `:TIF`, `:XCF`, `:XPM` or `:XV`. 

##### Example

    \(RWOPS-P SOURCE :IMAGE-TYPE :BMP\)
    \(IMAGE-P \"image.bmp\" :IMAGE-TYPE :BMP\)

##### Packages

* Supported in _lbm-SDL-IMAGE_"))

(defgeneric image-type-of (source)
  (:documentation
   "Returns the type of image in source `SOURCE`. 
Attempts to detect the image type using the *magic number* contained in the image if one is available. 
 Returns one of `:BMP`, `:GIF`, `:JPG`, `:LBM`, `:PCX`, `:PNG`, 
`:PNM`, `:TIF`, `:XCF`, `:XPM` or `:XV`, if the image type can be determined. 
Returns `NIL` if the image cannot be determined \(The *magic number* is not supported or the *magic number* is not found\).
 `NIL` is always returned for images of type `TGA` as a `TGA` image does not contain a *magic number*.

##### Example

    \(IMAGE-TYPE-OF SOURCE\)
    \(IMAGE-TYPE-OF \"image.bmp\"\)

##### Packages"))


(defgeneric window (width height &key bpp flags sw hw fullscreen async-blit any-format palette double-buffer opengl resizable no-frame
                          title-caption icon-caption position
                          video-driver audio-driver opengl-attributes)
  (:documentation
  "Creates a new SDL window of pixel width `WIDTH` and height `HEIGHT` using SDL_SetVideoMode.

Use `SDL-SW-SURFACE` if you plan on doing per-pixel manipulations, or blit surfaces with alpha channels, 
and require a high framerate. When you use hardware surfaces like `SDL-HW-SURFACE`, SDL copies the surfaces 
from video memory to system memory when you lock them, and back when you unlock them. This can cause a major 
performance hit. \(Be aware that you may request a hardware surface, but receive a software surface. 
Many platforms can only provide a hardware surface when using `SDL-FULL-SCREEN.\) 
`SDL-HW-SURFACE` is best used when the surfaces you'll be blitting can also be stored in video memory.

*Note:* To control the position on the screen when creating a windowed surface, set the environment variables 
`SDL_VIDEO_CENTERED=center` or `SDL_VIDEO_WINDOW_POS=x,y`. These may be set using [SDL-PUT-ENV](#sdl-put-env).

##### Parameters

* `WIDTH` the pixel width of the window, of type `INTEGER`.
* `HEIGHT` the pixel height of the window, of type `INTEGER`.
If `WIDTH` and `HEIGHT` are both `0`, then the width and height of the current video mode is used 
\(or the desktop mode, if no mode has been set\).
* `BPP` the number of bits per pixel. Defaults to `0` which is the current display bits per pixel.
*Note:* A `BPP` of `24` uses the packed representation of 3 bytes/pixel. 
For the more common 4 bytes/pixel mode, use a `BPP` of 32.
* `FLAGS` is a bitmasked logior of one or more of the following; [SDL-SW-SURFACE](#sdl-sw-surface), 
[SDL-HW-SURFACE](#sdl-hw-surface), [SDL-ASYNC-BLIT](#sdl-async-blit),
[SDL-ANY-FORMAT](#sdl-any-format), [SDL-HW-PALETTE](#sdl-hw-palette), 
[SDL-DOUBLEBUF](#sdl-doublebuf), [SDL-FULLSCREEN](#sdl-fullscreen), 
[SDL-OPENGL](#sdl-opengl), [SDL-RESIZABLE](#sdl-resizable) and [SDL-NO-FRAME](#SDL-NO-FRAME).
* `TITLE-CAPTION` is the title that appears in the Window title bar, of type `STRING`.
* `ICON-CAPTION` is the title that appears when the Window is minimized, of type `STRING`.
* `POSITION` is the x and y display positions of the window. When `NIL` will use the previous x and y positions,
when `T` will center the window on the screen, when `(VECTOR x y)` will set the window to the
specified x and y positions.


##### Returns

* Returns a new `DISPLAY-SURFACE` if successful, `NIL` if unsuccessful. Whatever flags SDL_SetVideoMode 
could satisfy are set in the flags member of `SURFACE`.
The `SURFACE` returned is freed by SDL and should never be freed by the caller.
This rule includes consecutive calls to `WINDOW` \(i.e. upon resize or resolution change\) - any existing surface 
will be released automatically by SDL.

##### Example

    \(WINDOW 320 240 :TITLE-CAPTION \"Random-Rects\" :ICON-CAPTION \"Random-Rects\"
                     :FLAGS \'(SDL-DOUBLEBUF SDL-FULLSCREEN\)\)"))

(defgeneric resize-window (width height &key bpp 
                                          title-caption icon-caption fullscreen 
                                          any-format resizable no-frame alpha-size  
                                          depth-size  stencil-size  red-size 
                                          green-size  blue-size  double-buffer
                                          swap-control )
  
  (:documentation
   "Resizes the current display to the specified `WIDTH` and `HEIGHT`. The pixel depth, title and icon captions, and surface flags will remain the same"))

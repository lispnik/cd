(in-package #:cd)

(export '(text
          font
          font-size
          font-style
          font-typeface
          native-font
          text-alignment
          text-orientation))

(defwrappers ("canvas-text" "text")
  (defun ^function-name (canvas x y text)
    "Draws a text in the position (x,y) according to the current font and text
alignment. It expects an ANSI string. Can have line breaks."
    (^cffi-function-name canvas (coerce x ^type) (coerce y ^type) text)))

;;; TODO convert this function to (setf (font canvas) (values ... )):

;; (defwrappers ("canvas-font" "font" "cd")
;;   (defun (setf ^function-name) (canvas typeface style size)
;;     "Selects a text font. The font type can be one of the standard type faces or
;; other driver dependent type face. Since font face names are not a standard
;; between drivers, a few names are specially handled to improve application
;; portability. If you want to use names that work for all systems we recommend
;; using: \"Courier\", \"Times\" and \"Helvetica\".

;; The style can be a combination of: 

;; :FONT-STYLE-PLAIN
;; :FONT-STYLE-BOLD
;; :FONT-STYLE-ITALIC
;; :FONT-STYLE-UNDERLINE
;; :FONT-STYLE-STRIKEOUT

;; Only the Windows and PDF drivers support underline and strikeout. The size is
;; provided in points (1/72 inch) or in pixels (using negative values).

;; Default values: \"System\", :FONT-STYLE-PLAIN, 12.

;; ;; FIXME support this too
;; You can specify partial parameters using NULL, -1 and 0 for typeface, style and
;; size. When these parameters are specified the current font parameter is
;; used. For example: CanvasFont(NULL, -1, 10) will only change the font size.

;; To convert between pixels and points use the function cdPixel2MM to convert from
;; pixels to millimeters and use the formula \"(value in points) = CD_MM2PT
;; * (value in millimeters).

;; In WC, the size is specified in millimeters, but is internally converted to
;; points.

;; Fonts can heavily benefit from the ANTIALIAS attribute where available in the
;; driver."
;;     (^cffi-function-name canvas typeface style (coerce size ^type))))

(defwrappers ("canvas-get-font" "font" "cd")
  (defun ^function-name (canvas)
    (let ((max-typeface-length 1000))
      (cffi:with-foreign-objects
          ((typeface-ptr :char max-typeface-length)
           (style-ptr :int)
           (size-ptr ^cffi-type))
        (^cffi-function-name canvas typeface-ptr style-ptr size-ptr)
        (values (cffi:foreign-string-to-lisp typeface-ptr :max-chars max-typeface-length)
                (cffi:mem-ref style-ptr 'cd-cffi::font-style)
                (cffi:mem-ref size-ptr ^cffi-type))))))

(defwrappers ("canvas-font" "font-typeface" "cd")
  (defun (setf ^function-name) (new-font-typeface canvas)
    "Set the current font typeface."
    (^cffi-function-name canvas new-font-typeface :query 0)
    new-font-typeface))

(defwrappers ("canvas-font" "font-style" "cd")
  (defun (setf ^function-name) (new-font-style canvas)
    "Set the current font style."
    (^cffi-function-name canvas nil new-font-style 0)
    new-font-style))

(defwrappers ("canvas-font" "font-size" "cd")
  (defun (setf ^function-name) (new-font-size canvas)
    "Set the current font size."
    (^cffi-function-name canvas nil :query new-font-size)
    new-font-size))

(defun (setf native-font) (new-native-font canvas)
  "Selects a font based on a string description. 

The description can depend on the driver and the platform, but a common
definition is available for all drivers. It does not need to be stored by the
application, as it is internally replicated by the library. The string is case
sensitive. It returns the previous string.

The string is parsed and the font typeface, style and size are set according to
the parsed values, as if cdCanvasFont was called. The native font string is
cleared when a font is set using cdCanvasFont.

The common format definition is similar to the the Pango library Font
Description, used by GTK+2. It is defined as having 3 parts: <font family>,
<font styles> <font size>. For ex: \"Times, Bold 18\", or \"Arial,Helvetica,
Italic Underline -24\". The supported styles include: Bold, Italic, Underline
and Strikeout. Underline, Strikeout, and negative pixel values are not supported
by the standard Pango Font Description. The Pango format include many other
definitions not supported by the CD format, they are just ignored.

The IUP \"FONT\" attribute internal formats are also accepted in all drivers and
platforms."
  (cd-cffi::%cd-canvas-native-font canvas new-native-font)
  new-native-font)

(defun native-font (canvas)
  "Return the selected native font in the common font definition form."
  (cd-cffi::%cd-canvas-native-font
   canvas
   (cffi:inc-pointer (cffi:make-pointer 0) -1)))

(defun (setf text-alignment) (new-alignment canvas)
  "Defines the vertical and horizontal alignment of a text as: 

:ALIGNMENT-NORTH
:ALIGNMENT-SOUTH
:ALIGNMENT-EAST
:ALIGNMENT-WEST
:ALIGNMENT-NORTH-EAST
:ALIGNMENT-NORTH-WEST
:ALIGNMENT-SOUTH-EAST
:ALIGNMENT-SOUTH-WEST
:ALIGNMENT-CENTER
:ALIGNMENT-BASE-LEFT
:ALIGNMENT-BASE-CENTER
:ALIGNMENT-BASE-RIGHT

Text Alignment

FIXME http://webserver2.tecgraf.puc-rio.br/cd/img/align.gif"
  (cd-cffi::%cd-canvas-text-alignment canvas new-alignment)
  new-alignment)

(defun text-alignment (canvas)
  "Return the currently configured text alignment."
  (cd-cffi::%cd-canvas-text-alignment canvas :query))

(defun (setf text-orientation) (new-orientation canvas)
  "Defines the text orientation, which is an angle provided in degrees relative
to the horizontal line according to which the text is drawn. The default value
is 0."
  (cd-cffi::%cd-canvas-text-orientation
   canvas
   (coerce new-orientation 'double-float))
  new-orientation)

(defun text-orientation (canvas)
  "Returns the currently configured text orientation in degrees."
  (cd-cffi::%cd-canvas-text-orientation
   canvas
   (coerce cd-cffi::+cd-query+ 'double-float)))

(defwrappers ("canvas-get-font-dimensions" "font-dimensions" "cd")
  "Returns the maximum width of a character, the line's height, the ascent and
descent of the characters of the currently selected font. The line's height is
the sum of the ascent and descent of a given additional space (if this is the
case). All values are given in pixels and are positive.

FIXME http://webserver2.tecgraf.puc-rio.br/cd/img/font_dim.gif"
  (defun ^function-name (canvas)
    (cffi:with-foreign-objects
        ((max-width ^cffi-type)
         (height ^cffi-type)
         (ascent ^cffi-type)
         (descent ^cffi-type))
      (^cffi-function-name canvas max-width height ascent descent)
      (values (cffi:mem-ref max-width ^cffi-type)
              (cffi:mem-ref height ^cffi-type)
              (cffi:mem-ref ascent ^cffi-type)
              (cffi:mem-ref descent ^cffi-type)))))

(defwrappers ("canvas-get-text-size" "text-size" "cd")
  "Returns the text size independent from orientation as width and height
values."
  (defun ^function-name (canvas text)
    (cffi:with-foreign-objects
        ((width ^cffi-type)
         (height ^cffi-type))
      (^cffi-function-name canvas text width height)
      (values (cffi:mem-ref width ^cffi-type)
              (cffi:mem-ref height ^cffi-type)))))

(defwrappers ("canvas-get-text-bounds" "text-bounds")
  "Returns the oriented bounding rectangle occupied by a text at a given
position.

The rectangle has the same dimentions returned by GetTextSize. The rectangle
corners are returned in counter-clock wise order starting with the bottom left
corner, arranged x0 y0 x1 y1 x2 y2 x3 y3."
  (defun ^function-name (canvas x y text)
    (cffi:with-foreign-object
        (result ^cffi-type 8)
      (^cffi-function-name canvas (coerce x ^type) (coerce y ^type) text result)
      (values-list (loop for i below 8
                         collect (cffi:mem-aref result ^cffi-type i))))))

(defwrappers ("canvas-get-text-box" "text-box")
  "Returns the horizontal bounding rectangle occupied by a text at a given
position.

If orientation is not 0 then its area is always larger than the area of the
rectangle returned by TEXT-BOUNDS."
  (defun ^function-name (canvas x y text)
    (cffi:with-foreign-objects
        ((xmin ^cffi-type)
         (xmax ^cffi-type)
         (ymin ^cffi-type)
         (ymax ^cffi-type))
      (^cffi-function-name
       canvas
       (coerce x ^type) (coerce y ^type)
       text
       xmin xmax ymin ymax)
      (values (cffi:mem-ref xmin ^cffi-type)
              (cffi:mem-ref xmax ^cffi-type)
              (cffi:mem-ref ymin ^cffi-type)
              (cffi:mem-ref ymax ^cffi-type)))))

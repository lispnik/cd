(in-package #:cd)

(export '(version
          version-date
          version-number
          +red+
          +dark-red+
          +green+
          +dark-green+
          +blue+
          +dark-blue+
          +yellow+
          +dark-yellow+
          +magenta+
          +dark-magenta+
          +cyan+
          +dark-cyan+
          +white+
          +black+
          +dark-gray+
          +gray+
          encode-color
          encode-color-alpha
          encode-alpha
          decode-color
          decode-color-alpha
          alpha
          red
          green
          blue
          color-planes
          palette))

(defalias version #'cd-cffi::%cd-version
  "Returns the current version number of the library. The string with the
version number has a format \"major.minor.build\". 

For instance, the string \"2.1.3\" has number 2 as the main (major) version
number, 1 as the secondary (minor) version number, and 3 as the build
number. The major version number represents a change in the structure or
behavior of functions; the minor version number represents one or more new
drivers and functions added to the library; and the build version number
represents one or more corrected bugs.")

(defalias version-date #'cd-cffi::%cd-version-date
  "Returns the release date of the current version of the library.")

(defalias version-number #'cd-cffi::%cd-version-date
  "Returns the current version number of the library.")

;;; TODO

;;; cdCanvasPlay
;;; wdCanvasPlay
;;; cdContextRegisterCallback
;;; cdResizeCB

(defmacro defcolor (name value)
  `(defconstant ,name ,value
     ,(format nil "Defined by RGB hexidecimal triplet: #~6,'0X" value)))

(defcolor +red+ #xff0000)
(defcolor +dark-red+ #x800000)
(defcolor +green+ #x00ff00)
(defcolor +dark-green+ #x008000)
(defcolor +blue+ #x0000ff)
(defcolor +dark-blue+ #x000080)
(defcolor +yellow+ #xffff00)
(defcolor +dark-yellow+ #x808000)
(defcolor +magenta+ #xff00ff)
(defcolor +dark-magenta+ #x800080)
(defcolor +cyan+ #x00ffff)
(defcolor +dark-cyan+ #x008080)
(defcolor +white+ #xffffff)
(defcolor +black+ #x000000)
(defcolor +dark-gray+ #x808080)
(defcolor +gray+ #xc0c0c0)

(defun encode-color (red green blue)
  "Returns a codified triple (r,g,b) in a long integer such as 0x00RRGGBB,
where RR are the red components, GG are the green ones and BB are the blue
ones. The code is used in the CD library to define colors. It can be used
without an active canvas."
  (cd-cffi::%cd-encode-color red green blue))

(defun encode-color-alpha (red green blue alpha)
  "Returns a codified quadriple (r,g,b,a) in a long integer such as 0xAARRGGBB,
where AA are the alpha components, RR are the red components, GG are the green
ones and BB are the blue ones. The code is used in the CD library to define
colors. It can be used without an active canvas. (since 5.9)"
  (cd-cffi::%cd-encode-color-alpha red green blue alpha))

(defun encode-alpha (color alpha)
  "Returns the given color coded with the alpha information. 

ATTENTION: At the moment only the Cairo, GDI+, XRender and IMAGERGB drivers
support alpha components in color coding. The internal representation of the
component is inverted, because the default value must be 0 and opaque for
backward compatibility, so you should use the DECODE-ALPHA, or ALPHA function to
retrieve the alpha component. 0 is transparent, 255 is opaque."
  (cd-cffi::%cd-encode-alpha color alpha))

(defun decode-color (color)
  "Returns the red, green and blue components of a color in the CD library. Can
be used without an active canvas."
  (check-type color (unsigned-byte 32))
  (values (ldb (byte 8 16) color)
          (ldb (byte 8 8) color)
          (ldb (byte 8 0) color)))

(defun decode-color-alpha (color)
  "Returns the given color coded with the alpha information. 

ATTENTION: At the moment only the Cairo, GDI+, XRender and IMAGERGB drivers
support alpha components in color coding. The internal representation of the
component is inverted, because the default value must be 0 and opaque for
backward compatibility, so you should use the cdDecodeAlpha function or the
cdAlpha macro to retrieve the alpha component. 0 is transparent, 255 is opaque."
  (check-type color (unsigned-byte 32))
  (values (ldb (byte 8 16) color)
          (ldb (byte 8 8) color)
          (ldb (byte 8 0) color)
          (logandc2 #xff (ldb (byte 8 24) color))))

(defun alpha (color)
  "Returns the alpha component of a color in the CD library. Can be used without
an active canvas. 0 is transparent, 255 is opaque."
  (logandc2 #xff (ldb (byte 8 24) color)))

(defun red (color)
  "Returns the red component of a color in the CD library. Can be used without
an active canvas."
  (ldb (byte 8 16) color))

(defun green (color)
  "Returns the green component of a color in the CD library. Can be used without
an active canvas."
  (ldb (byte 8 8) color))

(defun blue (color)
  "Returns the blue component of a color in the CD library. Can be used without
an active canvas."
  (ldb (byte 8 0) color))

(defun color-planes (canvas)
  "Returns a given number, for instance p, which defines the number of colors
supported by the current device as 2**p, representing the number of bits by
pixel."
  (cd-cffi::%cd-canvas-get-color-planes canvas))

(defun (setf palette) (new-colors canvas mode)
  "In systems limited to 256 palette colors, this function aims at adding n
colors to the system's palette. In such systems, the colors demanded forward or
backward which are not in the palette are approximated to the closest available
color.

Mode values:

:PALETTE-FORCE - ignores the system colors and interface elements, since the
menus and dialogues may be in illegible colors, but there will be more colors
available.

:PALETTE-POLITE (recommended)

It must always be used before drawing. It cannot be queried."
  (cffi:with-foreign-object
      (colors-ptr :long)
    (let ((length (length new-colors)))
      (loop for i below length
            do (setf (cffi:mem-ref colors-ptr :long)
                     (elt new-colors i)))
      (cd-cffi::%cd-canvas-palette canvas length colors-ptr mode))))

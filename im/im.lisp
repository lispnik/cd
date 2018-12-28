(defpackage #:cd-im
  (:use #:common-lisp
	#:cffi)
  (:export #:context-im-image)
  (:shadow)
  (:import-from	#:serapeum #:defalias))

(defpackage #:wd-im
  (:use #:common-lisp
	#:cffi)
  (:export)
  (:shadow)
  (:import-from	#:serapeum #:defalias))

(in-package #:cd-im)

(export '(context-im-image
          put-im-image
          pattern-im-image
          stipple-im-image))

(defalias context-im-image #'cd-im-cffi::%cd-context-im-image)

(defun put-im-image (canvas im-image x y w h)
  "Draws an imImage on any canvas. Image must be a displayable
image (imImageIsBitmap), and it can has an alpha channel."
  (cd-im-cffi::%cdf-canvas-put-im-image
   canvas im-image
   (coerce x 'double-float)
   (coerce y 'double-float)
   (coerce w 'double-float)
   (coerce h 'double-float)))

(defun pattern-im-image (canvas im-image)
  "Defines an imImage as a pattern on any canvas. Image must be a displayable
image (imImageIsBitmap)."
  (cd-im-cffi::%cd-canvas-stipple-im-image canvas im-image))

(defun stipple-im-image (canvas im-image)
  "Defines an imImage as a stipple on any canvas. Image must be a binary
image (color_space=IM_BINARY)."
  (cd-im-cffi::%cd-canvas-stipple-im-image canvas im-image))

(defun get-im-image (canvas im-image x y)
  "Retrieves the canvas contents in an imImage. Image must be a display RGB
image (color_space=IM_RGB and data_type=IM_BYTE)."
  (cd-im-cffi::%cd-canvas-get-im-image canvas im-image x y))

(in-package #:wd-im)

(export '(put-im-image
          get-im-image))

(defun put-im-image (canvas im-image x y w h)
  "Draws an imImage on any canvas. Image must be a displayable
image (imImageIsBitmap), and it can has an alpha channel."
  (cd-im-cffi::%wd-canvas-put-im-image
   canvas im-image
   (coerce x 'double-float)
   (coerce y 'double-float)
   (coerce w 'double-float)
   (coerce h 'double-float)))

(defun get-im-image (canvas im-image x y)
  "Retrieves the canvas contents in an imImage. Image must be a display RGB
image (color_space=IM_RGB and data_type=IM_BYTE)."
  (cd-im-cffi::%wd-canvas-get-im-image
   canvas
   im-image
   (coerce x 'double-float)
   (coerce y 'double-float)))

;;;; src/ffi/cd-irgb.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: cdirgb.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcfun ("cdContextImageRGB" %cd-context-image-rgb) cd-context)

(cffi:defcfun ("cdContextDBufferRGB" %cd-context-d-buffer-rgb) cd-context)

(cffi:defcfun ("cdRedImage" %cd-red-image) :pointer
  "DEPRECATED functions, use REDIMAGE, GREENIMAGE, BLUEIMAGE, and ALPHAIMAGE
attributes."
  (cnv cd-canvas))

(cffi:defcfun ("cdGreenImage" %cd-green-image) :pointer
  (cnv cd-canvas))

(cffi:defcfun ("cdBlueImage" %cd-blue-image) :pointer
  (cnv cd-canvas))

(cffi:defcfun ("cdAlphaImage" %cd-alpha-image) :pointer
  (cnv cd-canvas))

;;;; src/ffi/cd-im.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: cdim.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcfun ("cdContextImImage" %cd-context-im-image) cd-context)

(cffi:defcfun ("cdCanvasPatternImImage" %cd-canvas-pattern-im-image) :void
  (canvas cd-canvas)
  (image im-image))

(cffi:defcfun ("cdCanvasStippleImImage" %cd-canvas-stipple-im-image) :void
  (canvas cd-canvas)
  (image im-image))

(cffi:defcfun ("cdCanvasPutImImage" %cd-canvas-put-im-image) :void
  (canvas cd-canvas)
  (image im-image)
  (x :int)
  (y :int)
  (w :int)
  (h :int))

(cffi:defcfun ("cdCanvasGetImImage" %cd-canvas-get-im-image) :void
  (canvas cd-canvas)
  (image im-image)
  (x :int)
  (y :int))

(cffi:defcfun ("cdfCanvasPutImImage" %cdf-canvas-put-im-image) :void
  (canvas cd-canvas)
  (image im-image)
  (x :double)
  (y :double)
  (w :double)
  (h :double))

(cffi:defcfun ("wdCanvasPutImImage" %wd-canvas-put-im-image) :void
  (canvas cd-canvas)
  (image im-image)
  (x :double)
  (y :double)
  (w :double)
  (h :double))

(cffi:defcfun ("wdCanvasGetImImage" %wd-canvas-get-im-image) :void
  (canvas cd-canvas)
  (image im-image)
  (x :double)
  (y :double))

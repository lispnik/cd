(defpackage #:cd-im-cffi
  (:use #:common-lisp))

(in-package #:cd-im-cffi)

(cffi:define-foreign-library lib-cdim
  (:unix "libcdim.so")
  (:windows "cdim.dll")
  (t (:default "cdim")))

(cffi:use-foreign-library lib-cdim)

(cffi:defcfun (%cd-context-im-image "cdContextImImage") cd-cffi:cd-context)

(cffi:defcfun (%cd-canvas-pattern-im-image "cdCanvasPatternImImage") :void
  (canvas cd-cffi:cd-canvas)
  (image tecgraf-base:im-image))

(cffi:defcfun (%cd-canvas-stipple-im-image "cdCanvasStippleImImage") :void
  (canvas cd-cffi:cd-canvas)
  (image tecgraf-base:im-image))

(cd-cffi::defcfun-cd/cdf/wd target ("canvas-put-im-image" "CanvasPutImImage") :void
  (canvas cd-cffi:cd-canvas)
  (image tecgraf-base:im-image)
  (x target)
  (y target)
  (w target)
  (h target))

(cd-cffi::defcfun-cd/wd target ("canvas-get-im-image" "CanvasGetImImage") :void
  (canvas cd-cffi:cd-canvas)
  (image tecgraf-base:im-image)
  (x target)
  (y target))

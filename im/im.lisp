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

(defalias context-im-image #'cd-im-cffi::%cd-context-im-image)
(defalias put-im-image #'cd-im-cffi::%cdf-canvas-put-im-image)
(defalias put-im-image #'cd-im-cffi::%cdf-canvas-put-im-image)


(cffi:defcfun ("%cd-canvas-put-im-image" "cdCanvasPutImImage") :void
  (canvas ))
void cdCanvasPutImImage(cdCanvas* canvas, const imImage* image, int x, int y, int w, int h); [in C]
void cdfCanvasPutImImage(cdCanvas* canvas, const imImage* image, double x, double y, double w, double h); [in C]
void wdCanvasPutImImage(cdCanvas* canvas, const imImage* image, double x, double y, double w, double h); (WC) [in C]


(defpackage #:cd-context-plus
  (:use #:common-lisp
	#:cffi
	#:alexandria
	#:serapeum)
  (:import-from #:serapeum #:defalias)
  (:export #:initialize
           #:finish)
  (:shadow))

(in-package #:cd-context-plus)

(defalias initialize #'cd-context-plus-cffi::%cd-init-context-plus
  "Initializes the context driver to use another context replacing the standard
drivers. This functions is only available when a library containing a 'context
plus' context driver is used. See the Cairo, GDI+ and XRender base
drivers. Those libraries does not support XOR write mode, but has support for
anti-aliasing and alpha for transparency.")

(defalias finish #'cd-context-plus-cffi::%cd-finish-context-plus)

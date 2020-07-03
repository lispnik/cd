(defpackage #:cd-context-plus
  (:use #:common-lisp
	#:cffi
	#:alexandria)
  (:export #:initialize
           #:finish)
  (:shadow))

(in-package #:cd-context-plus)

(setf (fdefinition 'initialize) #'cd-context-plus-cffi::%cd-init-context-plus)
(setf (documentation 'initialie 'function)
      "Initializes the context driver to use another context replacing the standard
drivers. This functions is only available when a library containing a 'context
plus' context driver is used. See the Cairo, GDI+ and XRender base
drivers. Those libraries does not support XOR write mode, but has support for
anti-aliasing and alpha for transparency.")

(setf (fdefinition 'finish) #'cd-context-plus-cffi::%cd-finish-context-plus)

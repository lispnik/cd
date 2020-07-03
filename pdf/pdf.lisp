(defpackage #:cd-pdf
  (:use #:common-lisp
	#:cffi)
  (:export #:context-pdf)
  (:shadow))

(in-package #:cd-pdf)

(setf (fdefinition 'context-pdf) #'cd-pdf-cffi::%cd-context-pdf)

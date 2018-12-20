(defpackage #:cd-pdf
  (:use #:common-lisp
	#:cffi
	#:serapeum)
  (:export #:context-pdf)
  (:shadow))

(in-package #:cd-pdf)

(defalias context-pdf #'cd-pdf-cffi::%cd-context-pdf)

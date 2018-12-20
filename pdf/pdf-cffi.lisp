(defpackage #:cd-pdf-cffi
  (:use #:common-lisp))

(in-package #:cd-pdf-cffi)

(cffi:define-foreign-library lib-cdpdf
  (:unix "libcdpdf.so")
  (:windows "cdpdf.dll")
  (t (:default "cdpdf")))

(cffi:use-foreign-library lib-cdpdf)

(cffi:defcfun (%cd-context-pdf "cdContextPDF") cd-cffi:cd-context)

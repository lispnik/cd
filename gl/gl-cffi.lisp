(defpackage #:cd-gl-cffi
  (:use #:common-lisp))

(in-package #:cd-gl-cffi)

(cffi:define-foreign-library lib-cdgl
  (:unix "libcdgl.so")
  (:windows "cdgl.dll")
  (t (:default "cdgl")))

(cffi:use-foreign-library lib-cdgl)

(cffi:defcfun (%cd-context-gl "cdContextGL") cd-cffi:cd-context)

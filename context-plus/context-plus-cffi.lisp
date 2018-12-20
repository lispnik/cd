(defpackage #:cd-context-plus-cffi
  (:use #:common-lisp))

(in-package #:cd-context-plus-cffi)

(cffi:define-foreign-library lib-cdcontextplus
  (:unix "libcdcontextplus.so")
  (:windows "cdcontextplus.dll")
  (t (:default "cdcontextplus")))

(cffi:use-foreign-library lib-cdcontextplus)

(cffi:defcfun (%cd-init-context-plus "cdInitContextPlus") :void)

(cffi:defcfun (%cd-finish-context-plus "cdFinishContextPlus") :void)

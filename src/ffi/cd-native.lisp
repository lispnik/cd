;;;; src/ffi/cd-native.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: cdnative.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcfun ("cdContextNativeWindow" %cd-context-native-window) cd-context)

(cffi:defcfun ("cdGetScreenSize" %cd-get-screen-size) :void
  (width :pointer)
  (height :pointer)
  (width-mm :pointer)
  (height-mm :pointer))

(cffi:defcfun ("cdGetScreenColorPlanes" %cd-get-screen-color-planes) :int)

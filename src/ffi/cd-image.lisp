;;;; src/ffi/cd-image.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: cdimage.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcfun ("cdContextImage" %cd-context-image) cd-context)

;;;; src/ffi/cd-gl.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: cdgl.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcfun ("cdContextGL" %cd-context-gl) cd-context)

;;;; src/ffi/cd-ps.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: cdps.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcfun ("cdContextPS" %cd-context-ps) cd-context)

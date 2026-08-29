;;;; src/ffi/cd-cgm.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: cdcgm.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcfun ("cdContextCGM" %cd-context-cgm) cd-context)

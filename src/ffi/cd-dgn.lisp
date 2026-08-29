;;;; src/ffi/cd-dgn.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: cddgn.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcfun ("cdContextDGN" %cd-context-dgn) cd-context)

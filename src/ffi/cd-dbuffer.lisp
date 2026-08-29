;;;; src/ffi/cd-dbuffer.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: cddbuf.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcfun ("cdContextDBuffer" %cd-context-d-buffer) cd-context)

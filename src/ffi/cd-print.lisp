;;;; src/ffi/cd-print.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: cdprint.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcfun ("cdContextPrinter" %cd-context-printer) cd-context)

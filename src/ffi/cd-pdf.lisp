;;;; src/ffi/cd-pdf.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: cdpdf.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcfun ("cdContextPDF" %cd-context-pdf) cd-context)

;;;; src/ffi/cd-mf.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: cdmf.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcfun ("cdContextMetafile" %cd-context-metafile) cd-context)

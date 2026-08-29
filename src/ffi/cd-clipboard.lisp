;;;; src/ffi/cd-clipboard.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: cdclipbd.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcfun ("cdContextClipboard" %cd-context-clipboard) cd-context)

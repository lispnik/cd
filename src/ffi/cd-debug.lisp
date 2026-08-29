;;;; src/ffi/cd-debug.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: cddebug.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcfun ("cdContextDebug" %cd-context-debug) cd-context)

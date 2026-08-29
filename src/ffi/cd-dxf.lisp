;;;; src/ffi/cd-dxf.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: cddxf.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcfun ("cdContextDXF" %cd-context-dxf) cd-context)

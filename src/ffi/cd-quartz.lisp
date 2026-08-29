;;;; src/ffi/cd-quartz.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: cdquartz.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcfun ("cdContextQuartzBitmap" %cd-context-quartz-bitmap) cd-context
  "Offscreen Quartz canvas, available on macOS without a window server. The
creation data is \"widthxheight [resolution]\", for instance \"800x600\"
or \"800x600 3.8\", where the resolution is in pixels/mm.")

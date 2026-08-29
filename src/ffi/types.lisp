;;;; src/ffi/types.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; CD's opaque handles. Each is a :POINTER underneath, but naming
;;;; them makes a signature say which kind it wants.

(in-package #:cd.ffi)

(cffi:defctype cd-canvas :pointer)   ; cdCanvas*
(cffi:defctype cd-context :pointer)   ; cdContext*
(cffi:defctype cd-state :pointer)   ; cdState*
(cffi:defctype cd-image :pointer)   ; cdImage*
(cffi:defctype cd-bitmap :pointer)   ; cdBitmap*
(cffi:defctype cd-pattern :pointer)   ; cdPattern*
(cffi:defctype cd-stipple :pointer)   ; cdStipple*
(cffi:defctype im-image :pointer)   ; imImage*

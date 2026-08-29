;;;; src/package.lisp — package definitions.
;;;;
;;;; Two packages, replacing the twelve the previous version exposed (CD,
;;;; CD-CFFI, WD, CD-IM, CD-IM-CFFI, CD-GL, CD-PDF, CD-CONTEXT-PLUS and their
;;;; -cffi twins). That split mirrored CD's driver headers rather than
;;;; anything a caller reasons about, and it put the world-coordinate calls in
;;;; a separate package from the pixel ones despite their operating on the
;;;; same canvas.

(defpackage #:cd.ffi
  (:use #:common-lisp)
  (:documentation
   "Raw CFFI bindings: one file per upstream header, under src/ffi/.

Everything here is internal. Names are mechanical transcriptions of the C ones
(%CD-CANVAS-LINE for cdCanvasLine) and the arguments are C types, so callers
get foreign pointers and ints rather than Lisp values. The CD package reaches
in with double colons; nothing else should.

Files under src/ffi/ are drafted by tools/gen-bindings.lisp and then
hand-corrected."))

(defpackage #:cd
  (:use #:common-lisp)
  (:documentation
   "Common Lisp bindings to CD, Tecgraf's Canvas Draw 2D graphics library.

A canvas is a CLOS object created against a driver -- SVG, PostScript, an
in-memory image -- and released by CD:KILL, by CD:WITH-CANVAS on unwind, or as
a last resort by a finalizer. Drawing operations always take the canvas they
act on: CD's global \"active canvas\" API is deliberately not exposed, because
it is unsafe with more than one canvas and cannot be made safe across threads.

The world-coordinate layer (CD:WD-*) draws in the caller's own units, with CD
mapping them onto the canvas.")
  ;; Nothing shadowed. CD's operation names -- LINE, BOX, TEXT, SECTOR, CLIP,
  ;; FONT, MARK, VERTEX, BEGIN, END -- happen not to collide with CL, and a
  ;; shadow added on suspicion rather than on need is how a package ends up
  ;; unable to call CL:ERROR without writing it out. Where a good name does
  ;; collide it gets a prefix instead.
  )

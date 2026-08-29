;;;; src/drawing.lisp — primitives and attributes.
;;;;
;;;; Every operation takes the canvas it acts on. CD's global "active canvas"
;;;; API is not exposed: it cannot be made safe with more than one canvas or
;;;; more than one thread, and each of its functions has a cdCanvas*
;;;; counterpart wrapped here.
;;;;
;;;; CD offers integer and double versions of most primitives -- cdCanvasLine
;;;; and cdfCanvasLine. Rather than two Lisp names, each wrapper dispatches on
;;;; what it is given: pass integers and it calls the integer entry point, pass
;;;; anything else and it calls the double one. The distinction is real, not
;;;; cosmetic -- the integer path is what a pixel driver wants and the double
;;;; path is what a vector driver can use exactly -- but it is a distinction
;;;; about the arguments, so the arguments should decide it.

(in-package #:cd)

(export '(line box rect arc sector chord mark pixel text
          begin-shape end-shape vertex with-shape
          clear flush
          foreground background
          line-width line-style line-join line-cap
          interior-style hatch-style
          write-mode back-opacity fill-mode
          font text-alignment text-orientation
          font-dimensions text-size
          set-attribute attribute))

(defun %intp (&rest values)
  "True when every value is an integer, so the integer entry point applies."
  (every #'integerp values))

(defun %d (x) (coerce x 'double-float))

;;; Primitives ----------------------------------------------------------------

(defun line (canvas x1 y1 x2 y2)
  "Draw a line from (X1, Y1) to (X2, Y2).

Integer coordinates use CD's integer entry point; anything else uses the
double one, which vector drivers can honour exactly."
  (if (%intp x1 y1 x2 y2)
      (cd.ffi::%cd-canvas-line (handle canvas) x1 y1 x2 y2)
      (cd.ffi::%cdf-canvas-line (handle canvas) (%d x1) (%d y1) (%d x2) (%d y2)))
  canvas)

(defun box (canvas xmin xmax ymin ymax)
  "Draw a filled box.

Note CD's argument order: the two X bounds, then the two Y bounds -- not
(x y width height), and not (x1 y1 x2 y2)."
  (if (%intp xmin xmax ymin ymax)
      (cd.ffi::%cd-canvas-box (handle canvas) xmin xmax ymin ymax)
      (cd.ffi::%cdf-canvas-box (handle canvas) (%d xmin) (%d xmax)
                               (%d ymin) (%d ymax)))
  canvas)

(defun rect (canvas xmin xmax ymin ymax)
  "Draw the outline of a box, in the same argument order as BOX."
  (if (%intp xmin xmax ymin ymax)
      (cd.ffi::%cd-canvas-rect (handle canvas) xmin xmax ymin ymax)
      (cd.ffi::%cdf-canvas-rect (handle canvas) (%d xmin) (%d xmax)
                                (%d ymin) (%d ymax)))
  canvas)

(defun arc (canvas xc yc width height angle1 angle2)
  "Draw an elliptical arc centred at (XC, YC).

WIDTH and HEIGHT are the full axes of the bounding ellipse, not radii. Angles
are in degrees, counter-clockwise from the positive X axis."
  (if (%intp xc yc width height)
      (cd.ffi::%cd-canvas-arc (handle canvas) xc yc width height
                              (%d angle1) (%d angle2))
      (cd.ffi::%cdf-canvas-arc (handle canvas) (%d xc) (%d yc)
                               (%d width) (%d height) (%d angle1) (%d angle2)))
  canvas)

(defun sector (canvas xc yc width height angle1 angle2)
  "Draw a filled pie slice. Arguments as for ARC."
  (if (%intp xc yc width height)
      (cd.ffi::%cd-canvas-sector (handle canvas) xc yc width height
                                 (%d angle1) (%d angle2))
      (cd.ffi::%cdf-canvas-sector (handle canvas) (%d xc) (%d yc)
                                  (%d width) (%d height) (%d angle1) (%d angle2)))
  canvas)

(defun chord (canvas xc yc width height angle1 angle2)
  "Draw a filled chord -- the arc closed by a straight line. Arguments as ARC."
  (if (%intp xc yc width height)
      (cd.ffi::%cd-canvas-chord (handle canvas) xc yc width height
                                (%d angle1) (%d angle2))
      (cd.ffi::%cdf-canvas-chord (handle canvas) (%d xc) (%d yc)
                                 (%d width) (%d height) (%d angle1) (%d angle2)))
  canvas)

(defun mark (canvas x y)
  "Draw the current marker at (X, Y). See the :mark-type attribute."
  (if (%intp x y)
      (cd.ffi::%cd-canvas-mark (handle canvas) x y)
      (cd.ffi::%cdf-canvas-mark (handle canvas) (%d x) (%d y)))
  canvas)

(defun pixel (canvas x y color)
  "Set a single pixel. COLOR is any colour designator; see COLOR."
  (if (%intp x y)
      (cd.ffi::%cd-canvas-pixel (handle canvas) x y (color color))
      (cd.ffi::%cdf-canvas-pixel (handle canvas) (%d x) (%d y) (color color)))
  canvas)

(defun text (canvas x y string)
  "Draw STRING with its alignment point at (X, Y). See TEXT-ALIGNMENT."
  (if (%intp x y)
      (cd.ffi::%cd-canvas-text (handle canvas) x y string)
      (cd.ffi::%cdf-canvas-text (handle canvas) (%d x) (%d y) string))
  canvas)

;;; Polygons and paths --------------------------------------------------------

(defparameter *polygon-modes*
  '((:fill . 0) (:open-lines . 1) (:closed-lines . 2) (:clip . 3)
    (:bezier . 4) (:region . 5) (:path . 6))
  "CD's polygon modes, in the order cd.h's anonymous enum declares them.")

(defun begin-shape (canvas mode)
  "Begin a vertex sequence. MODE is a key from *POLYGON-MODES*.

Prefer WITH-SHAPE, which cannot leave a shape unterminated."
  (cd.ffi::%cd-canvas-begin
   (handle canvas)
   (or (cdr (assoc mode *polygon-modes*))
       (cl:error 'cd-error
                 :detail (format nil "~S is not a polygon mode; expected one of ~S"
                                 mode (mapcar #'car *polygon-modes*)))))
  canvas)

(defun end-shape (canvas)
  "End the vertex sequence begun by BEGIN-SHAPE, drawing it."
  (cd.ffi::%cd-canvas-end (handle canvas))
  canvas)

(defun vertex (canvas x y)
  "Add a vertex to the shape in progress."
  (if (%intp x y)
      (cd.ffi::%cd-canvas-vertex (handle canvas) x y)
      (cd.ffi::%cdf-canvas-vertex (handle canvas) (%d x) (%d y)))
  canvas)

(defmacro with-shape ((canvas mode) &body body)
  "Run BODY between BEGIN-SHAPE and END-SHAPE.

  (cd:with-shape (c :fill)
    (cd:vertex c 0 0) (cd:vertex c 50 0) (cd:vertex c 25 40))

The shape is ended however BODY leaves. That matters more than the usual
unwind-protect argument: a canvas left mid-shape has CD accumulating vertices
into a buffer, and the next unrelated drawing call joins the polygon."
  (alexandria:with-gensyms (c)
    `(let ((,c ,canvas))
       (begin-shape ,c ,mode)
       (unwind-protect (progn ,@body)
         (end-shape ,c)))))

;;; Whole-canvas operations ---------------------------------------------------

(defun clear (canvas)
  "Fill the whole canvas with the background colour."
  (cd.ffi::%cd-canvas-clear (handle canvas))
  canvas)

(defun flush (canvas)
  "Make drawing so far visible, or start a new page on a paged driver.

On PostScript and PDF this ends the current page and begins another, which is
how a multi-page document is produced."
  (cd.ffi::%cd-canvas-flush (handle canvas))
  canvas)

;;; Attributes ----------------------------------------------------------------
;;;
;;; CD's attribute setters double as getters: passing CD_QUERY (-1) returns the
;;; current value without changing it. Each wrapper is therefore a function and
;;; a SETF pair, which reads better from Lisp than threading the sentinel
;;; through by hand.

(defconstant +query+ -1
  "CD's sentinel for \"report the current value rather than setting one\".")

(defun foreground (canvas)
  "The current foreground colour, packed."
  (cd.ffi::%cd-canvas-foreground (handle canvas) +query+))

(defun (setf foreground) (value canvas)
  (cd.ffi::%cd-canvas-foreground (handle canvas) (color value))
  value)

(defun background (canvas)
  "The current background colour, packed."
  (cd.ffi::%cd-canvas-background (handle canvas) +query+))

(defun (setf background) (value canvas)
  (cd.ffi::%cd-canvas-background (handle canvas) (color value))
  value)

(defmacro %define-int-attribute (name c-function docstring &optional table)
  "Define an integer-valued attribute as a reader and a SETF pair.

TABLE, when given, maps keywords to CD's integers so callers write :bevel
rather than 2."
  (let ((setter (intern (format nil "SET-~A" (symbol-name name)))))
    `(progn
       (defun ,name (canvas)
         ,docstring
         (let ((raw (,c-function (handle canvas) +query+)))
           ,(if table
                `(or (car (rassoc raw ,table)) raw)
                'raw)))
       (defun ,setter (canvas value)
         (,c-function (handle canvas)
                      ,(if table
                           `(if (keywordp value)
                                (or (cdr (assoc value ,table))
                                    (cl:error 'cd-error
                                              :detail (format nil "~S is not valid here; expected one of ~S"
                                                              value (mapcar #'car ,table))))
                                value)
                           'value))
         value)
       (defun (setf ,name) (value canvas) (,setter canvas value)))))

(defparameter *line-styles*
  '((:continuous . 0) (:dashed . 1) (:dotted . 2) (:dash-dot . 3)
    (:dash-dot-dot . 4) (:custom . 5)))

(defparameter *line-joins* '((:miter . 0) (:bevel . 1) (:round . 2)))
(defparameter *line-caps* '((:flat . 0) (:square . 1) (:round . 2)))

(defparameter *interior-styles*
  '((:solid . 0) (:hatch . 1) (:stipple . 2) (:pattern . 3) (:hollow . 4)))

(defparameter *hatch-styles*
  '((:horizontal . 0) (:vertical . 1) (:fdiagonal . 2) (:bdiagonal . 3)
    (:cross . 4) (:diagcross . 5)))

(defparameter *write-modes* '((:replace . 0) (:xor . 1) (:not-xor . 2)))
(defparameter *back-opacities* '((:transparent . 0) (:opaque . 1)))
(defparameter *fill-modes* '((:even-odd . 0) (:winding . 1)))

(%define-int-attribute line-width cd.ffi::%cd-canvas-line-width
  "Line width in pixels. Settable.")
(%define-int-attribute line-style cd.ffi::%cd-canvas-line-style
  "Line style: :continuous, :dashed, :dotted, :dash-dot, :dash-dot-dot."
  *line-styles*)
(%define-int-attribute line-join cd.ffi::%cd-canvas-line-join
  "How line segments meet: :miter, :bevel or :round." *line-joins*)
(%define-int-attribute line-cap cd.ffi::%cd-canvas-line-cap
  "How line ends are drawn: :flat, :square or :round." *line-caps*)
(%define-int-attribute interior-style cd.ffi::%cd-canvas-interior-style
  "How filled shapes are filled: :solid, :hatch, :stipple, :pattern, :hollow."
  *interior-styles*)
(%define-int-attribute hatch-style cd.ffi::%cd-canvas-hatch
  "The hatch pattern used when INTERIOR-STYLE is :hatch." *hatch-styles*)
(%define-int-attribute write-mode cd.ffi::%cd-canvas-write-mode
  "How new drawing combines with what is there: :replace, :xor, :not-xor."
  *write-modes*)
(%define-int-attribute back-opacity cd.ffi::%cd-canvas-back-opacity
  "Whether text and hatch backgrounds are :opaque or :transparent."
  *back-opacities*)
(%define-int-attribute fill-mode cd.ffi::%cd-canvas-fill-mode
  "How self-intersecting polygons are filled: :even-odd or :winding."
  *fill-modes*)

;;; Text ----------------------------------------------------------------------

(defparameter *text-alignments*
  '((:north . 0) (:south . 1) (:east . 2) (:west . 3)
    (:north-east . 4) (:north-west . 5) (:south-east . 6) (:south-west . 7)
    (:center . 8) (:base-left . 9) (:base-center . 10) (:base-right . 11)))

(%define-int-attribute text-alignment cd.ffi::%cd-canvas-text-alignment
  "Where the point passed to TEXT sits relative to the string: :center,
:north-west, :base-left and so on." *text-alignments*)

(defun text-orientation (canvas)
  "Text rotation in degrees, counter-clockwise."
  (cd.ffi::%cd-canvas-text-orientation (handle canvas) (%d +query+)))

(defun (setf text-orientation) (value canvas)
  (cd.ffi::%cd-canvas-text-orientation (handle canvas) (%d value))
  value)

(defparameter *font-styles*
  '((:plain . 0) (:bold . 1) (:italic . 2) (:bold-italic . 3)
    (:underline . 4) (:strikeout . 8)))

(defun font (canvas &key face style size)
  "Read or set the current font.

With no arguments returns (VALUES FACE STYLE SIZE). With any of FACE, STYLE or
SIZE it sets the font, filling the others from the current one.

SIZE follows CD's convention: positive is points, negative is pixels.

  (cd:font c :face \"Helvetica\" :style :bold :size 12)"
  (if (or face style size)
      (multiple-value-bind (current-face current-style current-size) (font canvas)
        (cd.ffi::%cd-canvas-font
         (handle canvas)
         (or face current-face)
         (let ((s (or style current-style)))
           (if (keywordp s)
               (or (cdr (assoc s *font-styles*))
                   (cl:error 'cd-error
                             :detail (format nil "~S is not a font style; expected one of ~S"
                                             s (mapcar #'car *font-styles*))))
               s))
         (or size current-size))
        (values (or face current-face) (or style current-style)
                (or size current-size)))
      (cffi:with-foreign-objects ((style-out :int) (size-out :int)
                                  (face-out :char 1024))
        ;; cdCanvasGetFont writes the face into a caller-supplied buffer with
        ;; no length argument, so the buffer has to be generous.
        (cd.ffi::%cd-canvas-get-font (handle canvas) face-out style-out size-out)
        (let ((raw-style (cffi:mem-ref style-out :int)))
          (values (cffi:foreign-string-to-lisp face-out)
                  (or (car (rassoc raw-style *font-styles*)) raw-style)
                  (cffi:mem-ref size-out :int))))))

(defun font-dimensions (canvas)
  "(VALUES MAX-WIDTH HEIGHT ASCENT DESCENT) for the current font, in pixels."
  (cffi:with-foreign-objects ((w :int) (h :int) (ascent :int) (descent :int))
    (cd.ffi::%cd-canvas-get-font-dim (handle canvas) w h ascent descent)
    (values (cffi:mem-ref w :int) (cffi:mem-ref h :int)
            (cffi:mem-ref ascent :int) (cffi:mem-ref descent :int))))

(defun text-size (canvas string)
  "(VALUES WIDTH HEIGHT) that STRING would occupy in the current font."
  (cffi:with-foreign-objects ((w :int) (h :int))
    (cd.ffi::%cd-canvas-get-text-size (handle canvas) string w h)
    (values (cffi:mem-ref w :int) (cffi:mem-ref h :int))))

;;; Driver-specific attributes ------------------------------------------------

(defun set-attribute (canvas name value)
  "Set a driver-specific attribute by name.

The escape hatch for the many attributes CD exposes only as strings -- a PDF
canvas takes \"TITLE\" and \"AUTHOR\", an SVG one takes \"CSSFILE\". Unknown
names are ignored by the driver rather than reported."
  (cd.ffi::%cd-canvas-set-attribute (handle canvas) name value)
  value)

(defun attribute (canvas name)
  "Read a driver-specific attribute, or NIL if the driver has no such name."
  (cd.ffi::%cd-canvas-get-attribute (handle canvas) name))

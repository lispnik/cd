;;;; src/world.lisp — the world-coordinate layer.
;;;;
;;;; CD's wd* functions draw in the caller's own units, with CD mapping them
;;;; onto the canvas. Set a window in world coordinates and a viewport in
;;;; pixels, and everything after is expressed in the units the problem is
;;;; actually in -- which for a plot or a chart is the difference between the
;;;; drawing code and the arithmetic to place it.
;;;;
;;;; These keep the WD- prefix rather than overloading the pixel names.
;;;; cdCanvasLine and wdCanvasLine are different functions against the same
;;;; canvas, and a caller has to know which coordinate space they are in; a
;;;; single LINE that guessed from its argument types would guess wrong the
;;;; moment someone passed integers to the world layer.

(in-package #:cd)

(export '(wd-window wd-viewport
          wd-world-to-canvas wd-canvas-to-world
          wd-line wd-box wd-rect wd-arc wd-sector wd-chord
          wd-mark wd-pixel wd-text wd-vertex
          wd-line-width wd-mark-size wd-font
          wd-text-size wd-font-dimensions
          wd-vector-text wd-vector-text-size wd-vector-char-size
          with-wd-window))

;;; The mapping ---------------------------------------------------------------

(defun wd-window (canvas)
  "(VALUES XMIN XMAX YMIN YMAX) of the current world window."
  (cffi:with-foreign-objects ((xmin :double) (xmax :double)
                              (ymin :double) (ymax :double))
    (cd.ffi::%wd-canvas-get-window (handle canvas) xmin xmax ymin ymax)
    (values (cffi:mem-ref xmin :double) (cffi:mem-ref xmax :double)
            (cffi:mem-ref ymin :double) (cffi:mem-ref ymax :double))))

(defun (setf wd-window) (bounds canvas)
  "Set the world window from a list (XMIN XMAX YMIN YMAX)."
  (destructuring-bind (xmin xmax ymin ymax) bounds
    (cd.ffi::%wd-canvas-window (handle canvas)
                               (%d xmin) (%d xmax) (%d ymin) (%d ymax)))
  bounds)

(defun wd-viewport (canvas)
  "(VALUES XMIN XMAX YMIN YMAX) of the viewport, in pixels."
  (cffi:with-foreign-objects ((xmin :int) (xmax :int) (ymin :int) (ymax :int))
    (cd.ffi::%wd-canvas-get-viewport (handle canvas) xmin xmax ymin ymax)
    (values (cffi:mem-ref xmin :int) (cffi:mem-ref xmax :int)
            (cffi:mem-ref ymin :int) (cffi:mem-ref ymax :int))))

(defun (setf wd-viewport) (bounds canvas)
  "Set the viewport in pixels, from a list (XMIN XMAX YMIN YMAX)."
  (destructuring-bind (xmin xmax ymin ymax) bounds
    (cd.ffi::%wd-canvas-viewport (handle canvas) xmin xmax ymin ymax))
  bounds)

(defmacro with-wd-window ((canvas xmin xmax ymin ymax) &body body)
  "Draw BODY in the given world window, restoring the previous one after.

The window is canvas state like any other attribute, so a function that sets
one and does not restore it changes the meaning of every later coordinate."
  (alexandria:with-gensyms (c saved)
    `(let* ((,c ,canvas)
            (,saved (multiple-value-list (wd-window ,c))))
       (setf (wd-window ,c) (list ,xmin ,xmax ,ymin ,ymax))
       (unwind-protect (progn ,@body)
         (setf (wd-window ,c) ,saved)))))

(defun wd-world-to-canvas (canvas x y)
  "(VALUES X Y) in pixels for a point given in world coordinates."
  (cffi:with-foreign-objects ((cx :int) (cy :int))
    (cd.ffi::%wd-canvas-world2-canvas (handle canvas) (%d x) (%d y) cx cy)
    (values (cffi:mem-ref cx :int) (cffi:mem-ref cy :int))))

(defun wd-canvas-to-world (canvas x y)
  "(VALUES X Y) in world coordinates for a point given in pixels."
  (cffi:with-foreign-objects ((wx :double) (wy :double))
    (cd.ffi::%wd-canvas-canvas2-world (handle canvas) x y wx wy)
    (values (cffi:mem-ref wx :double) (cffi:mem-ref wy :double))))

;;; Primitives ----------------------------------------------------------------
;;;
;;; All doubles: the world layer has no integer entry points, because a world
;;; coordinate is a measurement rather than a pixel index.

(defun wd-line (canvas x1 y1 x2 y2)
  "Draw a line in world coordinates."
  (cd.ffi::%wd-canvas-line (handle canvas) (%d x1) (%d y1) (%d x2) (%d y2))
  canvas)

(defun wd-box (canvas xmin xmax ymin ymax)
  "Draw a filled box in world coordinates. Note the X-then-Y bound order."
  (cd.ffi::%wd-canvas-box (handle canvas) (%d xmin) (%d xmax) (%d ymin) (%d ymax))
  canvas)

(defun wd-rect (canvas xmin xmax ymin ymax)
  "Draw a box outline in world coordinates."
  (cd.ffi::%wd-canvas-rect (handle canvas) (%d xmin) (%d xmax) (%d ymin) (%d ymax))
  canvas)

(defun wd-arc (canvas xc yc width height angle1 angle2)
  "Draw an elliptical arc in world coordinates. Angles are degrees."
  (cd.ffi::%wd-canvas-arc (handle canvas) (%d xc) (%d yc)
                          (%d width) (%d height) (%d angle1) (%d angle2))
  canvas)

(defun wd-sector (canvas xc yc width height angle1 angle2)
  "Draw a filled pie slice in world coordinates."
  (cd.ffi::%wd-canvas-sector (handle canvas) (%d xc) (%d yc)
                             (%d width) (%d height) (%d angle1) (%d angle2))
  canvas)

(defun wd-chord (canvas xc yc width height angle1 angle2)
  "Draw a filled chord in world coordinates."
  (cd.ffi::%wd-canvas-chord (handle canvas) (%d xc) (%d yc)
                            (%d width) (%d height) (%d angle1) (%d angle2))
  canvas)

(defun wd-mark (canvas x y)
  "Draw the current marker at a world coordinate."
  (cd.ffi::%wd-canvas-mark (handle canvas) (%d x) (%d y))
  canvas)

(defun wd-pixel (canvas x y color)
  "Set the pixel nearest a world coordinate."
  (cd.ffi::%wd-canvas-pixel (handle canvas) (%d x) (%d y) (color color))
  canvas)

(defun wd-text (canvas x y string)
  "Draw text at a world coordinate."
  (cd.ffi::%wd-canvas-text (handle canvas) (%d x) (%d y) string)
  canvas)

(defun wd-vertex (canvas x y)
  "Add a vertex in world coordinates to the shape in progress.

Pairs with BEGIN-SHAPE and END-SHAPE, which are shared with the pixel layer:
CD has one shape in progress per canvas, not one per coordinate space."
  (cd.ffi::%wd-canvas-vertex (handle canvas) (%d x) (%d y))
  canvas)

;;; Attributes measured in world units ----------------------------------------

(defun wd-line-width (canvas)
  "Line width in world units."
  (cd.ffi::%wd-canvas-line-width (handle canvas) (%d +query+)))

(defun (setf wd-line-width) (value canvas)
  (cd.ffi::%wd-canvas-line-width (handle canvas) (%d value))
  value)

(defun wd-mark-size (canvas)
  "Marker size in world units."
  (cd.ffi::%wd-canvas-mark-size (handle canvas) (%d +query+)))

(defun (setf wd-mark-size) (value canvas)
  (cd.ffi::%wd-canvas-mark-size (handle canvas) (%d value))
  value)

(defun wd-font (canvas &key face style size)
  "Read or set the font with SIZE in world units rather than points.

With no arguments returns (VALUES FACE STYLE SIZE)."
  (if (or face style size)
      (multiple-value-bind (current-face current-style current-size)
          (wd-font canvas)
        (cd.ffi::%wd-canvas-font
         (handle canvas)
         (or face current-face)
         (let ((s (or style current-style)))
           (if (keywordp s)
               (or (cdr (assoc s *font-styles*))
                   (cl:error 'cd-error
                             :detail (format nil "~S is not a font style; expected one of ~S"
                                             s (mapcar #'car *font-styles*))))
               s))
         (%d (or size current-size)))
        (values (or face current-face) (or style current-style)
                (or size current-size)))
      (cffi:with-foreign-objects ((style-out :int) (size-out :double)
                                  (face-out :char 1024))
        (cd.ffi::%wd-canvas-get-font (handle canvas) face-out style-out size-out)
        (let ((raw-style (cffi:mem-ref style-out :int)))
          (values (cffi:foreign-string-to-lisp face-out)
                  (or (car (rassoc raw-style *font-styles*)) raw-style)
                  (cffi:mem-ref size-out :double))))))

(defun wd-text-size (canvas string)
  "(VALUES WIDTH HEIGHT) STRING would occupy, in world units."
  (cffi:with-foreign-objects ((w :double) (h :double))
    (cd.ffi::%wd-canvas-get-text-size (handle canvas) string w h)
    (values (cffi:mem-ref w :double) (cffi:mem-ref h :double))))

(defun wd-font-dimensions (canvas)
  "(VALUES MAX-WIDTH HEIGHT ASCENT DESCENT) in world units."
  (cffi:with-foreign-objects ((w :double) (h :double)
                              (ascent :double) (descent :double))
    (cd.ffi::%wd-canvas-get-font-dim (handle canvas) w h ascent descent)
    (values (cffi:mem-ref w :double) (cffi:mem-ref h :double)
            (cffi:mem-ref ascent :double) (cffi:mem-ref descent :double))))

;;; Vector text ---------------------------------------------------------------
;;;
;;; Text drawn as line segments rather than glyphs. Every driver can render it
;;; identically, and it scales and rotates with the transformation, which is
;;; what a plot axis label usually wants.

(defun wd-vector-text (canvas x y string)
  "Draw STRING as stroked line segments at a world coordinate."
  (cd.ffi::%wd-canvas-vector-text (handle canvas) (%d x) (%d y) string)
  canvas)

(defun wd-vector-char-size (canvas)
  "The height of a vector-text character, in world units."
  (cd.ffi::%wd-canvas-vector-char-size (handle canvas) (%d +query+)))

(defun (setf wd-vector-char-size) (value canvas)
  (cd.ffi::%wd-canvas-vector-char-size (handle canvas) (%d value))
  value)

(defun wd-vector-text-size (canvas string)
  "(VALUES WIDTH HEIGHT) STRING would occupy as vector text, in world units."
  (cffi:with-foreign-objects ((w :double) (h :double))
    (cd.ffi::%wd-canvas-get-vector-text-size (handle canvas) string w h)
    (values (cffi:mem-ref w :double) (cffi:mem-ref h :double))))

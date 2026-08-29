;;;; src/clipping.lisp — clipping, regions and the transformation matrix.

(in-package #:cd)

(export '(clip clip-area with-clip-area
          region-box point-in-region-p offset-region region-combine-mode
          transform transform-identity with-transform
          transform-translate transform-scale transform-rotate
          transform-multiply transform-point))

(defparameter *clip-modes*
  '((:off . 0) (:area . 1) (:polygon . 2) (:region . 3) (:path . 4))
  "CD's clipping modes, in the order cd.h's anonymous enum declares them.")

(defun clip (canvas)
  "The current clipping mode: :off, :area, :polygon, :region or :path."
  (let ((raw (cd.ffi::%cd-canvas-clip (handle canvas) +query+)))
    (or (car (rassoc raw *clip-modes*)) raw)))

(defun (setf clip) (mode canvas)
  (cd.ffi::%cd-canvas-clip
   (handle canvas)
   (if (keywordp mode)
       (or (cdr (assoc mode *clip-modes*))
           (cl:error 'cd-error
                     :detail (format nil "~S is not a clipping mode; expected one of ~S"
                                     mode (mapcar #'car *clip-modes*))))
       mode))
  mode)

(defun clip-area (canvas)
  "(VALUES XMIN XMAX YMIN YMAX) of the current clipping rectangle.

Note CD's ordering, which is the same as BOX's and not (x y w h)."
  (cffi:with-foreign-objects ((xmin :int) (xmax :int) (ymin :int) (ymax :int))
    (cd.ffi::%cd-canvas-get-clip-area (handle canvas) xmin xmax ymin ymax)
    (values (cffi:mem-ref xmin :int) (cffi:mem-ref xmax :int)
            (cffi:mem-ref ymin :int) (cffi:mem-ref ymax :int))))

(defun (setf clip-area) (bounds canvas)
  "Set the clipping rectangle from a list (XMIN XMAX YMIN YMAX).

Setting the area does not by itself switch clipping on -- CD keeps the region
and the mode separate, so this must be paired with (setf (clip canvas) :area).
WITH-CLIP-AREA does both."
  (destructuring-bind (xmin xmax ymin ymax) bounds
    (if (every #'integerp bounds)
        (cd.ffi::%cd-canvas-clip-area (handle canvas) xmin xmax ymin ymax)
        (cd.ffi::%cdf-canvas-clip-area (handle canvas) (%d xmin) (%d xmax)
                                       (%d ymin) (%d ymax))))
  bounds)

(defmacro with-clip-area ((canvas xmin xmax ymin ymax) &body body)
  "Clip drawing in BODY to the given rectangle, restoring the mode after.

Sets the area and turns clipping on, which CD treats as two separate things --
setting the rectangle alone changes nothing until the mode is :area, and that
asymmetry is a common way to spend an afternoon."
  (alexandria:with-gensyms (c previous)
    `(let* ((,c ,canvas)
            (,previous (clip ,c)))
       (setf (clip-area ,c) (list ,xmin ,xmax ,ymin ,ymax)
             (clip ,c) :area)
       (unwind-protect (progn ,@body)
         (setf (clip ,c) ,previous)))))

;;; Regions -------------------------------------------------------------------

(defun region-box (canvas)
  "(VALUES XMIN XMAX YMIN YMAX) bounding the current region."
  (cffi:with-foreign-objects ((xmin :int) (xmax :int) (ymin :int) (ymax :int))
    (cd.ffi::%cd-canvas-get-region-box (handle canvas) xmin xmax ymin ymax)
    (values (cffi:mem-ref xmin :int) (cffi:mem-ref xmax :int)
            (cffi:mem-ref ymin :int) (cffi:mem-ref ymax :int))))

(defun point-in-region-p (canvas x y)
  "True when (X, Y) falls inside the current region."
  (not (zerop (cd.ffi::%cd-canvas-is-point-in-region (handle canvas) x y))))

(defparameter *region-combine-modes*
  '((:union . 0) (:intersect . 1) (:difference . 2) (:not-intersect . 3))
  "How a new region combines with the existing one.")

(defun region-combine-mode (canvas)
  "How the next region operation combines with the current region."
  (let ((raw (cd.ffi::%cd-canvas-region-combine-mode (handle canvas) +query+)))
    (or (car (rassoc raw *region-combine-modes*)) raw)))

(defun (setf region-combine-mode) (mode canvas)
  (cd.ffi::%cd-canvas-region-combine-mode
   (handle canvas)
   (if (keywordp mode)
       (or (cdr (assoc mode *region-combine-modes*))
           (cl:error 'cd-error
                     :detail (format nil "~S is not a region combine mode; expected one of ~S"
                                     mode (mapcar #'car *region-combine-modes*))))
       mode))
  mode)

(defun offset-region (canvas dx dy)
  "Move the current region by (DX, DY)."
  (cd.ffi::%cd-canvas-offset-region (handle canvas) dx dy)
  canvas)

;;; Transformation ------------------------------------------------------------
;;;
;;; CD's transform is a 2x3 affine matrix given as six doubles in the order
;;; {a b c d e f}, mapping (x,y) to (a*x + b*y + c, d*x + e*y + f). Passing
;;; NULL resets it to the identity, which is why TRANSFORM-IDENTITY exists
;;; rather than a caller having to know that NULL means something.

(defun transform (canvas)
  "The current transformation as a list of six doubles, or NIL if none is set."
  (let ((pointer (cd.ffi::%cd-canvas-get-transform (handle canvas))))
    (unless (cffi:null-pointer-p pointer)
      (loop for i below 6 collect (cffi:mem-aref pointer :double i)))))

(defun (setf transform) (matrix canvas)
  "Set the transformation from a list of six doubles, or NIL for the identity."
  (if (null matrix)
      (cd.ffi::%cd-canvas-transform (handle canvas) (cffi:null-pointer))
      (cffi:with-foreign-object (m :double 6)
        (loop for value in matrix
              for i from 0
              do (setf (cffi:mem-aref m :double i) (%d value)))
        (cd.ffi::%cd-canvas-transform (handle canvas) m)))
  matrix)

(defun transform-identity (canvas)
  "Reset the transformation. CD spells this as passing NULL."
  (setf (transform canvas) nil)
  canvas)

(defun transform-translate (canvas dx dy)
  "Compose a translation onto the current transformation."
  (cd.ffi::%cd-canvas-transform-translate (handle canvas) (%d dx) (%d dy))
  canvas)

(defun transform-scale (canvas sx sy)
  "Compose a scale onto the current transformation."
  (cd.ffi::%cd-canvas-transform-scale (handle canvas) (%d sx) (%d sy))
  canvas)

(defun transform-rotate (canvas degrees)
  "Compose a rotation, in degrees counter-clockwise."
  (cd.ffi::%cd-canvas-transform-rotate (handle canvas) (%d degrees))
  canvas)

(defun transform-multiply (canvas matrix)
  "Compose MATRIX -- six doubles -- onto the current transformation."
  (cffi:with-foreign-object (m :double 6)
    (loop for value in matrix
          for i from 0
          do (setf (cffi:mem-aref m :double i) (%d value)))
    (cd.ffi::%cd-canvas-transform-multiply (handle canvas) m))
  canvas)

(defun transform-point (canvas x y)
  "(VALUES X' Y') for (X, Y) under the current transformation."
  (if (and (integerp x) (integerp y))
      (cffi:with-foreign-objects ((tx :int) (ty :int))
        (cd.ffi::%cd-canvas-transform-point (handle canvas) x y tx ty)
        (values (cffi:mem-ref tx :int) (cffi:mem-ref ty :int)))
      (cffi:with-foreign-objects ((tx :double) (ty :double))
        (cd.ffi::%cdf-canvas-transform-point (handle canvas) (%d x) (%d y) tx ty)
        (values (cffi:mem-ref tx :double) (cffi:mem-ref ty :double)))))

(defmacro with-transform ((canvas) &body body)
  "Run BODY with the transformation restored afterwards.

CD's transform is canvas state, so a function that rotates and does not undo
it leaves every later caller drawing at an angle."
  (alexandria:with-gensyms (c saved)
    `(let* ((,c ,canvas)
            (,saved (transform ,c)))
       (unwind-protect (progn ,@body)
         (setf (transform ,c) ,saved)))))

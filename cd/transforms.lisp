(in-package #:cd)

;;; Transform Matrix System

(deftype transformation-matrix ()
  "A 6-element transformation matrix (2D affine transform)"
  '(simple-array double-float (6)))

(defun make-identity-matrix ()
  "Create an identity transformation matrix"
  (make-array 6 :element-type 'double-float
              :initial-contents '(1.0d0 0.0d0 0.0d0 1.0d0 0.0d0 0.0d0)))

(defun make-translation-matrix (dx dy)
  "Create a translation transformation matrix"
  (declare (type real dx dy))
  (make-array 6 :element-type 'double-float
              :initial-contents (list 1.0d0 0.0d0 0.0d0 1.0d0
                                      (coerce dx 'double-float)
                                      (coerce dy 'double-float))))

(defun make-rotation-matrix (angle)
  "Create a rotation transformation matrix (angle in degrees)"
  (declare (type real angle))
  (let* ((radians (/ (* angle pi) 180.0d0))
         (cos-a (cos radians))
         (sin-a (sin radians)))
    (make-array 6 :element-type 'double-float
                :initial-contents (list cos-a sin-a (- sin-a) cos-a 0.0d0 0.0d0))))

(defun make-scaling-matrix (sx sy)
  "Create a scaling transformation matrix"
  (declare (type real sx sy))
  (make-array 6 :element-type 'double-float
              :initial-contents (list (coerce sx 'double-float) 0.0d0
                                      0.0d0 (coerce sy 'double-float)
                                      0.0d0 0.0d0)))

(defun multiply-matrices (m1 m2)
  "Multiply two transformation matrices"
  (declare (type transformation-matrix m1 m2))
  (let ((result (make-array 6 :element-type 'double-float)))
    (setf (aref result 0) (+ (* (aref m1 0) (aref m2 0)) (* (aref m1 1) (aref m2 2)))
          (aref result 1) (+ (* (aref m1 0) (aref m2 1)) (* (aref m1 1) (aref m2 3)))
          (aref result 2) (+ (* (aref m1 2) (aref m2 0)) (* (aref m1 3) (aref m2 2)))
          (aref result 3) (+ (* (aref m1 2) (aref m2 1)) (* (aref m1 3) (aref m2 3)))
          (aref result 4) (+ (* (aref m1 4) (aref m2 0)) (* (aref m1 5) (aref m2 2)) (aref m2 4))
          (aref result 5) (+ (* (aref m1 4) (aref m2 1)) (* (aref m1 5) (aref m2 3)) (aref m2 5)))
    result))

(defun (setf transform) (matrix canvas)
  "Set the transformation matrix for the canvas"
  (declare (type transformation-matrix matrix))
  (validate-canvas canvas)
  (cffi:with-foreign-object (foreign-matrix :double 6)
    (dotimes (i 6)
      (setf (cffi:mem-aref foreign-matrix :double i) (aref matrix i)))
    (cd-cffi::%cd-canvas-transform canvas foreign-matrix))
  matrix)

(defun transform (canvas)
  "Get the current transformation matrix"
  (validate-canvas canvas)
  (let ((foreign-matrix (cd-cffi::%cd-canvas-get-transform canvas))
        (matrix (make-array 6 :element-type 'double-float)))
    (when (not (cffi:null-pointer-p foreign-matrix))
      (dotimes (i 6)
        (setf (aref matrix i) (cffi:mem-aref foreign-matrix :double i))))
    matrix))

(defun transform-multiply (canvas matrix)
  "Multiply the current transformation matrix by the given matrix"
  (declare (type transformation-matrix matrix))
  (validate-canvas canvas)
  (cffi:with-foreign-object (foreign-matrix :double 6)
    (dotimes (i 6)
      (setf (cffi:mem-aref foreign-matrix :double i) (aref matrix i)))
    (cd-cffi::%cd-canvas-transform-multiply canvas foreign-matrix)))

(defun transform-translate (canvas dx dy)
  "Apply a translation to the current transformation"
  (declare (type real dx dy))
  (validate-canvas canvas)
  (cd-cffi::%cd-canvas-transform-translate canvas
                                           (coerce dx 'double-float)
                                           (coerce dy 'double-float)))

(defun transform-rotate (canvas angle)
  "Apply a rotation to the current transformation (angle in degrees)"
  (declare (type real angle))
  (validate-canvas canvas)
  (cd-cffi::%cd-canvas-transform-rotate canvas (coerce angle 'double-float)))

(defun transform-scale (canvas sx sy)
  "Apply scaling to the current transformation"
  (declare (type real sx sy))
  (validate-canvas canvas)
  (cd-cffi::%cd-canvas-transform-scale canvas
                                       (coerce sx 'double-float)
                                       (coerce sy 'double-float)))

(defun transform-point (canvas x y)
  "Transform a point using the current transformation matrix"
  (declare (type integer x y))
  (validate-canvas canvas)
  (cffi:with-foreign-objects ((tx :int) (ty :int))
    (cd-cffi::%cd-canvas-transform-point canvas x y tx ty)
    (values (cffi:mem-ref tx :int) (cffi:mem-ref ty :int))))

(defmacro with-transform ((canvas matrix) &body body)
  "Execute body with a temporary transformation matrix"
  (let ((old-matrix (gensym "OLD-MATRIX")))
    `(let ((,old-matrix (transform ,canvas)))
       (unwind-protect
            (progn
              (setf (transform ,canvas) ,matrix)
              ,@body)
         (setf (transform ,canvas) ,old-matrix)))))

(defmacro with-translation ((canvas dx dy) &body body)
  "Execute body with a temporary translation"
  `(with-transform (,canvas (make-translation-matrix ,dx ,dy))
     ,@body))

(defmacro with-rotation ((canvas angle) &body body)
  "Execute body with a temporary rotation"
  `(with-transform (,canvas (make-rotation-matrix ,angle))
     ,@body))

(defmacro with-scaling ((canvas sx sy) &body body)
  "Execute body with a temporary scaling"
  `(with-transform (,canvas (make-scaling-matrix ,sx ,sy))
     ,@body))

;;; Convenience Functions

(defun translate-coordinate-system (canvas dx dy)
  "Translate the coordinate system origin"
  (transform-translate canvas dx dy))

(defun rotate-coordinate-system (canvas angle)
  "Rotate the coordinate system"
  (transform-rotate canvas angle))

(defun scale-coordinate-system (canvas sx sy)
  "Scale the coordinate system"
  (transform-scale canvas sx sy))

(defun reset-transform (canvas)
  "Reset transformation to identity matrix"
  (setf (transform canvas) (make-identity-matrix)))
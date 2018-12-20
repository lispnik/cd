(in-package #:cd)

(export '(size
          update-y-axis
          invert-y-axis
          mm-to-pixel
          pixel-to-mm
          origin
          transform
          transform-multiply
          transform-translate
          transform-scale
          transform-rotate
          transform-point))

(defun size (canvas)
  "Returns the canvas width and height in pixels and width and height in
millimeters."
  (cffi:with-foreign-objects
      ((width-ptr :int)
       (height-ptr :int)
       (width-mm-ptr :double)
       (height-mm-ptr :double))
    (cd-cffi::%cd-canvas-get-size canvas width-ptr height-ptr width-mm-ptr height-mm-ptr)
    (values (cffi:mem-ref width-ptr :int)
            (cffi:mem-ref height-ptr :int)
            (cffi:mem-ref width-mm-ptr :double)
            (cffi:mem-ref height-mm-ptr :double))))

(defun update-y-axis (canvas y)
  "Invert the given Y coordinate if the native Y axis orientation is different
from the CD axis orientation. The CD axis orientation is always bottom-top. It
returns the new Y coordinate."
  (cffi:with-foreign-object (y-ptr :double)
    (setf (cffi:mem-ref y-ptr :double) (coerce y 'double-float))
    (cd-cffi::%cdf-canvas-update-y-axis canvas y-ptr)))

(defun invert-y-axis (canvas y)
  "Invert the given Y coordinate independent of the driver Y axis
orientation. It returns the new value."
  (cd-cffi::%cdf-canvas-invert-y-axis canvas (coerce y 'double-float)))

(defun mm-to-pixel (canvas dx-mm dy-mm)
  "Converts sizes in millimeters into pixels (canvas coordinates)."
  (cffi:with-foreign-objects
      ((dx-ptr :int)
       (dy-ptr :int))
    (cd-cffi::%cdf-canvas-mm-2-pixel
     canvas
     (coerce dx-mm 'double-float)
     (coerce dy-mm 'double-float)
     dx-ptr
     dy-ptr)
    (values (cffi:mem-ref dx-ptr :int)
            (cffi:mem-ref dy-ptr :int))))

(defun pixel-to-mm (canvas dx dy)
  "Converts sizes in pixels (canvas coordinates) into millimeters. Use this
function to obtain the horizontal and vertical resolution of the canvas by
passing 1 as parameter in dx and dy. The resolution value is obtained using the
formula res=1.0/mm."
  (cffi:with-foreign-objects
      ((mm-dx-ptr :double)
       (mm-dy-ptr :double))
    (cd-cffi::%cdf-canvas-pixel-2-mm canvas (coerce dx 'double-float) (coerce dy 'double-float) mm-dx-ptr mm-dy-ptr)
    (values (cffi:mem-ref mm-dx-ptr :double)
            (cffi:mem-ref mm-dy-ptr :double))))

(defun origin (canvas)
  "Returns the origin as x and y values."
  (cffi:with-foreign-objects
      ((x-ptr :double)
       (y-ptr :double))
    (cd-cffi::%cdf-canvas-get-origin canvas x-ptr y-ptr)
    (values (cffi:mem-ref x-ptr :double)
            (cffi:mem-ref y-ptr :double))))

(define-cd-setf-expander origin cd-cffi::%cdf-canvas-origin 2 double-float
  "Sets the origin translation values x and y.

Allows translating the origin - for instance, to the center of the canvas. The
function profits from the architecture of the library to simulate a translation
of the origin, which in fact is never actually passed to the canvas in the
respective driver. It is not related with WD nor Transformation Matrix. Default
values: (0, 0)")

(defun (setf transform) (matrix canvas)
  "Defines a transformation matrix with 6 elements. If the matrix is NIL, the
transformation is reset to the identity.

The matrix contains scale (sx,sy), rotation (angle) and
translation (dx,dy) elements as follows:

|x'|   |sx*cos(angle)    -sin(angle)  dx|   |x|                 |0   2   4|
|y'| = |   sin(angle)  sy*cos(angle)  dy| * |y|  with indices   |1   3   5|
                                            |1|
In other words:

matrix[0] = sx*cos(angle)    // Horizontal Scale and Rotation component
matrix[1] =    sin(angle)    // Rotation component (can also contain an horizontal shear component)
matrix[2] =   -sin(angle)    // Rotation component (can also contain a vertical shear component)
matrix[3] = sy*cos(angle)    // Vertical Scale and Rotation component
matrix[4] = dx               // Horizontal Translation component
matrix[5] = dy               // Vertical Translation component

But notice that the indices are different from those of VECTOR-TEXT-TRANSFORM.

Functions that retrieve images from the canvas are not affected by the
transformation matrix, such as IMAGE, IMAGE-RGB and SCROLL-AREA.

Transformation matrix is independent of the World Coordinate and Origin
functions. And those are affected if a transformation is set, just like other
regular primitives.

The transformation matrix and world coordinates perform similar functions. World
coordinates were developed before the transformation matrix support. The
transformation matrix operates at a lower level than world coordinates, and, as
such, might be faster, but might behave differently on different
platforms. World coordinates behave consistently across platforms."
  (check-type matrix sequence)
  (assert (or (null matrix) (= 6 (length matrix))))
  (if matrix 
      (cffi:with-foreign-object
          (matrix-ptr :double 6)
        (loop for i below 6
              for e = (elt matrix i)
              do (setf (cffi:mem-aref matrix-ptr :double i)
                       (coerce e 'double-float)))
        (cd-cffi::%cd-canvas-transform canvas matrix-ptr))
      (cd-cffi::%cd-canvas-transform canvas (cffi:null-pointer)))
  matrix)

(defun transform (canvas)
  "Returns the transformation matrix as a sequence. If the identity is set,
returns NIL."
  (let ((matrix-ptr (cd-cffi::%cd-canvas-get-transform canvas)))
    (if (cffi:null-pointer-p matrix-ptr)
        nil
        (loop for i below 6
              collect (cffi:mem-aref matrix-ptr :double i) into result
              finally (return (make-array 6 :initial-contents result))))))

(defun transform-multiply (canvas matrix)
  "Left multiply the current transformation by the given transformation."
  (check-type matrix sequence)
  (assert (or (null matrix) (= 6 (length matrix))))
  (when matrix
    (cffi:with-foreign-object
	(matrix-ptr :double 6)
      (loop for i below 6
	    do (setf (cffi:mem-aref matrix-ptr :double i)
		     (coerce (elt matrix i) 'double-float)))
      (cd-cffi::%cd-canvas-transform-multiply canvas matrix))))

(defun transform-translate (canvas dx dy)
  "Applies a translation to the current transformation."
  (cd-cffi::%cd-canvas-transform-translate canvas (coerce dx 'double-float) (coerce dy 'double-float)))

(defun transform-scale (canvas sx sy)
  "Applies a scale to the current transformation."
  (cd-cffi::%cd-canvas-transform-translate canvas (coerce sx 'double-float) (coerce sy 'double-float)))

(defun transform-rotate (canvas angle)
  "Applies a rotation to the current transformation. Angle is in degrees,
oriented counter-clockwise from the horizontal axis."
  (cd-cffi::%cd-canvas-transform-rotate canvas (coerce angle 'double-float)))

(defun transform-point (canvas x y)
  "Applies a transformation to a given point. Returns the transformed point x
and y values."
  (cffi:with-foreign-objects
      ((x-ptr :double)
       (y-ptr :double))
    (cd-cffi::%cdf-canvas-transform-point
     canvas
     (coerce x 'double-float)
     (coerce y 'double-float)
     x-ptr
     y-ptr)
    (values (cffi:mem-ref x-ptr :double)
            (cffi:mem-ref y-ptr :double))))

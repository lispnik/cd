(in-package #:cd)

(export '(vector-text-transform
          vector-text-size
          vector-character-size
          vector-font-size
          reset-vector-font
          vector-font
          vector-text-size
          vector-text-bounds
          vector-text-box))

(export '(wd::vector-text-bounds) (find-package "WD"))

(defwrappers ("canvas-vector-text" "vector-text" "cd")
  "Draws a vector text in position (x,y), respecting the alignment defined by
TEXT-ALIGNMENT. It ignores the configuration BACKGROUND-OPACITY, being always
transparent. It accepts strings with multiple lines using '\n'. It is ESSENTIAL
to call VECTOR-TEXT-SIZE or VECTOR-CHARACTER-SIZE before using this function.

Vector text uses a font created from line segments. It is very useful to be
scaled and very fast. You must set the text size before drawing any text. The
default direction is horizontal from left to right.

TODO http://webserver2.tecgraf.puc-rio.br/cd/img/vector_text.gif"
  (defun ^function-name (canvas x y text)
    (^cffi-function-name canvas (coerce x ^type) (coerce y ^type) text)))

(define-cd-setf-expander
    vector-text-direction cd-cffi::%cdf-canvas-vector-text-direction 4 'double-float
  "Defines the text direction by means of two points, (x1,y1) and (x2,y2), given
as values x1, y1, x2, y2. The default direction is horizontal from left to
right. It is independent from the transformation matrix.")

(defun (setf vector-text-transform) (matrix canvas)
  "Defines a transformation matrix with 6 elements. If the matrix is NIL, no
transformation is set. The default is no transformation. The origin is the left
bottom corner of matrix. 

The matrix contains scale, rotation and translation elements. It is applied
after computing the position and orientation normal to the vector text. We can
describe the elements as follows:

|x'|   | scl_x*cos(ang)       -sin(ang)  trans_x |   |x|                | 3   4   5| 
|y'| = |       sin(ang)  scl_y*cos(ang)  trans_y | * |y| with indices   | 0   1   2|
                                                     |1|

It has the same effect of the TRANSFORM but notice that the indices are
different."
  (check-type matrix sequence)
  (assert (or (null matrix) (= 6 (length matrix))))
  (when matrix
    (cffi:with-foreign-object
	(matrix-ptr :double 6)
      (loop for i below 6
            for e = (elt matrix i)
            do (setf (cffi:mem-aref matrix-ptr :double i)
                     (coerce e 'double-float)))
      (cd-cffi::%cd-canvas-vector-text-transform canvas matrix-ptr)
      matrix)))

(defun vector-text-transform (canvas)
  "Return the currently configured vector text transform."
  (let ((matrix-ptr (cd-cffi::%cd-canvas-vector-text-transform canvas (cffi:null-pointer))))
    (loop for i below 6
          collect (cffi:mem-aref matrix-ptr :double i) into list
          finally (return (make-array 6 :initial-contents list)))))

(define-setf-expander vector-text-size (canvas text &environment env)
  "Modifies the font size of the vector text so that it fits the string in the
box defined by the supplied width and height values. Values provided: WIDTH,
HEIGHT"
  (multiple-value-bind
        (temp-vars temp-forms store-vars setter-form getter-form)
      (get-setf-expansion canvas env)
    (declare (ignore store-vars setter-form))
    (let* ((new-value-vars (list (gensym) (gensym)))
           (new-value-vars-double (mapcar (lambda (x)
                                            `(coerce ,x 'double-float))
                                          new-value-vars)))
      (values `(,@temp-vars)
              `(,@temp-forms)
              `,new-value-vars
              `(progn
                 (cd-cffi::%cdf-canvas-vector-text-size
                  ,getter-form
                  ,@new-value-vars-double
                  ,text)
                 (values ,@new-value-vars))
              `(vector-text-size canvas text)))))

(defun (setf vector-character-size) (new-size canvas)
  "Modifies the font size by specifying the height of the characters."
  (cd-cffi::%cdf-canvas-vector-char-size canvas (coerce new-size 'double-float))
  new-size)

(define-cd-setf-expander
    vector-font-size cd-cffi::%cd-canvas-vector-font-size 2 double-float
  "Directly modifies the font size using x and y values. Set x and y values
equal to maintain the original aspect ratio of the font.")

(defun vector-font-size (canvas)
  "Returns the vector font size."
  (cffi:with-foreign-objects
      ((size-x-ptr :double)
       (size-y-ptr :double))
    (cd-cffi::%cd-canvas-get-vector-font-size canvas size-x-ptr size-y-ptr)
    (values (cffi:mem-ref size-x-ptr :double)
            (cffi:mem-ref size-y-ptr :double))))

(defun reset-vector-font (canvas)
  "Reset the vector font to the embedded font, \"Simplex II\" and return NIL.
Bellow is the character code table of the default font."
  (cd-cffi::%cd-canvas-vector-font canvas (cffi:null-pointer))
  nil)

(defun (setf vector-font) (pathname canvas)
  "Replaces the current vector font with a font stored in a file with a given
name. Returns the name of the font loaded or NULL, if it fails. If filename is
NIL, it activates the default font \"Simplex II\" (There is no file associated
to this font, it is an embedded font).

The library will attempt to load a font from the current directory, if it fails
then it will try the directory defined by the environment variable \"CDDIR\", if
it fails, it will attempt to load it using the filename as a string containing
the font as if the file was loaded into that string, if it fails again then
restarts the default font is used and NIL is returned.

The file format is compatible with the GKS file format (text mode)."
  (let* ((filename (if (null pathname)
                       (cffi:null-pointer)
                       (etypecase pathname
                         (pathname (namestring pathname))
                         (string pathname)))))
    (unless (null (cd-cffi::%cd-canvas-vector-font canvas filename))
      pathname)))

(defun vector-text-size (canvas text)
  "Returns the text size independent from orientation as width and height
values."
  (cffi:with-foreign-objects
      ((w-ptr :double)
       (h-ptr :double))
    (cd-cffi::%cdf-canvas-vector-text-size canvas text w-ptr h-ptr)
    (values (cffi:mem-ref w-ptr :double)
            (cffi:mem-ref h-ptr :double))))

(defun wd::vector-text-bounds (canvas text x y)
  "Returns the oriented bounding rectangle occupied by a text at a given
position. The rectangle has the same dimentions returned by
VECTOR-TEXT-SIZE. The rectangle corners are returned in counter-clock wise order
as values starting with the bottom left corner,
arranged x0, y0, x1, y1, x2, y2, x3, y3."
  (cffi:with-foreign-object
      (rect-ptr :double 8)
    (cd-cffi::%cdf-canvas-get-vector-text-bounds
     canvas
     text
     (coerce x 'double-float) (coerce y 'double-float)
     rect-ptr)
    (loop for i below 8
          collect (cffi:mem-aref rect-ptr :double i) into result
          finally (return (values-list result)))))

(defun vector-text-box (canvas text x y)
  "Returns the horizontal bounding rectangle occupied by a text at a given
position. If orientation is not 0 then its area is always larger than the area
of the rectangle returned by VECTOR-TEXT-BOUNDS. VAlues are returned: xmin,
xmax, ymin, ymax."
  (cffi:with-foreign-objects
      ((xmin-ptr :double)
       (xmax-ptr :double)
       (ymin-ptr :double)
       (ymax-ptr :double))
    (cd-cffi::%cdf-canvas-get-vector-text-box
     canvas
     (coerce x 'double-float)
     (coerce y 'double-float)
     text
     xmin-ptr xmax-ptr ymin-ptr ymax-ptr)
    (values (cffi:mem-ref xmin-ptr :double)
            (cffi:mem-ref xmax-ptr :double)
            (cffi:mem-ref ymin-ptr :double)
            (cffi:mem-ref ymax-ptr :double))))

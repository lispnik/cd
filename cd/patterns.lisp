(in-package #:cd)

;;; Pattern and Stipple Fill Operations

(export '(pattern
          stipple
          pattern-size
          stipple-size))

(defun (setf pattern) (pattern canvas)
  "Set a pattern for fill operations. Pattern should be a 2D array of color values."
  (with-validation 'set-pattern
    (validate-canvas canvas 'set-pattern)
    (validate-array pattern 'pattern 'set-pattern :element-type 'integer :min-size 1))

  (with-cd-error-checking ('set-pattern :canvas canvas)
    (let ((dims (array-dimensions pattern)))
      (unless (= (length dims) 2)
        (error 'cd-parameter-error
               :parameter 'pattern
               :value pattern
               :operation 'set-pattern
               :expected "2D array"))

      (let ((width (first dims))
            (height (second dims)))
        (cffi:with-foreign-object (pattern-data :long (* width height))
          (loop for y from 0 below height do
            (loop for x from 0 below width do
              (setf (cffi:mem-aref pattern-data :long (+ (* y width) x))
                    (aref pattern y x))))
          (cd-cffi::%cd-canvas-pattern canvas width height pattern-data))))
    pattern))

(defun pattern (canvas)
  "Get the current pattern from canvas."
  (with-validation 'get-pattern
    (validate-canvas canvas 'get-pattern))

  (with-cd-error-checking ('get-pattern :canvas canvas)
    (cffi:with-foreign-objects ((width-ptr :int)
                               (height-ptr :int))
      (let ((pattern-ptr (cd-cffi::%cd-canvas-get-pattern canvas width-ptr height-ptr)))
        (unless (cffi:null-pointer-p pattern-ptr)
          (let ((width (cffi:mem-ref width-ptr :int))
                (height (cffi:mem-ref height-ptr :int)))
            (make-array (list height width)
                       :initial-contents
                       (loop for y from 0 below height
                             collect (loop for x from 0 below width
                                         collect (cffi:mem-aref pattern-ptr :long
                                                               (+ (* y width) x)))))))))))

(defun pattern-size (canvas)
  "Get the dimensions of the current pattern."
  (with-validation 'pattern-size
    (validate-canvas canvas 'pattern-size))

  (with-cd-error-checking ('pattern-size :canvas canvas)
    (cffi:with-foreign-objects ((width-ptr :int)
                               (height-ptr :int))
      (cd-cffi::%cd-canvas-get-pattern canvas width-ptr height-ptr)
      (values (cffi:mem-ref width-ptr :int)
              (cffi:mem-ref height-ptr :int)))))

(defun (setf stipple) (stipple canvas)
  "Set a stipple pattern for fill operations. Stipple should be a 2D array of boolean values."
  (with-validation 'set-stipple
    (validate-canvas canvas 'set-stipple)
    (validate-array stipple 'stipple 'set-stipple :min-size 1))

  (with-cd-error-checking ('set-stipple :canvas canvas)
    (let ((dims (array-dimensions stipple)))
      (unless (= (length dims) 2)
        (error 'cd-parameter-error
               :parameter 'stipple
               :value stipple
               :operation 'set-stipple
               :expected "2D array"))

      (let ((width (first dims))
            (height (second dims)))
        (cffi:with-foreign-object (stipple-data :unsigned-char (* width height))
          (loop for y from 0 below height do
            (loop for x from 0 below width do
              (setf (cffi:mem-aref stipple-data :unsigned-char (+ (* y width) x))
                    (if (aref stipple y x) 1 0))))
          (cd-cffi::%cd-canvas-stipple canvas width height stipple-data))))
    stipple))

(defun stipple (canvas)
  "Get the current stipple pattern from canvas."
  (with-validation 'get-stipple
    (validate-canvas canvas 'get-stipple))

  (with-cd-error-checking ('get-stipple :canvas canvas)
    (cffi:with-foreign-objects ((width-ptr :int)
                               (height-ptr :int))
      (let ((stipple-ptr (cd-cffi::%cd-canvas-get-stipple canvas width-ptr height-ptr)))
        (unless (cffi:null-pointer-p stipple-ptr)
          (let ((width (cffi:mem-ref width-ptr :int))
                (height (cffi:mem-ref height-ptr :int)))
            (make-array (list height width)
                       :element-type 'boolean
                       :initial-contents
                       (loop for y from 0 below height
                             collect (loop for x from 0 below width
                                         collect (not (zerop (cffi:mem-aref stipple-ptr
                                                                           :unsigned-char
                                                                           (+ (* y width) x)))))))))))))

(defun stipple-size (canvas)
  "Get the dimensions of the current stipple pattern."
  (with-validation 'stipple-size
    (validate-canvas canvas 'stipple-size))

  (with-cd-error-checking ('stipple-size :canvas canvas)
    (cffi:with-foreign-objects ((width-ptr :int)
                               (height-ptr :int))
      (cd-cffi::%cd-canvas-get-stipple canvas width-ptr height-ptr)
      (values (cffi:mem-ref width-ptr :int)
              (cffi:mem-ref height-ptr :int)))))

;; World coordinate versions
(defun (setf wd:pattern) (pattern canvas width-mm height-mm)
  "Set a pattern with world coordinate dimensions."
  (with-validation 'set-wd-pattern
    (validate-canvas canvas 'set-wd-pattern)
    (validate-array pattern 'pattern 'set-wd-pattern :element-type 'integer :min-size 1)
    (validate-coordinate width-mm 'width-mm 'set-wd-pattern :min 0)
    (validate-coordinate height-mm 'height-mm 'set-wd-pattern :min 0))

  (with-cd-error-checking ('set-wd-pattern :canvas canvas)
    (let ((dims (array-dimensions pattern)))
      (let ((width (first dims))
            (height (second dims)))
        (cffi:with-foreign-object (pattern-data :long (* width height))
          (loop for y from 0 below height do
            (loop for x from 0 below width do
              (setf (cffi:mem-aref pattern-data :long (+ (* y width) x))
                    (aref pattern y x))))
          (cd-cffi::%wd-canvas-pattern canvas width height pattern-data
                                      (coerce width-mm 'double-float)
                                      (coerce height-mm 'double-float))))))
  pattern)

(defun (setf wd:stipple) (stipple canvas width-mm height-mm)
  "Set a stipple pattern with world coordinate dimensions."
  (with-validation 'set-wd-stipple
    (validate-canvas canvas 'set-wd-stipple)
    (validate-array stipple 'stipple 'set-wd-stipple :min-size 1)
    (validate-coordinate width-mm 'width-mm 'set-wd-stipple :min 0)
    (validate-coordinate height-mm 'height-mm 'set-wd-stipple :min 0))

  (with-cd-error-checking ('set-wd-stipple :canvas canvas)
    (let ((dims (array-dimensions stipple)))
      (let ((width (first dims))
            (height (second dims)))
        (cffi:with-foreign-object (stipple-data :unsigned-char (* width height))
          (loop for y from 0 below height do
            (loop for x from 0 below width do
              (setf (cffi:mem-aref stipple-data :unsigned-char (+ (* y width) x))
                    (if (aref stipple y x) 1 0))))
          (cd-cffi::%wd-canvas-stipple canvas width height stipple-data
                                      (coerce width-mm 'double-float)
                                      (coerce height-mm 'double-float))))))
  stipple)

;; Export to WD package as well
(export '(pattern stipple) (find-package "WD"))
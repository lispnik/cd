(in-package #:cd)

;;; Parameter Validation System

(defvar *cd-validation-enabled* t
  "When non-nil, enables parameter validation for CD functions.")

(defmacro when-validating (&body body)
  "Execute body only when validation is enabled."
  `(when *cd-validation-enabled*
     ,@body))

(defun validate-canvas (canvas operation)
  "Validate that canvas is a valid CD canvas pointer."
  (when-validating
    (unless (and canvas (not (cffi:null-pointer-p canvas)))
      (error 'cd-parameter-error
             :parameter 'canvas
             :value canvas
             :operation operation
             :expected "non-null canvas pointer"))))

(defun validate-context (context operation)
  "Validate that context is a valid CD context pointer."
  (when-validating
    (unless (and context (not (cffi:null-pointer-p context)))
      (error 'cd-parameter-error
             :parameter 'context
             :value context
             :operation operation
             :expected "non-null context pointer"))))

(defun validate-coordinate (value name operation &key (min nil) (max nil))
  "Validate a coordinate value."
  (when-validating
    (unless (numberp value)
      (error 'cd-parameter-error
             :parameter name
             :value value
             :operation operation
             :expected "number"))
    (when (and min (< value min))
      (error 'cd-parameter-error
             :parameter name
             :value value
             :operation operation
             :expected (format nil "number >= ~A" min)))
    (when (and max (> value max))
      (error 'cd-parameter-error
             :parameter name
             :value value
             :operation operation
             :expected (format nil "number <= ~A" max)))))

(defun validate-color (color operation)
  "Validate a color value."
  (when-validating
    (unless (and (integerp color) (>= color 0))
      (error 'cd-parameter-error
             :parameter 'color
             :value color
             :operation operation
             :expected "non-negative integer"))))

(defun validate-string (string name operation &key (allow-null nil))
  "Validate a string parameter."
  (when-validating
    (unless (or (stringp string) (and allow-null (null string)))
      (error 'cd-parameter-error
             :parameter name
             :value string
             :operation operation
             :expected (if allow-null "string or nil" "string")))))

(defun validate-dimensions (width height operation)
  "Validate width and height dimensions."
  (when-validating
    (validate-coordinate width 'width operation :min 1)
    (validate-coordinate height 'height operation :min 1)))

(defun validate-enum (value valid-values name operation)
  "Validate that value is one of the valid enum values."
  (when-validating
    (unless (member value valid-values)
      (error 'cd-parameter-error
             :parameter name
             :value value
             :operation operation
             :expected (format nil "one of ~{~A~^, ~}" valid-values)))))

(defun validate-array (array name operation &key (element-type t) (min-size 0))
  "Validate array parameters."
  (when-validating
    (unless (and (arrayp array)
                 (>= (array-total-size array) min-size)
                 (subtypep (array-element-type array) element-type))
      (error 'cd-parameter-error
             :parameter name
             :value array
             :operation operation
             :expected (format nil "array with element-type ~A and size >= ~A"
                              element-type min-size)))))

(defmacro with-validation (operation &body body)
  "Execute body with enhanced error context for validation errors."
  (let ((op-var (gensym "operation")))
    `(let ((,op-var ,operation))
       (handler-bind ((cd-parameter-error
                       (lambda (condition)
                         (unless (error-operation condition)
                           (setf (slot-value condition 'operation) ,op-var)))))
         ,@body))))

(export '(*cd-validation-enabled* when-validating with-validation
          validate-canvas validate-context validate-coordinate
          validate-color validate-string validate-dimensions
          validate-enum validate-array))
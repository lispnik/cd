(in-package #:cd-tests)

;;; Test Utilities

(defparameter *test-output-dir* "/tmp/cd-tests/")
(defparameter *test-canvas-width* 200)
(defparameter *test-canvas-height* 150)

(defun ensure-test-directory ()
  "Ensure test output directory exists."
  (ensure-directories-exist *test-output-dir*))

(defun test-file-path (filename)
  "Generate full path for test output file."
  (merge-pathnames filename *test-output-dir*))

(defmacro with-test-canvas ((canvas-var context &optional spec) &body body)
  "Create test canvas with error handling."
  `(handler-case
       (with-enhanced-canvas (,canvas-var ,context ,spec)
         ,@body
         t)  ; Return success
     (cd-error (e)
       (format t "~&Test canvas creation failed: ~A~%" e)
       nil)))

(defmacro with-debug-canvas ((canvas-var) &body body)
  "Create debug canvas for testing."
  `(with-test-canvas (,canvas-var (context-debug))
     ,@body))

(defmacro with-svg-canvas ((canvas-var filename) &body body)
  "Create SVG canvas for visual test output."
  `(with-test-canvas (,canvas-var (context-svg) (test-file-path ,filename))
     ,@body))

(defun test-contexts ()
  "Return list of contexts available for testing."
  (remove-if #'null
             (list (ignore-errors (context-debug))
                   (ignore-errors (context-image-rgb))
                   (ignore-errors (context-svg))
                   (ignore-errors (context-picture)))))

(defun test-canvas-size (canvas)
  "Get canvas size for testing."
  (multiple-value-bind (width height width-mm height-mm)
      (size canvas)
    (declare (ignore width-mm height-mm))
    (values width height)))

(defun create-test-pattern (width height)
  "Create a test pattern for pattern/stipple testing."
  (make-array (list height width)
              :initial-function (lambda (y x)
                                  (if (evenp (+ x y))
                                      +red+ +blue+))))

(defun create-test-stipple (width height)
  "Create a test stipple pattern."
  (make-array (list height width)
              :element-type 'boolean
              :initial-function (lambda (y x)
                                  (evenp (+ x y)))))

(defun create-test-rgb-data (width height)
  "Create test RGB data arrays."
  (let ((size (* width height)))
    (values
     (make-array size :element-type '(unsigned-byte 8)
                :initial-function (lambda (i) (mod (* i 3) 256)))
     (make-array size :element-type '(unsigned-byte 8)
                :initial-function (lambda (i) (mod (* i 5) 256)))
     (make-array size :element-type '(unsigned-byte 8)
                :initial-function (lambda (i) (mod (* i 7) 256))))))

(defun create-test-rgba-data (width height)
  "Create test RGBA data arrays."
  (multiple-value-bind (r g b) (create-test-rgb-data width height)
    (values r g b
            (make-array (* width height) :element-type '(unsigned-byte 8)
                       :initial-element 255))))

(defun colors-equal-p (color1 color2 &optional (tolerance 0))
  "Compare two colors with optional tolerance."
  (multiple-value-bind (r1 g1 b1) (decode-color color1)
    (multiple-value-bind (r2 g2 b2) (decode-color color2)
      (and (<= (abs (- r1 r2)) tolerance)
           (<= (abs (- g1 g2)) tolerance)
           (<= (abs (- b1 b2)) tolerance)))))

(defmacro test-with-multiple-contexts (name contexts &body body)
  "Run test with multiple contexts."
  `(test ,name
     (dolist (context ,contexts)
       (when context
         (format t "~&  Testing with context type: ~A~%" (context-type context))
         (locally ,@body)))))

(defun benchmark-operation (name operation &optional (iterations 1000))
  "Benchmark an operation and return timing information."
  (let ((start-time (get-internal-real-time)))
    (dotimes (i iterations)
      (funcall operation))
    (let ((end-time (get-internal-real-time)))
      (let ((elapsed (/ (- end-time start-time) internal-time-units-per-second)))
        (format t "~&Benchmark ~A: ~D iterations in ~F seconds (~F ops/sec)~%"
                name iterations elapsed (/ iterations elapsed))
        elapsed))))

(defun test-error-condition (expected-condition operation &rest args)
  "Test that an operation signals expected condition."
  (handler-case
      (progn
        (apply operation args)
        nil)  ; If we get here, no error was signaled
    (condition (c)
      (typep c expected-condition))))

(defun validate-test-results (results expected-count)
  "Validate that test results meet expectations."
  (let ((passed (count t results))
        (failed (count nil results)))
    (format t "~&Test Results: ~D passed, ~D failed out of ~D total~%"
            passed failed (length results))
    (is (= passed expected-count)
        "Expected ~D tests to pass, but ~D passed" expected-count passed)
    (is (= failed 0)
        "Expected no failures, but ~D tests failed" failed)))
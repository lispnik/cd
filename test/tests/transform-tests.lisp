(in-package #:cd-tests)

;;; Transform Matrix Tests

(in-suite transformation-tests) ; Reuse existing suite

(test transform-matrix-creation
  "Test transformation matrix creation"
  ;; Test identity matrix
  (let ((identity (make-identity-matrix)))
    (is (arrayp identity))
    (is (= (length identity) 6))
    (is (= (aref identity 0) 1.0d0))
    (is (= (aref identity 3) 1.0d0))
    (is (= (aref identity 1) 0.0d0)))

  ;; Test translation matrix
  (let ((translation (make-translation-matrix 10 20)))
    (is (= (aref translation 4) 10.0d0))
    (is (= (aref translation 5) 20.0d0)))

  ;; Test rotation matrix
  (let ((rotation (make-rotation-matrix 90)))
    (is (arrayp rotation))
    ;; 90 degree rotation should have cos(90)=0, sin(90)=1
    (is (< (abs (aref rotation 0)) 0.001)) ; cos(90°) ≈ 0
    (is (< (abs (- (aref rotation 1) 1.0d0)) 0.001))) ; sin(90°) ≈ 1

  ;; Test scaling matrix
  (let ((scaling (make-scaling-matrix 2 3)))
    (is (= (aref scaling 0) 2.0d0))
    (is (= (aref scaling 3) 3.0d0))))

(test transform-matrix-multiplication
  "Test matrix multiplication"
  (let ((m1 (make-translation-matrix 10 0))
        (m2 (make-translation-matrix 5 0)))
    (let ((result (multiply-matrices m1 m2)))
      ;; Translation of 10 + 5 = 15
      (is (= (aref result 4) 15.0d0)))))

(test transform-canvas-operations
  "Test transformation operations on canvas"
  (with-debug-canvas (canvas)
    ;; Test setting transform
    (let ((matrix (make-translation-matrix 100 50)))
      (finishes (setf (transform canvas) matrix))
      (let ((retrieved (transform canvas)))
        (is (arrayp retrieved))))

    ;; Test transform multiply
    (finishes (transform-multiply canvas (make-scaling-matrix 2 2)))

    ;; Test individual transforms
    (finishes (transform-translate canvas 10 20))
    (finishes (transform-rotate canvas 45))
    (finishes (transform-scale canvas 1.5 1.5))

    ;; Test point transformation
    (multiple-value-bind (tx ty) (transform-point canvas 100 100)
      (is (integerp tx))
      (is (integerp ty)))

    ;; Test reset
    (finishes (reset-transform canvas))))

(test transform-convenience-macros
  "Test transformation convenience macros"
  (with-debug-canvas (canvas)
    ;; Test with-transform
    (with-transform (canvas (make-translation-matrix 50 50))
      (finishes (line canvas 0 0 10 10)))

    ;; Test with-translation
    (with-translation (canvas 30 30)
      (finishes (rect canvas 10 10 20 20)))

    ;; Test with-rotation
    (with-rotation (canvas 45)
      (finishes (text canvas 50 50 "Rotated")))

    ;; Test with-scaling
    (with-scaling (canvas 2 2)
      (finishes (mark canvas 25 25)))))

(run! 'transformation-tests)
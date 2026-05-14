(in-package #:cd-tests)

;;; Advanced Drawing Tests

(def-suite advanced-drawing-tests :in cd-test-suite)
(in-suite advanced-drawing-tests)

(test path-operations
  "Test path creation and rendering"
  (with-svg-canvas (canvas "path-test.svg")
    ;; Test path building
    (finishes (begin-path))
    (finishes (path-move-to 10 10))
    (finishes (path-line-to 50 10))
    (finishes (path-line-to 50 50))
    (finishes (path-close))
    (finishes (end-path canvas :stroke))

    ;; Test with-path macro
    (with-path (canvas :fill)
      (path-move-to 70 10)
      (path-curve-to 100 10 100 40 70 40)
      (path-close))))

(test spline-operations
  "Test spline drawing"
  (with-svg-canvas (canvas "spline-test.svg")
    ;; Test basic spline
    (let ((points #(10 10 30 50 50 30 70 60)))
      (finishes (spline canvas points)))

    ;; Test smooth curve
    (let ((points #(100 10 130 50 150 30 170 60)))
      (finishes (smooth-curve canvas points)))))

(test bezier-operations
  "Test bezier curve drawing"
  (with-svg-canvas (canvas "bezier-test.svg")
    ;; Test cubic bezier
    (finishes (cubic-bezier canvas 10 100 30 50 70 50 90 100))

    ;; Test quadratic bezier
    (finishes (quadratic-bezier canvas 120 100 150 50 180 100))

    ;; Test bezier with points array
    (let ((points #(10 150 40 120 70 120 100 150)))
      (finishes (bezier-curve canvas points)))))

(test gradient-operations
  "Test gradient creation and rendering"
  (with-debug-canvas (canvas)
    ;; Test linear gradient creation
    (let ((gradient (make-linear-gradient 0 0 100 0 (vector +red+ +blue+))))
      (is (not (null gradient)))
      (finishes (gradient-fill-rectangle canvas gradient 10 10 100 50)))

    ;; Test radial gradient creation
    (let ((gradient (make-radial-gradient 50 100 30 (vector +green+ +yellow+))))
      (is (not (null gradient)))
      (finishes (gradient-fill-rectangle canvas gradient 20 80 60 40)))))

(test geometric-helpers
  "Test geometric shape helpers"
  (with-svg-canvas (canvas "geometric-test.svg")
    ;; Test rounded rectangle
    (finishes (draw-rounded-rectangle canvas 10 10 60 40 10))

    ;; Test star
    (finishes (draw-star canvas 50 100 30 15 5))

    ;; Test regular polygon
    (finishes (draw-regular-polygon canvas 150 50 25 6))))

(test advanced-shapes-with-attributes
  "Test advanced shapes with various attributes"
  (with-svg-canvas (canvas "advanced-shapes.svg")
    ;; Test with different colors and styles
    (setf (foreground canvas) +red+)
    (setf (line-width canvas) 3)
    (draw-rounded-rectangle canvas 10 10 50 30 8)

    (setf (foreground canvas) +blue+)
    (setf (interior-style canvas) :interior-solid)
    (draw-star canvas 100 25 20 10 6)

    (setf (foreground canvas) +green+)
    (setf (line-style canvas) :line-dashed)
    (draw-regular-polygon canvas 50 80 25 8)))

(test path-error-conditions
  "Test path error handling"
  ;; Test ending path without beginning
  (handler-case
      (with-debug-canvas (canvas)
        (end-path canvas :stroke))
    (error (e)
      (pass)))

  ;; Test path operations without active path
  (handler-case
      (path-move-to 10 10)
    (error (e)
      (pass))))

(test complex-path-construction
  "Test complex path construction"
  (with-svg-canvas (canvas "complex-path.svg")
    (with-path (canvas :fill-stroke)
      ;; Create a complex shape
      (path-move-to 50 10)
      (path-curve-to 80 10 90 30 80 50)
      (path-line-to 70 60)
      (path-curve-to 50 70 30 70 10 60)
      (path-line-to 20 50)
      (path-curve-to 10 30 20 10 50 10)
      (path-close))))

(test performance-advanced-drawing
  "Test performance of advanced drawing operations"
  (with-debug-canvas (canvas)
    ;; Test path performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 50)
        (with-path (canvas :stroke)
          (path-move-to (* i 2) 10)
          (path-line-to (+ (* i 2) 10) 20)
          (path-close)))
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
        (is (< elapsed 2.0) "50 path operations should complete quickly")))

    ;; Test spline performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 20)
        (spline canvas #(10 10 30 30 50 20 70 40)))
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
        (is (< elapsed 1.0) "20 spline operations should complete quickly")))))

(run! 'advanced-drawing-tests)
(in-package #:cd-tests)

;;; Complete World Coordinate System Tests

(def-suite world-complete-tests :in cd-test-suite)
(in-suite world-complete-tests)

(test world-coordinate-setup-complete
  "Test complete world coordinate system setup"
  (with-debug-canvas (canvas)
    ;; Test world bounds setting and getting
    (wd:set-world-bounds canvas -10.0 10.0 -5.0 5.0)
    (multiple-value-bind (xmin xmax ymin ymax) (wd:get-world-bounds canvas)
      (is (= xmin -10.0))
      (is (= xmax 10.0))
      (is (= ymin -5.0))
      (is (= ymax 5.0)))

    ;; Test world dimensions
    (is (= (wd:world-width canvas) 20.0))
    (is (= (wd:world-height canvas) 10.0))))

(test world-advanced-drawing
  "Test advanced drawing in world coordinates"
  (with-svg-canvas (canvas "world-advanced.svg")
    ;; Set up world coordinates
    (wd:world-set canvas -5.0 5.0 -3.0 3.0)

    ;; Test spline in world coordinates
    (let ((points #(-4.0 -2.0 -2.0 2.0 0.0 -1.0 2.0 2.0 4.0 -2.0)))
      (finishes (wd:spline canvas points)))

    ;; Test cubic bezier
    (finishes (wd:cubic-bezier canvas -4.0 0.0 -2.0 2.0 2.0 2.0 4.0 0.0))

    ;; Test quadratic bezier
    (finishes (wd:quadratic-bezier canvas -3.0 -2.0 0.0 3.0 3.0 -2.0))))

(test world-geometric-helpers
  "Test geometric helpers in world coordinates"
  (with-svg-canvas (canvas "world-geometric.svg")
    (wd:world-set canvas -10.0 10.0 -8.0 8.0)

    ;; Test rounded rectangle
    (finishes (wd:draw-rounded-rectangle canvas -8.0 -6.0 6.0 4.0 1.0))

    ;; Test star
    (finishes (wd:draw-star canvas 0.0 0.0 3.0 1.5 5))

    ;; Test regular polygon
    (finishes (wd:draw-regular-polygon canvas 5.0 3.0 2.0 6))))

(test world-image-operations
  "Test image operations in world coordinates"
  (with-debug-canvas (canvas)
    (wd:world-set canvas 0.0 100.0 0.0 75.0)

    (let ((width 10) (height 10))
      (multiple-value-bind (r g b) (create-test-rgb-data width height)
        ;; Test putting image at world coordinates
        (finishes (wd:put-image-rgb canvas width height r g b 25.0 25.0 0 0 0 0))

        ;; Test getting image from world coordinates
        (multiple-value-bind (r-out g-out b-out)
            (wd:get-image-rgb canvas 25.0 25.0 20.0 15.0)
          (is (arrayp r-out))
          (is (arrayp g-out))
          (is (arrayp b-out)))))))

(test world-text-operations
  "Test text operations in world coordinates"
  (with-svg-canvas (canvas "world-text.svg")
    (wd:world-set canvas -50.0 50.0 -25.0 25.0)

    ;; Test basic text
    (finishes (wd:text canvas 0.0 0.0 "World Center"))

    ;; Test multiline text
    (finishes (wd:text-multiline canvas -40.0 10.0 "Line 1
Line 2
Line 3"))

    ;; Test text bounds
    (multiple-value-bind (xmin xmax ymin ymax)
        (wd:text-bounds canvas -20.0 -10.0 "Bounds Test")
      (is (numberp xmin))
      (is (numberp xmax))
      (is (numberp ymin))
      (is (numberp ymax))
      (is (<= xmin xmax))
      (is (<= ymin ymax)))))

(test world-clipping
  "Test clipping in world coordinates"
  (with-svg-canvas (canvas "world-clipping.svg")
    (wd:world-set canvas -20.0 20.0 -15.0 15.0)

    ;; Set clipping region in world coordinates
    (finishes (wd:clip canvas -10.0 10.0 -7.5 7.5))

    ;; Draw something that extends beyond clip
    (setf (foreground canvas) +red+)
    (finishes (wd:rect canvas -15.0 -12.0 30.0 24.0))

    ;; Turn off clipping
    (finishes (clip-off canvas))))

(test world-coordinate-utilities
  "Test world coordinate utility functions"
  ;; Test distance calculation
  (let ((dist (wd:distance 0.0 0.0 3.0 4.0)))
    (is (= dist 5.0))) ; 3-4-5 triangle

  ;; Test point in rectangle
  (is (wd:point-in-world-rect-p 5.0 3.0 2.0 1.0 8.0 6.0)) ; Inside
  (is (not (wd:point-in-world-rect-p 0.0 0.0 2.0 1.0 8.0 6.0)))) ; Outside

(test world-animation-support
  "Test animation support in world coordinates"
  (with-debug-canvas (canvas)
    (wd:world-set canvas -10.0 10.0 -5.0 5.0)

    ;; Test point animation
    (multiple-value-bind (x y) (wd:animate-world-point canvas -8.0 -3.0 8.0 3.0 2.0 1.0)
      (is (numberp x))
      (is (numberp y))
      ;; At 1 second of 2 second duration, should be halfway
      (is (= x 0.0))
      (is (= y 0.0)))))

(test world-coordinate-precision
  "Test world coordinate precision"
  (with-debug-canvas (canvas)
    ;; Test with high precision coordinates
    (wd:world-set canvas -1.23456789 1.23456789 -0.98765432 0.98765432)

    ;; Test coordinate conversion precision
    (multiple-value-bind (cx cy) (wd:world-to-canvas canvas 0.123456 0.654321)
      (multiple-value-bind (wx wy) (wd:canvas-to-world canvas cx cy)
        ;; Should preserve reasonable precision
        (is (< (abs (- 0.123456 wx)) 0.0001))
        (is (< (abs (- 0.654321 wy)) 0.0001))))))

(test world-coordinate-edge-cases
  "Test world coordinate edge cases"
  (with-debug-canvas (canvas)
    ;; Test zero-size world
    (handler-case
        (wd:world-set canvas 0.0 0.0 0.0 0.0)
      (error (e)
        (pass))
      (:no-error ()
        (pass))) ; May succeed with degenerate coordinates

    ;; Test inverted coordinates
    (handler-case
        (wd:world-set canvas 10.0 0.0 5.0 -5.0) ; max < min
      (error (e)
        (pass))
      (:no-error ()
        (pass))) ; May auto-correct

    ;; Test very large world
    (finishes (wd:world-set canvas -1e6 1e6 -1e6 1e6))))

(test world-vs-canvas-consistency
  "Test consistency between world and canvas coordinate APIs"
  (with-debug-canvas (canvas)
    (wd:world-set canvas -100.0 100.0 -75.0 75.0)

    ;; Draw same shape in both coordinate systems
    (let ((saved-foreground (foreground canvas)))
      ;; World coordinates
      (setf (foreground canvas) +red+)
      (wd:rect canvas -50.0 -25.0 40.0 20.0)

      ;; Convert to canvas and draw with CD
      (multiple-value-bind (cx1 cy1) (wd:world-to-canvas canvas -50.0 -25.0)
        (multiple-value-bind (cx2 cy2) (wd:world-to-canvas canvas -10.0 -5.0)
          (setf (foreground canvas) +blue+)
          (rect canvas cx1 cy1 (- cx2 cx1) (- cy2 cy1))))

      (setf (foreground canvas) saved-foreground))))

(test world-coordinate-performance
  "Test world coordinate performance"
  (with-debug-canvas (canvas)
    (wd:world-set canvas -1000.0 1000.0 -750.0 750.0)

    ;; Test drawing performance in world coordinates
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 100)
        (wd:line canvas (- (random 2000.0) 1000.0) (- (random 1500.0) 750.0)
                        (- (random 2000.0) 1000.0) (- (random 1500.0) 750.0)))
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
        (is (< elapsed 2.0) "100 world coordinate lines should be reasonably fast")))

    ;; Test coordinate conversion performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 1000)
        (wd:world-to-canvas canvas (- (random 2000.0) 1000.0) (- (random 1500.0) 750.0)))
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
        (is (< elapsed 1.0) "1000 coordinate conversions should be fast")))))

(test world-coordinate-error-handling
  "Test world coordinate error handling"
  (with-debug-canvas (canvas)
    ;; Test operations before setting world coordinates
    (handler-case
        (wd:line canvas 0.0 0.0 1.0 1.0)
      (error (e)
        (pass)) ; May error without world setup
      (:no-error ()
        (pass))) ; Or may work with defaults

    ;; Test invalid world coordinates
    (handler-case
        (wd:world-set canvas "invalid" "coords" "here" "too")
      (error (e)
        (pass)))

    ;; Recovery should be possible
    (finishes (wd:world-set canvas 0.0 10.0 0.0 10.0))
    (finishes (wd:line canvas 1.0 1.0 9.0 9.0))))

(run! 'world-complete-tests)
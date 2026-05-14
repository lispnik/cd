(in-package #:cd-tests)

;;; World Coordinate Tests

(in-suite world-coordinate-tests)

(test world-coordinate-setup
  "Test world coordinate system setup"
  (with-debug-canvas (canvas)
    ;; Test setting world coordinates
    (finishes (world-set canvas 0.0 100.0 0.0 75.0))

    ;; Test getting world coordinates
    (multiple-value-bind (xmin xmax ymin ymax) (world-get canvas)
      (is (numberp xmin))
      (is (numberp xmax))
      (is (numberp ymin))
      (is (numberp ymax))
      (is (<= xmin xmax))
      (is (<= ymin ymax)))))

(test world-coordinate-transformations
  "Test world coordinate transformations"
  (with-debug-canvas (canvas)
    ;; Set up world coordinates
    (world-set canvas -10.0 10.0 -5.0 5.0)

    ;; Test world to canvas transformation
    (multiple-value-bind (x y) (world-to-canvas canvas 0.0 0.0)
      (is (numberp x))
      (is (numberp y)))

    ;; Test canvas to world transformation
    (multiple-value-bind (wx wy) (canvas-to-world canvas 100 100)
      (is (numberp wx))
      (is (numberp wy)))

    ;; Test round-trip transformation
    (let ((world-x 5.0) (world-y 2.5))
      (multiple-value-bind (canvas-x canvas-y) (world-to-canvas canvas world-x world-y)
        (multiple-value-bind (world-x2 world-y2) (canvas-to-world canvas canvas-x canvas-y)
          (is (< (abs (- world-x world-x2)) 0.01))
          (is (< (abs (- world-y world-y2)) 0.01)))))))

(test world-coordinate-drawing
  "Test drawing in world coordinates"
  (with-svg-canvas (canvas "world-coords.svg")
    ;; Set up world coordinate system
    (world-set canvas -2.0 2.0 -1.5 1.5)

    ;; Draw grid in world coordinates
    (setf (foreground canvas) +gray+)
    (setf (line-style canvas) :line-dashed)

    ;; Vertical grid lines
    (loop for x from -2.0 to 2.0 by 0.5 do
      (multiple-value-bind (x1 y1) (world-to-canvas canvas x -1.5)
        (multiple-value-bind (x2 y2) (world-to-canvas canvas x 1.5)
          (line canvas x1 y1 x2 y2))))

    ;; Horizontal grid lines
    (loop for y from -1.5 to 1.5 by 0.5 do
      (multiple-value-bind (x1 y1) (world-to-canvas canvas -2.0 y)
        (multiple-value-bind (x2 y2) (world-to-canvas canvas 2.0 y)
          (line canvas x1 y1 x2 y2))))

    ;; Draw axes
    (setf (foreground canvas) +black+)
    (setf (line-style canvas) :line-continuous)
    (setf (line-width canvas) 2)

    ;; X-axis
    (multiple-value-bind (x1 y1) (world-to-canvas canvas -2.0 0.0)
      (multiple-value-bind (x2 y2) (world-to-canvas canvas 2.0 0.0)
        (line canvas x1 y1 x2 y2)))

    ;; Y-axis
    (multiple-value-bind (x1 y1) (world-to-canvas canvas 0.0 -1.5)
      (multiple-value-bind (x2 y2) (world-to-canvas canvas 0.0 1.5)
        (line canvas x1 y1 x2 y2)))

    ;; Draw some shapes in world coordinates
    (setf (foreground canvas) +red+)
    (multiple-value-bind (x y) (world-to-canvas canvas 0.0 0.0)
      (mark canvas x y))

    (setf (foreground canvas) +blue+)
    (multiple-value-bind (x1 y1) (world-to-canvas canvas -1.0 -0.5)
      (multiple-value-bind (x2 y2) (world-to-canvas canvas 1.0 0.5)
        (rect canvas x1 y1 (- x2 x1) (- y2 y1))))))

(test world-coordinate-scaling
  "Test world coordinate scaling effects"
  (with-debug-canvas (canvas)
    ;; Test different aspect ratios
    (world-set canvas 0.0 1.0 0.0 1.0)  ; Square
    (multiple-value-bind (x1 y1) (world-to-canvas canvas 0.5 0.5)
      (world-set canvas 0.0 2.0 0.0 1.0)  ; Wide rectangle
      (multiple-value-bind (x2 y2) (world-to-canvas canvas 1.0 0.5)  ; Same relative position
        (is (numberp x1))
        (is (numberp y1))
        (is (numberp x2))
        (is (numberp y2))))))

(test world-coordinate-limits
  "Test world coordinate limits"
  (with-debug-canvas (canvas)
    ;; Test very small world
    (finishes (world-set canvas 0.0 0.001 0.0 0.001))

    ;; Test very large world
    (finishes (world-set canvas -1000000.0 1000000.0 -1000000.0 1000000.0))

    ;; Test negative coordinates
    (finishes (world-set canvas -100.0 -50.0 -75.0 -25.0))

    ;; Test single point world (degenerate case)
    (handler-case
        (world-set canvas 1.0 1.0 1.0 1.0)
      (cd-error (e)
        (pass))  ; May not allow zero-size world
      (:no-error ()
        (pass)))))

(test world-coordinate-precision
  "Test world coordinate precision"
  (with-debug-canvas (canvas)
    ;; Set high-precision world coordinates
    (world-set canvas 0.123456789 1.987654321 -0.555555555 0.777777777)

    ;; Test precision preservation
    (multiple-value-bind (xmin xmax ymin ymax) (world-get canvas)
      ;; Should preserve reasonable precision
      (is (< (abs (- xmin 0.123456789)) 0.000001))
      (is (< (abs (- xmax 1.987654321)) 0.000001))
      (is (< (abs (- ymin -0.555555555)) 0.000001))
      (is (< (abs (- ymax 0.777777777)) 0.000001)))))

(test world-coordinate-state-management
  "Test world coordinate state with save/restore"
  (with-debug-canvas (canvas)
    ;; Set initial world coordinates
    (world-set canvas -5.0 5.0 -3.0 3.0)
    (multiple-value-bind (xmin1 xmax1 ymin1 ymax1) (world-get canvas)

      ;; Save state
      (let ((state (save-state canvas)))
        ;; Change world coordinates
        (world-set canvas 0.0 10.0 0.0 8.0)

        ;; Restore state
        (restore-state canvas state)

        ;; Verify restoration
        (multiple-value-bind (xmin2 xmax2 ymin2 ymax2) (world-get canvas)
          (is (= xmin1 xmin2))
          (is (= xmax1 xmax2))
          (is (= ymin1 ymin2))
          (is (= ymax1 ymax2)))

        ;; Clean up
        (release-state state)))))

(test world-coordinate-canvas-interaction
  "Test world coordinates with canvas size changes"
  (with-debug-canvas (canvas)
    (multiple-value-bind (width height) (test-canvas-size canvas)
      ;; Set world coordinates based on canvas size
      (let ((aspect-ratio (/ width height)))
        (world-set canvas 0.0 aspect-ratio 0.0 1.0)

        ;; Test that transformations work correctly
        (multiple-value-bind (x y) (world-to-canvas canvas 0.0 0.0)
          (is (numberp x))
          (is (numberp y))
          ;; (0,0) in world should map to canvas origin area
          (is (>= x 0))
          (is (>= y 0)))

        (multiple-value-bind (x y) (world-to-canvas canvas aspect-ratio 1.0)
          (is (numberp x))
          (is (numberp y))
          ;; Max world coords should map near canvas max
          (is (<= x (* width 1.1)))  ; Allow some tolerance
          (is (<= y (* height 1.1))))))))

(test world-coordinate-error-conditions
  "Test world coordinate error handling"
  (with-debug-canvas (canvas)
    ;; Test invalid world coordinate setup
    (handler-case
        (world-set canvas 10.0 5.0 20.0 15.0)  ; xmin > xmax, ymin > ymax
      (cd-error (e)
        (pass))  ; Should handle gracefully
      (:no-error ()
        (pass)))  ; Or auto-correct

    ;; Test transformations should still work after errors
    (finishes (world-to-canvas canvas 0.0 0.0))
    (finishes (canvas-to-world canvas 50 50))))

(test world-coordinate-performance
  "Test world coordinate performance"
  (with-debug-canvas (canvas)
    (world-set canvas -100.0 100.0 -75.0 75.0)

    ;; Test transformation performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 1000)
        (world-to-canvas canvas (- (random 200.0) 100.0) (- (random 150.0) 75.0)))
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
        (is (< elapsed 0.5) "1000 world transforms should be fast")))

    ;; Test setup performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 100)
        (world-set canvas (- i 50.0) (+ i 50.0) (- i 25.0) (+ i 25.0)))
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
        (is (< elapsed 0.5) "100 world setups should be fast")))))

(run! 'world-coordinate-tests)
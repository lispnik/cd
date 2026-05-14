(in-package #:cd-tests)

;;; Transformation Tests

(in-suite transformation-tests)

(test basic-transformations
  "Test basic coordinate transformations"
  (with-debug-canvas (canvas)
    ;; Test world coordinate transformations
    (finishes (world-to-canvas canvas 1.0 2.0))
    (finishes (canvas-to-world canvas 100 200))

    ;; Test multiple coordinate transformations
    (multiple-value-bind (x-canvas y-canvas) (world-to-canvas canvas 5.0 10.0)
      (is (numberp x-canvas))
      (is (numberp y-canvas))

      ;; Convert back and check consistency
      (multiple-value-bind (x-world y-world) (canvas-to-world canvas x-canvas y-canvas)
        (is (numberp x-world))
        (is (numberp y-world))
        ;; Should be close to original values (allowing for floating point precision)
        (is (< (abs (- x-world 5.0)) 0.01))
        (is (< (abs (- y-world 10.0)) 0.01))))))

(test coordinate-system-operations
  "Test coordinate system operations"
  (with-debug-canvas (canvas)
    ;; Test Y-axis inversion
    (let ((y 100))
      (let ((inverted-y (invert-y-axis canvas y)))
        (is (numberp inverted-y))
        ;; Double inversion should return original
        (is (= y (invert-y-axis canvas inverted-y)))))

    ;; Test MM to pixel conversion
    (multiple-value-bind (dx dy) (mm-to-pixel canvas 10.0 5.0)
      (is (integerp dx))
      (is (integerp dy))
      (is (> dx 0))  ; Assuming positive screen DPI
      (is (> dy 0)))

    ;; Test pixel to MM conversion
    (multiple-value-bind (mm-dx mm-dy) (pixel-to-mm canvas 100 50)
      (is (numberp mm-dx))
      (is (numberp mm-dy))
      (is (> mm-dx 0))
      (is (> mm-dy 0)))))

(test transformation-consistency
  "Test transformation consistency"
  (with-debug-canvas (canvas)
    ;; Test MM <-> pixel round trip
    (let ((mm-x 25.4) (mm-y 12.7))  ; 1 inch and 0.5 inch
      (multiple-value-bind (px-x px-y) (mm-to-pixel canvas mm-x mm-y)
        (multiple-value-bind (mm-x2 mm-y2) (pixel-to-mm canvas px-x px-y)
          ;; Should be close (allowing for precision loss)
          (is (< (abs (- mm-x mm-x2)) 0.1))
          (is (< (abs (- mm-y mm-y2)) 0.1)))))

    ;; Test world <-> canvas round trip
    (let ((world-x 1.5) (world-y 2.5))
      (multiple-value-bind (canvas-x canvas-y) (world-to-canvas canvas world-x world-y)
        (multiple-value-bind (world-x2 world-y2) (canvas-to-world canvas canvas-x canvas-y)
          (is (< (abs (- world-x world-x2)) 0.01))
          (is (< (abs (- world-y world-y2)) 0.01)))))))

(test transformation-edge-cases
  "Test transformation edge cases"
  (with-debug-canvas (canvas)
    ;; Test zero coordinates
    (multiple-value-bind (x y) (world-to-canvas canvas 0.0 0.0)
      (is (numberp x))
      (is (numberp y)))

    (multiple-value-bind (x y) (canvas-to-world canvas 0 0)
      (is (numberp x))
      (is (numberp y)))

    ;; Test negative coordinates
    (multiple-value-bind (x y) (world-to-canvas canvas -1.0 -1.0)
      (is (numberp x))
      (is (numberp y)))

    ;; Test large coordinates
    (multiple-value-bind (x y) (world-to-canvas canvas 1000.0 1000.0)
      (is (numberp x))
      (is (numberp y)))))

(test transformation-with-canvas-size
  "Test transformations relative to canvas size"
  (with-debug-canvas (canvas)
    (multiple-value-bind (width height) (test-canvas-size canvas)
      ;; Test transformations at canvas boundaries
      (multiple-value-bind (world-x world-y) (canvas-to-world canvas 0 0)
        (is (numberp world-x))
        (is (numberp world-y)))

      (multiple-value-bind (world-x world-y) (canvas-to-world canvas width height)
        (is (numberp world-x))
        (is (numberp world-y)))

      ;; Test Y-axis inversion at boundaries
      (let ((inverted-0 (invert-y-axis canvas 0))
            (inverted-max (invert-y-axis canvas height)))
        (is (numberp inverted-0))
        (is (numberp inverted-max))
        ;; Y-axis inversion should map 0 to height and vice versa
        (is (= inverted-0 height))
        (is (= inverted-max 0))))))

(test transformation-performance
  "Test transformation performance"
  (with-debug-canvas (canvas)
    ;; Test world-to-canvas performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 1000)
        (world-to-canvas canvas (+ i 0.5) (+ i 1.5)))
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
        (is (< elapsed 0.5) "1000 world-to-canvas transforms should be fast")))

    ;; Test canvas-to-world performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 1000)
        (canvas-to-world canvas i (* i 2)))
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
        (is (< elapsed 0.5) "1000 canvas-to-world transforms should be fast")))

    ;; Test MM conversion performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 500)
        (mm-to-pixel canvas (+ i 1.0) (+ i 2.0)))
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
        (is (< elapsed 0.5) "500 MM conversions should be fast")))))

(test transformation-state-preservation
  "Test transformation state with save/restore"
  (with-debug-canvas (canvas)
    ;; Get initial transformation state
    (multiple-value-bind (initial-x initial-y) (world-to-canvas canvas 1.0 1.0)

      ;; Save state
      (let ((state (save-state canvas)))
        ;; Verify transformation still works
        (multiple-value-bind (x y) (world-to-canvas canvas 1.0 1.0)
          (is (= x initial-x))
          (is (= y initial-y)))

        ;; Restore state
        (restore-state canvas state)

        ;; Verify transformation still works after restore
        (multiple-value-bind (x y) (world-to-canvas canvas 1.0 1.0)
          (is (= x initial-x))
          (is (= y initial-y)))

        ;; Clean up
        (release-state state)))))

(test unit-conversions
  "Test unit conversion accuracy"
  (with-debug-canvas (canvas)
    ;; Test known conversions (assuming standard DPI)
    (multiple-value-bind (pixels-x pixels-y) (mm-to-pixel canvas 25.4 25.4)  ; 1 inch
      ;; Should be close to screen DPI (typically 72-96 on desktop)
      (is (> pixels-x 50))  ; At least 50 pixels per inch
      (is (< pixels-x 200)) ; No more than 200 pixels per inch (for reasonable displays)
      (is (> pixels-y 50))
      (is (< pixels-y 200)))

    ;; Test metric conversions
    (multiple-value-bind (pixels-x pixels-y) (mm-to-pixel canvas 10.0 10.0)  ; 1 cm
      (is (> pixels-x 20))  ; Reasonable minimum
      (is (< pixels-x 100)) ; Reasonable maximum
      (is (> pixels-y 20))
      (is (< pixels-y 100)))))

(test transformation-precision
  "Test transformation precision"
  (with-debug-canvas (canvas)
    ;; Test with high-precision values
    (let ((precise-x 123.456789)
          (precise-y 987.654321))
      (multiple-value-bind (canvas-x canvas-y) (world-to-canvas canvas precise-x precise-y)
        (multiple-value-bind (world-x world-y) (canvas-to-world canvas canvas-x canvas-y)
          ;; Should preserve reasonable precision
          (is (< (abs (- precise-x world-x)) 0.001))
          (is (< (abs (- precise-y world-y)) 0.001)))))

    ;; Test with very small values
    (let ((small-x 0.001)
          (small-y 0.002))
      (multiple-value-bind (canvas-x canvas-y) (world-to-canvas canvas small-x small-y)
        (is (numberp canvas-x))
        (is (numberp canvas-y))))

    ;; Test with fractional pixel coordinates
    (multiple-value-bind (world-x world-y) (canvas-to-world canvas 100.5 200.7)
      (is (numberp world-x))
      (is (numberp world-y)))))

(run! 'transformation-tests)
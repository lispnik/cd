(in-package #:cd-tests)

;;; Integration Tests

(in-suite integration-tests)

(test complete-drawing-workflow
  "Test complete drawing workflow integration"
  (with-svg-canvas (canvas "integration-complete.svg")
    ;; Set up coordinate system
    (world-set canvas -10.0 10.0 -7.5 7.5)

    ;; Save initial state
    (let ((initial-state (save-state canvas)))

      ;; Draw coordinate grid
      (setf (foreground canvas) +gray+)
      (setf (line-style canvas) :line-dashed)
      (setf (line-width canvas) 1)

      (loop for x from -10.0 to 10.0 by 2.0 do
        (multiple-value-bind (x1 y1) (world-to-canvas canvas x -7.5)
          (multiple-value-bind (x2 y2) (world-to-canvas canvas x 7.5)
            (line canvas x1 y1 x2 y2))))

      (loop for y from -7.5 to 7.5 by 2.5 do
        (multiple-value-bind (x1 y1) (world-to-canvas canvas -10.0 y)
          (multiple-value-bind (x2 y2) (world-to-canvas canvas 10.0 y)
            (line canvas x1 y1 x2 y2))))

      ;; Draw axes with different style
      (setf (foreground canvas) +black+)
      (setf (line-style canvas) :line-continuous)
      (setf (line-width canvas) 3)

      ;; X-axis
      (multiple-value-bind (x1 y1) (world-to-canvas canvas -10.0 0.0)
        (multiple-value-bind (x2 y2) (world-to-canvas canvas 10.0 0.0)
          (line canvas x1 y1 x2 y2)))

      ;; Y-axis
      (multiple-value-bind (x1 y1) (world-to-canvas canvas 0.0 -7.5)
        (multiple-value-bind (x2 y2) (world-to-canvas canvas 0.0 7.5)
          (line canvas x1 y1 x2 y2)))

      ;; Set clipping region
      (multiple-value-bind (x1 y1) (world-to-canvas canvas -8.0 -6.0)
        (multiple-value-bind (x2 y2) (world-to-canvas canvas 8.0 6.0)
          (clip canvas x1 y1 (- x2 x1) (- y2 y1))))

      ;; Draw geometric shapes
      (setf (foreground canvas) +red+)
      (setf (interior-style canvas) :interior-solid)
      (multiple-value-bind (x y) (world-to-canvas canvas -6.0 3.0)
        (box canvas x y 40 30))

      ;; Draw circle using arc
      (setf (foreground canvas) +blue+)
      (multiple-value-bind (x y) (world-to-canvas canvas 0.0 0.0)
        (arc canvas x y 60 60 0 360))

      ;; Draw filled circle using sector
      (setf (foreground canvas) +green+)
      (multiple-value-bind (x y) (world-to-canvas canvas 4.0 -3.0)
        (sector canvas x y 50 50 0 360))

      ;; Add text labels
      (setf (foreground canvas) +black+)
      (setf (font canvas) "Arial,12")
      (multiple-value-bind (x y) (world-to-canvas canvas -6.0 4.0)
        (text canvas x y "Rectangle"))
      (multiple-value-bind (x y) (world-to-canvas canvas -1.0 -1.0)
        (text canvas x y "Circle"))
      (multiple-value-bind (x y) (world-to-canvas canvas 3.0 -4.0)
        (text canvas x y "Filled Circle"))

      ;; Turn off clipping
      (clip-off canvas)

      ;; Restore initial state
      (restore-state canvas initial-state)
      (release-state initial-state))))

(test pattern-and-text-integration
  "Test integration of patterns with text and shapes"
  (with-svg-canvas (canvas "integration-patterns.svg")
    ;; Create pattern
    (let ((pattern (create-test-pattern 12 12)))
      (setf (pattern canvas) pattern)
      (setf (interior-style canvas) :interior-pattern)

      ;; Draw patterned shapes
      (box canvas 20 20 60 40)
      (sector canvas 120 40 30 30 45 225)

      ;; Add text over pattern
      (setf (foreground canvas) +white+)
      (setf (font canvas) "Arial,14,1")  ; Bold
      (text canvas 30 35 "Patterned")

      ;; Create stipple for contrast
      (let ((stipple (create-test-stipple 8 8)))
        (setf (stipple canvas) stipple)
        (setf (interior-style canvas) :interior-stipple)
        (setf (foreground canvas) +red+)
        (box canvas 20 80 60 40)

        ;; Text over stipple
        (setf (foreground canvas) +black+)
        (text canvas 30 95 "Stippled")))))

(test multi-context-workflow
  "Test workflow using multiple contexts"
  (let ((contexts (test-contexts)))
    (when (>= (length contexts) 2)
      ;; Use first context for data generation
      (let ((canvas1 (create-canvas (first contexts))))
        (activate canvas1)

        ;; Draw some content
        (setf (foreground canvas1) +red+)
        (rect canvas1 10 10 50 30)
        (setf (foreground canvas1) +blue+)
        (arc canvas1 60 25 20 20 0 180)

        ;; Get image data
        (let ((r-data nil) (g-data nil) (b-data nil))
          (handler-case
              (multiple-value-setq (r-data g-data b-data)
                (get-image-rgb canvas1 0 0 100 50))
            (cd-error (e)
              (pass)))

          (deactivate canvas1)
          (kill canvas1)

          ;; Use second context to display data
          (when (and r-data g-data b-data)
            (let ((canvas2 (create-canvas (second contexts))))
              (activate canvas2)

              ;; Put the image data
              (put-image-rgb canvas2 100 50 r-data g-data b-data 10 10 0 0 0 0)

              (deactivate canvas2)
              (kill canvas2))))))))

(test server-image-workflow
  "Test complete server image workflow"
  (with-debug-canvas (canvas)
    (let ((width 20) (height 15))
      ;; Create test data
      (multiple-value-bind (r g b a) (create-test-rgba-data width height)
        ;; Create server image
        (let ((image (create-image-rgba canvas width height r g b a)))

          ;; Use image in various operations
          (activate canvas)

          ;; Put image at different locations and sizes
          (put-image-stretch canvas image 10 10 width height 0 0 0 0)
          (put-image-stretch canvas image 50 10 (* width 2) (* height 2) 0 0 0 0)
          (put-image-stretch canvas image 10 50 (/ width 2) (/ height 2) 0 0 0 0)

          ;; Get image data back
          (multiple-value-bind (r-out g-out b-out a-out) (get-image-rgba-server image)
            (is (arrayp r-out))
            (is (arrayp g-out))
            (is (arrayp b-out))
            (is (arrayp a-out)))

          (deactivate canvas)
          ;; Clean up
          (kill-image image))))))

(test coordinate-system-integration
  "Test integration of coordinate systems with drawing"
  (with-svg-canvas (canvas "integration-coords.svg")
    ;; Set up world coordinate system
    (world-set canvas 0.0 100.0 0.0 75.0)

    ;; Function to draw in world coordinates
    (labels ((draw-world-rect (wx wy wwidth wheight)
               (multiple-value-bind (x1 y1) (world-to-canvas canvas wx wy)
                 (multiple-value-bind (x2 y2) (world-to-canvas canvas
                                                                (+ wx wwidth)
                                                                (+ wy wheight))
                   (rect canvas x1 y1 (- x2 x1) (- y2 y1)))))

             (draw-world-text (wx wy text)
               (multiple-value-bind (x y) (world-to-canvas canvas wx wy)
                 (text canvas x y text))))

      ;; Draw using world coordinates
      (setf (foreground canvas) +red+)
      (draw-world-rect 10.0 10.0 30.0 20.0)

      (setf (foreground canvas) +blue+)
      (draw-world-rect 60.0 40.0 25.0 15.0)

      (setf (foreground canvas) +black+)
      (draw-world-text 15.0 15.0 "World Coords")

      ;; Mix world and canvas coordinates
      (setf (foreground canvas) +green+)
      (line canvas 0 0 50 50)  ; Canvas coordinates
      (multiple-value-bind (wx wy) (canvas-to-world canvas 50 50)
        (draw-world-text wx wy "Mixed")))))

(test error-recovery-integration
  "Test error recovery in complex workflows"
  (with-debug-canvas (canvas)
    (let ((operations-completed 0)
          (errors-handled 0))

      ;; Complex workflow with potential errors
      (dolist (operation
               (list
                ;; Valid operations
                (lambda () (setf (foreground canvas) +red+))
                (lambda () (line canvas 10 10 50 50))
                ;; Potentially invalid operations
                (lambda () (setf (line-width canvas) -5))  ; Invalid
                (lambda () (text canvas 10 30 "Test"))
                (lambda () (setf (foreground canvas) "invalid"))  ; Invalid
                (lambda () (rect canvas 20 20 30 20))
                ;; More valid operations
                (lambda () (arc canvas 60 30 15 15 0 180))))

        (handler-case
            (progn
              (funcall operation)
              (incf operations-completed))
          (error (e)
            (incf errors-handled)
            ;; Continue with next operation
            (continue))))

      ;; Should complete most operations despite errors
      (is (> operations-completed 4))
      (is (> errors-handled 0))

      ;; Canvas should still be usable
      (finishes (setf (foreground canvas) +blue+))
      (finishes (clear canvas)))))

(test font-and-measurement-integration
  "Test integration of font management and text measurement"
  (with-debug-canvas (canvas)
    ;; Test various fonts and measurements
    (dolist (font-spec '("Arial,12" "Times,14" "Courier,10"))
      (handler-case
          (progn
            (setf (font canvas) font-spec)
            (let ((test-text "Integration Test"))

              ;; Measure text
              (multiple-value-bind (width height) (text-size canvas test-text)
                (is (integerp width))
                (is (integerp height))

                ;; Get font metrics
                (let ((ascent (font-dim canvas :font-ascent))
                      (descent (font-dim canvas :font-descent)))
                  (is (numberp ascent))
                  (is (numberp descent))

                  ;; Use measurements for positioning
                  (text canvas 10 (+ 30 ascent) test-text)

                  ;; Draw baseline
                  (setf (foreground canvas) +red+)
                  (line canvas 10 30 (+ 10 width) 30)))))
        (cd-error (e)
          (pass))))))  ; Font might not be available

(test comprehensive-state-management
  "Test comprehensive state save/restore integration"
  (with-debug-canvas (canvas)
    ;; Set complex initial state
    (setf (foreground canvas) +red+)
    (setf (background canvas) +white+)
    (setf (line-width canvas) 3)
    (setf (line-style canvas) :line-dashed)
    (setf (font canvas) "Arial,12")

    ;; Save state at multiple levels
    (let ((state1 (save-state canvas)))
      ;; Modify state
      (setf (foreground canvas) +green+)
      (setf (line-width canvas) 5)

      (let ((state2 (save-state canvas)))
        ;; Modify again
        (setf (foreground canvas) +blue+)
        (setf (line-style canvas) :line-dotted)
        (setf (font canvas) "Times,14")

        ;; Draw with current state
        (line canvas 10 10 50 50)
        (text canvas 10 70 "State 2")

        ;; Restore to state2
        (restore-state canvas state2)
        (is (= (foreground canvas) +green+))
        (is (= (line-width canvas) 5))

        ;; Draw with state2
        (line canvas 60 10 100 50)
        (text canvas 60 70 "State 1")

        (release-state state2))

      ;; Restore to state1
      (restore-state canvas state1)
      (is (= (foreground canvas) +red+))
      (is (= (line-width canvas) 3))

      ;; Draw with original state
      (line canvas 110 10 150 50)
      (text canvas 110 70 "Original")

      (release-state state1))))

(test performance-integration
  "Test performance of integrated operations"
  (with-debug-canvas (canvas)
    ;; Time complex integrated workflow
    (let ((start-time (get-internal-real-time)))

      (dotimes (i 50)
        ;; Save state
        (let ((state (save-state canvas)))
          ;; Set attributes
          (setf (foreground canvas) (encode-color (mod (* i 5) 256)
                                                   (mod (* i 7) 256)
                                                   (mod (* i 11) 256)))
          (setf (line-width canvas) (+ 1 (mod i 5)))

          ;; Draw shapes
          (rect canvas (* i 3) 10 10 10)
          (arc canvas (* i 3) 30 8 8 0 180)

          ;; Restore state
          (restore-state canvas state)
          (release-state state)))

      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
        (is (< elapsed 2.0) "Integrated operations should complete in reasonable time")
        (format t "~&Integration performance: ~F seconds for 50 iterations~%" elapsed)))))

(run! 'integration-tests)
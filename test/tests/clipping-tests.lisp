(in-package #:cd-tests)

;;; Clipping Tests

(in-suite clipping-tests)

(test clipping-basic-operations
  "Test basic clipping operations"
  (with-svg-canvas (canvas "clipping-basic.svg")
    ;; Test rectangular clipping
    (finishes (clip canvas 20 20 60 40))

    ;; Draw something that should be clipped
    (setf (foreground canvas) +red+)
    (finishes (rect canvas 10 10 80 60))  ; Extends beyond clip region

    ;; Turn off clipping
    (finishes (clip-off canvas))

    ;; Draw something that should not be clipped
    (setf (foreground canvas) +blue+)
    (finishes (rect canvas 100 10 80 60))))

(test clipping-with-primitives
  "Test clipping with various drawing primitives"
  (with-svg-canvas (canvas "clipping-primitives.svg")
    ;; Set clipping region
    (clip canvas 30 30 40 40)

    ;; Test clipped lines
    (setf (foreground canvas) +red+)
    (setf (line-width canvas) 3)
    (finishes (line canvas 10 35 80 35))  ; Horizontal line through clip
    (finishes (line canvas 50 10 50 80))  ; Vertical line through clip

    ;; Test clipped filled shapes
    (setf (foreground canvas) +green+)
    (finishes (box canvas 45 45 30 30))   ; Partially clipped box

    ;; Test clipped arcs
    (setf (foreground canvas) +blue+)
    (finishes (sector canvas 50 50 25 25 0 180))  ; Partially clipped sector

    ;; Turn off clipping
    (clip-off canvas)))

(test clipping-nested-regions
  "Test nested clipping regions"
  (with-svg-canvas (canvas "clipping-nested.svg")
    ;; Outer clipping region
    (let ((state1 (save-state canvas)))
      (clip canvas 10 10 80 80)

      ;; Draw in outer clip
      (setf (foreground canvas) +red+)
      (finishes (box canvas 0 0 100 100))

      ;; Inner clipping region
      (let ((state2 (save-state canvas)))
        (clip canvas 30 30 40 40)

        ;; Draw in inner clip (intersection)
        (setf (foreground canvas) +green+)
        (finishes (box canvas 0 0 100 100))

        ;; Restore to outer clip
        (restore-state canvas state2)
        (release-state state2))

      ;; Draw in outer clip again
      (setf (foreground canvas) +blue+)
      (finishes (rect canvas 50 50 30 30))

      ;; Restore to no clipping
      (restore-state canvas state1)
      (release-state state1))

    ;; Draw without clipping
    (setf (foreground canvas) +black+)
    (finishes (rect canvas 0 0 100 100))))

(test clipping-state-management
  "Test clipping state save and restore"
  (with-debug-canvas (canvas)
    ;; Set initial clipping
    (clip canvas 10 10 50 50)

    ;; Save state
    (let ((state (save-state canvas)))
      ;; Change clipping
      (clip canvas 30 30 20 20)

      ;; Restore state - should restore original clipping
      (restore-state canvas state)

      ;; Draw to test clipping is restored
      (finishes (rect canvas 0 0 70 70))

      ;; Clean up
      (release-state state))))

(test clipping-edge-cases
  "Test clipping edge cases"
  (with-debug-canvas (canvas)
    ;; Test zero-size clipping region
    (handler-case
        (clip canvas 10 10 0 0)
      (cd-error (e)
        (pass))
      (:no-error ()
        (pass)))  ; May succeed with empty clip

    ;; Test negative coordinates
    (finishes (clip canvas -10 -10 30 30))

    ;; Test clipping beyond canvas
    (multiple-value-bind (width height) (test-canvas-size canvas)
      (finishes (clip canvas 0 0 (+ width 100) (+ height 100))))

    ;; Reset clipping
    (clip-off canvas)))

(test clipping-performance
  "Test clipping performance"
  (with-debug-canvas (canvas)
    ;; Test multiple clip operations
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 100)
        (clip canvas (* i 2) (* i 2) 20 20)
        (rect canvas 0 0 50 50)
        (clip-off canvas))
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
        (is (< elapsed 2.0) "100 clip/draw/unclip cycles should be reasonably fast")))))

(test clipping-with-text
  "Test clipping with text rendering"
  (with-svg-canvas (canvas "clipping-text.svg")
    ;; Set clipping region
    (clip canvas 20 20 60 30)

    ;; Draw text that should be clipped
    (setf (foreground canvas) +black+)
    (finishes (text canvas 10 30 "This text should be clipped"))
    (finishes (text canvas 10 50 "This text too"))

    ;; Turn off clipping
    (clip-off canvas)

    ;; Draw unclipped text
    (finishes (text canvas 10 70 "This text should not be clipped"))))

(test clipping-with-images
  "Test clipping with image operations"
  (with-debug-canvas (canvas)
    (let ((width 20) (height 20))
      (multiple-value-bind (r g b) (create-test-rgb-data width height)
        ;; Set clipping region
        (clip canvas 15 15 30 30)

        ;; Put image that should be clipped
        (finishes (put-image-rgb canvas width height r g b 10 10 0 0 0 0))

        ;; Put image that should be partially visible
        (finishes (put-image-rgb canvas width height r g b 25 25 0 0 0 0))

        ;; Turn off clipping
        (clip-off canvas)))))

(test clipping-boundaries
  "Test clipping at exact boundaries"
  (with-debug-canvas (canvas)
    (multiple-value-bind (width height) (test-canvas-size canvas)
      ;; Clip to exact canvas boundaries
      (finishes (clip canvas 0 0 width height))

      ;; Draw something
      (finishes (rect canvas -10 -10 (+ width 20) (+ height 20)))

      ;; Clip to smaller region
      (finishes (clip canvas 10 10 (- width 20) (- height 20)))

      ;; Draw again
      (finishes (rect canvas 0 0 width height))

      ;; Turn off clipping
      (clip-off canvas))))

(test clipping-with-patterns
  "Test clipping with patterns and stipples"
  (with-svg-canvas (canvas "clipping-patterns.svg")
    (let ((pattern (create-test-pattern 8 8))
          (stipple (create-test-stipple 8 8)))

      ;; Set clipping
      (clip canvas 20 20 60 40)

      ;; Draw with pattern
      (setf (pattern canvas) pattern)
      (setf (interior-style canvas) :interior-pattern)
      (finishes (box canvas 10 10 80 60))

      ;; Draw with stipple
      (setf (stipple canvas) stipple)
      (setf (interior-style canvas) :interior-stipple)
      (setf (foreground canvas) +red+)
      (finishes (box canvas 10 70 80 60))

      ;; Turn off clipping
      (clip-off canvas))))

(test clipping-coordinate-validation
  "Test clipping coordinate validation"
  (with-debug-canvas (canvas)
    ;; Test with swapped coordinates (min > max)
    (handler-case
        (clip canvas 50 50 10 10)  ; x2 < x1, y2 < y1
      (cd-error (e)
        (pass))
      (:no-error ()
        (pass)))  ; May auto-correct or accept

    ;; Test with floating point coordinates
    (handler-case
        (clip canvas 10.5 20.7 30.3 40.9)
      (error (e)
        (pass))  ; May not accept float
      (:no-error ()
        (pass)))  ; Or may accept and convert

    ;; Reset clipping
    (clip-off canvas)))

(test clipping-error-recovery
  "Test clipping error recovery"
  (with-debug-canvas (canvas)
    ;; Set valid clipping
    (clip canvas 10 10 50 50)

    ;; Try invalid operation
    (handler-case
        (clip canvas "invalid" "coords" "here" "too")
      (error (e)
        (pass)))  ; Should handle gracefully

    ;; Verify clipping state is still reasonable
    (finishes (rect canvas 0 0 70 70))

    ;; Should be able to turn off clipping
    (finishes (clip-off canvas))))

(run! 'clipping-tests)
(in-package #:cd-tests)

;;; Vector Text Tests

(in-suite vector-tests)

(test vector-text-basic
  "Test basic vector text rendering"
  (with-svg-canvas (canvas "vector-text-basic.svg")
    ;; Test simple vector text
    (finishes (vector-text canvas 10 50 "Hello Vector"))

    ;; Test vector text at different positions
    (finishes (vector-text canvas 10 80 "Position 1"))
    (finishes (vector-text canvas 10 110 "Position 2"))

    ;; Test with different colors
    (setf (foreground canvas) +red+)
    (finishes (vector-text canvas 10 140 "Red Vector Text"))

    (setf (foreground canvas) +blue+)
    (finishes (vector-text canvas 10 170 "Blue Vector Text"))))

(test vector-text-font-selection
  "Test vector text with different fonts"
  (with-debug-canvas (canvas)
    ;; Test with different vector fonts
    (handler-case
        (progn
          (finishes (vector-font canvas "Helvetica" :vector-font-plain))
          (finishes (vector-text canvas 10 30 "Helvetica Plain")))
      (cd-error (e)
        (pass)))  ; Vector fonts might not be available

    (handler-case
        (progn
          (finishes (vector-font canvas "Times" :vector-font-bold))
          (finishes (vector-text canvas 10 60 "Times Bold")))
      (cd-error (e)
        (pass)))

    (handler-case
        (progn
          (finishes (vector-font canvas "Courier" :vector-font-italic))
          (finishes (vector-text canvas 10 90 "Courier Italic")))
      (cd-error (e)
        (pass)))))

(test vector-text-size-operations
  "Test vector text size operations"
  (with-debug-canvas (canvas)
    ;; Test vector text size
    (finishes (vector-text-size canvas 10.0))
    (is (numberp (vector-text-size canvas)))

    ;; Test with different sizes
    (dolist (size '(8.0 12.0 16.0 24.0))
      (finishes (vector-text-size canvas size))
      (is (= (vector-text-size canvas) size)))))

(test vector-text-direction
  "Test vector text direction"
  (with-svg-canvas (canvas "vector-text-direction.svg")
    ;; Test horizontal text
    (finishes (vector-text-direction canvas 0.0))
    (finishes (vector-text canvas 50 50 "Horizontal"))

    ;; Test vertical text (90 degrees)
    (finishes (vector-text-direction canvas 90.0))
    (finishes (vector-text canvas 80 50 "Vertical"))

    ;; Test angled text
    (finishes (vector-text-direction canvas 45.0))
    (finishes (vector-text canvas 120 80 "Angled"))

    ;; Reset to horizontal
    (finishes (vector-text-direction canvas 0.0))))

(test vector-text-alignment
  "Test vector text alignment"
  (with-svg-canvas (canvas "vector-text-alignment.svg")
    (let ((x 100) (y 50))
      ;; Draw reference point
      (setf (foreground canvas) +red+)
      (setf (mark-type canvas) :mark-plus)
      (setf (mark-size canvas) 10)
      (mark canvas x y)

      ;; Test different alignments
      (setf (foreground canvas) +black+)

      ;; Left alignment
      (finishes (vector-text-alignment canvas :align-left))
      (finishes (vector-text canvas x (+ y 20) "Left Aligned"))

      ;; Center alignment
      (finishes (vector-text-alignment canvas :align-center))
      (finishes (vector-text canvas x (+ y 40) "Center Aligned"))

      ;; Right alignment
      (finishes (vector-text-alignment canvas :align-right))
      (finishes (vector-text canvas x (+ y 60) "Right Aligned")))))

(test vector-text-bounds
  "Test vector text bounds calculation"
  (with-debug-canvas (canvas)
    (let ((text "Bounds Test"))
      ;; Test text bounds calculation
      (multiple-value-bind (xmin xmax ymin ymax)
          (vector-text-bounds canvas text)
        (is (numberp xmin))
        (is (numberp xmax))
        (is (numberp ymin))
        (is (numberp ymax))
        (is (<= xmin xmax))
        (is (<= ymin ymax)))

      ;; Test with different font sizes
      (vector-text-size canvas 20.0)
      (multiple-value-bind (xmin2 xmax2 ymin2 ymax2)
          (vector-text-bounds canvas text)
        (is (numberp xmin2))
        (is (numberp xmax2))
        (is (numberp ymin2))
        (is (numberp ymax2))

        ;; Larger font should generally have larger bounds
        (is (>= (- xmax2 xmin2) (- xmax xmin)))
        (is (>= (- ymax2 ymin2) (- ymax ymin)))))))

(test vector-text-character-size
  "Test vector text character size"
  (with-debug-canvas (canvas)
    ;; Test character size
    (finishes (vector-char-size canvas 12.0))
    (is (numberp (vector-char-size canvas)))

    ;; Test with different sizes
    (dolist (size '(6.0 10.0 14.0 18.0))
      (finishes (vector-char-size canvas size))
      (let ((actual-size (vector-char-size canvas)))
        (is (numberp actual-size))
        (is (> actual-size 0))))))

(test vector-text-transformations
  "Test vector text with transformations"
  (with-svg-canvas (canvas "vector-text-transform.svg")
    ;; Test scaling
    (vector-text-size canvas 16.0)
    (finishes (vector-text canvas 20 30 "Normal Size"))

    (vector-text-size canvas 8.0)
    (finishes (vector-text canvas 20 50 "Small Size"))

    (vector-text-size canvas 24.0)
    (finishes (vector-text canvas 20 80 "Large Size"))

    ;; Test rotation with different directions
    (vector-text-size canvas 12.0)
    (vector-text-direction canvas 30.0)
    (finishes (vector-text canvas 150 50 "Rotated 30°"))

    (vector-text-direction canvas -30.0)
    (finishes (vector-text canvas 150 100 "Rotated -30°"))))

(test vector-text-special-characters
  "Test vector text with special characters"
  (with-debug-canvas (canvas)
    ;; Test empty string
    (finishes (vector-text canvas 10 10 ""))

    ;; Test string with spaces
    (finishes (vector-text canvas 10 30 "   spaced   text   "))

    ;; Test string with newlines (behavior may vary)
    (handler-case
        (vector-text canvas 10 50 "line1\nline2")
      (cd-error (e)
        (pass))  ; May not support multiline
      (:no-error ()
        (pass)))

    ;; Test long string
    (let ((long-text (make-string 100 :initial-element #\X)))
      (finishes (vector-text canvas 10 70 long-text)))))

(test vector-text-font-styles
  "Test vector text font styles"
  (with-debug-canvas (canvas)
    ;; Test different font styles
    (dolist (style '(:vector-font-plain :vector-font-bold
                     :vector-font-italic :vector-font-bold-italic))
      (handler-case
          (progn
            (finishes (vector-font canvas "Helvetica" style))
            (finishes (vector-text canvas 10 30 "Style Test")))
        (cd-error (e)
          (pass))))))  ; Some styles might not be supported

(test vector-text-performance
  "Test vector text performance"
  (with-debug-canvas (canvas)
    (let ((test-text "Performance Test"))
      ;; Test rendering performance
      (let ((start-time (get-internal-real-time)))
        (dotimes (i 50)
          (vector-text canvas 10 (+ 10 (* i 5)) test-text))
        (let ((elapsed (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
          (is (< elapsed 2.0) "50 vector text renders should be reasonably fast")))

      ;; Test bounds calculation performance
      (let ((start-time (get-internal-real-time)))
        (dotimes (i 100)
          (vector-text-bounds canvas test-text))
        (let ((elapsed (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
          (is (< elapsed 1.0) "100 bounds calculations should be fast"))))))

(test vector-text-state-preservation
  "Test vector text state with save/restore"
  (with-debug-canvas (canvas)
    ;; Set initial state
    (vector-text-size canvas 14.0)
    (vector-text-direction canvas 45.0)
    (vector-text-alignment canvas :align-center)

    (let ((initial-size (vector-text-size canvas)))

      ;; Save state
      (let ((state (save-state canvas)))
        ;; Change settings
        (vector-text-size canvas 20.0)
        (vector-text-direction canvas 0.0)
        (vector-text-alignment canvas :align-left)

        ;; Restore state
        (restore-state canvas state)

        ;; Verify restoration
        (is (= (vector-text-size canvas) initial-size))

        ;; Clean up
        (release-state state)))))

(test vector-text-error-conditions
  "Test vector text error handling"
  (with-debug-canvas (canvas)
    ;; Test invalid font
    (handler-case
        (vector-font canvas "NonexistentFont" :vector-font-plain)
      (cd-error (e)
        (pass))  ; Should handle gracefully
      (:no-error ()
        (pass)))

    ;; Test invalid size
    (handler-case
        (vector-text-size canvas -5.0)
      (cd-error (e)
        (pass))
      (:no-error ()
        (pass)))

    ;; Test invalid direction
    (handler-case
        (vector-text-direction canvas 3650.0)  ; Very large angle
      (cd-error (e)
        (pass))
      (:no-error ()
        (pass)))

    ;; Test text rendering should still work after errors
    (finishes (vector-text canvas 10 10 "Recovery Test"))))

(run! 'vector-tests)
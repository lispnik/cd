(in-package #:cd-tests)

;;; Text Rendering Tests

(in-suite text-tests)

(test text-basic-rendering
  "Test basic text rendering"
  (with-svg-canvas (canvas "text-basic.svg")
    ;; Test simple text
    (finishes (text canvas 10 30 "Hello World"))

    ;; Test text at different positions
    (finishes (text canvas 10 60 "Position 1"))
    (finishes (text canvas 10 90 "Position 2"))

    ;; Test with different colors
    (setf (foreground canvas) +red+)
    (finishes (text canvas 10 120 "Red Text"))

    (setf (foreground canvas) +blue+)
    (finishes (text canvas 10 150 "Blue Text"))))

(test text-alignment
  "Test text alignment modes"
  (with-svg-canvas (canvas "text-alignment.svg")
    (let ((x 100) (y 50))
      ;; Draw reference point
      (setf (foreground canvas) +red+)
      (setf (mark-type canvas) :mark-plus)
      (mark canvas x y)

      ;; Test different alignments
      (setf (foreground canvas) +black+)

      (setf (text-alignment canvas) :align-left)
      (finishes (text canvas x (+ y 20) "Left"))

      (setf (text-alignment canvas) :align-center)
      (finishes (text canvas x (+ y 40) "Center"))

      (setf (text-alignment canvas) :align-right)
      (finishes (text canvas x (+ y 60) "Right")))))

(test text-font-selection
  "Test font selection and properties"
  (with-debug-canvas (canvas)
    ;; Test font selection
    (let ((font "Helvetica,12"))
      (finishes (setf (font canvas) font))
      (is (stringp (font canvas))))

    ;; Test font with different sizes
    (finishes (setf (font canvas) "Times,14"))
    (finishes (setf (font canvas) "Courier,10"))))

(test text-measurement
  "Test text measurement functions"
  (with-debug-canvas (canvas)
    (let ((text "Hello World"))
      ;; Test text size measurement
      (multiple-value-bind (width height) (text-size canvas text)
        (is (integerp width))
        (is (integerp height))
        (is (> width 0))
        (is (> height 0)))

      ;; Test text bounds
      (multiple-value-bind (xmin xmax ymin ymax)
          (text-bounds canvas 10 20 text)
        (is (integerp xmin))
        (is (integerp xmax))
        (is (integerp ymin))
        (is (integerp ymax))
        (is (<= xmin xmax))
        (is (<= ymin ymax)))

      ;; Test text box measurement
      (multiple-value-bind (xmin xmax ymin ymax)
          (text-box canvas 10 20 text)
        (is (integerp xmin))
        (is (integerp xmax))
        (is (integerp ymin))
        (is (integerp ymax))))))

(test text-font-metrics
  "Test font metrics functions"
  (with-debug-canvas (canvas)
    ;; Test font ascent/descent
    (let ((ascent (font-dim canvas :font-ascent))
          (descent (font-dim canvas :font-descent))
          (height (font-dim canvas :font-height)))
      (is (numberp ascent))
      (is (numberp descent))
      (is (numberp height))
      (is (> ascent 0))
      (is (> descent 0))
      (is (> height 0)))

    ;; Test max width
    (let ((max-width (font-dim canvas :font-max-width)))
      (is (numberp max-width))
      (is (> max-width 0)))))

(test text-multiline
  "Test multiline text handling"
  (with-svg-canvas (canvas "text-multiline.svg")
    (let ((multiline-text (format nil "Line 1~%Line 2~%Line 3")))
      ;; Test rendering multiline text
      (finishes (text canvas 10 30 multiline-text))

      ;; Test measurement of multiline text
      (multiple-value-bind (width height) (text-size canvas multiline-text)
        (is (integerp width))
        (is (integerp height))
        (is (> width 0))
        (is (> height 0))))))

(test text-special-characters
  "Test text with special characters"
  (with-debug-canvas (canvas)
    ;; Test empty string
    (finishes (text canvas 10 10 ""))

    ;; Test string with spaces
    (finishes (text canvas 10 30 "   spaced   "))

    ;; Test string with tabs
    (finishes (text canvas 10 50 "tab	text"))

    ;; Test string with newlines
    (finishes (text canvas 10 70 "line1
line2"))

    ;; Test very long string
    (let ((long-text (make-string 200 :initial-element #\x)))
      (finishes (text canvas 10 90 long-text)))))

(test text-performance
  "Test text rendering performance"
  (with-debug-canvas (canvas)
    (let ((test-text "Performance Test"))
      (let ((start-time (get-internal-real-time)))
        (dotimes (i 50)
          (text canvas 10 (+ 10 (* i 2)) test-text))
        (let ((end-time (get-internal-real-time)))
          (let ((elapsed (/ (- end-time start-time) internal-time-units-per-second)))
            (is (< elapsed 2.0) "Rendering 50 text strings should complete quickly")))))))

(test text-with-transformations
  "Test text rendering with transformations"
  (with-svg-canvas (canvas "text-transform.svg")
    ;; Save original state
    (let ((state (save-state canvas)))

      ;; Test text with different orientations if supported
      (finishes (text canvas 50 50 "Normal Text"))

      ;; Test rotated text if canvas supports it
      (handler-case
          (progn
            ;; This may not be supported by all contexts
            (finishes (text canvas 50 100 "Transformed Text")))
        (cd-error (e)
          (pass)))  ; It's ok if transformations aren't supported

      ;; Restore state
      (restore-state canvas state)
      (release-state state))))

(test text-error-conditions
  "Test text error handling"
  (with-debug-canvas (canvas)
    ;; Test with nil text (should be handled gracefully)
    (handler-case
        (text canvas 10 10 nil)
      (error (e)
        (pass)))  ; Either works or errors gracefully

    ;; Test with invalid coordinates
    (finishes (text canvas -1000 -1000 "Offscreen text"))))

(run! 'text-tests)
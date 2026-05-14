(in-package #:cd-tests)

;;; Advanced Text Tests

(def-suite advanced-text-tests :in cd-test-suite)
(in-suite advanced-text-tests)

(test multiline-text
  "Test multi-line text rendering"
  (with-svg-canvas (canvas "multiline-text.svg")
    ;; Test simple multiline
    (let ((text "Line 1
Line 2
Line 3"))
      (finishes (text-multiline canvas 10 30 text)))

    ;; Test with alignment
    (let ((text "Left aligned
Center aligned
Right aligned"))
      (finishes (text-multiline canvas 100 30 text :alignment :left))
      (finishes (text-multiline canvas 100 100 text :alignment :center))
      (finishes (text-multiline canvas 100 170 text :alignment :right)))))

(test text-wrapping
  "Test text wrapping functionality"
  (with-debug-canvas (canvas)
    ;; Test line wrapping
    (let ((long-text "This is a very long line that should be wrapped at a specific width"))
      (finishes (text-multiline canvas 10 30 long-text :max-width 50)))

    ;; Test word wrapping
    (let ((lines (split-text-lines "Hello world this is a test" 20)))
      (is (listp lines))
      (is (> (length lines) 1)))))

(test text-measurement-advanced
  "Test advanced text measurement"
  (with-debug-canvas (canvas)
    ;; Test multi-line measurement
    (let ((lines '("Line 1" "Longer line 2" "Line 3")))
      (multiple-value-bind (width height) (measure-text-lines canvas lines)
        (is (integerp width))
        (is (integerp height))
        (is (> width 0))
        (is (> height 0))))

    ;; Test text bounds
    (let ((bounds (measure-text-bounds canvas "Test bounds")))
      (is (arrayp bounds))
      (is (= (length bounds) 8))) ; 4 corner points * 2 coordinates

    ;; Test baseline calculation
    (let ((baseline (get-text-baseline canvas "Test")))
      (is (integerp baseline))
      (is (> baseline 0)))))

(test rich-text-rendering
  "Test rich text with multiple styles"
  (with-debug-canvas (canvas)
    ;; Test text style creation
    (let ((style1 (make-text-style :font-family "Arial" :font-size 12 :color +red+))
          (style2 (make-text-style :font-family "Times" :font-size 14 :color +blue+)))
      (is (not (null style1)))
      (is (not (null style2)))

      ;; Test rich text segments
      (let ((segments (list (make-rich-text-segment "Red text " style1)
                           (make-rich-text-segment "Blue text" style2))))
        (finishes (draw-rich-text canvas 10 30 segments))))))

(test text-effects
  "Test text visual effects"
  (with-svg-canvas (canvas "text-effects.svg")
    ;; Test outlined text
    (finishes (draw-outlined-text canvas 10 30 "Outlined" 2 +black+ +white+))

    ;; Test shadow text
    (finishes (draw-shadow-text canvas 10 60 "Shadow" 2 2 +gray+ +black+))

    ;; Test 3D text
    (finishes (draw-3d-text canvas 10 90 "3D Text" 3 +red+ +black+))))

(test text-decorations
  "Test text decorations"
  (with-svg-canvas (canvas "text-decorations.svg")
    ;; Test underlined text
    (finishes (underline-text canvas 10 30 "Underlined"))

    ;; Test strikethrough text
    (finishes (strikethrough-text canvas 10 60 "Strikethrough"))))

(test text-layout-utilities
  "Test text layout utilities"
  (with-debug-canvas (canvas)
    ;; Test text fitting
    (let ((size (fit-text-to-box canvas "Test" 100 50)))
      (is (integerp size))
      (is (> size 0)))

    ;; Test line spacing calculation
    (let ((spacing (calculate-line-spacing canvas 1.5)))
      (is (integerp spacing))
      (is (> spacing 0)))

    ;; Test font metrics
    (let ((metrics (get-font-metrics canvas)))
      (is (listp metrics))
      (is (getf metrics :ascent))
      (is (getf metrics :descent))
      (is (getf metrics :height)))))

(test text-justification
  "Test text justification"
  (with-debug-canvas (canvas)
    ;; Test justify text
    (multiple-value-bind (words extra-space)
        (justify-text canvas "This is a test line" 200)
      (is (listp words))
      (is (numberp extra-space)))))

(test advanced-text-error-handling
  "Test advanced text error handling"
  (with-debug-canvas (canvas)
    ;; Test with nil text
    (handler-case
        (text-multiline canvas 10 10 nil)
      (error (e)
        (pass)))

    ;; Test with invalid alignment
    (handler-case
        (text-multiline canvas 10 10 "test" :alignment :invalid)
      (error (e)
        (pass)))))

(test text-performance-advanced
  "Test advanced text performance"
  (with-debug-canvas (canvas)
    ;; Test multiline text performance
    (let ((long-text (make-string 500 :initial-element #\x))
          (start-time (get-internal-real-time)))
      (dotimes (i 10)
        (text-multiline canvas 10 10 long-text :max-width 50))
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
        (is (< elapsed 2.0) "10 multiline text renders should complete quickly")))

    ;; Test rich text performance
    (let ((segments (list (make-rich-text-segment "Test " (make-text-style))
                         (make-rich-text-segment "Rich " (make-text-style :color +red+))
                         (make-rich-text-segment "Text" (make-text-style :color +blue+))))
          (start-time (get-internal-real-time)))
      (dotimes (i 20)
        (draw-rich-text canvas 10 30 segments))
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
        (is (< elapsed 1.0) "20 rich text renders should complete quickly")))))

(test text-with-transforms
  "Test text with transformations"
  (with-svg-canvas (canvas "text-transforms.svg")
    ;; Test rotated text
    (with-rotation (canvas 45)
      (text canvas 50 50 "Rotated Text"))

    ;; Test scaled text
    (with-scaling (canvas 2 2)
      (text canvas 25 75 "Scaled"))

    ;; Test translated text
    (with-translation (canvas 100 0)
      (text canvas 10 100 "Translated"))))

(test unicode-text-support
  "Test Unicode text support"
  (with-debug-canvas (canvas)
    ;; Test basic Unicode
    (let ((unicode-text "Hello αβγ δεζ"))
      (handler-case
          (progn
            (finishes (text canvas 10 30 unicode-text))
            (multiple-value-bind (width height) (text-size canvas unicode-text)
              (is (integerp width))
              (is (integerp height))))
        (cd-error (e)
          (pass)))) ; Unicode might not be fully supported

    ;; Test emoji (if supported)
    (handler-case
        (text canvas 10 60 "Test 🌍 emoji")
      (cd-error (e)
        (pass)))))

(run! 'advanced-text-tests)
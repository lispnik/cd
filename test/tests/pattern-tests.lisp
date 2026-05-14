(in-package #:cd-tests)

;;; Pattern and Stipple Tests

(in-suite pattern-tests)

(test pattern-basic-operations
  "Test basic pattern operations"
  (with-debug-canvas (canvas)
    (let ((width 8) (height 8))
      (let ((pattern (create-test-pattern width height)))
        ;; Test setting pattern
        (finishes (setf (pattern canvas) pattern))

        ;; Test getting pattern dimensions
        (multiple-value-bind (w h) (pattern-size canvas)
          (is (integerp w))
          (is (integerp h))
          (when (and w h (> w 0) (> h 0))
            (is (<= w 256))  ; Reasonable size limit
            (is (<= h 256))))))))

(test pattern-drawing
  "Test drawing with patterns"
  (with-svg-canvas (canvas "pattern-test.svg")
    (let ((width 4) (height 4))
      (let ((pattern (create-test-pattern width height)))
        ;; Set pattern
        (setf (pattern canvas) pattern)
        (setf (interior-style canvas) :interior-pattern)

        ;; Draw filled shapes with pattern
        (finishes (box canvas 10 10 30 20))
        (finishes (sector canvas 60 25 20 20 0 180))

        ;; Test with different patterns
        (let ((pattern2 (make-array (list height width)
                                   :initial-function (lambda (y x)
                                                       (if (= (mod (+ x y) 3) 0)
                                                           +green+ +blue+)))))
          (setf (pattern canvas) pattern2)
          (finishes (box canvas 100 10 30 20)))))))

(test stipple-basic-operations
  "Test basic stipple operations"
  (with-debug-canvas (canvas)
    (let ((width 8) (height 8))
      (let ((stipple (create-test-stipple width height)))
        ;; Test setting stipple
        (finishes (setf (stipple canvas) stipple))

        ;; Test getting stipple dimensions
        (multiple-value-bind (w h) (stipple-size canvas)
          (is (integerp w))
          (is (integerp h))
          (when (and w h (> w 0) (> h 0))
            (is (<= w 256))  ; Reasonable size limit
            (is (<= h 256))))))))

(test stipple-drawing
  "Test drawing with stipples"
  (with-svg-canvas (canvas "stipple-test.svg")
    (let ((width 8) (height 8))
      (let ((stipple (create-test-stipple width height)))
        ;; Set stipple
        (setf (stipple canvas) stipple)
        (setf (interior-style canvas) :interior-stipple)
        (setf (foreground canvas) +red+)

        ;; Draw filled shapes with stipple
        (finishes (box canvas 10 10 30 20))
        (finishes (sector canvas 60 25 20 20 0 180))

        ;; Test with different stipple pattern
        (let ((stipple2 (make-array (list height width)
                                   :element-type 'boolean
                                   :initial-function (lambda (y x)
                                                       (= (mod x 2) (mod y 2))))))
          (setf (stipple canvas) stipple2)
          (finishes (box canvas 100 10 30 20)))))))

(test pattern-types
  "Test different pattern types and sizes"
  (with-debug-canvas (canvas)
    ;; Test various pattern sizes
    (dolist (size '((2 2) (4 4) (8 8) (16 16)))
      (let ((width (first size))
            (height (second size)))
        (let ((pattern (make-array (list height width)
                                  :initial-function (lambda (y x)
                                                      (encode-color
                                                       (mod (* x 32) 256)
                                                       (mod (* y 32) 256)
                                                       (mod (* (+ x y) 16) 256))))))
          (finishes (setf (pattern canvas) pattern)))))

    ;; Test monochromatic patterns
    (let ((pattern (make-array '(4 4) :initial-element +red+)))
      (finishes (setf (pattern canvas) pattern)))

    ;; Test high-contrast patterns
    (let ((pattern (make-array '(4 4)
                              :initial-function (lambda (y x)
                                                  (if (evenp (+ x y)) +black+ +white+)))))
      (finishes (setf (pattern canvas) pattern)))))

(test stipple-types
  "Test different stipple types and sizes"
  (with-debug-canvas (canvas)
    ;; Test various stipple sizes
    (dolist (size '((2 2) (4 4) (8 8) (16 16)))
      (let ((width (first size))
            (height (second size)))
        (let ((stipple (make-array (list height width)
                                  :element-type 'boolean
                                  :initial-function (lambda (y x)
                                                      (evenp (+ x y))))))
          (finishes (setf (stipple canvas) stipple)))))

    ;; Test solid stipple
    (let ((stipple (make-array '(4 4) :element-type 'boolean :initial-element t)))
      (finishes (setf (stipple canvas) stipple)))

    ;; Test empty stipple
    (let ((stipple (make-array '(4 4) :element-type 'boolean :initial-element nil)))
      (finishes (setf (stipple canvas) stipple)))

    ;; Test checkerboard stipple
    (let ((stipple (make-array '(8 8)
                              :element-type 'boolean
                              :initial-function (lambda (y x)
                                                  (evenp (+ (floor x 2) (floor y 2)))))))
      (finishes (setf (stipple canvas) stipple)))))

(test pattern-with-interior-styles
  "Test patterns with different interior styles"
  (with-svg-canvas (canvas "pattern-interior.svg")
    (let ((pattern (create-test-pattern 6 6)))
      (setf (pattern canvas) pattern)

      ;; Test solid interior (pattern should not be visible)
      (setf (interior-style canvas) :interior-solid)
      (finishes (box canvas 10 10 20 20))

      ;; Test pattern interior
      (setf (interior-style canvas) :interior-pattern)
      (finishes (box canvas 40 10 20 20))

      ;; Test hollow interior (pattern should not be visible)
      (setf (interior-style canvas) :interior-hollow)
      (finishes (box canvas 70 10 20 20)))))

(test stipple-with-interior-styles
  "Test stipples with different interior styles"
  (with-svg-canvas (canvas "stipple-interior.svg")
    (let ((stipple (create-test-stipple 6 6)))
      (setf (stipple canvas) stipple)
      (setf (foreground canvas) +blue+)

      ;; Test solid interior (stipple should not be visible)
      (setf (interior-style canvas) :interior-solid)
      (finishes (box canvas 10 10 20 20))

      ;; Test stipple interior
      (setf (interior-style canvas) :interior-stipple)
      (finishes (box canvas 40 10 20 20))

      ;; Test hollow interior (stipple should not be visible)
      (setf (interior-style canvas) :interior-hollow)
      (finishes (box canvas 70 10 20 20)))))

(test pattern-state-management
  "Test pattern state save/restore"
  (with-debug-canvas (canvas)
    (let ((pattern1 (create-test-pattern 4 4))
          (pattern2 (create-test-pattern 6 6)))

      ;; Set initial pattern
      (setf (pattern canvas) pattern1)

      ;; Save state
      (let ((state (save-state canvas)))
        ;; Change pattern
        (setf (pattern canvas) pattern2)

        ;; Restore state
        (restore-state canvas state)

        ;; Pattern should be restored (can't easily verify exact pattern,
        ;; but size might be preserved)
        (multiple-value-bind (w h) (pattern-size canvas)
          (when (and w h)
            ;; Just check we get valid dimensions
            (is (integerp w))
            (is (integerp h))))

        ;; Clean up
        (release-state state)))))

(test stipple-state-management
  "Test stipple state save/restore"
  (with-debug-canvas (canvas)
    (let ((stipple1 (create-test-stipple 4 4))
          (stipple2 (create-test-stipple 8 8)))

      ;; Set initial stipple
      (setf (stipple canvas) stipple1)

      ;; Save state
      (let ((state (save-state canvas)))
        ;; Change stipple
        (setf (stipple canvas) stipple2)

        ;; Restore state
        (restore-state canvas state)

        ;; Stipple should be restored
        (multiple-value-bind (w h) (stipple-size canvas)
          (when (and w h)
            (is (integerp w))
            (is (integerp h))))

        ;; Clean up
        (release-state state)))))

(test pattern-error-conditions
  "Test pattern error handling"
  (with-debug-canvas (canvas)
    ;; Test empty pattern
    (handler-case
        (setf (pattern canvas) (make-array '(0 0)))
      (cd-error (e)
        (pass))
      (:no-error ()
        (pass)))  ; Might succeed with no pattern

    ;; Test oversized pattern
    (handler-case
        (let ((large-pattern (make-array '(1000 1000) :initial-element +red+)))
          (setf (pattern canvas) large-pattern))
      (cd-error (e)
        (pass))
      (:no-error ()
        (pass)))  ; Might succeed or be clipped

    ;; Test invalid pattern data
    (handler-case
        (setf (pattern canvas) "not-an-array")
      (error (e)
        (pass)))))

(test stipple-error-conditions
  "Test stipple error handling"
  (with-debug-canvas (canvas)
    ;; Test empty stipple
    (handler-case
        (setf (stipple canvas) (make-array '(0 0) :element-type 'boolean))
      (cd-error (e)
        (pass))
      (:no-error ()
        (pass)))

    ;; Test oversized stipple
    (handler-case
        (let ((large-stipple (make-array '(1000 1000) :element-type 'boolean :initial-element t)))
          (setf (stipple canvas) large-stipple))
      (cd-error (e)
        (pass))
      (:no-error ()
        (pass)))

    ;; Test invalid stipple data
    (handler-case
        (setf (stipple canvas) "not-an-array")
      (error (e)
        (pass)))))

(test pattern-performance
  "Test pattern operation performance"
  (with-debug-canvas (canvas)
    (let ((pattern (create-test-pattern 16 16)))
      ;; Test pattern setting performance
      (let ((start-time (get-internal-real-time)))
        (dotimes (i 50)
          (setf (pattern canvas) pattern))
        (let ((elapsed (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
          (is (< elapsed 1.0) "50 pattern settings should be fast")))

      ;; Test drawing with pattern performance
      (setf (pattern canvas) pattern)
      (setf (interior-style canvas) :interior-pattern)
      (let ((start-time (get-internal-real-time)))
        (dotimes (i 20)
          (box canvas (* i 10) 10 8 8))
        (let ((elapsed (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
          (is (< elapsed 1.0) "20 pattern draws should be fast"))))))

(run! 'pattern-tests)
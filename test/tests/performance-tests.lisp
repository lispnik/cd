(in-package #:cd-tests)

;;; Performance Tests

(in-suite performance-tests)

(test drawing-primitives-performance
  "Test performance of basic drawing primitives"
  (with-debug-canvas (canvas)
    (activate canvas)

    ;; Test line drawing performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 1000)
        (line canvas 10 10 50 50))
      (let ((line-time (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
        (format t "~&1000 lines: ~F seconds (~F lines/sec)~%"
                line-time (/ 1000 line-time))
        (is (< line-time 2.0) "Line drawing should be reasonably fast")))

    ;; Test rectangle performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 500)
        (rect canvas (* i 0.1) 60 10 10))
      (let ((rect-time (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
        (format t "~&500 rectangles: ~F seconds (~F rects/sec)~%"
                rect-time (/ 500 rect-time))))

    ;; Test filled box performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 300)
        (box canvas (* i 0.2) 80 8 8))
      (let ((box-time (/ (- (get-internal-real-time) start-time)
                        internal-time-units-per-second)))
        (format t "~&300 filled boxes: ~F seconds (~F boxes/sec)~%"
                box-time (/ 300 box-time))))

    ;; Test arc performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 200)
        (arc canvas 100 (* i 0.5) 10 10 0 180))
      (let ((arc-time (/ (- (get-internal-real-time) start-time)
                        internal-time-units-per-second)))
        (format t "~&200 arcs: ~F seconds (~F arcs/sec)~%"
                arc-time (/ 200 arc-time))))

    (deactivate canvas)))

(test pixel-operations-performance
  "Test performance of pixel operations"
  (with-debug-canvas (canvas)
    (activate canvas)

    ;; Test individual pixel performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 1000)
        (pixel canvas (mod i 100) (floor i 100) +red+))
      (let ((pixel-time (/ (- (get-internal-real-time) start-time)
                          internal-time-units-per-second)))
        (format t "~&1000 pixels: ~F seconds (~F pixels/sec)~%"
                pixel-time (/ 1000 pixel-time))))

    ;; Test mark performance
    (setf (mark-type canvas) :mark-circle)
    (setf (mark-size canvas) 5)
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 500)
        (mark canvas (mod (* i 2) 100) (+ 120 (floor i 50))))
      (let ((mark-time (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
        (format t "~&500 marks: ~F seconds (~F marks/sec)~%"
                mark-time (/ 500 mark-time))))

    (deactivate canvas)))

(test text-performance
  "Test text rendering performance"
  (with-debug-canvas (canvas)
    (activate canvas)

    ;; Test text rendering performance
    (let ((test-text "Performance Test Text")
          (start-time (get-internal-real-time)))
      (dotimes (i 200)
        (text canvas 10 (+ 10 (* i 1)) test-text))
      (let ((text-time (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
        (format t "~&200 text strings: ~F seconds (~F text/sec)~%"
                text-time (/ 200 text-time))))

    ;; Test text measurement performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 500)
        (text-size canvas "Measurement Test"))
      (let ((measure-time (/ (- (get-internal-real-time) start-time)
                            internal-time-units-per-second)))
        (format t "~&500 text measurements: ~F seconds (~F measurements/sec)~%"
                measure-time (/ 500 measure-time))))

    (deactivate canvas)))

(test attribute-performance
  "Test attribute setting performance"
  (with-debug-canvas (canvas)
    (activate canvas)

    ;; Test color setting performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 2000)
        (setf (foreground canvas) (encode-color (mod i 256) (mod (* i 2) 256) (mod (* i 3) 256))))
      (let ((color-time (/ (- (get-internal-real-time) start-time)
                          internal-time-units-per-second)))
        (format t "~&2000 color changes: ~F seconds (~F changes/sec)~%"
                color-time (/ 2000 color-time))))

    ;; Test line width performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 1000)
        (setf (line-width canvas) (+ 1 (mod i 10))))
      (let ((width-time (/ (- (get-internal-real-time) start-time)
                          internal-time-units-per-second)))
        (format t "~&1000 line width changes: ~F seconds (~F changes/sec)~%"
                width-time (/ 1000 width-time))))

    ;; Test font setting performance
    (let ((fonts '("Arial,12" "Times,14" "Courier,10")))
      (let ((start-time (get-internal-real-time)))
        (dotimes (i 300)
          (handler-case
              (setf (font canvas) (nth (mod i 3) fonts))
            (cd-error (e)
              (pass))))
        (let ((font-time (/ (- (get-internal-real-time) start-time)
                           internal-time-units-per-second)))
          (format t "~&300 font changes: ~F seconds (~F changes/sec)~%"
                  font-time (/ 300 font-time)))))

    (deactivate canvas)))

(test image-operations-performance
  "Test image operation performance"
  (with-debug-canvas (canvas)
    (activate canvas)

    (let ((width 50) (height 50))
      (multiple-value-bind (r g b) (create-test-rgb-data width height)
        ;; Test image put performance
        (let ((start-time (get-internal-real-time)))
          (dotimes (i 20)
            (put-image-rgb canvas width height r g b (* i 10) 10 0 0 0 0))
          (let ((put-time (/ (- (get-internal-real-time) start-time)
                            internal-time-units-per-second)))
            (format t "~&20 image puts (50x50): ~F seconds (~F puts/sec)~%"
                    put-time (/ 20 put-time))))

        ;; Test image get performance
        (let ((start-time (get-internal-real-time)))
          (dotimes (i 20)
            (get-image-rgb canvas (* i 10) 10 width height))
          (let ((get-time (/ (- (get-internal-real-time) start-time)
                            internal-time-units-per-second)))
            (format t "~&20 image gets (50x50): ~F seconds (~F gets/sec)~%"
                    get-time (/ 20 get-time))))))

    (deactivate canvas)))

(test server-image-performance
  "Test server image performance"
  (with-debug-canvas (canvas)
    (activate canvas)

    (let ((width 40) (height 40))
      (multiple-value-bind (r g b) (create-test-rgb-data width height)
        ;; Test server image creation performance
        (let ((images '())
              (start-time (get-internal-real-time)))
          (dotimes (i 10)
            (push (create-image-rgb canvas width height r g b) images))
          (let ((create-time (/ (- (get-internal-real-time) start-time)
                               internal-time-units-per-second)))
            (format t "~&10 server image creates (40x40): ~F seconds (~F creates/sec)~%"
                    create-time (/ 10 create-time)))

          ;; Test server image put performance
          (let ((start-time (get-internal-real-time)))
            (dolist (image images)
              (dotimes (j 5)
                (put-image-stretch canvas image (* j 20) 100 width height 0 0 0 0)))
            (let ((put-time (/ (- (get-internal-real-time) start-time)
                              internal-time-units-per-second)))
              (format t "~&50 server image puts: ~F seconds (~F puts/sec)~%"
                      put-time (/ 50 put-time))))

          ;; Clean up images
          (dolist (image images)
            (kill-image image)))))

    (deactivate canvas)))

(test transformation-performance
  "Test coordinate transformation performance"
  (with-debug-canvas (canvas)
    ;; Test world coordinate transformations
    (world-set canvas -100.0 100.0 -75.0 75.0)

    (let ((start-time (get-internal-real-time)))
      (dotimes (i 2000)
        (world-to-canvas canvas (- (random 200.0) 100.0) (- (random 150.0) 75.0)))
      (let ((w2c-time (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
        (format t "~&2000 world-to-canvas: ~F seconds (~F transforms/sec)~%"
                w2c-time (/ 2000 w2c-time))))

    (let ((start-time (get-internal-real-time)))
      (dotimes (i 2000)
        (canvas-to-world canvas (random 200) (random 150)))
      (let ((c2w-time (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
        (format t "~&2000 canvas-to-world: ~F seconds (~F transforms/sec)~%"
                c2w-time (/ 2000 c2w-time))))

    ;; Test MM conversions
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 1000)
        (mm-to-pixel canvas (random 100.0) (random 100.0)))
      (let ((mm2px-time (/ (- (get-internal-real-time) start-time)
                          internal-time-units-per-second)))
        (format t "~&1000 mm-to-pixel: ~F seconds (~F conversions/sec)~%"
                mm2px-time (/ 1000 mm2px-time))))))

(test state-management-performance
  "Test state save/restore performance"
  (with-debug-canvas (canvas)
    (activate canvas)

    ;; Test state save/restore performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 100)
        (let ((state (save-state canvas)))
          ;; Change some state
          (setf (foreground canvas) (encode-color (mod i 256) 0 0))
          (setf (line-width canvas) (+ 1 (mod i 5)))
          ;; Restore
          (restore-state canvas state)
          (release-state state)))
      (let ((state-time (/ (- (get-internal-real-time) start-time)
                          internal-time-units-per-second)))
        (format t "~&100 state save/restore cycles: ~F seconds (~F cycles/sec)~%"
                state-time (/ 100 state-time))))

    (deactivate canvas)))

(test color-operations-performance
  "Test color operation performance"
  ;; Test color encoding performance
  (let ((start-time (get-internal-real-time)))
    (dotimes (i 5000)
      (encode-color (mod i 256) (mod (* i 2) 256) (mod (* i 3) 256)))
    (let ((encode-time (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
      (format t "~&5000 color encodings: ~F seconds (~F encodings/sec)~%"
              encode-time (/ 5000 encode-time))))

  ;; Test color decoding performance
  (let ((colors (make-array 1000)))
    (dotimes (i 1000)
      (setf (aref colors i) (encode-color (mod i 256) (mod (* i 2) 256) (mod (* i 3) 256))))

    (let ((start-time (get-internal-real-time)))
      (dotimes (i 1000)
        (decode-color (aref colors i)))
      (let ((decode-time (/ (- (get-internal-real-time) start-time)
                           internal-time-units-per-second)))
        (format t "~&1000 color decodings: ~F seconds (~F decodings/sec)~%"
                decode-time (/ 1000 decode-time)))))

  ;; Test color space conversions
  (let ((start-time (get-internal-real-time)))
    (dotimes (i 1000)
      (rgb-to-hsv (mod i 256) (mod (* i 2) 256) (mod (* i 3) 256)))
    (let ((hsv-time (/ (- (get-internal-real-time) start-time)
                      internal-time-units-per-second)))
      (format t "~&1000 RGB->HSV conversions: ~F seconds (~F conversions/sec)~%"
              hsv-time (/ 1000 hsv-time)))))

(test canvas-control-performance
  "Test canvas control operation performance"
  (with-debug-canvas (canvas)
    ;; Test activation/deactivation performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 100)
        (activate canvas)
        (deactivate canvas))
      (let ((control-time (/ (- (get-internal-real-time) start-time)
                            internal-time-units-per-second)))
        (format t "~&100 activate/deactivate cycles: ~F seconds (~F cycles/sec)~%"
                control-time (/ 100 control-time))))

    ;; Test clear performance
    (activate canvas)
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 50)
        (clear canvas))
      (let ((clear-time (/ (- (get-internal-real-time) start-time)
                          internal-time-units-per-second)))
        (format t "~&50 clear operations: ~F seconds (~F clears/sec)~%"
                clear-time (/ 50 clear-time))))

    ;; Test flush performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 100)
        (flush canvas))
      (let ((flush-time (/ (- (get-internal-real-time) start-time)
                          internal-time-units-per-second)))
        (format t "~&100 flush operations: ~F seconds (~F flushes/sec)~%"
                flush-time (/ 100 flush-time))))

    (deactivate canvas)))

(test comprehensive-performance-suite
  "Comprehensive performance test suite"
  (format t "~&~%=== CD Library Performance Test Results ===~%")

  (with-debug-canvas (canvas)
    (activate canvas)

    ;; Mixed operation performance test
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 100)
        ;; Set attributes
        (setf (foreground canvas) (encode-color (mod i 256) 128 64))
        (setf (line-width canvas) (+ 1 (mod i 3)))

        ;; Draw primitives
        (line canvas (* i 2) 10 (+ (* i 2) 20) 30)
        (rect canvas (* i 2) 40 15 10)
        (mark canvas (+ (* i 2) 10) 60)

        ;; Clear occasionally
        (when (zerop (mod i 20))
          (clear canvas)))

      (let ((mixed-time (/ (- (get-internal-real-time) start-time)
                          internal-time-units-per-second)))
        (format t "~&Mixed operations test: ~F seconds for 100 iterations~%" mixed-time)
        (format t "~&  (~F ops/sec, ~F ms/op)~%"
                (/ 100 mixed-time) (* mixed-time 10))))

    (deactivate canvas))

  (format t "~&~%=== End Performance Tests ===~%"))

(run! 'performance-tests)
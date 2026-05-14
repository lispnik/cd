(in-package #:cd-tests)

;;; Drawing Primitives Tests

(in-suite drawing-tests)

(test primitive-pixel
  "Test pixel drawing"
  (with-svg-canvas (canvas "pixel-test.svg")
    (finishes (pixel canvas 10 10 +red+))
    (finishes (pixel canvas 20 20 +green+))
    (finishes (pixel canvas 30 30 +blue+))))

(test primitive-mark
  "Test mark drawing"
  (with-svg-canvas (canvas "mark-test.svg")
    ;; Test different mark types
    (setf (mark-type canvas) :mark-plus)
    (setf (mark-size canvas) 10)
    (finishes (mark canvas 10 10))

    (setf (mark-type canvas) :mark-star)
    (setf (mark-size canvas) 15)
    (finishes (mark canvas 30 30))

    (setf (mark-type canvas) :mark-circle)
    (setf (mark-size canvas) 8)
    (finishes (mark canvas 50 50))))

(test primitive-line
  "Test line drawing"
  (with-svg-canvas (canvas "line-test.svg")
    ;; Test basic line
    (finishes (line canvas 0 0 50 50))

    ;; Test line with different styles
    (setf (line-style canvas) :line-dashed)
    (setf (line-width canvas) 3)
    (finishes (line canvas 60 10 110 60))

    ;; Test line with different caps
    (setf (line-cap canvas) :cap-round)
    (finishes (line canvas 120 10 170 60))))

(test primitive-rectangle
  "Test rectangle drawing"
  (with-svg-canvas (canvas "rectangle-test.svg")
    ;; Test basic rectangle
    (finishes (rect canvas 10 10 40 30))

    ;; Test with different line styles
    (setf (line-style canvas) :line-dashed)
    (finishes (rect canvas 60 10 40 30))

    ;; Test filled rectangle (box)
    (setf (foreground canvas) +blue+)
    (finishes (box canvas 110 10 40 30))))

(test primitive-arc
  "Test arc drawing"
  (with-svg-canvas (canvas "arc-test.svg")
    ;; Test basic arc
    (finishes (arc canvas 50 50 40 40 0 180))

    ;; Test full circle
    (finishes (arc canvas 150 50 30 30 0 360))

    ;; Test quarter circle
    (finishes (arc canvas 50 150 40 40 0 90))))

(test primitive-sector
  "Test sector (pie slice) drawing"
  (with-svg-canvas (canvas "sector-test.svg")
    ;; Test basic sector
    (setf (foreground canvas) +red+)
    (finishes (sector canvas 50 50 40 40 0 90))

    ;; Test different sectors
    (setf (foreground canvas) +green+)
    (finishes (sector canvas 150 50 30 30 90 180))

    (setf (foreground canvas) +blue+)
    (finishes (sector canvas 50 150 40 40 180 270))))

(test primitive-chord
  "Test chord drawing"
  (with-svg-canvas (canvas "chord-test.svg")
    ;; Test basic chord
    (setf (foreground canvas) +red+)
    (finishes (chord canvas 50 50 40 40 30 150))

    ;; Test different chord
    (setf (foreground canvas) +green+)
    (finishes (chord canvas 150 50 30 30 45 225))))

(test primitive-polygon
  "Test polygon drawing"
  (with-svg-canvas (canvas "polygon-test.svg")
    ;; Test triangle
    (let ((points #(10 10 50 10 30 40)))
      (finishes (polygon canvas :line points)))

    ;; Test filled triangle
    (let ((points #(70 10 110 10 90 40)))
      (setf (foreground canvas) +blue+)
      (finishes (polygon canvas :fill points)))

    ;; Test pentagon
    (let ((points #(150 10 170 20 165 40 135 40 130 20)))
      (setf (foreground canvas) +green+)
      (finishes (polygon canvas :fill-stroke points)))))

(test primitive-polyline
  "Test polyline drawing"
  (with-svg-canvas (canvas "polyline-test.svg")
    ;; Test basic polyline
    (let ((points #(10 10 30 40 50 20 70 50)))
      (finishes (polyline canvas points)))

    ;; Test with different line styles
    (setf (line-style canvas) :line-dashed)
    (setf (line-width canvas) 3)
    (let ((points #(90 10 110 40 130 20 150 50)))
      (finishes (polyline canvas points)))))

(test primitive-bezier
  "Test Bezier curve drawing"
  (with-svg-canvas (canvas "bezier-test.svg")
    ;; Test cubic Bezier
    (let ((points #(10 50 30 10 70 10 90 50)))
      (finishes (bezier canvas points)))

    ;; Test with different line styles
    (setf (line-style canvas) :line-dashed)
    (setf (line-width canvas) 2)
    (let ((points #(110 50 130 100 170 100 190 50)))
      (finishes (bezier canvas points)))))

(test drawing-with-attributes
  "Test drawing with various attributes"
  (with-svg-canvas (canvas "attributes-test.svg")
    ;; Test with different foreground colors
    (setf (foreground canvas) +red+)
    (finishes (rect canvas 10 10 20 20))

    (setf (foreground canvas) +green+)
    (finishes (rect canvas 40 10 20 20))

    (setf (foreground canvas) +blue+)
    (finishes (rect canvas 70 10 20 20))

    ;; Test with different line widths
    (setf (foreground canvas) +black+)
    (setf (line-width canvas) 1)
    (finishes (line canvas 10 40 30 60))

    (setf (line-width canvas) 3)
    (finishes (line canvas 40 40 60 60))

    (setf (line-width canvas) 5)
    (finishes (line canvas 70 40 90 60))))

(test drawing-coordinate-validation
  "Test drawing with edge case coordinates"
  (with-debug-canvas (canvas)
    (multiple-value-bind (width height) (test-canvas-size canvas)
      ;; Test coordinates at canvas boundaries
      (finishes (pixel canvas 0 0 +red+))
      (finishes (pixel canvas (1- width) (1- height) +green+))

      ;; Test negative coordinates (should be handled gracefully)
      (finishes (line canvas -10 -10 10 10))

      ;; Test coordinates beyond canvas (should be clipped)
      (finishes (line canvas 0 0 (+ width 100) (+ height 100))))))

(test drawing-performance-basic
  "Basic drawing performance test"
  (with-debug-canvas (canvas)
    ;; Test drawing many primitives
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 100)
        (pixel canvas (mod i 50) (floor i 50) +red+))
      (let ((end-time (get-internal-real-time)))
        (let ((elapsed (/ (- end-time start-time) internal-time-units-per-second)))
          (is (< elapsed 1.0) "Drawing 100 pixels should complete quickly"))))))

(test drawing-with-clipping
  "Test drawing with clipping regions"
  (with-svg-canvas (canvas "clipping-test.svg")
    ;; Set a clipping rectangle
    (finishes (clip canvas 20 20 60 40))

    ;; Draw something that extends beyond clip
    (setf (foreground canvas) +red+)
    (finishes (rect canvas 10 10 80 60))

    ;; Reset clipping
    (finishes (clip-off canvas))))

(run! 'drawing-tests)
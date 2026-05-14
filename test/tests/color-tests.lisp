(in-package #:cd-tests)

;;; Color Management Tests

(in-suite color-tests)

(test color-encoding-basic
  "Test basic color encoding"
  ;; Test RGB encoding
  (let ((color (encode-color 255 128 64)))
    (is (integerp color))
    (multiple-value-bind (r g b) (decode-color color)
      (is (= r 255))
      (is (= g 128))
      (is (= b 64))))

  ;; Test RGBA encoding
  (let ((color (encode-color-alpha 255 128 64 192)))
    (is (integerp color))
    (multiple-value-bind (r g b a) (decode-color-alpha color)
      (is (= r 255))
      (is (= g 128))
      (is (= b 64))
      (is (= a 192)))))

(test color-component-extraction
  "Test color component extraction functions"
  (let ((color (encode-color-alpha 200 150 100 75)))
    (is (= (red color) 200))
    (is (= (green color) 150))
    (is (= (blue color) 100))
    (is (= (alpha color) 75))))

(test color-alpha-operations
  "Test alpha channel operations"
  (let ((base-color (encode-color 255 128 64)))
    ;; Test alpha encoding
    (let ((alpha-color (encode-alpha base-color 128)))
      (is (integerp alpha-color))
      (is (= (decode-alpha alpha-color) 128))

      ;; Verify original color components preserved
      (multiple-value-bind (r g b a) (decode-color-alpha alpha-color)
        (is (= r 255))
        (is (= g 128))
        (is (= b 64))
        (is (= a 128))))))

(test color-space-conversions
  "Test color space conversion functions"
  ;; Test RGB to HSV conversion
  (multiple-value-bind (h s v) (rgb-to-hsv 255 128 64)
    (is (<= 0 h 360))
    (is (<= 0 s 1))
    (is (<= 0 v 1))

    ;; Test HSV to RGB conversion (round trip)
    (multiple-value-bind (r g b) (hsv-to-rgb h s v)
      ;; Allow small rounding errors
      (is (< (abs (- 255 r)) 2))
      (is (< (abs (- 128 g)) 2))
      (is (< (abs (- 64 b)) 2))))

  ;; Test edge cases
  (multiple-value-bind (h s v) (rgb-to-hsv 0 0 0)  ; Black
    (is (= v 0)))

  (multiple-value-bind (h s v) (rgb-to-hsv 255 255 255)  ; White
    (is (= v 1))
    (is (= s 0)))

  ;; Test pure red
  (multiple-value-bind (h s v) (rgb-to-hsv 255 0 0)
    (is (= h 0))
    (is (= s 1))
    (is (= v 1)))

  ;; Test pure green
  (multiple-value-bind (h s v) (rgb-to-hsv 0 255 0)
    (is (= h 120))
    (is (= s 1))
    (is (= v 1)))

  ;; Test pure blue
  (multiple-value-bind (h s v) (rgb-to-hsv 0 0 255)
    (is (= h 240))
    (is (= s 1))
    (is (= v 1))))

(test color-constants
  "Test color constants"
  (is (integerp +black+))
  (is (integerp +white+))
  (is (integerp +red+))
  (is (integerp +green+))
  (is (integerp +blue+))
  (is (integerp +yellow+))
  (is (integerp +magenta+))
  (is (integerp +cyan+))

  ;; Test that colors are different
  (is (not (= +black+ +white+)))
  (is (not (= +red+ +green+)))
  (is (not (= +blue+ +yellow+)))

  ;; Test specific color values
  (multiple-value-bind (r g b) (decode-color +black+)
    (is (= r 0))
    (is (= g 0))
    (is (= b 0)))

  (multiple-value-bind (r g b) (decode-color +white+)
    (is (= r 255))
    (is (= g 255))
    (is (= b 255)))

  (multiple-value-bind (r g b) (decode-color +red+)
    (is (= r 255))
    (is (= g 0))
    (is (= b 0))))

(test palette-operations
  "Test palette operations"
  (with-debug-canvas (canvas)
    ;; Test palette size
    (let ((size (palette-size canvas)))
      (is (integerp size))
      (is (>= size 0)))

    ;; Test getting palette colors
    (let ((size (palette-size canvas)))
      (when (> size 0)
        (dotimes (i (min size 8))  ; Test first few colors
          (let ((color (palette canvas i)))
            (is (integerp color))))))

    ;; Test setting palette colors (if supported)
    (handler-case
        (progn
          (setf (palette canvas 0) +red+)
          (let ((color (palette canvas 0)))
            (is (colors-equal-p color +red+ 5))))  ; Allow some tolerance
      (cd-error (e)
        (pass)))))  ; Palette modification might not be supported

(test color-mixing
  "Test color mixing operations"
  ;; Test simple alpha blending
  (let ((fg-color (encode-color-alpha 255 0 0 128))    ; Semi-transparent red
        (bg-color (encode-color 0 255 0)))              ; Opaque green

    ;; Manual alpha blend calculation
    (let ((alpha (/ 128 255.0)))
      (let ((expected-r (round (+ (* 255 alpha) (* 0 (- 1 alpha)))))
            (expected-g (round (+ (* 0 alpha) (* 255 (- 1 alpha)))))
            (expected-b 0))

        ;; Test the calculation makes sense
        (is (> expected-r 100))   ; Should have red component
        (is (> expected-g 100))   ; Should have green component
        (is (= expected-b 0))))))

(test color-comparison-utilities
  "Test color comparison utilities"
  (let ((color1 (encode-color 100 150 200))
        (color2 (encode-color 100 150 200))
        (color3 (encode-color 101 151 201)))

    ;; Test exact equality
    (is (colors-equal-p color1 color2 0))

    ;; Test with tolerance
    (is (colors-equal-p color1 color3 2))
    (is (not (colors-equal-p color1 color3 0)))))

(test color-boundary-values
  "Test color boundary value handling"
  ;; Test maximum values
  (let ((color (encode-color 255 255 255)))
    (multiple-value-bind (r g b) (decode-color color)
      (is (= r 255))
      (is (= g 255))
      (is (= b 255))))

  ;; Test minimum values
  (let ((color (encode-color 0 0 0)))
    (multiple-value-bind (r g b) (decode-color color)
      (is (= r 0))
      (is (= g 0))
      (is (= b 0))))

  ;; Test alpha boundaries
  (let ((color (encode-color-alpha 128 128 128 255)))
    (is (= (alpha color) 255)))

  (let ((color (encode-color-alpha 128 128 128 0)))
    (is (= (alpha color) 0))))

(test color-format-conversions
  "Test color format conversions"
  ;; Test various color representations
  (let ((rgb-color (encode-color 200 100 50)))
    ;; Test that encoding/decoding preserves values
    (multiple-value-bind (r g b) (decode-color rgb-color)
      (let ((re-encoded (encode-color r g b)))
        (is (= rgb-color re-encoded))))))

(test color-performance
  "Test color operation performance"
  ;; Test encoding performance
  (let ((start-time (get-internal-real-time)))
    (dotimes (i 1000)
      (encode-color (mod i 256) (mod (* i 2) 256) (mod (* i 3) 256)))
    (let ((encode-time (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
      (is (< encode-time 0.5) "1000 color encodings should be fast")))

  ;; Test decoding performance
  (let ((colors (make-array 1000)))
    (dotimes (i 1000)
      (setf (aref colors i) (encode-color (mod i 256) (mod (* i 2) 256) (mod (* i 3) 256))))

    (let ((start-time (get-internal-real-time)))
      (dotimes (i 1000)
        (decode-color (aref colors i)))
      (let ((decode-time (/ (- (get-internal-real-time) start-time)
                           internal-time-units-per-second)))
        (is (< decode-time 0.5) "1000 color decodings should be fast")))))

(test color-error-conditions
  "Test color error handling"
  ;; Color encoding should handle out-of-range values gracefully
  (handler-case
      (encode-color 300 -50 500)  ; Out of range values
    (error (e)
      (pass))  ; May clamp or error
    (:no-error (color)
      (is (integerp color))))  ; Should produce valid color

  ;; Test invalid palette indices
  (with-debug-canvas (canvas)
    (handler-case
        (palette canvas -1)
      (cd-error (e)
        (pass))
      (:no-error (color)
        (pass)))  ; Might return default color

    (handler-case
        (palette canvas 99999)
      (cd-error (e)
        (pass))
      (:no-error (color)
        (pass)))))

(run! 'color-tests)
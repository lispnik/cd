(in-package #:cd-tests)

;;; Basic CD Library Tests

(in-suite basic-tests)

(test version-info
  "Test version information functions"
  (finishes (version))
  (finishes (version-date))
  (finishes (version-number))
  (is (stringp (version)))
  (is (stringp (version-date)))
  (is (integerp (version-number))))

(test constants
  "Test that basic constants are defined"
  (is (integerp +black+))
  (is (integerp +white+))
  (is (integerp +red+))
  (is (integerp +green+))
  (is (integerp +blue+))
  (is (integerp +query+)))

(test color-encoding-decoding
  "Test color encoding and decoding functions"
  (let ((red 255) (green 128) (blue 64) (alpha 192))
    ;; Test basic RGB encoding/decoding
    (let ((color (encode-color red green blue)))
      (multiple-value-bind (r g b) (decode-color color)
        (is (= r red))
        (is (= g green))
        (is (= b blue))))

    ;; Test RGBA encoding/decoding
    (let ((color (encode-color-alpha red green blue alpha)))
      (multiple-value-bind (r g b a) (decode-color-alpha color)
        (is (= r red))
        (is (= g green))
        (is (= b blue))
        (is (= a alpha))))

    ;; Test alpha encoding
    (let* ((base-color (encode-color red green blue))
           (alpha-color (encode-alpha base-color alpha)))
      (is (= (decode-alpha alpha-color) alpha)))

    ;; Test component extraction
    (let ((color (encode-color-alpha red green blue alpha)))
      (is (= (red color) red))
      (is (= (green color) green))
      (is (= (blue color) blue))
      (is (= (alpha color) alpha)))))

(test color-space-conversion
  "Test color space conversion functions"
  ;; Test RGB to HSV and back
  (let ((r 255) (g 128) (b 64))
    (multiple-value-bind (h s v) (rgb-to-hsv r g b)
      (is (<= 0 h 360))
      (is (<= 0 s 1))
      (is (<= 0 v 1))

      (multiple-value-bind (r2 g2 b2) (hsv-to-rgb h s v)
        ;; Allow small rounding errors
        (is (< (abs (- r r2)) 2))
        (is (< (abs (- g g2)) 2))
        (is (< (abs (- b b2)) 2)))))

  ;; Test edge cases
  (multiple-value-bind (h s v) (rgb-to-hsv 0 0 0)  ; Black
    (is (= v 0)))

  (multiple-value-bind (h s v) (rgb-to-hsv 255 255 255)  ; White
    (is (= v 1))
    (is (= s 0))))

(test context-functions
  "Test context creation and introspection"
  (let ((contexts (test-contexts)))
    (is (> (length contexts) 0) "At least one context should be available")

    (dolist (context contexts)
      (is (not (cffi:null-pointer-p context)))

      ;; Test context capabilities
      (let ((caps (context-capabilities context)))
        (is (listp caps)))

      ;; Test context type
      (let ((type (context-type context)))
        (is (member type '(:window :device :image :file))))

      ;; Test context plus detection
      (is (or (context-plus-p context)
              (not (context-plus-p context)))))))

(test canvas-creation-destruction
  "Test basic canvas creation and destruction"
  (dolist (context (test-contexts))
    (let ((canvas (create-canvas context)))
      (is (not (cffi:null-pointer-p canvas)))

      ;; Test canvas context retrieval
      (is (cffi:pointer-eq context (context canvas)))

      ;; Clean up
      (finishes (kill canvas)))))

(test canvas-size-operations
  "Test canvas size operations"
  (with-debug-canvas (canvas)
    (multiple-value-bind (width height width-mm height-mm) (size canvas)
      (is (integerp width))
      (is (integerp height))
      (is (numberp width-mm))
      (is (numberp height-mm))
      (is (> width 0))
      (is (> height 0)))))

(test coordinate-conversion
  "Test coordinate conversion functions"
  (with-debug-canvas (canvas)
    ;; Test Y-axis inversion
    (let ((y 100))
      (let ((inverted-y (invert-y-axis canvas y)))
        (is (integerp inverted-y))
        ;; Inverting twice should give original value
        (is (= y (invert-y-axis canvas inverted-y)))))

    ;; Test MM to pixel conversion
    (multiple-value-bind (dx dy) (mm-to-pixel canvas 10.0 5.0)
      (is (integerp dx))
      (is (integerp dy)))

    ;; Test pixel to MM conversion
    (multiple-value-bind (mm-dx mm-dy) (pixel-to-mm canvas 100 50)
      (is (numberp mm-dx))
      (is (numberp mm-dy)))))

(test basic-attributes
  "Test basic attribute setting and getting"
  (with-debug-canvas (canvas)
    ;; Test foreground color
    (setf (foreground canvas) +red+)
    (is (= (foreground canvas) +red+))

    ;; Test background color
    (setf (background canvas) +blue+)
    (is (= (background canvas) +blue+))

    ;; Test line width
    (setf (line-width canvas) 5)
    (is (= (line-width canvas) 5))

    ;; Test line style
    (setf (line-style canvas) :line-dashed)
    (is (eq (line-style canvas) :line-dashed))))

(test simulation-mode
  "Test simulation mode operations"
  (with-debug-canvas (canvas)
    ;; Test simulation activation
    (let ((previous (simulate canvas '(:line :rect))))
      (is (listp previous)))

    ;; Test deactivation
    (let ((previous (simulate canvas '(:none))))
      (is (listp previous)))))

(test canvas-state-management
  "Test canvas state save/restore"
  (with-debug-canvas (canvas)
    (setf (foreground canvas) +red+)
    (setf (line-width canvas) 10)

    ;; Save state
    (let ((state (save-state canvas)))
      (is (not (cffi:null-pointer-p state)))

      ;; Change some attributes
      (setf (foreground canvas) +blue+)
      (setf (line-width canvas) 1)

      ;; Restore state
      (restore-state canvas state)
      (is (= (foreground canvas) +red+))
      (is (= (line-width canvas) 10))

      ;; Clean up state
      (finishes (release-state state)))))

(test basic-drawing-primitives
  "Test basic drawing primitive functions"
  (with-svg-canvas (canvas "basic-primitives.svg")
    ;; Test pixel
    (finishes (pixel canvas 10 10 +red+))

    ;; Test mark
    (finishes (mark canvas 20 20))

    ;; Test line
    (finishes (line canvas 0 0 50 50))

    ;; Test rectangle
    (finishes (rect canvas 60 90 10 40))

    ;; Test filled box
    (finishes (box canvas 100 130 10 40))

    ;; Test arc
    (finishes (arc canvas 150 25 30 30 0 180))

    ;; Test sector
    (finishes (sector canvas 150 75 30 30 0 180))

    ;; Test chord
    (finishes (chord canvas 150 125 30 30 0 180))))

(test attribute-operations
  "Test attribute setting operations"
  (with-debug-canvas (canvas)
    ;; Test write mode
    (setf (write-mode canvas) :write-xor)
    (is (eq (write-mode canvas) :write-xor))

    ;; Test background opacity
    (setf (background-opacity canvas) :opacity-transparent)
    (is (eq (background-opacity canvas) :opacity-transparent))

    ;; Test interior style
    (setf (interior-style canvas) :interior-solid)
    (is (eq (interior-style canvas) :interior-solid))

    ;; Test hatch style
    (setf (hatch canvas) :hatch-horizontal)
    (is (eq (hatch canvas) :hatch-horizontal))

    ;; Test line join
    (setf (line-join canvas) :join-round)
    (is (eq (line-join canvas) :join-round))

    ;; Test line cap
    (setf (line-cap canvas) :cap-round)
    (is (eq (line-cap canvas) :cap-round))

    ;; Test mark type and size
    (setf (mark-type canvas) :mark-circle)
    (is (eq (mark-type canvas) :mark-circle))
    (setf (mark-size canvas) 20)
    (is (= (mark-size canvas) 20))))

(test canvas-control-operations
  "Test canvas control operations"
  (with-debug-canvas (canvas)
    ;; Test clear
    (finishes (clear canvas))

    ;; Test flush
    (finishes (flush canvas))

    ;; Test activation/deactivation
    (finishes (activate canvas))
    (finishes (deactivate canvas))))

(run! 'basic-tests)
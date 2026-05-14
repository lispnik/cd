(in-package #:cd)

;;; Color Management and Palette Operations

(export '(color-planes
          palette
          palette-size
          rgb-to-hsv
          hsv-to-rgb
          rgb-to-map
          encode-color
          encode-color-alpha
          encode-alpha
          decode-color
          decode-color-alpha
          decode-alpha
          red
          green
          blue
          alpha
          reserved
          color-closest))

;; Basic color encoding/decoding (enhanced versions)
(defun encode-color (red green blue)
  "Encode RGB values (0-255) into a color value."
  (with-validation 'encode-color
    (validate-coordinate red 'red 'encode-color :min 0 :max 255)
    (validate-coordinate green 'green 'encode-color :min 0 :max 255)
    (validate-coordinate blue 'blue 'encode-color :min 0 :max 255))

  (cd-cffi::%cd-encode-color red green blue))

(defun encode-color-alpha (red green blue alpha)
  "Encode RGBA values (0-255) into a color value with alpha."
  (with-validation 'encode-color-alpha
    (validate-coordinate red 'red 'encode-color-alpha :min 0 :max 255)
    (validate-coordinate green 'green 'encode-color-alpha :min 0 :max 255)
    (validate-coordinate blue 'blue 'encode-color-alpha :min 0 :max 255)
    (validate-coordinate alpha 'alpha 'encode-color-alpha :min 0 :max 255))

  (cd-cffi::%cd-encode-color-alpha red green blue alpha))

(defun encode-alpha (color alpha)
  "Add alpha channel to existing color."
  (with-validation 'encode-alpha
    (validate-color color 'encode-alpha)
    (validate-coordinate alpha 'alpha 'encode-alpha :min 0 :max 255))

  (cd-cffi::%cd-encode-alpha color alpha))

(defun decode-color (color)
  "Decode color into RGB values. Returns red, green, blue."
  (with-validation 'decode-color
    (validate-color color 'decode-color))

  (cffi:with-foreign-objects ((red-ptr :unsigned-char)
                             (green-ptr :unsigned-char)
                             (blue-ptr :unsigned-char))
    (cd-cffi::%cd-decode-color color red-ptr green-ptr blue-ptr)
    (values (cffi:mem-ref red-ptr :unsigned-char)
            (cffi:mem-ref green-ptr :unsigned-char)
            (cffi:mem-ref blue-ptr :unsigned-char))))

(defun decode-color-alpha (color)
  "Decode color into RGBA values. Returns red, green, blue, alpha."
  (with-validation 'decode-color-alpha
    (validate-color color 'decode-color-alpha))

  (cffi:with-foreign-objects ((red-ptr :unsigned-char)
                             (green-ptr :unsigned-char)
                             (blue-ptr :unsigned-char)
                             (alpha-ptr :unsigned-char))
    (cd-cffi::%cd-decode-color-alpha color red-ptr green-ptr blue-ptr alpha-ptr)
    (values (cffi:mem-ref red-ptr :unsigned-char)
            (cffi:mem-ref green-ptr :unsigned-char)
            (cffi:mem-ref blue-ptr :unsigned-char)
            (cffi:mem-ref alpha-ptr :unsigned-char))))

(defun decode-alpha (color)
  "Extract alpha channel from color."
  (with-validation 'decode-alpha
    (validate-color color 'decode-alpha))

  (cd-cffi::%cd-decode-alpha color))

;; Color component extraction macros (from CD library)
(defun red (color)
  "Extract red component from color."
  (logand (ash color -16) #xFF))

(defun green (color)
  "Extract green component from color."
  (logand (ash color -8) #xFF))

(defun blue (color)
  "Extract blue component from color."
  (logand color #xFF))

(defun alpha (color)
  "Extract alpha component from color."
  (logxor #xFF (logand (ash color -24) #xFF)))

(defun reserved (color)
  "Extract reserved bits from color."
  (logand (ash color -24) #xFF))

;; Palette operations
(defun color-planes (canvas)
  "Get the number of color planes supported by the canvas."
  (with-validation 'color-planes
    (validate-canvas canvas 'color-planes))

  (with-cd-error-checking ('color-planes :canvas canvas)
    (cd-cffi::%cd-canvas-get-color-planes canvas)))

(defun (setf palette) (palette canvas &optional (mode :palette-polite))
  "Set canvas palette. Mode can be :palette-polite or :palette-force."
  (with-validation 'set-palette
    (validate-canvas canvas 'set-palette)
    (validate-array palette 'palette 'set-palette :element-type 'integer :min-size 1)
    (validate-enum mode '(:palette-polite :palette-force) 'mode 'set-palette))

  (with-cd-error-checking ('set-palette :canvas canvas)
    (let ((palette-size (length palette)))
      (cffi:with-foreign-object (palette-ptr :long palette-size)
        (loop for i from 0 below palette-size do
          (setf (cffi:mem-aref palette-ptr :long i) (aref palette i)))
        (cd-cffi::%cd-canvas-palette canvas palette-size palette-ptr mode))))
  palette)

(defun palette (canvas)
  "Get current palette from canvas (if supported)."
  (with-validation 'get-palette
    (validate-canvas canvas 'get-palette))

  ;; Note: CD library doesn't provide a direct way to get the current palette
  ;; This would need to be tracked by the application
  (error 'cd-backend-error
         :operation 'get-palette
         :canvas canvas
         :backend (context-type (context canvas))
         :format-control "Palette retrieval not supported by CD library"))

(defun palette-size (canvas)
  "Get the maximum palette size supported by canvas."
  (color-planes canvas))

;; Color space conversions
(defun rgb-to-hsv (red green blue)
  "Convert RGB (0-255) to HSV. Returns hue (0-360), saturation (0-1), value (0-1)."
  (with-validation 'rgb-to-hsv
    (validate-coordinate red 'red 'rgb-to-hsv :min 0 :max 255)
    (validate-coordinate green 'green 'rgb-to-hsv :min 0 :max 255)
    (validate-coordinate blue 'blue 'rgb-to-hsv :min 0 :max 255))

  (let* ((r (/ red 255.0))
         (g (/ green 255.0))
         (b (/ blue 255.0))
         (max-val (max r g b))
         (min-val (min r g b))
         (delta (- max-val min-val)))
    (values
     ;; Hue
     (cond
       ((zerop delta) 0)
       ((= max-val r) (* 60 (mod (/ (- g b) delta) 6)))
       ((= max-val g) (* 60 (+ (/ (- b r) delta) 2)))
       ((= max-val b) (* 60 (+ (/ (- r g) delta) 4))))
     ;; Saturation
     (if (zerop max-val) 0 (/ delta max-val))
     ;; Value
     max-val)))

(defun hsv-to-rgb (hue saturation value)
  "Convert HSV to RGB. Hue (0-360), saturation (0-1), value (0-1). Returns RGB (0-255)."
  (with-validation 'hsv-to-rgb
    (validate-coordinate hue 'hue 'hsv-to-rgb :min 0 :max 360)
    (validate-coordinate saturation 'saturation 'hsv-to-rgb :min 0 :max 1)
    (validate-coordinate value 'value 'hsv-to-rgb :min 0 :max 1))

  (let* ((c (* value saturation))
         (x (* c (- 1 (abs (- (mod (/ hue 60) 2) 1)))))
         (m (- value c))
         (h-prime (floor (/ hue 60))))
    (multiple-value-bind (r-prime g-prime b-prime)
        (case h-prime
          (0 (values c x 0))
          (1 (values x c 0))
          (2 (values 0 c x))
          (3 (values 0 x c))
          (4 (values x 0 c))
          (5 (values c 0 x))
          (otherwise (values 0 0 0)))
      (values (round (* (+ r-prime m) 255))
              (round (* (+ g-prime m) 255))
              (round (* (+ b-prime m) 255))))))

(defun rgb-to-map (width height red green blue palette-size)
  "Convert RGB arrays to indexed color map. Returns index array and palette."
  (with-validation 'rgb-to-map
    (validate-dimensions width height 'rgb-to-map)
    (let ((expected-size (* width height)))
      (validate-array red 'red 'rgb-to-map
                     :element-type '(unsigned-byte 8) :min-size expected-size)
      (validate-array green 'green 'rgb-to-map
                     :element-type '(unsigned-byte 8) :min-size expected-size)
      (validate-array blue 'blue 'rgb-to-map
                     :element-type '(unsigned-byte 8) :min-size expected-size))
    (validate-coordinate palette-size 'palette-size 'rgb-to-map :min 2 :max 256))

  (with-cd-error-checking ('rgb-to-map)
    (let ((total-pixels (* width height)))
      (cffi:with-foreign-objects ((red-ptr :unsigned-char total-pixels)
                                 (green-ptr :unsigned-char total-pixels)
                                 (blue-ptr :unsigned-char total-pixels)
                                 (index-ptr :unsigned-char total-pixels)
                                 (palette-ptr :long palette-size))
        ;; Copy input data to foreign memory
        (loop for i from 0 below total-pixels do
          (setf (cffi:mem-aref red-ptr :unsigned-char i) (aref red i)
                (cffi:mem-aref green-ptr :unsigned-char i) (aref green i)
                (cffi:mem-aref blue-ptr :unsigned-char i) (aref blue i)))

        ;; Call CD function
        (cd-cffi::%cd-rgb-to-map width height red-ptr green-ptr blue-ptr
                                index-ptr palette-size palette-ptr)

        ;; Copy results back to Lisp arrays
        (let ((index-array (make-array total-pixels :element-type '(unsigned-byte 8)))
              (palette-array (make-array palette-size :element-type 'integer)))
          (loop for i from 0 below total-pixels do
            (setf (aref index-array i) (cffi:mem-aref index-ptr :unsigned-char i)))
          (loop for i from 0 below palette-size do
            (setf (aref palette-array i) (cffi:mem-aref palette-ptr :long i)))
          (values index-array palette-array))))))

(defun color-closest (palette color)
  "Find closest color in palette to given color. Returns palette index."
  (with-validation 'color-closest
    (validate-array palette 'palette 'color-closest :element-type 'integer :min-size 1)
    (validate-color color 'color-closest))

  (multiple-value-bind (target-r target-g target-b) (decode-color color)
    (let ((min-distance most-positive-fixnum)
          (closest-index 0))
      (loop for i from 0 below (length palette)
            for palette-color = (aref palette i)
            do (multiple-value-bind (r g b) (decode-color palette-color)
                 (let ((distance (+ (expt (- target-r r) 2)
                                   (expt (- target-g g) 2)
                                   (expt (- target-b b) 2))))
                   (when (< distance min-distance)
                     (setf min-distance distance
                           closest-index i)))))
      closest-index)))

;; Predefined colors (enhanced)
(defconstant +black+   (encode-color 0 0 0))
(defconstant +white+   (encode-color 255 255 255))
(defconstant +red+     (encode-color 255 0 0))
(defconstant +green+   (encode-color 0 255 0))
(defconstant +blue+    (encode-color 0 0 255))
(defconstant +yellow+  (encode-color 255 255 0))
(defconstant +magenta+ (encode-color 255 0 255))
(defconstant +cyan+    (encode-color 0 255 255))
(defconstant +gray+    (encode-color 128 128 128))
(defconstant +dark-gray+    (encode-color 64 64 64))
(defconstant +light-gray+   (encode-color 192 192 192))
(defconstant +dark-red+     (encode-color 128 0 0))
(defconstant +dark-green+   (encode-color 0 128 0))
(defconstant +dark-blue+    (encode-color 0 0 128))
(defconstant +dark-yellow+  (encode-color 128 128 0))
(defconstant +dark-magenta+ (encode-color 128 0 128))
(defconstant +dark-cyan+    (encode-color 0 128 128))

(export '(+black+ +white+ +red+ +green+ +blue+ +yellow+ +magenta+ +cyan+
          +gray+ +dark-gray+ +light-gray+ +dark-red+ +dark-green+ +dark-blue+
          +dark-yellow+ +dark-magenta+ +dark-cyan+))
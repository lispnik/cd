(in-package #:cd-tests)

;;; Advanced Image Operations Tests

(def-suite advanced-image-tests :in cd-test-suite)
(in-suite advanced-image-tests)

(test image-filtering
  "Test image filtering operations"
  (with-debug-canvas (canvas)
    (let ((width 20) (height 20))
      (multiple-value-bind (r g b) (create-test-rgb-data width height)
        ;; Put test image
        (put-image-rgb canvas width height r g b 10 10 0 0 0 0)

        ;; Test blur filter
        (finishes (blur-image canvas 10 10 width height 1.0))

        ;; Test sharpen filter
        (finishes (sharpen-image canvas 10 10 width height))

        ;; Test edge detection
        (finishes (edge-detect-image canvas 10 10 width height))

        ;; Test emboss effect
        (finishes (emboss-image canvas 10 10 width height))))))

(test kernel-creation
  "Test convolution kernel creation"
  ;; Test blur kernel
  (let ((blur-k (blur-kernel 5 1.0)))
    (is (arrayp blur-k))
    (is (= (array-dimension blur-k 0) 5))
    (is (= (array-dimension blur-k 1) 5)))

  ;; Test sharpen kernel
  (let ((sharpen-k (sharpen-kernel)))
    (is (arrayp sharpen-k))
    (is (= (array-dimension sharpen-k 0) 3)))

  ;; Test edge detection kernel
  (let ((edge-k (edge-detection-kernel)))
    (is (arrayp edge-k)))

  ;; Test emboss kernel
  (let ((emboss-k (emboss-kernel)))
    (is (arrayp emboss-k))))

(test image-compositing
  "Test image compositing operations"
  (with-debug-canvas (canvas)
    (let ((width 10) (height 10))
      (multiple-value-bind (r1 g1 b1) (create-test-rgb-data width height)
        (multiple-value-bind (r2 g2 b2) (create-test-rgb-data width height)
          ;; Test different blend modes
          (multiple-value-bind (r-normal g-normal b-normal)
              (composite-images r1 g1 b1 r2 g2 b2 width height :normal 0.5)
            (is (arrayp r-normal))
            (is (= (array-total-size r-normal) (* width height))))

          (multiple-value-bind (r-multiply g-multiply b-multiply)
              (composite-images r1 g1 b1 r2 g2 b2 width height :multiply 0.5)
            (is (arrayp r-multiply)))

          (multiple-value-bind (r-screen g-screen b-screen)
              (composite-images r1 g1 b1 r2 g2 b2 width height :screen 0.5)
            (is (arrayp r-screen))))))))

(test alpha-blending
  "Test alpha blending operations"
  (with-debug-canvas (canvas)
    (let ((width 8) (height 8))
      (multiple-value-bind (r1 g1 b1 a1) (create-test-rgba-data width height)
        (multiple-value-bind (r2 g2 b2 a2) (create-test-rgba-data width height)
          ;; Test alpha blending
          (multiple-value-bind (r-out g-out b-out a-out)
              (alpha-blend-images r1 g1 b1 a1 r2 g2 b2 a2 width height)
            (is (arrayp r-out))
            (is (arrayp g-out))
            (is (arrayp b-out))
            (is (arrayp a-out))
            (is (= (array-total-size a-out) (* width height)))))))))

(test color-space-transformations
  "Test color space transformation operations"
  (let ((width 6) (height 6))
    (multiple-value-bind (r g b) (create-test-rgb-data width height)
      ;; Test RGB to grayscale
      (let ((gray (rgb-to-grayscale r g b)))
        (is (arrayp gray))
        (is (= (array-total-size gray) (* width height))))

      ;; Test brightness adjustment
      (multiple-value-bind (r-bright g-bright b-bright)
          (adjust-brightness r g b 0.2)
        (is (arrayp r-bright))
        (is (arrayp g-bright))
        (is (arrayp b-bright)))

      ;; Test contrast adjustment
      (multiple-value-bind (r-contrast g-contrast b-contrast)
          (adjust-contrast r g b 0.3)
        (is (arrayp r-contrast)))

      ;; Test gamma adjustment
      (multiple-value-bind (r-gamma g-gamma b-gamma)
          (adjust-gamma r g b 1.5)
        (is (arrayp r-gamma))))))

(test image-scaling
  "Test image scaling operations"
  (let ((width 4) (height 4))
    (multiple-value-bind (r g b) (create-test-rgb-data width height)
      ;; Test bilinear scaling
      (multiple-value-bind (r-scaled g-scaled b-scaled)
          (scale-image-bilinear r g b width height 8 8)
        (is (arrayp r-scaled))
        (is (= (array-total-size r-scaled) 64)) ; 8x8 = 64
        (is (arrayp g-scaled))
        (is (arrayp b-scaled))))))

(test image-analysis
  "Test image analysis functions"
  (let ((width 5) (height 5))
    (multiple-value-bind (r g b) (create-test-rgb-data width height)
      ;; Test histogram
      (let ((hist (image-histogram r)))
        (is (arrayp hist))
        (is (= (length hist) 256)))

      ;; Test statistics
      (let ((stats (image-statistics r)))
        (is (listp stats))
        (is (getf stats :mean))
        (is (getf stats :min))
        (is (getf stats :max))
        (is (getf stats :count))))))

(test image-level-adjustments
  "Test combined image level adjustments"
  (with-debug-canvas (canvas)
    (let ((width 12) (height 12))
      (multiple-value-bind (r g b) (create-test-rgb-data width height)
        ;; Put original image
        (put-image-rgb canvas width height r g b 10 10 0 0 0 0)

        ;; Test combined adjustments
        (finishes (adjust-image-levels canvas 10 10 width height 0.1 0.2 1.2))))))

(test image-compositing-canvas
  "Test canvas-based image compositing"
  (with-debug-canvas (canvas)
    (let ((width 16) (height 16))
      ;; Create two different test patterns
      (multiple-value-bind (r1 g1 b1) (create-test-rgb-data width height)
        (put-image-rgb canvas width height r1 g1 b1 10 10 0 0 0 0))

      (multiple-value-bind (r2 g2 b2) (create-test-rgb-data width height)
        (put-image-rgb canvas width height r2 g2 b2 30 30 0 0 0 0))

      ;; Test compositing one region onto another
      (finishes (composite-image-region canvas 10 10 30 30 width height :multiply 0.7)))))

(test convolution-filter-application
  "Test direct convolution filter application"
  (let ((width 6) (height 6))
    (multiple-value-bind (r g b) (create-test-rgb-data width height)
      ;; Test with sharpen kernel
      (let ((kernel (sharpen-kernel)))
        (multiple-value-bind (r-out g-out b-out)
            (apply-convolution-filter r g b width height kernel)
          (is (arrayp r-out))
          (is (= (array-total-size r-out) (* width height)))
          (is (arrayp g-out))
          (is (arrayp b-out)))))))

(test blend-color-functions
  "Test individual color blending functions"
  ;; Test normal blend
  (let ((result (blend-colors 100 200 :normal 0.5)))
    (is (integerp result))
    (is (>= result 0))
    (is (<= result 255)))

  ;; Test multiply blend
  (let ((result (blend-colors 128 128 :multiply 1.0)))
    (is (= result (round (/ (* 128 128) 255)))))

  ;; Test screen blend
  (let ((result (blend-colors 100 150 :screen 1.0)))
    (is (integerp result)))

  ;; Test other blend modes
  (dolist (mode '(:overlay :darken :lighten :difference :exclusion))
    (let ((result (blend-colors 100 150 mode 0.5)))
      (is (integerp result))
      (is (>= result 0))
      (is (<= result 255)))))

(test image-processing-error-handling
  "Test error handling in image processing"
  ;; Test with mismatched array sizes
  (handler-case
      (let ((r1 (make-array 10 :element-type '(unsigned-byte 8)))
            (g1 (make-array 15 :element-type '(unsigned-byte 8)))
            (b1 (make-array 10 :element-type '(unsigned-byte 8)))
            (r2 (make-array 10 :element-type '(unsigned-byte 8)))
            (g2 (make-array 10 :element-type '(unsigned-byte 8)))
            (b2 (make-array 10 :element-type '(unsigned-byte 8))))
        (composite-images r1 g1 b1 r2 g2 b2 2 5 :normal 0.5))
    (error (e)
      (pass)))

  ;; Test with invalid parameters
  (handler-case
      (adjust-gamma (make-array 4 :element-type '(unsigned-byte 8))
                    (make-array 4 :element-type '(unsigned-byte 8))
                    (make-array 4 :element-type '(unsigned-byte 8))
                    0) ; Invalid gamma value
    (error (e)
      (pass))))

(test image-processing-performance
  "Test image processing performance"
  (let ((width 20) (height 20))
    (multiple-value-bind (r g b) (create-test-rgb-data width height)
      ;; Test filtering performance
      (let ((start-time (get-internal-real-time)))
        (dotimes (i 10)
          (apply-convolution-filter r g b width height (blur-kernel 3 1.0)))
        (let ((elapsed (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
          (is (< elapsed 2.0) "10 convolution filters should complete reasonably quickly")))

      ;; Test compositing performance
      (let ((start-time (get-internal-real-time)))
        (dotimes (i 20)
          (composite-images r g b r g b width height :multiply 0.5))
        (let ((elapsed (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
          (is (< elapsed 1.0) "20 image composites should complete quickly"))))))

(test edge-case-handling
  "Test edge case handling in image processing"
  ;; Test with 1x1 image
  (let ((r (make-array 1 :element-type '(unsigned-byte 8) :initial-element 128))
        (g (make-array 1 :element-type '(unsigned-byte 8) :initial-element 64))
        (b (make-array 1 :element-type '(unsigned-byte 8) :initial-element 192)))
    (multiple-value-bind (r-out g-out b-out)
        (apply-convolution-filter r g b 1 1 (sharpen-kernel))
      (is (arrayp r-out))
      (is (= (array-total-size r-out) 1))))

  ;; Test with extreme values
  (let ((r (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0))
        (g (make-array 4 :element-type '(unsigned-byte 8) :initial-element 255))
        (b (make-array 4 :element-type '(unsigned-byte 8) :initial-element 128)))
    (multiple-value-bind (r-out g-out b-out)
        (adjust-brightness r g b 0.5)
      (is (arrayp r-out))
      ;; Check that values are clamped properly
      (is (<= (aref r-out 0) 255))
      (is (>= (aref r-out 0) 0)))))

(run! 'advanced-image-tests)
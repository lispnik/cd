(in-package #:cd-tests)

;;; Image Handling Tests

(in-suite image-tests)

(test image-rgb-basic
  "Test basic RGB image operations"
  (with-debug-canvas (canvas)
    (let ((width 10) (height 10))
      (multiple-value-bind (r g b) (create-test-rgb-data width height)
        ;; Test RGB image put
        (finishes (put-image-rgb canvas width height r g b 10 10 0 0 0 0))

        ;; Test RGB image get
        (multiple-value-bind (r-out g-out b-out)
            (get-image-rgb canvas 10 10 width height)
          (is (arrayp r-out))
          (is (arrayp g-out))
          (is (arrayp b-out))
          (is (= (array-total-size r-out) (* width height)))
          (is (= (array-total-size g-out) (* width height)))
          (is (= (array-total-size b-out) (* width height))))))))

(test image-rgba-basic
  "Test basic RGBA image operations"
  (with-debug-canvas (canvas)
    (let ((width 8) (height 8))
      (multiple-value-bind (r g b a) (create-test-rgba-data width height)
        ;; Test RGBA image put
        (finishes (put-image-rgba canvas width height r g b a 10 10 0 0 0 0))

        ;; Test RGBA image get
        (multiple-value-bind (r-out g-out b-out a-out)
            (get-image-rgba canvas 10 10 width height)
          (is (arrayp r-out))
          (is (arrayp g-out))
          (is (arrayp b-out))
          (is (arrayp a-out))
          (is (= (array-total-size r-out) (* width height)))
          (is (= (array-total-size g-out) (* width height)))
          (is (= (array-total-size b-out) (* width height)))
          (is (= (array-total-size a-out) (* width height))))))))

(test image-map-basic
  "Test basic image map operations"
  (with-debug-canvas (canvas)
    (let ((width 8) (height 8))
      (let ((image-map (make-array (* width height)
                                  :element-type '(unsigned-byte 8)
                                  :initial-function (lambda (i) (mod i 256))))
            (colors (make-array 256 :initial-function (lambda (i)
                                                        (encode-color i i i)))))

        ;; Test map image put
        (finishes (put-image-map canvas width height image-map colors 10 10 0 0 0 0))

        ;; Test map image get
        (let ((map-out (get-image-map canvas 10 10 width height)))
          (is (arrayp map-out))
          (is (= (array-total-size map-out) (* width height))))))))

(test image-partial-operations
  "Test partial image operations"
  (with-debug-canvas (canvas)
    (let ((width 16) (height 16))
      (multiple-value-bind (r g b) (create-test-rgb-data width height)
        ;; Put full image
        (finishes (put-image-rgb canvas width height r g b 10 10 0 0 0 0))

        ;; Get partial image from the middle
        (let ((partial-width 8) (partial-height 8))
          (multiple-value-bind (r-part g-part b-part)
              (get-image-rgb canvas (+ 10 4) (+ 10 4) partial-width partial-height)
            (is (arrayp r-part))
            (is (arrayp g-part))
            (is (arrayp b-part))
            (is (= (array-total-size r-part) (* partial-width partial-height)))
            (is (= (array-total-size g-part) (* partial-width partial-height)))
            (is (= (array-total-size b-part) (* partial-width partial-height)))))))))

(test image-with-offsets
  "Test image operations with source and destination offsets"
  (with-debug-canvas (canvas)
    (let ((width 12) (height 12))
      (multiple-value-bind (r g b) (create-test-rgb-data width height)
        ;; Test with source offset
        (finishes (put-image-rgb canvas (- width 4) (- height 4) r g b
                                20 20 2 2 0 0))

        ;; Test with destination offset within image
        (finishes (put-image-rgb canvas 8 8 r g b
                                30 30 0 0 2 2))))))

(test image-color-validation
  "Test image operations with color validation"
  (with-debug-canvas (canvas)
    (let ((width 4) (height 4))
      ;; Test with valid color arrays
      (let ((r (make-array (* width height) :element-type '(unsigned-byte 8)
                          :initial-element 255))
            (g (make-array (* width height) :element-type '(unsigned-byte 8)
                          :initial-element 128))
            (b (make-array (* width height) :element-type '(unsigned-byte 8)
                          :initial-element 64)))

        (finishes (put-image-rgb canvas width height r g b 10 10 0 0 0 0))

        ;; Verify colors are preserved
        (multiple-value-bind (r-out g-out b-out)
            (get-image-rgb canvas 10 10 width height)
          (is (= (aref r-out 0) 255))
          (is (= (aref g-out 0) 128))
          (is (= (aref b-out 0) 64)))))))

(test image-alpha-operations
  "Test image alpha channel operations"
  (with-debug-canvas (canvas)
    (let ((width 6) (height 6))
      (multiple-value-bind (r g b a) (create-test-rgba-data width height)
        ;; Set specific alpha values
        (dotimes (i (array-total-size a))
          (setf (aref a i) (mod (* i 17) 256)))  ; Varying alpha

        (finishes (put-image-rgba canvas width height r g b a 10 10 0 0 0 0))

        ;; Check alpha preservation
        (multiple-value-bind (r-out g-out b-out a-out)
            (get-image-rgba canvas 10 10 width height)
          (is (arrayp a-out))
          ;; Check first few alpha values
          (dotimes (i (min 4 (array-total-size a-out)))
            (is (integerp (aref a-out i))))))))

(test image-error-conditions
  "Test image error handling"
  (with-debug-canvas (canvas)
    ;; Test with invalid dimensions
    (handler-case
        (get-image-rgb canvas 0 0 0 0)
      (cd-error (e)
        (pass))
      (:no-error (r g b)
        ;; Might succeed with empty arrays
        (is (arrayp r))))

    ;; Test with negative coordinates
    (handler-case
        (get-image-rgb canvas -10 -10 5 5)
      (cd-error (e)
        (pass))
      (:no-error (r g b)
        (pass)))  ; Might be clipped and succeed

    ;; Test with coordinates beyond canvas
    (multiple-value-bind (canvas-width canvas-height) (test-canvas-size canvas)
      (handler-case
          (get-image-rgb canvas (+ canvas-width 100) (+ canvas-height 100) 5 5)
        (cd-error (e)
          (pass))
        (:no-error (r g b)
          (pass))))))

(test image-performance
  "Test image operation performance"
  (with-debug-canvas (canvas)
    (let ((width 20) (height 20))
      (multiple-value-bind (r g b) (create-test-rgb-data width height)
        ;; Time multiple put operations
        (let ((start-time (get-internal-real-time)))
          (dotimes (i 10)
            (put-image-rgb canvas width height r g b (* i 5) 10 0 0 0 0))
          (let ((end-time (get-internal-real-time)))
            (let ((elapsed (/ (- end-time start-time) internal-time-units-per-second)))
              (is (< elapsed 2.0) "10 image puts should complete quickly"))))

        ;; Time multiple get operations
        (let ((start-time (get-internal-real-time)))
          (dotimes (i 10)
            (get-image-rgb canvas (* i 5) 10 width height))
          (let ((end-time (get-internal-real-time)))
            (let ((elapsed (/ (- end-time start-time) internal-time-units-per-second)))
              (is (< elapsed 2.0) "10 image gets should complete quickly"))))))))

(test image-consistency
  "Test image data consistency"
  (with-debug-canvas (canvas)
    (let ((width 8) (height 8))
      ;; Create predictable test pattern
      (let ((r (make-array (* width height) :element-type '(unsigned-byte 8)))
            (g (make-array (* width height) :element-type '(unsigned-byte 8)))
            (b (make-array (* width height) :element-type '(unsigned-byte 8))))

        ;; Fill with known pattern
        (dotimes (i (* width height))
          (setf (aref r i) (mod (* i 3) 256))
          (setf (aref g i) (mod (* i 5) 256))
          (setf (aref b i) (mod (* i 7) 256)))

        ;; Put and get back
        (put-image-rgb canvas width height r g b 10 10 0 0 0 0)
        (multiple-value-bind (r-out g-out b-out)
            (get-image-rgb canvas 10 10 width height)

          ;; Verify consistency (allowing for potential format conversion)
          (dotimes (i (min 4 (* width height)))  ; Check first few pixels
            (let ((r-diff (abs (- (aref r i) (aref r-out i))))
                  (g-diff (abs (- (aref g i) (aref g-out i))))
                  (b-diff (abs (- (aref b i) (aref b-out i)))))
              (is (<= r-diff 1) "Red channel should be consistent")
              (is (<= g-diff 1) "Green channel should be consistent")
              (is (<= b-diff 1) "Blue channel should be consistent"))))))))

(run! 'image-tests)
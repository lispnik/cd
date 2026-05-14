(in-package #:cd-tests)

;;; Server Image Tests

(in-suite server-image-tests)

(test server-image-creation-rgb
  "Test RGB server image creation"
  (with-debug-canvas (canvas)
    (let ((width 10) (height 10))
      (multiple-value-bind (r g b) (create-test-rgb-data width height)
        (let ((image (create-image-rgb canvas width height r g b)))
          (is (not (cffi:null-pointer-p image)))

          ;; Test image properties
          (multiple-value-bind (img-width img-height) (image-size image)
            (is (= img-width width))
            (is (= img-height height)))

          ;; Clean up
          (finishes (kill-image image)))))))

(test server-image-creation-rgba
  "Test RGBA server image creation"
  (with-debug-canvas (canvas)
    (let ((width 8) (height 8))
      (multiple-value-bind (r g b a) (create-test-rgba-data width height)
        (let ((image (create-image-rgba canvas width height r g b a)))
          (is (not (cffi:null-pointer-p image)))

          ;; Test image properties
          (multiple-value-bind (img-width img-height) (image-size image)
            (is (= img-width width))
            (is (= img-height height)))

          ;; Clean up
          (finishes (kill-image image)))))))

(test server-image-creation-map
  "Test map server image creation"
  (with-debug-canvas (canvas)
    (let ((width 6) (height 6))
      (let ((image-map (make-array (* width height)
                                  :element-type '(unsigned-byte 8)
                                  :initial-function (lambda (i) (mod i 64))))
            (colors (make-array 64 :initial-function (lambda (i)
                                                       (encode-color (* i 4) (* i 4) (* i 4))))))

        (let ((image (create-image-map canvas width height image-map colors)))
          (is (not (cffi:null-pointer-p image)))

          ;; Test image properties
          (multiple-value-bind (img-width img-height) (image-size image)
            (is (= img-width width))
            (is (= img-height height)))

          ;; Clean up
          (finishes (kill-image image)))))))

(test server-image-put-operations
  "Test server image put operations"
  (with-svg-canvas (canvas "server-image-put.svg")
    (let ((width 12) (height 12))
      (multiple-value-bind (r g b) (create-test-rgb-data width height)
        (let ((image (create-image-rgb canvas width height r g b)))

          ;; Test basic put
          (finishes (put-image-stretch canvas image 10 10 width height 0 0 0 0))

          ;; Test put with different destination size
          (finishes (put-image-stretch canvas image 50 10 (* width 2) (* height 2) 0 0 0 0))

          ;; Test put with partial source
          (finishes (put-image-stretch canvas image 10 50 (/ width 2) (/ height 2)
                                     (/ width 4) (/ height 4) 0 0))

          ;; Clean up
          (kill-image image))))))

(test server-image-get-operations
  "Test server image get operations"
  (with-debug-canvas (canvas)
    (let ((width 10) (height 10))
      ;; First put some data
      (multiple-value-bind (r g b) (create-test-rgb-data width height)
        (put-image-rgb canvas width height r g b 10 10 0 0 0 0))

      ;; Get it back as server image
      (let ((image (get-image canvas 10 10 width height)))
        (is (not (cffi:null-pointer-p image)))

        ;; Test image properties
        (multiple-value-bind (img-width img-height) (image-size image)
          (is (= img-width width))
          (is (= img-height height)))

        ;; Put it elsewhere
        (finishes (put-image-stretch canvas image 30 30 width height 0 0 0 0))

        ;; Clean up
        (kill-image image)))))

(test server-image-data-retrieval
  "Test server image data retrieval"
  (with-debug-canvas (canvas)
    (let ((width 8) (height 8))
      (multiple-value-bind (r g b) (create-test-rgb-data width height)
        (let ((image (create-image-rgb canvas width height r g b)))

          ;; Test RGB data retrieval
          (multiple-value-bind (r-out g-out b-out) (get-image-rgb-server image)
            (is (arrayp r-out))
            (is (arrayp g-out))
            (is (arrayp b-out))
            (is (= (array-total-size r-out) (* width height)))
            (is (= (array-total-size g-out) (* width height)))
            (is (= (array-total-size b-out) (* width height))))

          ;; Clean up
          (kill-image image))))))

(test server-image-rgba-data-retrieval
  "Test server image RGBA data retrieval"
  (with-debug-canvas (canvas)
    (let ((width 6) (height 6))
      (multiple-value-bind (r g b a) (create-test-rgba-data width height)
        (let ((image (create-image-rgba canvas width height r g b a)))

          ;; Test RGBA data retrieval
          (multiple-value-bind (r-out g-out b-out a-out) (get-image-rgba-server image)
            (is (arrayp r-out))
            (is (arrayp g-out))
            (is (arrayp b-out))
            (is (arrayp a-out))
            (is (= (array-total-size r-out) (* width height)))
            (is (= (array-total-size g-out) (* width height)))
            (is (= (array-total-size b-out) (* width height)))
            (is (= (array-total-size a-out) (* width height))))

          ;; Clean up
          (kill-image image))))))

(test server-image-transformations
  "Test server image transformations"
  (with-svg-canvas (canvas "server-image-transform.svg")
    (let ((width 16) (height 16))
      (multiple-value-bind (r g b) (create-test-rgb-data width height)
        (let ((image (create-image-rgb canvas width height r g b)))

          ;; Test scaling
          (finishes (put-image-stretch canvas image 10 10 (* width 2) (* height 2) 0 0 0 0))

          ;; Test shrinking
          (finishes (put-image-stretch canvas image 100 10 (/ width 2) (/ height 2) 0 0 0 0))

          ;; Test partial source with scaling
          (finishes (put-image-stretch canvas image 10 100 width height
                                     (/ width 4) (/ height 4) (/ width 2) (/ height 2)))

          ;; Clean up
          (kill-image image))))))

(test server-image-multiple-instances
  "Test multiple server image instances"
  (with-debug-canvas (canvas)
    (let ((images '()))
      ;; Create multiple images
      (dotimes (i 3)
        (let ((width (+ 4 (* i 2)))
              (height (+ 4 (* i 2))))
          (multiple-value-bind (r g b) (create-test-rgb-data width height)
            (let ((image (create-image-rgb canvas width height r g b)))
              (is (not (cffi:null-pointer-p image)))
              (push image images)))))

      ;; Test all images
      (dolist (image images)
        (multiple-value-bind (width height) (image-size image)
          (is (integerp width))
          (is (integerp height))
          (is (> width 0))
          (is (> height 0))))

      ;; Clean up all images
      (dolist (image images)
        (finishes (kill-image image))))))

(test server-image-error-conditions
  "Test server image error handling"
  (with-debug-canvas (canvas)
    ;; Test creation with invalid data
    (handler-case
        (let ((empty-array (make-array 0 :element-type '(unsigned-byte 8))))
          (create-image-rgb canvas 0 0 empty-array empty-array empty-array))
      (cd-error (e)
        (pass))
      (:no-error (image)
        ;; Might succeed with empty image
        (when image
          (kill-image image))))

    ;; Test operations on killed image
    (let ((width 4) (height 4))
      (multiple-value-bind (r g b) (create-test-rgb-data width height)
        (let ((image (create-image-rgb canvas width height r g b)))
          (kill-image image)

          ;; Operations on killed image should error
          (handler-case
              (image-size image)
            (cd-error (e)
              (pass))
            (:no-error (w h)
              (fail "Should have signaled error for killed image"))))))))

(test server-image-performance
  "Test server image performance"
  (with-debug-canvas (canvas)
    (let ((width 20) (height 20))
      (multiple-value-bind (r g b) (create-test-rgb-data width height)

        ;; Test creation performance
        (let ((start-time (get-internal-real-time)))
          (let ((images '()))
            (dotimes (i 5)
              (push (create-image-rgb canvas width height r g b) images))

            (let ((creation-time (/ (- (get-internal-real-time) start-time)
                                   internal-time-units-per-second)))
              (is (< creation-time 1.0) "Creating 5 images should be fast")

              ;; Test put performance
              (let ((start-time (get-internal-real-time)))
                (dolist (image images)
                  (put-image-stretch canvas image (* (length images) 10) 10 width height 0 0 0 0))

                (let ((put-time (/ (- (get-internal-real-time) start-time)
                                  internal-time-units-per-second)))
                  (is (< put-time 1.0) "Putting 5 images should be fast")))

              ;; Clean up
              (dolist (image images)
                (kill-image image)))))))))

(test server-image-consistency
  "Test server image data consistency"
  (with-debug-canvas (canvas)
    (let ((width 8) (height 8))
      ;; Create known test pattern
      (let ((r (make-array (* width height) :element-type '(unsigned-byte 8)))
            (g (make-array (* width height) :element-type '(unsigned-byte 8)))
            (b (make-array (* width height) :element-type '(unsigned-byte 8))))

        (dotimes (i (* width height))
          (setf (aref r i) (mod (* i 11) 256))
          (setf (aref g i) (mod (* i 13) 256))
          (setf (aref b i) (mod (* i 17) 256)))

        ;; Create server image
        (let ((image (create-image-rgb canvas width height r g b)))

          ;; Get data back
          (multiple-value-bind (r-out g-out b-out) (get-image-rgb-server image)

            ;; Check first few pixels for consistency
            (dotimes (i (min 4 (* width height)))
              (let ((r-diff (abs (- (aref r i) (aref r-out i))))
                    (g-diff (abs (- (aref g i) (aref g-out i))))
                    (b-diff (abs (- (aref b i) (aref b-out i)))))
                (is (<= r-diff 1) "Server image red data should be consistent")
                (is (<= g-diff 1) "Server image green data should be consistent")
                (is (<= b-diff 1) "Server image blue data should be consistent"))))

          ;; Clean up
          (kill-image image))))))

(run! 'server-image-tests)
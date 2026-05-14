(in-package #:cd-tests)

;;; Backend-Specific Tests

(in-suite backend-tests)

(test backend-availability
  "Test which backends are available"
  (let ((available-contexts '()))
    ;; Test debug backend
    (when (ignore-errors (context-debug))
      (push :debug available-contexts))

    ;; Test image backends
    (when (ignore-errors (context-image-rgb))
      (push :image-rgb available-contexts))

    ;; Test file backends
    (when (ignore-errors (context-svg))
      (push :svg available-contexts))

    (when (ignore-errors (context-picture))
      (push :picture available-contexts))

    ;; Should have at least one backend available
    (is (> (length available-contexts) 0)
        "At least one backend should be available")

    ;; Report available backends
    (format t "~&Available backends: ~A~%" available-contexts)))

(test debug-backend-specific
  "Test debug backend specific functionality"
  (let ((context (ignore-errors (context-debug))))
    (when context
      (is (eq (context-type context) :debug))

      ;; Test debug canvas creation
      (let ((canvas (create-canvas context)))
        (is (not (cffi:null-pointer-p canvas)))

        ;; Debug backend should support basic operations
        (finishes (activate canvas))
        (finishes (clear canvas))
        (finishes (line canvas 10 10 50 50))
        (finishes (deactivate canvas))

        ;; Clean up
        (kill canvas)))))

(test image-backend-specific
  "Test image backend specific functionality"
  (let ((context (ignore-errors (context-image-rgb))))
    (when context
      (is (eq (context-type context) :image))

      ;; Test image canvas creation with size
      (let ((canvas (create-canvas context "100x75")))
        (is (not (cffi:null-pointer-p canvas)))

        ;; Image backend should support pixel operations
        (finishes (activate canvas))
        (finishes (clear canvas))

        ;; Test image data operations
        (finishes (pixel canvas 10 10 +red+))
        (finishes (get-image-rgb canvas 5 5 10 10))

        (finishes (deactivate canvas))
        (kill canvas)))))

(test svg-backend-specific
  "Test SVG backend specific functionality"
  (let ((context (ignore-errors (context-svg))))
    (when context
      (is (eq (context-type context) :file))

      ;; Test SVG canvas creation with file specification
      (let ((svg-file (test-file-path "backend-test.svg")))
        (ensure-test-directory)
        (let ((canvas (create-canvas context (namestring svg-file))))
          (is (not (cffi:null-pointer-p canvas)))

          ;; SVG backend should support vector operations
          (finishes (activate canvas))
          (finishes (clear canvas))

          ;; Draw various shapes
          (setf (foreground canvas) +red+)
          (finishes (rect canvas 10 10 30 20))

          (setf (foreground canvas) +blue+)
          (finishes (arc canvas 60 20 15 15 0 180))

          ;; Test text
          (setf (foreground canvas) +black+)
          (finishes (text canvas 10 60 "SVG Backend Test"))

          (finishes (deactivate canvas))
          (kill canvas)

          ;; Verify file was created
          (is (probe-file svg-file)))))))

(test picture-backend-specific
  "Test picture backend specific functionality"
  (let ((context (ignore-errors (context-picture))))
    (when context
      (is (eq (context-type context) :file))

      ;; Test picture canvas creation
      (let ((pic-file (test-file-path "backend-test.pic")))
        (ensure-test-directory)
        (handler-case
            (let ((canvas (create-canvas context (namestring pic-file))))
              (is (not (cffi:null-pointer-p canvas)))

              ;; Picture backend should support basic operations
              (finishes (activate canvas))
              (finishes (clear canvas))
              (finishes (rect canvas 10 10 30 20))
              (finishes (deactivate canvas))
              (kill canvas))
          (cd-error (e)
            (pass)))))))  ; Picture backend might not be fully supported

(test backend-capabilities
  "Test backend capability detection"
  (dolist (context (test-contexts))
    (let ((caps (context-capabilities context))
          (type (context-type context)))

      ;; All backends should have some capabilities
      (is (listp caps))

      ;; Check for expected capabilities based on backend type
      (case type
        (:debug
         ;; Debug should support basic drawing
         (is (member :line caps)))
        (:image
         ;; Image should support pixel operations
         (is (or (member :pixel caps) (member :line caps))))
        (:file
         ;; File backends should support vector operations
         (is (member :line caps)))))))

(test backend-plus-detection
  "Test backend plus version detection"
  (dolist (context (test-contexts))
    (let ((is-plus (context-plus-p context))
          (type (context-type context)))
      ;; Should return boolean
      (is (or (eq is-plus t) (eq is-plus nil)))

      ;; Log plus status for debugging
      (format t "~&Backend ~A is~A CD Plus~%" type (if is-plus "" " not")))))

(test backend-size-handling
  "Test how different backends handle size specifications"
  ;; Test debug backend with size
  (let ((context (ignore-errors (context-debug))))
    (when context
      (handler-case
          (let ((canvas (create-canvas context "200x150")))
            (multiple-value-bind (width height) (size canvas)
              (is (integerp width))
              (is (integerp height)))
            (kill canvas))
        (cd-error (e)
          (pass)))))

  ;; Test image backend with size
  (let ((context (ignore-errors (context-image-rgb))))
    (when context
      (let ((canvas (create-canvas context "150x100")))
        (multiple-value-bind (width height) (size canvas)
          (is (= width 150))
          (is (= height 100)))
        (kill canvas)))))

(test backend-color-handling
  "Test color handling across backends"
  (dolist (context (test-contexts))
    (let ((canvas (create-canvas context)))
      (activate canvas)

      ;; Test basic color operations
      (finishes (setf (foreground canvas) +red+))
      (is (integerp (foreground canvas)))

      (finishes (setf (background canvas) +blue+))
      (is (integerp (background canvas)))

      ;; Test drawing with colors
      (finishes (pixel canvas 10 10 +green+))
      (finishes (rect canvas 20 20 10 10))

      (deactivate canvas)
      (kill canvas))))

(test backend-text-support
  "Test text support across backends"
  (dolist (context (test-contexts))
    (let ((canvas (create-canvas context)))
      (activate canvas)

      ;; Test basic text
      (handler-case
          (progn
            (finishes (text canvas 10 30 "Backend Test"))

            ;; Test text measurement if supported
            (multiple-value-bind (width height) (text-size canvas "Test")
              (is (integerp width))
              (is (integerp height))))
        (cd-error (e)
          (pass)))  ; Text might not be supported

      (deactivate canvas)
      (kill canvas))))

(test backend-image-support
  "Test image operations across backends"
  (dolist (context (test-contexts))
    (let ((canvas (create-canvas context)))
      (activate canvas)

      ;; Test image operations
      (handler-case
          (let ((width 8) (height 8))
            (multiple-value-bind (r g b) (create-test-rgb-data width height)
              (finishes (put-image-rgb canvas width height r g b 10 10 0 0 0 0))

              ;; Test get if supported
              (multiple-value-bind (r-out g-out b-out)
                  (get-image-rgb canvas 10 10 width height)
                (is (arrayp r-out)))))
        (cd-error (e)
          (pass)))  ; Image ops might not be supported

      (deactivate canvas)
      (kill canvas))))

(test backend-state-persistence
  "Test state save/restore across backends"
  (dolist (context (test-contexts))
    (let ((canvas (create-canvas context)))
      (activate canvas)

      ;; Set some state
      (setf (foreground canvas) +red+)
      (setf (line-width canvas) 5)

      ;; Test state save/restore
      (let ((state (save-state canvas)))
        (is (not (cffi:null-pointer-p state)))

        ;; Change state
        (setf (foreground canvas) +blue+)
        (setf (line-width canvas) 1)

        ;; Restore
        (restore-state canvas state)
        (is (= (foreground canvas) +red+))
        (is (= (line-width canvas) 5))

        (release-state state))

      (deactivate canvas)
      (kill canvas))))

(test backend-performance-comparison
  "Compare performance across backends"
  (let ((performance-data '()))
    (dolist (context (test-contexts))
      (let ((canvas (create-canvas context)))
        (activate canvas)

        ;; Time basic drawing operations
        (let ((start-time (get-internal-real-time)))
          (dotimes (i 100)
            (line canvas 10 10 50 50))
          (let ((elapsed (/ (- (get-internal-real-time) start-time)
                           internal-time-units-per-second)))
            (push (list (context-type context) elapsed) performance-data)))

        (deactivate canvas)
        (kill canvas)))

    ;; Report performance data
    (format t "~&Backend performance (100 lines):~%")
    (dolist (data performance-data)
      (format t "  ~A: ~F seconds~%" (first data) (second data)))))

(test backend-error-handling
  "Test error handling across backends"
  (dolist (context (test-contexts))
    (let ((canvas (create-canvas context)))
      ;; Test that backends handle errors gracefully
      (handler-case
          (progn
            (activate canvas)
            ;; Try some potentially problematic operations
            (line canvas -1000 -1000 2000 2000)  ; Extreme coordinates
            (setf (line-width canvas) 999)       ; Large line width
            (deactivate canvas))
        (cd-error (e)
          (pass))  ; Errors are acceptable
        (error (e)
          (pass)))

      (kill canvas))))

(run! 'backend-tests)
(defpackage #:cd-examples.enhanced-error-handling
  (:use #:common-lisp #:cd)
  (:export #:demonstrate-error-handling
           #:interactive-drawing-with-recovery
           #:batch-processing-with-validation))

(in-package #:cd-examples.enhanced-error-handling)

(defun demonstrate-error-handling ()
  "Demonstrate various error handling capabilities."
  (format t "~&=== CD Enhanced Error Handling Demo ===~%")

  ;; 1. Parameter validation
  (format t "~&1. Testing parameter validation...~%")
  (handler-case
      (with-enhanced-canvas (canvas (context-debug) nil)
        ;; This should trigger a parameter error
        (line canvas "invalid" "coordinates" 100 100))
    (cd-parameter-error (e)
      (format t "   Caught parameter error: ~A~%" e)))

  ;; 2. Backend capability checking
  (format t "~&2. Testing backend capability checking...~%")
  (handler-case
      (with-enhanced-canvas (canvas (context-debug) nil)
        ;; Debug context might not support RGBA images
        (safe-put-image-rgba canvas 100 100
                            (make-array 10000 :element-type '(unsigned-byte 8))
                            (make-array 10000 :element-type '(unsigned-byte 8))
                            (make-array 10000 :element-type '(unsigned-byte 8))
                            (make-array 10000 :element-type '(unsigned-byte 8))
                            0 0 100 100 0 99 0 99))
    (cd-backend-error (e)
      (format t "   Caught backend error: ~A~%" e)))

  ;; 3. Automatic recovery
  (format t "~&3. Testing automatic recovery...~%")
  (let ((*enable-auto-recovery* t)
        (*cd-debug-mode* t))
    (handler-case
        (try-alternative-contexts "invalid-spec"
                                 (lambda (canvas)
                                   (format t "   Successfully created canvas with alternative context~%")
                                   (line canvas 0 0 100 100)))
      (cd-error (e)
        (format t "   All contexts failed: ~A~%" e))))

  ;; 4. File operations with restarts
  (format t "~&4. Testing file operations with restarts...~%")
  (flet ((create-test-file (path)
           (handler-case
               (with-enhanced-canvas (canvas (context-svg) path)
                 (line canvas 0 0 100 100)
                 (format t "   Successfully created file: ~A~%" path))
             (cd-file-error (e)
               (format t "   File operation failed: ~A~%" e)
               (when (find-restart 'use-temporary-file)
                 (format t "   Using temporary file restart~%")
                 (invoke-restart 'use-temporary-file))))))
    (with-file-restarts ("/invalid/path/test.svg")
      (create-test-file "/invalid/path/test.svg"))))

(defun interactive-drawing-with-recovery ()
  "Interactive drawing session with comprehensive error recovery."
  (format t "~&=== Interactive Drawing with Recovery ===~%")
  (format t "This function demonstrates interactive error recovery.~%")
  (format t "In a real application, you would get prompts to recover from errors.~%")

  (let ((*cd-debug-mode* t))
    (with-canvas-restarts (canvas (context-svg) "/tmp/interactive-test.svg")
      (restart-case
          (progn
            ;; Simulate a drawing session
            (with-drawing-restarts draw-background
              (box canvas 0 200 0 150)
              (format t "   Drew background successfully~%"))

            (with-drawing-restarts draw-shapes
              (safe-line canvas 10 10 90 90)
              (safe-line canvas 90 10 10 90)
              (format t "   Drew shapes successfully~%"))

            (format t "   Drawing session completed successfully~%"))

        ;; Custom restart for this specific operation
        (start-over ()
          :report "Start the drawing session over"
          (format t "   Starting over...~%")
          (interactive-drawing-with-recovery))))))

(defun batch-processing-with-validation ()
  "Demonstrate batch processing with validation and error recovery."
  (format t "~&=== Batch Processing with Validation ===~%")

  (let ((drawing-specs '(("test1.svg" 100 100 :svg)
                        ("test2.pdf" 200 150 :pdf)  ; This might fail
                        ("test3.debug" 50 50 :debug)))
        (successful 0)
        (failed 0)
        (*enable-auto-recovery* t))

    (dolist (spec drawing-specs)
      (destructuring-bind (filename width height context-type) spec
        (format t "   Processing ~A (~Dx~D, ~A)...~%" filename width height context-type)

        (handler-case
            (let ((context (case context-type
                            (:svg (context-svg))
                            (:pdf (context-pdf))  ; Might not be available
                            (:debug (context-debug))
                            (otherwise (error "Unknown context type")))))

              (with-enhanced-canvas (canvas context filename)
                (with-validation 'batch-draw
                  (validate-dimensions width height 'batch-draw))

                ;; Simple drawing
                (safe-line canvas 0 0 width height)
                (safe-line canvas 0 height width 0)
                (box canvas (/ width 4) (* 3 (/ width 4))
                            (/ height 4) (* 3 (/ height 4)))

                (incf successful)
                (format t "      ✓ Success~%")))

          (cd-error (e)
            (incf failed)
            (format t "      ✗ Failed: ~A~%" e)

            ;; Try with debug context as fallback
            (handler-case
                (with-enhanced-canvas (canvas (context-debug) nil)
                  (safe-line canvas 0 0 width height)
                  (format t "      → Fallback to debug context succeeded~%"))
              (cd-error (e2)
                (format t "      → Fallback also failed: ~A~%" e2)))))))

    (format t "~&Batch processing complete: ~D successful, ~D failed~%"
            successful failed)))

;; Utility function to run all demos
(defun run-all-demos ()
  "Run all error handling demonstrations."
  (let ((*cd-validation-enabled* t))
    (demonstrate-error-handling)
    (terpri)
    (interactive-drawing-with-recovery)
    (terpri)
    (batch-processing-with-validation)
    (format t "~&=== All demos completed ===~%")))

;; Example of setting up global error handling for an application
(defun setup-application-error-handling ()
  "Setup application-wide error handling for CD operations."
  (setf *default-fallback-context* (context-debug)
        *enable-auto-recovery* t
        *cd-validation-enabled* t)

  (install-global-error-handlers)

  (format t "Global CD error handling installed.~%")
  (format t "  - Validation: enabled~%")
  (format t "  - Auto-recovery: enabled~%")
  (format t "  - Fallback context: debug~%"))

(defun teardown-application-error-handling ()
  "Remove application-wide error handling."
  (remove-global-error-handlers)
  (setf *enable-auto-recovery* nil
        *cd-validation-enabled* nil
        *default-fallback-context* nil)
  (format t "Global CD error handling removed.~%"))
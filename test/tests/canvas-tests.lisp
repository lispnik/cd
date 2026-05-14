(in-package #:cd-tests)

;;; Canvas Management Tests

(in-suite canvas-tests)

(test canvas-creation-basic
  "Test basic canvas creation"
  (dolist (context (test-contexts))
    (let ((canvas (create-canvas context)))
      (is (not (cffi:null-pointer-p canvas)))

      ;; Test canvas context retrieval
      (is (cffi:pointer-eq context (context canvas)))

      ;; Clean up
      (finishes (kill canvas)))))

(test canvas-creation-with-spec
  "Test canvas creation with specifications"
  (let ((context (context-debug)))
    (when context
      ;; Test with size specification
      (let ((canvas (create-canvas context "200x150")))
        (is (not (cffi:null-pointer-p canvas)))
        (finishes (kill canvas))))))

(test canvas-size-properties
  "Test canvas size properties"
  (with-debug-canvas (canvas)
    (multiple-value-bind (width height width-mm height-mm) (size canvas)
      (is (integerp width))
      (is (integerp height))
      (is (numberp width-mm))
      (is (numberp height-mm))
      (is (> width 0))
      (is (> height 0))
      (is (> width-mm 0))
      (is (> height-mm 0)))))

(test canvas-activation-deactivation
  "Test canvas activation and deactivation"
  (with-debug-canvas (canvas)
    (finishes (activate canvas))
    (finishes (deactivate canvas))

    ;; Test multiple activations
    (finishes (activate canvas))
    (finishes (activate canvas))
    (finishes (deactivate canvas))))

(test canvas-clear-flush
  "Test canvas clear and flush operations"
  (with-debug-canvas (canvas)
    (finishes (clear canvas))
    (finishes (flush canvas))

    ;; Test multiple clears
    (finishes (clear canvas))
    (finishes (clear canvas))))

(test canvas-coordinate-system
  "Test canvas coordinate system operations"
  (with-debug-canvas (canvas)
    ;; Test Y-axis inversion
    (let ((y 100))
      (let ((inverted-y (invert-y-axis canvas y)))
        (is (integerp inverted-y))
        ;; Inverting twice should give original value
        (is (= y (invert-y-axis canvas inverted-y)))))

    ;; Test coordinate conversions
    (multiple-value-bind (dx dy) (mm-to-pixel canvas 10.0 5.0)
      (is (integerp dx))
      (is (integerp dy)))

    (multiple-value-bind (mm-dx mm-dy) (pixel-to-mm canvas 100 50)
      (is (numberp mm-dx))
      (is (numberp mm-dy)))))

(test canvas-state-management-basic
  "Test basic canvas state save/restore"
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

(test canvas-state-management-nested
  "Test nested state save/restore"
  (with-debug-canvas (canvas)
    ;; Set initial state
    (setf (foreground canvas) +red+)
    (setf (line-width canvas) 5)

    ;; Save first level
    (let ((state1 (save-state canvas)))
      (setf (foreground canvas) +green+)
      (setf (line-width canvas) 10)

      ;; Save second level
      (let ((state2 (save-state canvas)))
        (setf (foreground canvas) +blue+)
        (setf (line-width canvas) 15)

        ;; Restore second level
        (restore-state canvas state2)
        (is (= (foreground canvas) +green+))
        (is (= (line-width canvas) 10))

        (release-state state2))

      ;; Restore first level
      (restore-state canvas state1)
      (is (= (foreground canvas) +red+))
      (is (= (line-width canvas) 5))

      (release-state state1))))

(test canvas-error-handling
  "Test canvas error handling"
  ;; Test operations on killed canvas
  (let ((context (context-debug)))
    (when context
      (let ((canvas (create-canvas context)))
        (kill canvas)
        ;; Operations on killed canvas should handle gracefully
        (handler-case
            (progn
              (activate canvas)
              (fail "Should have signaled error for killed canvas"))
          (cd-error (e)
            (pass)))))))

(test canvas-capabilities-check
  "Test canvas capabilities checking"
  (with-debug-canvas (canvas)
    (let* ((context (context canvas))
           (caps (context-capabilities context)))
      (is (listp caps))

      ;; Test that we can check for specific capabilities
      (dolist (cap caps)
        (is (keywordp cap))))))

(test canvas-multiple-canvases
  "Test multiple canvas management"
  (let ((context (context-debug)))
    (when context
      (let ((canvases '()))
        ;; Create multiple canvases
        (dotimes (i 3)
          (push (create-canvas context) canvases))

        ;; Verify all were created
        (dolist (canvas canvases)
          (is (not (cffi:null-pointer-p canvas)))
          (is (cffi:pointer-eq context (context canvas))))

        ;; Clean up all canvases
        (dolist (canvas canvases)
          (finishes (kill canvas)))))))

(run! 'canvas-tests)
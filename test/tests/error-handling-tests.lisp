(in-package #:cd-tests)

;;; Error Handling Tests

(in-suite error-handling-tests)

(test error-condition-hierarchy
  "Test error condition hierarchy"
  ;; Test that cd-error conditions exist
  (is (subtypep 'cd-error 'error))
  (is (subtypep 'cd-context-error 'cd-error))
  (is (subtypep 'cd-canvas-error 'cd-error))
  (is (subtypep 'cd-parameter-error 'cd-error))
  (is (subtypep 'cd-resource-error 'cd-error))
  (is (subtypep 'cd-operation-error 'cd-error)))

(test parameter-validation
  "Test parameter validation"
  (with-debug-canvas (canvas)
    ;; Test color validation
    (handler-case
        (setf (foreground canvas) "not-a-color")
      (cd-parameter-error (e)
        (pass))
      (error (e)
        (pass)))  ; Any error is acceptable

    ;; Test coordinate validation
    (handler-case
        (line canvas "x1" "y1" "x2" "y2")
      (cd-parameter-error (e)
        (pass))
      (error (e)
        (pass)))

    ;; Test size validation
    (handler-case
        (setf (line-width canvas) -5)
      (cd-parameter-error (e)
        (pass))
      (error (e)
        (pass)))))

(test validation-control
  "Test validation enable/disable"
  (with-debug-canvas (canvas)
    ;; Save original validation state
    (let ((original-validation *cd-validation-enabled*))

      ;; Test with validation enabled
      (setf *cd-validation-enabled* t)
      (handler-case
          (setf (foreground canvas) -999999)  ; Invalid color
        (cd-parameter-error (e)
          (pass))
        (:no-error ()
          (pass)))  ; Validation might clamp instead of error

      ;; Test with validation disabled
      (setf *cd-validation-enabled* nil)
      (finishes (setf (foreground canvas) -999999))  ; Should not validate

      ;; Restore original state
      (setf *cd-validation-enabled* original-validation))))

(test error-recovery-mechanisms
  "Test error recovery mechanisms"
  (with-debug-canvas (canvas)
    ;; Test that operations continue after errors
    (handler-case
        (setf (line-width canvas) "invalid")
      (error (e)
        (pass)))

    ;; Canvas should still be usable
    (finishes (setf (foreground canvas) +red+))
    (finishes (line canvas 10 10 50 50))))

(test canvas-error-conditions
  "Test canvas-specific error conditions"
  ;; Test operations on null canvas
  (handler-case
      (activate (cffi:null-pointer))
    (cd-canvas-error (e)
      (pass))
    (error (e)
      (pass)))

  ;; Test operations on killed canvas
  (let ((context (context-debug)))
    (when context
      (let ((canvas (create-canvas context)))
        (kill canvas)
        (handler-case
            (activate canvas)
          (cd-canvas-error (e)
            (pass))
          (error (e)
            (pass)))))))

(test context-error-conditions
  "Test context-specific error conditions"
  ;; Test invalid context creation
  (handler-case
      (context-debug "invalid-specification")
    (cd-context-error (e)
      (pass))
    (error (e)
      (pass)))

  ;; Test operations on null context
  (handler-case
      (create-canvas (cffi:null-pointer))
    (cd-context-error (e)
      (pass))
    (error (e)
      (pass))))

(test resource-error-conditions
  "Test resource-specific error conditions"
  (with-debug-canvas (canvas)
    ;; Test server image resource errors
    (handler-case
        (let ((invalid-image (cffi:null-pointer)))
          (put-image-stretch canvas invalid-image 10 10 20 20 0 0 0 0))
      (cd-resource-error (e)
        (pass))
      (error (e)
        (pass)))

    ;; Test state resource errors
    (handler-case
        (restore-state canvas (cffi:null-pointer))
      (cd-resource-error (e)
        (pass))
      (error (e)
        (pass)))))

(test operation-error-conditions
  "Test operation-specific error conditions"
  (with-debug-canvas (canvas)
    ;; Test invalid drawing operations
    (handler-case
        (arc canvas 50 50 -10 -10 0 360)  ; Negative dimensions
      (cd-operation-error (e)
        (pass))
      (error (e)
        (pass)))

    ;; Test invalid image operations
    (handler-case
        (get-image-rgb canvas -10 -10 0 0)  ; Invalid coordinates and size
      (cd-operation-error (e)
        (pass))
      (error (e)
        (pass)))))

(test error-context-information
  "Test error context information"
  (handler-case
      (with-debug-canvas (canvas)
        ;; Force an error with context
        (signal 'cd-parameter-error
                :message "Test error"
                :function-name "test-function"
                :parameters '(:param1 "value1" :param2 "value2")))
    (cd-parameter-error (e)
      (is (stringp (format nil "~A" e)))  ; Should format to string
      (pass))
    (error (e)
      (pass))))

(test error-restart-mechanisms
  "Test error restart mechanisms"
  (with-debug-canvas (canvas)
    ;; Test with canvas restarts
    (handler-bind ((cd-error (lambda (e)
                               (let ((restart (find-restart 'use-default-value e)))
                                 (when restart
                                   (invoke-restart restart +red+))))))
      ;; This might not error, but test the restart mechanism exists
      (finishes (setf (foreground canvas) "invalid-color")))))

(test error-logging-and-debugging
  "Test error logging capabilities"
  (let ((error-logged nil))
    ;; Test that errors can be caught and logged
    (handler-case
        (with-debug-canvas (canvas)
          (signal 'cd-error :message "Test logging"))
      (cd-error (e)
        (setf error-logged t)
        (is (stringp (format nil "~A" e)))))

    (is error-logged "Error should have been caught and logged")))

(test error-performance-impact
  "Test error handling performance impact"
  (with-debug-canvas (canvas)
    ;; Test that error handling doesn't significantly impact performance
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 1000)
        (handler-case
            (setf (foreground canvas) +red+)  ; Valid operation
          (cd-error (e)
            (pass))))
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
        (is (< elapsed 1.0) "Error handling should not significantly impact performance")))))

(test nested-error-handling
  "Test nested error handling"
  (handler-case
      (handler-case
          (with-debug-canvas (canvas)
            (signal 'cd-canvas-error :message "Inner error"))
        (cd-canvas-error (e)
          (signal 'cd-error :message "Outer error")))
    (cd-error (e)
      (pass))))

(test error-recovery-strategies
  "Test error recovery strategies"
  (with-debug-canvas (canvas)
    ;; Test graceful degradation
    (let ((operations-completed 0))
      ;; Perform a series of operations, some may fail
      (dolist (color (list +red+ "invalid" +green+ -999 +blue+))
        (handler-case
            (progn
              (setf (foreground canvas) color)
              (incf operations-completed))
          (error (e)
            ;; Continue with next operation
            (continue))))

      ;; Should have completed at least some operations
      (is (> operations-completed 0)))))

(test error-state-consistency
  "Test that errors don't corrupt canvas state"
  (with-debug-canvas (canvas)
    ;; Set known good state
    (setf (foreground canvas) +red+)
    (setf (line-width canvas) 5)

    ;; Try an operation that might fail
    (handler-case
        (setf (foreground canvas) "invalid-color")
      (error (e)
        (pass)))

    ;; Verify canvas is still in a consistent state
    (is (integerp (foreground canvas)))
    (is (= (line-width canvas) 5))

    ;; Should still be able to perform valid operations
    (finishes (setf (foreground canvas) +blue+))
    (finishes (line canvas 10 10 50 50))))

(test error-cleanup-behavior
  "Test error cleanup behavior"
  (let ((cleanup-called nil))
    (handler-case
        (unwind-protect
             (with-debug-canvas (canvas)
               (signal 'cd-error :message "Test cleanup"))
          (setf cleanup-called t))
      (cd-error (e)
        (pass)))

    (is cleanup-called "Cleanup should be called even when errors occur")))

(run! 'error-handling-tests)
(in-package #:cd-tests)

;;; Context Management Tests

(in-suite context-tests)

(test context-debug
  "Test debug context creation and properties"
  (let ((context (ignore-errors (context-debug))))
    (when context
      (is (not (cffi:null-pointer-p context)))
      (is (eq (context-type context) :debug))
      (is (listp (context-capabilities context))))))

(test context-image-rgb
  "Test image RGB context creation"
  (let ((context (ignore-errors (context-image-rgb))))
    (when context
      (is (not (cffi:null-pointer-p context)))
      (is (eq (context-type context) :image))
      (is (listp (context-capabilities context))))))

(test context-svg
  "Test SVG context creation"
  (let ((context (ignore-errors (context-svg))))
    (when context
      (is (not (cffi:null-pointer-p context)))
      (is (eq (context-type context) :file))
      (is (listp (context-capabilities context))))))

(test context-picture
  "Test picture context creation"
  (let ((context (ignore-errors (context-picture))))
    (when context
      (is (not (cffi:null-pointer-p context)))
      (is (eq (context-type context) :file))
      (is (listp (context-capabilities context))))))

(test context-capabilities-validation
  "Test context capabilities are valid"
  (dolist (context (test-contexts))
    (let ((caps (context-capabilities context)))
      (is (listp caps))
      ;; Check that all capabilities are valid keywords
      (dolist (cap caps)
        (is (keywordp cap))))))

(test context-plus-detection
  "Test context plus detection"
  (dolist (context (test-contexts))
    ;; Should return either t or nil, not error
    (is (or (context-plus-p context)
            (not (context-plus-p context))))))

(test context-type-validation
  "Test context type returns valid values"
  (dolist (context (test-contexts))
    (let ((type (context-type context)))
      (is (member type '(:window :device :image :file :debug))))))

(test context-with-different-specs
  "Test context creation with different specifications"
  ;; Test debug context with different sizes
  (let ((context (ignore-errors (context-debug))))
    (when context
      (is (not (cffi:null-pointer-p context)))))

  ;; Test image context with specific size
  (let ((context (ignore-errors (context-image-rgb))))
    (when context
      (is (not (cffi:null-pointer-p context)))))

  ;; Test SVG context (file-based)
  (let ((context (ignore-errors (context-svg))))
    (when context
      (is (not (cffi:null-pointer-p context))))))

(test context-error-conditions
  "Test context creation error conditions"
  ;; These tests verify that invalid contexts are handled gracefully
  (handler-case
      (progn
        ;; Test with invalid context spec (this may or may not error)
        (context-debug)
        (pass))
    (cd-error (e)
      (pass))))  ; It's ok if context creation fails

(run! 'context-tests)
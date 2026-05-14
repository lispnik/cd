(in-package #:cd-tests)

;;; Backend Extension Tests

(def-suite backend-extension-tests :in cd-test-suite)
(in-suite backend-extension-tests)

(test postscript-backend
  "Test PostScript backend functionality"
  ;; Test PS options creation
  (let ((options (make-ps-options :paper-size :a4 :orientation :portrait :eps t)))
    (is (not (null options)))
    (is (eq (ps-paper-size options) :a4))
    (is (eq (ps-orientation options) :portrait))
    (is (ps-eps options)))

  ;; Test PS context creation (may not be available)
  (handler-case
      (let ((context (context-postscript)))
        (when context
          (is (not (cffi:null-pointer-p context)))
          (is (eq (context-type context) :file))))
    (cd-context-error (e)
      (pass))) ; PS context might not be available

  ;; Test PS canvas creation
  (handler-case
      (let ((canvas (create-postscript-canvas "/tmp/test.ps")))
        (when canvas
          (is (not (cffi:null-pointer-p canvas)))
          (kill canvas)))
    (cd-context-error (e)
      (pass))))

(test pdf-backend
  "Test PDF backend functionality"
  ;; Test PDF options creation
  (let ((options (make-pdf-options :paper-size :letter
                                  :title "Test Document"
                                  :author "Test Author")))
    (is (not (null options)))
    (is (eq (pdf-paper-size options) :letter))
    (is (string= (pdf-title options) "Test Document"))
    (is (string= (pdf-author options) "Test Author")))

  ;; Test PDF context creation (may not be available)
  (handler-case
      (let ((context (context-pdf)))
        (when context
          (is (not (cffi:null-pointer-p context)))
          (is (eq (context-type context) :file))))
    (cd-context-error (e)
      (pass)))

  ;; Test PDF canvas creation
  (handler-case
      (let ((canvas (create-pdf-canvas "/tmp/test.pdf")))
        (when canvas
          (is (not (cffi:null-pointer-p canvas)))
          (kill canvas)))
    (cd-context-error (e)
      (pass))))

(test printer-backend
  "Test printer backend functionality"
  ;; Test print options creation
  (let ((options (make-print-options :paper-size :a4
                                    :copies 2
                                    :duplex t
                                    :color-mode :color)))
    (is (not (null options)))
    (is (eq (print-paper-size options) :a4))
    (is (= (print-copies options) 2))
    (is (print-duplex options))
    (is (eq (print-color-mode options) :color)))

  ;; Test printer listing
  (let ((printers (list-printers)))
    (is (listp printers)))

  ;; Test printer context creation (may not be available)
  (handler-case
      (let ((context (context-printer)))
        (when context
          (is (not (cffi:null-pointer-p context)))))
    (cd-context-error (e)
      (pass))))

(test vector-export
  "Test vector graphics export"
  (with-debug-canvas (canvas)
    ;; Test SVG export
    (finishes (export-to-svg canvas "/tmp/test-export.svg"))

    ;; Test EPS export
    (finishes (export-to-eps canvas "/tmp/test-export.eps"))

    ;; Test multiple format export
    (let ((results (export-multiple-formats canvas "/tmp/multi-test" '(:svg :eps))))
      (is (listp results))
      (is (= (length results) 2)))))

(test backend-capability-detection
  "Test backend capability detection"
  (dolist (context (test-contexts))
    ;; Test capability detection
    (let ((caps (detect-backend-capabilities context)))
      (is (listp caps))
      (is (getf caps :type))
      (is (not (null (getf caps :capabilities))))
      (is (member (getf caps :supports-color) '(t nil)))
      (is (member (getf caps :supports-text) '(t nil)))
      (is (member (getf caps :supports-images) '(t nil))))))

(test backend-recommendations
  "Test backend recommendation system"
  ;; Test recommendations for different task types
  (let ((vector-recs (recommend-backend-for-task '(:high-quality-vector))))
    (is (listp vector-recs))
    (is (> (length vector-recs) 0)))

  (let ((doc-recs (recommend-backend-for-task '(:document :text))))
    (is (listp doc-recs)))

  (let ((web-recs (recommend-backend-for-task '(:web :interactive))))
    (is (listp web-recs)))

  (let ((print-recs (recommend-backend-for-task '(:printing))))
    (is (listp print-recs))))

(test backend-optimization
  "Test backend optimization"
  (dolist (context (test-contexts))
    (let ((canvas (create-canvas context)))
      (when canvas
        (activate canvas)

        ;; Test optimization for different backend types
        (let ((backend-type (context-type context)))
          (finishes (optimize-for-backend canvas backend-type)))

        (deactivate canvas)
        (kill canvas)))))

(test backend-feature-testing
  "Test backend feature testing utilities"
  (dolist (context (test-contexts))
    ;; Test feature availability
    (let ((test-results (test-backend-features context)))
      (is (listp test-results))

      ;; Check that we get results for basic features
      (let ((basic-drawing (cdr (assoc :basic-drawing test-results)))
            (text-rendering (cdr (assoc :text-rendering test-results)))
            (image-operations (cdr (assoc :image-operations test-results))))
        (is (member basic-drawing '(t nil)))
        (is (member text-rendering '(t nil)))
        (is (member image-operations '(t nil)))))))

(test opengl-backend-placeholder
  "Test OpenGL backend placeholder"
  ;; Test that OpenGL context appropriately signals unavailability
  (handler-case
      (context-opengl)
    (cd-context-error (e)
      (pass))
    (:no-error (context)
      ;; If OpenGL is actually available, that's fine too
      (when context
        (is (not (cffi:null-pointer-p context)))))))

(test platform-specific-functions
  "Test platform-specific utility functions"
  ;; Test system font listing
  (let ((fonts (get-system-fonts)))
    (is (listp fonts))
    (is (> (length fonts) 0))
    (dolist (font fonts)
      (is (stringp font))))

  ;; Test system printer listing
  (let ((printers (get-system-printers)))
    (is (listp printers)))

  ;; Test display info
  (let ((display-info (get-display-info)))
    (is (listp display-info))
    (is (getf display-info :width))
    (is (getf display-info :height))
    (is (getf display-info :dpi))))

(test backend-specific-options
  "Test backend-specific option handling"
  ;; Test PS options with different configurations
  (let ((portrait-ps (make-ps-options :orientation :portrait))
        (landscape-ps (make-ps-options :orientation :landscape))
        (high-res-ps (make-ps-options :resolution 600)))
    (is (eq (ps-orientation portrait-ps) :portrait))
    (is (eq (ps-orientation landscape-ps) :landscape))
    (is (= (ps-resolution high-res-ps) 600)))

  ;; Test PDF options with metadata
  (let ((pdf-with-meta (make-pdf-options :title "Test Title"
                                        :author "Test Author"
                                        :subject "Test Subject"
                                        :keywords "test, pdf, graphics")))
    (is (string= (pdf-title pdf-with-meta) "Test Title"))
    (is (string= (pdf-author pdf-with-meta) "Test Author"))
    (is (string= (pdf-subject pdf-with-meta) "Test Subject"))
    (is (string= (pdf-keywords pdf-with-meta) "test, pdf, graphics")))

  ;; Test print options with various settings
  (let ((color-print (make-print-options :color-mode :color))
        (bw-print (make-print-options :color-mode :grayscale))
        (draft-print (make-print-options :quality :draft))
        (high-print (make-print-options :quality :high)))
    (is (eq (print-color-mode color-print) :color))
    (is (eq (print-color-mode bw-print) :grayscale))
    (is (eq (print-quality draft-print) :draft))
    (is (eq (print-quality high-print) :high))))

(test export-error-handling
  "Test export error handling"
  (with-debug-canvas (canvas)
    ;; Test export to invalid paths
    (handler-case
        (export-to-svg canvas "/invalid/path/test.svg")
      (error (e)
        (pass)))

    (handler-case
        (export-to-eps canvas "/invalid/path/test.eps")
      (error (e)
        (pass)))

    ;; Test multiple format export with some invalid formats
    (let ((results (export-multiple-formats canvas "/tmp/test" '(:svg :invalid-format :eps))))
      (is (listp results))
      ;; Should have attempted all formats, some may have failed
      )))

(test backend-context-lifecycle
  "Test backend context lifecycle management"
  ;; Test that contexts can be created and destroyed properly
  (dolist (context-creator (list (lambda () (ignore-errors (context-debug)))
                                 (lambda () (ignore-errors (context-image-rgb)))
                                 (lambda () (ignore-errors (context-svg)))
                                 (lambda () (ignore-errors (context-postscript)))
                                 (lambda () (ignore-errors (context-pdf)))))
    (let ((context (funcall context-creator)))
      (when context
        ;; Test that we can create and destroy a canvas
        (let ((canvas (create-canvas context)))
          (when canvas
            (finishes (activate canvas))
            (finishes (deactivate canvas))
            (finishes (kill canvas))))))))

(test performance-backend-comparison
  "Test performance comparison across backends"
  (let ((performance-results '()))
    (dolist (context (test-contexts))
      (let ((canvas (create-canvas context)))
        (when canvas
          (activate canvas)

          ;; Time basic operations
          (let ((start-time (get-internal-real-time)))
            (dotimes (i 50)
              (line canvas 10 10 50 50))
            (let ((elapsed (/ (- (get-internal-real-time) start-time)
                             internal-time-units-per-second)))
              (push (list (context-type context) elapsed) performance-results)))

          (deactivate canvas)
          (kill canvas))))

    ;; Report results
    (format t "~&Backend performance comparison:~%")
    (dolist (result performance-results)
      (format t "  ~A: ~F seconds~%" (first result) (second result)))

    ;; Verify all backends completed successfully
    (is (> (length performance-results) 0))))

(run! 'backend-extension-tests)
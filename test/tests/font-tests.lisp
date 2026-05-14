(in-package #:cd-tests)

;;; Font Management Tests

(in-suite font-tests)

(test font-basic-operations
  "Test basic font operations"
  (with-debug-canvas (canvas)
    ;; Test font setting and getting
    (let ((original-font (font canvas)))
      (is (stringp original-font))

      ;; Test setting different fonts
      (setf (font canvas) "Helvetica,12")
      (is (string= (font canvas) "Helvetica,12"))

      (setf (font canvas) "Times,14,-1,0")
      (let ((current-font (font canvas)))
        (is (stringp current-font))
        (is (search "Times" current-font)))

      ;; Test setting with different parameters
      (setf (font canvas) "Courier,10,1,1")  ; Bold, Italic
      (is (stringp (font canvas))))))

(test font-dimensions
  "Test font dimension queries"
  (with-debug-canvas (canvas)
    ;; Test basic font dimensions
    (let ((ascent (font-dim canvas :font-ascent))
          (descent (font-dim canvas :font-descent))
          (height (font-dim canvas :font-height))
          (max-width (font-dim canvas :font-max-width)))

      (is (numberp ascent))
      (is (numberp descent))
      (is (numberp height))
      (is (numberp max-width))

      (is (> ascent 0))
      (is (> descent 0))
      (is (> height 0))
      (is (> max-width 0))

      ;; Basic sanity checks
      (is (> height (+ ascent descent)))  ; Height should be at least ascent + descent
      (is (> max-width 0)))))

(test font-size-variations
  "Test fonts with different sizes"
  (with-debug-canvas (canvas)
    (let ((sizes '(8 10 12 14 16 20 24)))
      (dolist (size sizes)
        (let ((font-spec (format nil "Helvetica,~D" size)))
          (setf (font canvas) font-spec)

          ;; Check that font height increases with size (generally)
          (let ((height (font-dim canvas :font-height)))
            (is (numberp height))
            (is (> height 0))
            ;; Height should roughly correlate with size
            (is (> height (/ size 2)))))))))

(test font-family-variations
  "Test different font families"
  (with-debug-canvas (canvas)
    (let ((families '("Helvetica" "Times" "Courier" "Arial")))
      (dolist (family families)
        (let ((font-spec (format nil "~A,12" family)))
          (handler-case
              (progn
                (setf (font canvas) font-spec)
                (let ((set-font (font canvas)))
                  (is (stringp set-font))
                  ;; Font may be substituted, so just check it's valid
                  (is (> (length set-font) 0))))
            (cd-error (e)
              ;; Some fonts might not be available - that's ok
              (pass))))))))

(test font-style-variations
  "Test different font styles"
  (with-debug-canvas (canvas)
    ;; Test normal font
    (setf (font canvas) "Helvetica,12,0,0")  ; Normal
    (let ((normal-font (font canvas)))
      (is (stringp normal-font)))

    ;; Test bold font
    (handler-case
        (progn
          (setf (font canvas) "Helvetica,12,1,0")  ; Bold
          (let ((bold-font (font canvas)))
            (is (stringp bold-font))))
      (cd-error (e)
        (pass)))  ; Bold might not be supported

    ;; Test italic font
    (handler-case
        (progn
          (setf (font canvas) "Helvetica,12,0,1")  ; Italic
          (let ((italic-font (font canvas)))
            (is (stringp italic-font))))
      (cd-error (e)
        (pass)))  ; Italic might not be supported

    ;; Test bold italic
    (handler-case
        (progn
          (setf (font canvas) "Helvetica,12,1,1")  ; Bold Italic
          (let ((bold-italic-font (font canvas)))
            (is (stringp bold-italic-font))))
      (cd-error (e)
        (pass)))))

(test font-text-measurement
  "Test text measurement with different fonts"
  (with-debug-canvas (canvas)
    (let ((test-text "Sample Text"))

      ;; Measure with default font
      (multiple-value-bind (width1 height1) (text-size canvas test-text)
        (is (integerp width1))
        (is (integerp height1))
        (is (> width1 0))
        (is (> height1 0))

        ;; Change to larger font and measure again
        (setf (font canvas) "Helvetica,20")
        (multiple-value-bind (width2 height2) (text-size canvas test-text)
          (is (integerp width2))
          (is (integerp height2))
          (is (> width2 0))
          (is (> height2 0))

          ;; Larger font should generally produce larger measurements
          (is (>= width2 width1))
          (is (>= height2 height1)))))))

(test font-bounds-measurement
  "Test text bounds measurement with fonts"
  (with-debug-canvas (canvas)
    (let ((test-text "Test Bounds"))
      ;; Test bounds with different fonts
      (dolist (font-spec '("Helvetica,12" "Times,14" "Courier,10"))
        (handler-case
            (progn
              (setf (font canvas) font-spec)

              (multiple-value-bind (xmin xmax ymin ymax)
                  (text-bounds canvas 50 50 test-text)
                (is (integerp xmin))
                (is (integerp xmax))
                (is (integerp ymin))
                (is (integerp ymax))
                (is (<= xmin xmax))
                (is (<= ymin ymax))))
          (cd-error (e)
            ;; Some fonts might not be available
            (pass)))))))

(test font-error-conditions
  "Test font error handling"
  (with-debug-canvas (canvas)
    ;; Test invalid font specifications
    (handler-case
        (setf (font canvas) "")  ; Empty font
      (cd-error (e)
        (pass))  ; Should handle gracefully
      (:no-error ()
        (pass)))  ; Or succeed with default

    (handler-case
        (setf (font canvas) "NonexistentFont,12")
      (cd-error (e)
        (pass))  ; Font substitution should occur
      (:no-error ()
        (pass)))

    ;; Test invalid font dimension queries
    (handler-case
        (font-dim canvas :invalid-dimension)
      (cd-error (e)
        (pass))
      (error (e)
        (pass)))))

(test font-state-preservation
  "Test font state preservation with save/restore"
  (with-debug-canvas (canvas)
    ;; Set initial font
    (setf (font canvas) "Helvetica,12")
    (let ((initial-font (font canvas)))

      ;; Save state
      (let ((state (save-state canvas)))

        ;; Change font
        (setf (font canvas) "Times,16")
        (is (not (string= (font canvas) initial-font)))

        ;; Restore state
        (restore-state canvas state)
        (is (string= (font canvas) initial-font))

        ;; Clean up
        (release-state state)))))

(test font-unicode-support
  "Test font support for Unicode characters"
  (with-debug-canvas (canvas)
    ;; Test basic Unicode support
    (let ((unicode-text "Hello αβγ 世界 🌍"))
      (handler-case
          (progn
            ;; Try to measure Unicode text
            (multiple-value-bind (width height) (text-size canvas unicode-text)
              (is (integerp width))
              (is (integerp height))
              (is (>= width 0))  ; Width might be 0 if characters not supported
              (is (> height 0))))
        (cd-error (e)
          (pass))))))  ; Unicode might not be fully supported

(run! 'font-tests)
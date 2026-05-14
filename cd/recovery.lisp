(in-package #:cd)

;;; Error Recovery and Restarts

(defvar *default-fallback-context* nil
  "Default context to use when primary context fails.")

(defvar *enable-auto-recovery* t
  "When non-nil, enables automatic recovery attempts for certain errors.")

(defmacro with-canvas-restarts ((canvas-var context spec) &body body)
  "Execute body with restarts for canvas-related errors."
  `(restart-case
       (let ((,canvas-var (create-canvas ,context ,spec)))
         ,@body)
     (retry-canvas-creation ()
       :report "Retry canvas creation with same parameters"
       (let ((,canvas-var (create-canvas ,context ,spec)))
         ,@body))
     (use-debug-canvas ()
       :report "Use debug canvas instead of original context"
       (let ((,canvas-var (create-canvas (context-debug) nil)))
         ,@body))
     (use-image-canvas (width height)
       :report "Use RGB image canvas with specified dimensions"
       :interactive (lambda ()
                      (format *query-io* "Enter width: ")
                      (let ((w (read *query-io*)))
                        (format *query-io* "Enter height: ")
                        (let ((h (read *query-io*)))
                          (list w h))))
       (let ((,canvas-var (create-canvas (context-image-rgb)
                                        (format nil "~Dx~D" width height))))
         ,@body))
     (skip-canvas-operation ()
       :report "Skip this canvas operation entirely"
       nil)))

(defmacro with-drawing-restarts (operation &body body)
  "Execute body with restarts for drawing operations."
  `(restart-case
       (progn ,@body)
     (skip-drawing-operation ()
       :report ,(format nil "Skip the ~A operation and continue" operation)
       (when *cd-debug-mode*
         (format *debug-io* "~&CD: Skipped ~A operation~%" ',operation))
       nil)
     (retry-drawing-operation ()
       :report ,(format nil "Retry the ~A operation" operation)
       (progn ,@body))
     (use-fallback-method ()
       :report ,(format nil "Use fallback implementation for ~A" operation)
       ;; This would need operation-specific fallback implementations
       (when *cd-debug-mode*
         (format *debug-io* "~&CD: Using fallback for ~A~%" ',operation)))))

(defmacro with-file-restarts ((pathname) &body body)
  "Execute body with restarts for file operations."
  `(restart-case
       (progn ,@body)
     (retry-with-new-path (new-path)
       :report "Retry with a different file path"
       :interactive (lambda ()
                      (format *query-io* "Enter new file path: ")
                      (list (read-line *query-io*)))
       (let ((,pathname new-path))
         ,@body))
     (use-temporary-file ()
       :report "Use a temporary file instead"
       (let ((,pathname (format nil "/tmp/cd-temp-~A.tmp" (get-universal-time))))
         (when *cd-debug-mode*
           (format *debug-io* "~&CD: Using temporary file: ~A~%" ,pathname))
         ,@body))
     (skip-file-operation ()
       :report "Skip file operation (may result in data loss)"
       nil)))

(defun handle-backend-error (condition)
  "Provide common restarts for backend errors."
  (restart-case
      (error condition)
    (use-alternative-backend ()
      :report "Try with debug context instead"
      :test (lambda (c)
              (and (typep c 'cd-backend-error)
                   (not (eq (error-backend c) :debug))))
      (context-debug))
    (ignore-capability-error ()
      :report "Ignore missing capability and continue"
      :test (lambda (c)
              (typep c 'cd-backend-error))
      nil)
    (disable-feature ()
      :report "Disable this feature for remainder of session"
      :test (lambda (c)
              (typep c 'cd-backend-error))
      ;; Could set a global flag to disable the feature
      nil)))

(defmacro with-automatic-recovery (&body body)
  "Execute body with automatic error recovery when enabled."
  `(if *enable-auto-recovery*
       (handler-bind ((cd-backend-error #'handle-backend-error)
                      (cd-canvas-error (lambda (c)
                                        (when *default-fallback-context*
                                          (invoke-restart 'use-alternative-backend))))
                      (cd-file-error (lambda (c)
                                      (when (find-restart 'use-temporary-file)
                                        (invoke-restart 'use-temporary-file)))))
         ,@body)
       (progn ,@body)))

(defun safe-canvas-with-recovery (context spec function)
  "Create canvas and call function with comprehensive error recovery."
  (with-canvas-restarts (canvas context spec)
    (unwind-protect
         (with-automatic-recovery
           (with-drawing-restarts canvas-operation
             (funcall function canvas)))
      ;; Always try to clean up
      (when (and (boundp 'canvas) canvas)
        (ignore-errors (kill canvas))))))

(defmacro with-enhanced-canvas ((canvas context spec) &body body)
  "Enhanced version of with-canvas that includes error recovery."
  `(safe-canvas-with-recovery ,context ,spec
                             (lambda (,canvas)
                               ,@body)))

(defun install-global-error-handlers ()
  "Install global error handlers for CD operations."
  (setf *debugger-hook*
        (lambda (condition me)
          (when (typep condition 'cd-error)
            (format *error-output* "~&CD Error: ~A~%" condition)
            (when *cd-debug-mode*
              (format *error-output* "Available restarts:~%")
              (loop for restart in (compute-restarts condition)
                    for i from 0
                    do (format *error-output* "  ~D: ~A~%" i restart)))
            ;; Call the previous debugger hook if any
            (when me (funcall me condition nil))))))

(defun remove-global-error-handlers ()
  "Remove CD-specific global error handlers."
  (setf *debugger-hook* nil))

;; Utility functions for common error scenarios

(defun try-alternative-contexts (spec function &optional contexts)
  "Try function with alternative contexts until one succeeds."
  (let ((contexts-to-try (or contexts
                            (list (context-debug)
                                  (context-image-rgb)
                                  (context-picture)))))
    (dolist (context contexts-to-try)
      (handler-case
          (return-from try-alternative-contexts
            (with-enhanced-canvas (canvas context spec)
              (funcall function canvas)))
        (cd-error (e)
          (when *cd-debug-mode*
            (format *debug-io* "~&CD: Context ~A failed: ~A~%" context e))
          ;; Continue to next context
          nil)))
    ;; If we get here, all contexts failed
    (error 'cd-context-error
           :operation 'try-alternative-contexts
           :context contexts-to-try)))

(export '(*default-fallback-context* *enable-auto-recovery*
          with-canvas-restarts with-drawing-restarts with-file-restarts
          handle-backend-error with-automatic-recovery
          safe-canvas-with-recovery with-enhanced-canvas
          install-global-error-handlers remove-global-error-handlers
          try-alternative-contexts))
(in-package #:cd)

;;; Return Value Checking and Error Context

(defvar *cd-debug-mode* nil
  "When non-nil, enables verbose error reporting and debugging.")

(defvar *cd-operation-context* nil
  "Current operation context for enhanced error reporting.")

(defmacro with-cd-context (operation-description &body body)
  "Execute body with enhanced error context."
  `(let ((*cd-operation-context* ,operation-description))
     (when *cd-debug-mode*
       (format *debug-io* "~&CD: Starting ~A~%" *cd-operation-context*))
     (handler-bind ((error (lambda (condition)
                             (when *cd-debug-mode*
                               (format *debug-io* "~&CD: Error in ~A: ~A~%"
                                      *cd-operation-context* condition)))))
       ,@body)))

(defun check-cd-status (status operation &key canvas context)
  "Check CD status return value and signal appropriate error."
  (case status
    (:error
     (error 'cd-canvas-error
            :operation operation
            :canvas canvas
            :context context))
    (otherwise status)))

(defun check-null-result (result operation &key canvas context what)
  "Check for null pointer result and signal error."
  (when (cffi:null-pointer-p result)
    (error (cond
             ((string= what "canvas") 'initialization-error)
             ((string= what "context") 'cd-context-error)
             (t 'cd-resource-error))
           :operation operation
           :canvas canvas
           :context context
           :resource-type (or what "resource")))
  result)

(defun check-canvas-capability (canvas capability operation)
  "Check if canvas supports a required capability."
  (let* ((context (context canvas))
         (caps (context-capabilities context))
         (backend-type (context-type context)))
    (unless (member capability caps)
      (error 'cd-backend-error
             :operation operation
             :canvas canvas
             :context context
             :backend backend-type
             :capability capability))))

(defmacro with-cd-error-checking ((operation &key canvas context) &body body)
  "Execute body with comprehensive error checking."
  `(with-cd-context ,operation
     (handler-case
         (let ((result (progn ,@body)))
           (when *cd-debug-mode*
             (format *debug-io* "~&CD: Completed ~A successfully~%" ,operation))
           result)
       (cffi:foreign-function-call-error (e)
         (error 'cd-backend-error
                :operation ,operation
                :canvas ,canvas
                :context ,context
                :backend (when ,context (context-type ,context))))
       (storage-condition (e)
         (error 'cd-memory-error
                :operation ,operation
                :canvas ,canvas
                :context ,context)))))

(defun enhanced-create-canvas (context &optional spec)
  "Create canvas with enhanced error checking."
  (with-validation 'create-canvas
    (validate-context context 'create-canvas)
    (when spec
      (validate-string spec 'spec 'create-canvas :allow-null t)))

  (with-cd-error-checking ('create-canvas :context context)
    (let* ((cffi-spec (cond
                        ((null spec) (cffi:null-pointer))
                        ((stringp spec) (cffi:foreign-string-alloc spec))
                        (t spec)))
           (canvas-ptr (cd-cffi::%cd-create-canvas context cffi-spec)))
      (when (stringp spec)
        (cffi:foreign-string-free cffi-spec))
      (check-null-result canvas-ptr 'create-canvas
                        :context context :what "canvas"))))

(defun enhanced-activate (canvas)
  "Activate canvas with enhanced error checking."
  (with-validation 'activate
    (validate-canvas canvas 'activate))

  (with-cd-error-checking ('activate :canvas canvas)
    (check-cd-status (cd-cffi::%cd-canvas-activate canvas) 'activate
                     :canvas canvas)))

(defun safe-canvas-operation (operation canvas &rest args)
  "Safely execute a canvas operation with error checking."
  (with-validation operation
    (validate-canvas canvas operation))

  (with-cd-error-checking (operation :canvas canvas)
    (apply (symbol-function operation) canvas args)))

(defmacro define-safe-wrapper (name cffi-function &key
                               (validators '())
                               (capability nil)
                               (check-result nil))
  "Define a safe wrapper around a CFFI function."
  `(defun ,name (&rest args)
     ,(format nil "Safe wrapper around ~A with error checking." cffi-function)
     (let ((canvas (first args)))
       (with-validation ',name
         (validate-canvas canvas ',name)
         ,@(mapcar (lambda (validator)
                     `(,@validator))
                   validators))

       ,@(when capability
           `((check-canvas-capability canvas ,capability ',name)))

       (with-cd-error-checking (',name :canvas canvas)
         (let ((result (,cffi-function ,@(mapcar (lambda (arg) `(nth ,(position arg args) args))
                                                (loop for i from 0 below (length args)
                                                      collect i)))))
           ,@(when check-result
               `((funcall ,check-result result ',name :canvas canvas)))
           result)))))

;; Example usage:
(define-safe-wrapper safe-line cd-cffi::%cd-canvas-line
  :validators ((validate-coordinate (second args) 'x1 'safe-line)
               (validate-coordinate (third args) 'y1 'safe-line)
               (validate-coordinate (fourth args) 'x2 'safe-line)
               (validate-coordinate (fifth args) 'y2 'safe-line)))

(define-safe-wrapper safe-put-image-rgba cd-cffi::%cd-canvas-put-image-rect-rgba
  :capability :imagergba
  :validators ((validate-dimensions (second args) (third args) 'safe-put-image-rgba)))

(export '(*cd-debug-mode* *cd-operation-context* with-cd-context
          check-cd-status check-null-result check-canvas-capability
          with-cd-error-checking enhanced-create-canvas enhanced-activate
          safe-canvas-operation define-safe-wrapper
          safe-line safe-put-image-rgba))
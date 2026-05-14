(in-package #:cd)

;;; CD Error Condition Hierarchy

(define-condition cd-error (error)
  ((operation :initarg :operation :reader error-operation :initform nil)
   (canvas :initarg :canvas :reader error-canvas :initform nil)
   (context :initarg :context :reader error-context :initform nil))
  (:report (lambda (condition stream)
             (format stream "CD Error during ~@[~A~]~@[ on canvas ~A~]~@[ with context ~A~]"
                     (error-operation condition)
                     (error-canvas condition)
                     (error-context condition))))
  (:documentation "Base condition for all CD library errors"))

(define-condition cd-resource-error (cd-error)
  ((resource-type :initarg :resource-type :reader error-resource-type))
  (:report (lambda (condition stream)
             (format stream "CD ~A resource error during ~@[~A~]"
                     (error-resource-type condition)
                     (error-operation condition))))
  (:documentation "Errors related to resource allocation/management"))

(define-condition cd-canvas-error (cd-resource-error)
  ()
  (:default-initargs :resource-type "canvas")
  (:documentation "Canvas-related errors"))

(define-condition cd-context-error (cd-resource-error)
  ()
  (:default-initargs :resource-type "context")
  (:documentation "Context-related errors"))

(define-condition cd-parameter-error (cd-error)
  ((parameter :initarg :parameter :reader error-parameter)
   (value :initarg :value :reader error-value)
   (expected :initarg :expected :reader error-expected))
  (:report (lambda (condition stream)
             (format stream "Invalid parameter ~A for ~A: got ~S, expected ~A"
                     (error-parameter condition)
                     (error-operation condition)
                     (error-value condition)
                     (error-expected condition))))
  (:documentation "Invalid parameter values"))

(define-condition cd-backend-error (cd-error)
  ((backend :initarg :backend :reader error-backend)
   (capability :initarg :capability :reader error-capability :initform nil))
  (:report (lambda (condition stream)
             (if (error-capability condition)
                 (format stream "Backend ~A does not support capability ~A required for ~A"
                         (error-backend condition)
                         (error-capability condition)
                         (error-operation condition))
                 (format stream "Backend ~A error during ~A"
                         (error-backend condition)
                         (error-operation condition)))))
  (:documentation "Backend-specific functionality errors"))

(define-condition cd-drawing-error (cd-error)
  ((coordinates :initarg :coordinates :reader error-coordinates :initform nil))
  (:report (lambda (condition stream)
             (format stream "Drawing error during ~A~@[ at coordinates ~A~]"
                     (error-operation condition)
                     (error-coordinates condition))))
  (:documentation "Errors during drawing operations"))

(define-condition cd-file-error (cd-error)
  ((pathname :initarg :pathname :reader error-pathname)
   (reason :initarg :reason :reader error-reason))
  (:report (lambda (condition stream)
             (format stream "File error during ~A with ~A: ~A"
                     (error-operation condition)
                     (error-pathname condition)
                     (error-reason condition))))
  (:documentation "File-related errors"))

(define-condition cd-memory-error (cd-resource-error)
  ((size :initarg :size :reader error-size :initform nil))
  (:report (lambda (condition stream)
             (format stream "Memory allocation error~@[ for ~A bytes~] during ~A"
                     (error-size condition)
                     (error-operation condition))))
  (:default-initargs :resource-type "memory")
  (:documentation "Memory allocation errors"))

;;; Re-export existing conditions with better hierarchy
(define-condition initialization-error (cd-canvas-error)
  ((spec :initarg :spec :reader initialization-error-spec))
  (:report (lambda (condition stream)
             (format stream "Error creating canvas from spec ~S"
                     (initialization-error-spec condition))))
  (:documentation "Signaled when a canvas cannot be created."))

(define-condition activation-error (cd-canvas-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Canvas activation failed~@[ for canvas ~A~]"
                     (error-canvas condition))))
  (:documentation "Signaled when a canvas cannot be activated."))

(export '(cd-error cd-resource-error cd-canvas-error cd-context-error
          cd-parameter-error cd-backend-error cd-drawing-error
          cd-file-error cd-memory-error
          initialization-error activation-error
          error-operation error-canvas error-context error-parameter
          error-value error-expected error-backend error-capability
          error-coordinates error-pathname error-reason error-size
          error-resource-type initialization-error-spec))
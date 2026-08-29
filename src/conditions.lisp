;;;; src/conditions.lisp — the condition hierarchy.
;;;;
;;;; Free of foreign calls, so it loads before libcd is open and can report a
;;;; failure to open it.
;;;;
;;;; CD reports failure differently from IM, and more thinly. There is no
;;;; error-code enum: cdCreateCanvas returns NULL, cdCanvasActivate returns
;;;; CD_ERROR, and most drawing calls return nothing at all and simply do
;;;; nothing when the canvas cannot honour them. So the classes here describe
;;;; what the binding could determine, not a code the library handed over.

(in-package #:cd)

(export '(cd-condition
          cd-error
          error-detail
          driver-error
          driver-not-available
          driver-not-available-name
          canvas-creation-error
          invalid-canvas
          invalid-canvas-object
          library-not-found
          library-not-found-candidates
          unsupported-operation))

(defgeneric error-detail (condition)
  (:documentation
   "Context the binding added -- the driver, filename or operation involved --
or NIL. CD's failures carry no payload of their own, so without this a report
can say only that a canvas could not be created, not what it was for."))

(define-condition cd-condition (condition)
  ()
  (:documentation "Root of every condition this library signals."))

(define-condition cd-error (cd-condition cl:error)
  ((detail :initarg :detail :initform nil :reader error-detail
           :documentation "Driver, filename or operation context, or NIL."))
  (:documentation "Base class for every failure this library reports.")
  (:report (lambda (c stream)
             (format stream "CD error~@[: ~A~]" (error-detail c)))))

;;; Drivers -------------------------------------------------------------------

(define-condition driver-error (cd-error) ()
  (:documentation "Something went wrong with a CD driver."))

(define-condition driver-not-available (driver-error)
  ((name :initarg :name :initform nil :reader driver-not-available-name
         :documentation "The driver that is missing, e.g. \"PDF\"."))
  (:documentation
   "A driver this build of CD does not contain.

Normal, not exceptional: which drivers exist is decided by CMake options when
libcd is compiled, so a build with CD_ENABLE_PDF=OFF genuinely has no PDF
context. CD:DRIVERS reports what the loaded library actually has.")
  (:report (lambda (c stream)
             (format stream "The ~A driver is not available in this build of CD.~
                             ~%CD:DRIVERS lists the drivers that are."
                     (or (driver-not-available-name c) "requested")))))

(define-condition canvas-creation-error (driver-error) ()
  (:documentation
   "cdCreateCanvas returned NULL.

CD does not say why. The usual causes are a driver-specific data string it
could not parse, a file it could not open for writing, or a size it rejected;
the detail slot carries whatever the binding passed in, which is the only
evidence available.")
  (:report (lambda (c stream)
             (format stream "CD could not create the canvas~@[ (~A)~].~
                             ~%CD reports no reason; check the path is writable ~
                             and the size is positive."
                     (error-detail c)))))

;;; Canvases ------------------------------------------------------------------

(define-condition invalid-canvas (cd-error)
  ((canvas :initarg :canvas :initform nil :reader invalid-canvas-object))
  (:documentation
   "An operation on a canvas that has already been killed.

Reaching this is much better than the alternative: the previous binding handed
callers a bare foreign pointer, so the same mistake drew into freed memory.")
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "Operation on a canvas that has already been killed."))))

(define-condition unsupported-operation (cd-error) ()
  (:documentation
   "The active driver does not implement this operation.

CD drivers vary in what they support -- a PostScript canvas has no pixel
read-back, a metafile cannot report font metrics -- and the C library's usual
answer is to do nothing quietly. Where the binding can tell in advance, it
says so instead.")
  (:report (lambda (c stream)
             (format stream "This CD driver does not support ~A."
                     (or (error-detail c) "that operation")))))

;;; Library loading -----------------------------------------------------------

(define-condition library-not-found (cd-error)
  ((candidates :initarg :candidates :initform nil
               :reader library-not-found-candidates))
  (:documentation "libcd could not be opened.")
  (:report (lambda (c stream)
             (format stream "Cannot load the CD library~@[ (~A)~].~
                             ~@[~%Tried:~{~%  ~A~}~]~
                             ~%Set CD_LIBRARY_PATH to the directory holding it."
                     (error-detail c)
                     (library-not-found-candidates c)))))

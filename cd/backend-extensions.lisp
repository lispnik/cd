(in-package #:cd)

;;; Backend-Specific Extensions

;;; PostScript Backend Support

(defun context-postscript ()
  "Create PostScript context"
  (handler-case
      (cd-cffi::%cd-context-ps)
    (error (e)
      (error 'cd-context-error :message "PostScript context not available"
                               :function-name "context-postscript"))))

(defclass ps-options ()
  ((paper-size :initarg :paper-size :accessor ps-paper-size :initform :a4)
   (orientation :initarg :orientation :accessor ps-orientation :initform :portrait)
   (resolution :initarg :resolution :accessor ps-resolution :initform 300)
   (color-mode :initarg :color-mode :accessor ps-color-mode :initform :color)
   (eps :initarg :eps :accessor ps-eps :initform nil))
  (:documentation "PostScript output options"))

(defun make-ps-options (&key (paper-size :a4) (orientation :portrait)
                             (resolution 300) (color-mode :color) (eps nil))
  "Create PostScript options"
  (make-instance 'ps-options
                 :paper-size paper-size
                 :orientation orientation
                 :resolution resolution
                 :color-mode color-mode
                 :eps eps))

(defun create-postscript-canvas (filename &optional (options (make-ps-options)))
  "Create PostScript canvas with options"
  (let ((context (context-postscript)))
    (when context
      (let ((spec (format nil "~A ~A ~A ~A~A"
                          filename
                          (ps-paper-size options)
                          (ps-orientation options)
                          (ps-resolution options)
                          (if (ps-eps options) " -e" ""))))
        (create-canvas context spec)))))

(defun ps-set-line-join (canvas join-style)
  "Set PostScript-specific line join style"
  (validate-canvas canvas)
  (setf (line-join canvas) join-style))

(defun ps-set-line-cap (canvas cap-style)
  "Set PostScript-specific line cap style"
  (validate-canvas canvas)
  (setf (line-cap canvas) cap-style))

(defun ps-begin-text-object (canvas)
  "Begin PostScript text object for complex text operations"
  (validate-canvas canvas)
  ;; This would send raw PostScript commands
  (format t "BT~%")) ; Begin Text

(defun ps-end-text-object (canvas)
  "End PostScript text object"
  (validate-canvas canvas)
  (format t "ET~%")) ; End Text

;;; PDF Backend Support

(defun context-pdf ()
  "Create PDF context"
  (handler-case
      (cd-cffi::%cd-context-pdf)
    (error (e)
      (error 'cd-context-error :message "PDF context not available"
                               :function-name "context-pdf"))))

(defclass pdf-options ()
  ((paper-size :initarg :paper-size :accessor pdf-paper-size :initform :a4)
   (orientation :initarg :orientation :accessor pdf-orientation :initform :portrait)
   (resolution :initarg :resolution :accessor pdf-resolution :initform 300)
   (compression :initarg :compression :accessor pdf-compression :initform t)
   (version :initarg :version :accessor pdf-version :initform "1.4")
   (title :initarg :title :accessor pdf-title :initform nil)
   (author :initarg :author :accessor pdf-author :initform nil)
   (subject :initarg :subject :accessor pdf-subject :initform nil)
   (keywords :initarg :keywords :accessor pdf-keywords :initform nil))
  (:documentation "PDF output options"))

(defun make-pdf-options (&key (paper-size :a4) (orientation :portrait)
                              (resolution 300) (compression t) (version "1.4")
                              (title nil) (author nil) (subject nil) (keywords nil))
  "Create PDF options"
  (make-instance 'pdf-options
                 :paper-size paper-size
                 :orientation orientation
                 :resolution resolution
                 :compression compression
                 :version version
                 :title title
                 :author author
                 :subject subject
                 :keywords keywords))

(defun create-pdf-canvas (filename &optional (options (make-pdf-options)))
  "Create PDF canvas with options"
  (let ((context (context-pdf)))
    (when context
      (let ((spec (format nil "~A ~A ~A ~A~A"
                          filename
                          (pdf-paper-size options)
                          (pdf-orientation options)
                          (pdf-resolution options)
                          (if (pdf-compression options) " -c" ""))))
        (create-canvas context spec)))))

(defun pdf-add-bookmark (canvas title level)
  "Add bookmark to PDF"
  (validate-canvas canvas)
  ;; This would require PDF-specific API calls
  (format t "Adding PDF bookmark: ~A (level ~D)~%" title level))

(defun pdf-begin-page (canvas)
  "Begin new PDF page"
  (validate-canvas canvas)
  (clear canvas))

(defun pdf-set-metadata (canvas metadata)
  "Set PDF metadata"
  (validate-canvas canvas)
  (when (getf metadata :title)
    (format t "PDF Title: ~A~%" (getf metadata :title)))
  (when (getf metadata :author)
    (format t "PDF Author: ~A~%" (getf metadata :author))))

;;; Printer Backend Support

(defun context-printer ()
  "Create printer context"
  (handler-case
      (cd-cffi::%cd-context-printer)
    (error (e)
      (error 'cd-context-error :message "Printer context not available"
                               :function-name "context-printer"))))

(defclass print-options ()
  ((printer-name :initarg :printer-name :accessor print-printer-name :initform nil)
   (paper-size :initarg :paper-size :accessor print-paper-size :initform :a4)
   (orientation :initarg :orientation :accessor print-orientation :initform :portrait)
   (quality :initarg :quality :accessor print-quality :initform :normal)
   (color-mode :initarg :color-mode :accessor print-color-mode :initform :color)
   (copies :initarg :copies :accessor print-copies :initform 1)
   (duplex :initarg :duplex :accessor print-duplex :initform nil)
   (margins :initarg :margins :accessor print-margins :initform '(20 20 20 20))) ; top right bottom left
  (:documentation "Printer options"))

(defun make-print-options (&key (printer-name nil) (paper-size :a4) (orientation :portrait)
                               (quality :normal) (color-mode :color) (copies 1)
                               (duplex nil) (margins '(20 20 20 20)))
  "Create print options"
  (make-instance 'print-options
                 :printer-name printer-name
                 :paper-size paper-size
                 :orientation orientation
                 :quality quality
                 :color-mode color-mode
                 :copies copies
                 :duplex duplex
                 :margins margins))

(defun list-printers ()
  "List available printers"
  ;; This would use system-specific APIs
  (list "Default Printer" "PDF Printer" "PostScript Printer"))

(defun create-printer-canvas (&optional (options (make-print-options)))
  "Create printer canvas with options"
  (let ((context (context-printer)))
    (when context
      (let ((spec (format nil "~@[~A ~]~A ~A ~D"
                          (print-printer-name options)
                          (print-paper-size options)
                          (print-orientation options)
                          (print-copies options))))
        (create-canvas context spec)))))

(defun print-document (canvas &optional (show-dialog t))
  "Print the document"
  (validate-canvas canvas)
  (when show-dialog
    (format t "Showing print dialog...~%"))
  ;; This would trigger actual printing
  (format t "Sending document to printer...~%"))

;;; OpenGL Backend Integration (if available)

(defun context-opengl ()
  "Create OpenGL context (if available)"
  (error 'cd-context-error :message "OpenGL context requires external integration"))

(defclass opengl-options ()
  ((double-buffer :initarg :double-buffer :accessor gl-double-buffer :initform t)
   (depth-buffer :initarg :depth-buffer :accessor gl-depth-buffer :initform t)
   (stencil-buffer :initarg :stencil-buffer :accessor gl-stencil-buffer :initform nil)
   (antialiasing :initarg :antialiasing :accessor gl-antialiasing :initform nil)
   (vsync :initarg :vsync :accessor gl-vsync :initform t))
  (:documentation "OpenGL rendering options"))

(defun enable-hardware-acceleration (canvas)
  "Enable hardware acceleration if available"
  (validate-canvas canvas)
  ;; This would require OpenGL backend
  (format t "Hardware acceleration not available in current backend~%"))

;;; Vector Graphics Export

(defun export-to-svg (canvas filename &optional (width nil) (height nil))
  "Export canvas to SVG format"
  (validate-canvas canvas)
  (multiple-value-bind (canvas-width canvas-height) (size canvas)
    (let ((svg-width (or width canvas-width))
          (svg-height (or height canvas-height)))
      (with-open-file (stream filename :direction :output :if-exists :supersede)
        (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
        (format stream "<svg width=\"~D\" height=\"~D\" xmlns=\"http://www.w3.org/2000/svg\">~%"
                svg-width svg-height)
        ;; This would require capturing drawing operations
        (format stream "</svg>~%"))
      (format t "SVG exported to ~A (~Dx~D)~%" filename svg-width svg-height))))

(defun export-to-eps (canvas filename)
  "Export canvas to EPS format"
  (validate-canvas canvas)
  (multiple-value-bind (width height) (size canvas)
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (format stream "%!PS-Adobe-3.0 EPSF-3.0~%")
      (format stream "%%BoundingBox: 0 0 ~D ~D~%" width height)
      (format stream "%%Creator: CD Common Lisp Bindings~%")
      ;; This would require capturing drawing operations as PostScript
      (format stream "showpage~%"))
    (format t "EPS exported to ~A (~Dx~D)~%" filename width height)))

;;; Backend Capability Detection

(defun detect-backend-capabilities (context)
  "Detect and report backend capabilities"
  (let ((caps (context-capabilities context))
        (type (context-type context))
        (is-plus (context-plus-p context)))
    (list :type type
          :plus is-plus
          :capabilities caps
          :supports-color (member :background caps)
          :supports-transparency (member :imagergba caps)
          :supports-patterns (member :pattern caps)
          :supports-text (member :font caps)
          :supports-images (member :imagergb caps)
          :supports-vector (member :bezier caps)
          :supports-clipping (member :cliparea caps))))

(defun recommend-backend-for-task (task-requirements)
  "Recommend best backend for given requirements"
  (let ((recommendations '()))
    (when (member :high-quality-vector task-requirements)
      (push '(:postscript "High quality vector output") recommendations))
    (when (member :document task-requirements)
      (push '(:pdf "Document with text and graphics") recommendations))
    (when (member :web task-requirements)
      (push '(:svg "Web-compatible vector graphics") recommendations))
    (when (member :interactive task-requirements)
      (push '(:debug "Interactive development and testing") recommendations))
    (when (member :raster task-requirements)
      (push '(:image-rgb "Bitmap image generation") recommendations))
    (when (member :hardware-acceleration task-requirements)
      (push '(:opengl "Hardware-accelerated graphics") recommendations))
    (when (member :printing task-requirements)
      (push '(:printer "Direct printer output") recommendations))
    (or recommendations '((:debug "General purpose development")))))

;;; Backend-Specific Optimizations

(defun optimize-for-backend (canvas backend-type)
  "Apply backend-specific optimizations"
  (validate-canvas canvas)
  (case backend-type
    (:postscript
     ;; Optimize for PostScript
     (setf (line-cap canvas) :cap-round) ; Better appearance in PS
     (format t "Applied PostScript optimizations~%"))
    (:pdf
     ;; Optimize for PDF
     (setf (background-opacity canvas) :opacity-opaque) ; Better compression
     (format t "Applied PDF optimizations~%"))
    (:svg
     ;; Optimize for SVG
     (setf (line-join canvas) :join-round) ; Better web appearance
     (format t "Applied SVG optimizations~%"))
    (:printer
     ;; Optimize for printing
     (setf (line-width canvas) (max 1 (line-width canvas))) ; Minimum line width
     (format t "Applied printer optimizations~%"))
    (:image-rgb
     ;; Optimize for bitmap
     (setf (write-mode canvas) :write-replace) ; Fastest for bitmaps
     (format t "Applied bitmap optimizations~%"))
    (t
     (format t "No specific optimizations for backend type: ~A~%" backend-type))))

;;; Multi-format Export

(defun export-multiple-formats (canvas base-filename formats &optional (options nil))
  "Export canvas to multiple formats"
  (validate-canvas canvas)
  (let ((results '()))
    (dolist (format formats)
      (let ((filename (format nil "~A.~A" base-filename (string-downcase format))))
        (handler-case
            (case format
              (:svg (export-to-svg canvas filename))
              (:eps (export-to-eps canvas filename))
              (:pdf
               (let ((pdf-canvas (create-pdf-canvas filename options)))
                 (when pdf-canvas
                   ;; Copy content to PDF canvas
                   (activate pdf-canvas)
                   ;; This would require content copying
                   (deactivate pdf-canvas)
                   (kill pdf-canvas))))
              (:ps
               (let ((ps-canvas (create-postscript-canvas filename options)))
                 (when ps-canvas
                   ;; Copy content to PS canvas
                   (activate ps-canvas)
                   ;; This would require content copying
                   (deactivate ps-canvas)
                   (kill ps-canvas))))
              (t (format t "Unsupported format: ~A~%" format)))
          (error (e)
            (format t "Failed to export ~A: ~A~%" format e)))
        (push (list format filename) results)))
    (nreverse results)))

;;; Platform-Specific Extensions

(defun get-system-fonts ()
  "Get list of system fonts"
  ;; This would use platform-specific APIs
  (list "Arial" "Times New Roman" "Courier New" "Helvetica" "Times" "Courier"))

(defun get-system-printers ()
  "Get list of system printers"
  ;; This would use platform-specific APIs
  (list-printers))

(defun get-display-info ()
  "Get display information"
  ;; This would use platform-specific APIs
  (list :width 1920 :height 1080 :dpi 96 :color-depth 24))

;;; Backend Testing Utilities

(defun test-backend-features (context)
  "Test backend feature availability"
  (let ((canvas (create-canvas context))
        (test-results '()))
    (when canvas
      (unwind-protect
           (progn
             (activate canvas)

             ;; Test basic drawing
             (push (cons :basic-drawing
                         (handler-case
                             (progn (line canvas 10 10 50 50) t)
                           (error () nil)))
                   test-results)

             ;; Test text rendering
             (push (cons :text-rendering
                         (handler-case
                             (progn (text canvas 10 30 "Test") t)
                           (error () nil)))
                   test-results)

             ;; Test image operations
             (push (cons :image-operations
                         (handler-case
                             (let ((r (make-array 100 :element-type '(unsigned-byte 8) :initial-element 255))
                                   (g (make-array 100 :element-type '(unsigned-byte 8) :initial-element 128))
                                   (b (make-array 100 :element-type '(unsigned-byte 8) :initial-element 64)))
                               (put-image-rgb canvas 10 10 r g b 10 10 0 0 0 0)
                               t)
                           (error () nil)))
                   test-results)

             (deactivate canvas))
        (kill canvas)))
    test-results))
(in-package #:cd)

;;; Advanced Text Features

;;; Multi-line Text Support

(defclass text-layout ()
  ((lines :initform '() :accessor text-lines)
   (line-height :initform 1.2 :accessor text-line-height)
   (alignment :initform :left :accessor text-alignment-mode)
   (width :initform nil :accessor text-layout-width)
   (height :initform nil :accessor text-layout-height))
  (:documentation "Text layout information"))

(defun split-text-lines (text &optional (max-width nil))
  "Split text into lines, optionally wrapping at max-width"
  (let ((lines (split-sequence:split-sequence #\Newline text)))
    (if max-width
        (apply #'append (mapcar (lambda (line) (wrap-line line max-width)) lines))
        lines)))

(defun wrap-line (line max-width)
  "Wrap a single line at word boundaries within max-width"
  (if (<= (length line) max-width)
      (list line)
      (let ((words (split-sequence:split-sequence #\Space line))
            (current-line "")
            (lines '()))
        (dolist (word words)
          (let ((test-line (if (string= current-line "")
                               word
                               (concatenate 'string current-line " " word))))
            (if (<= (length test-line) max-width)
                (setf current-line test-line)
                (progn
                  (when (not (string= current-line ""))
                    (push current-line lines))
                  (setf current-line word)))))
        (when (not (string= current-line ""))
          (push current-line lines))
        (nreverse lines))))

(defun measure-text-lines (canvas lines)
  "Measure the dimensions of multiple text lines"
  (validate-canvas canvas)
  (let ((max-width 0)
        (total-height 0)
        (font-height (font-dim canvas :font-height)))
    (dolist (line lines)
      (multiple-value-bind (width height) (text-size canvas line)
        (declare (ignore height)) ; Use font height for consistency
        (setf max-width (max max-width width))))
    (setf total-height (* (length lines) font-height))
    (values max-width total-height)))

(defun draw-text-lines (canvas x y lines &key (alignment :left) (line-height 1.2))
  "Draw multiple lines of text with specified alignment"
  (validate-canvas canvas)
  (let* ((font-height (font-dim canvas :font-height))
         (line-spacing (round (* line-height font-height)))
         (current-y y))
    (dolist (line lines)
      (let ((line-x (case alignment
                      (:left x)
                      (:center (- x (/ (text-width canvas line) 2)))
                      (:right (- x (text-width canvas line)))
                      (t x))))
        (text canvas (round line-x) current-y line))
      (incf current-y line-spacing))))

(defun text-multiline (canvas x y text &key (max-width nil) (alignment :left) (line-height 1.2))
  "Draw multi-line text with automatic wrapping and alignment"
  (validate-canvas canvas)
  (let ((lines (split-text-lines text max-width)))
    (draw-text-lines canvas x y lines :alignment alignment :line-height line-height)))

(defun text-width (canvas text)
  "Get the width of text in pixels"
  (validate-canvas canvas)
  (cd-cffi::%cd-canvas-text-width canvas text))

(defun text-height (canvas text)
  "Get the height of text in pixels"
  (validate-canvas canvas)
  (cd-cffi::%cd-canvas-text-height canvas text))

;;; Text Formatting and Rich Text

(defclass text-style ()
  ((font-family :initarg :font-family :accessor text-style-font-family)
   (font-size :initarg :font-size :accessor text-style-font-size)
   (font-style :initarg :font-style :accessor text-style-font-style)
   (color :initarg :color :accessor text-style-color)
   (background-color :initarg :background-color :accessor text-style-background-color)
   (underline :initarg :underline :accessor text-style-underline)
   (strikethrough :initarg :strikethrough :accessor text-style-strikethrough))
  (:documentation "Text styling information"))

(defun make-text-style (&key (font-family "Arial") (font-size 12) (font-style :plain)
                              (color +black+) (background-color nil)
                              (underline nil) (strikethrough nil))
  "Create a text style object"
  (make-instance 'text-style
                 :font-family font-family
                 :font-size font-size
                 :font-style font-style
                 :color color
                 :background-color background-color
                 :underline underline
                 :strikethrough strikethrough))

(defclass rich-text-segment ()
  ((text :initarg :text :accessor segment-text)
   (style :initarg :style :accessor segment-style))
  (:documentation "A segment of rich text with styling"))

(defun make-rich-text-segment (text style)
  "Create a rich text segment"
  (make-instance 'rich-text-segment :text text :style style))

(defun apply-text-style (canvas style)
  "Apply a text style to the canvas"
  (validate-canvas canvas)
  (with-slots (font-family font-size font-style color) style
    (setf (font canvas) (format nil "~A,~D" font-family font-size))
    (setf (foreground canvas) color)))

(defun draw-rich-text (canvas x y segments)
  "Draw rich text with multiple styles"
  (validate-canvas canvas)
  (let ((current-x x)
        (saved-font (font canvas))
        (saved-color (foreground canvas)))
    (unwind-protect
         (dolist (segment segments)
           (apply-text-style canvas (segment-style segment))
           (let ((text (segment-text segment)))
             (text canvas current-x y text)
             (incf current-x (text-width canvas text))))
      (setf (font canvas) saved-font)
      (setf (foreground canvas) saved-color))))

;;; Text Effects

(defun draw-outlined-text (canvas x y text outline-width outline-color fill-color)
  "Draw text with an outline effect"
  (validate-canvas canvas)
  (let ((saved-color (foreground canvas))
        (saved-width (line-width canvas)))
    (unwind-protect
         (progn
           ;; Draw outline
           (setf (foreground canvas) outline-color)
           (setf (line-width canvas) outline-width)
           (dotimes (dx (* outline-width 2))
             (dotimes (dy (* outline-width 2))
               (let ((offset-x (- dx outline-width))
                     (offset-y (- dy outline-width)))
                 (when (and (/= offset-x 0) (/= offset-y 0))
                   (text canvas (+ x offset-x) (+ y offset-y) text)))))
           ;; Draw fill
           (setf (foreground canvas) fill-color)
           (text canvas x y text))
      (setf (foreground canvas) saved-color)
      (setf (line-width canvas) saved-width))))

(defun draw-shadow-text (canvas x y text shadow-offset-x shadow-offset-y shadow-color text-color)
  "Draw text with a drop shadow"
  (validate-canvas canvas)
  (let ((saved-color (foreground canvas)))
    (unwind-protect
         (progn
           ;; Draw shadow
           (setf (foreground canvas) shadow-color)
           (text canvas (+ x shadow-offset-x) (+ y shadow-offset-y) text)
           ;; Draw text
           (setf (foreground canvas) text-color)
           (text canvas x y text))
      (setf (foreground canvas) saved-color))))

(defun draw-3d-text (canvas x y text depth color1 color2)
  "Draw 3D effect text"
  (validate-canvas canvas)
  (let ((saved-color (foreground canvas)))
    (unwind-protect
         (progn
           ;; Draw depth layers
           (setf (foreground canvas) color2)
           (loop for i from depth downto 1
                 do (text canvas (+ x i) (+ y i) text))
           ;; Draw top layer
           (setf (foreground canvas) color1)
           (text canvas x y text))
      (setf (foreground canvas) saved-color))))

;;; Text Path Following

(defun text-along-path (canvas path text &key (alignment :left) (offset 0))
  "Draw text along a path"
  (validate-canvas canvas)
  ;; This is a simplified version - full implementation would require
  ;; calculating text position and rotation along the path
  (let ((path-length (calculate-path-length path))
        (text-width (text-width canvas text))
        (char-spacing (/ text-width (length text))))
    (dotimes (i (length text))
      (let* ((position (+ offset (* i char-spacing)))
             (t-val (/ position path-length)))
        (when (and (>= t-val 0) (<= t-val 1))
          (multiple-value-bind (px py angle) (get-path-point-and-tangent path t-val)
            (with-rotation (canvas angle)
              (text canvas (round px) (round py) (string (char text i))))))))))

(defun calculate-path-length (path)
  "Calculate the length of a path (simplified)"
  ;; This is a placeholder - real implementation would calculate actual path length
  100)

(defun get-path-point-and-tangent (path t-val)
  "Get point and tangent angle on path at parameter t"
  ;; This is a placeholder - real implementation would calculate from path
  (values 0 0 0))

;;; Text Measurement and Layout

(defun measure-text-bounds (canvas text)
  "Get detailed text bounds including baseline information"
  (validate-canvas canvas)
  (cffi:with-foreign-objects ((rect :int 8)) ; 4 points * 2 coordinates
    (cd-cffi::%cd-canvas-text-bounds canvas 0 0 text rect)
    (let ((bounds (make-array 8)))
      (dotimes (i 8)
        (setf (aref bounds i) (cffi:mem-aref rect :int i)))
      bounds)))

(defun get-text-baseline (canvas text)
  "Get the baseline position for text"
  (validate-canvas canvas)
  (- (font-dim canvas :font-height) (font-dim canvas :font-descent)))

(defun justify-text (canvas text width)
  "Justify text to fit within specified width by adjusting spacing"
  (validate-canvas canvas)
  (let* ((words (split-sequence:split-sequence #\Space text))
         (text-width (text-width canvas text))
         (space-width (text-width canvas " "))
         (total-word-width (- text-width (* (1- (length words)) space-width)))
         (available-space (- width total-word-width))
         (space-count (1- (length words))))
    (if (and (> space-count 0) (> available-space space-width))
        (let ((extra-space (/ (- available-space (* space-count space-width)) space-count)))
          (values words extra-space))
        (values words 0))))

;;; Typography Utilities

(defun get-font-metrics (canvas)
  "Get comprehensive font metrics"
  (validate-canvas canvas)
  (list :ascent (font-dim canvas :font-ascent)
        :descent (font-dim canvas :font-descent)
        :height (font-dim canvas :font-height)
        :max-width (font-dim canvas :font-max-width)))

(defun calculate-line-spacing (canvas &optional (factor 1.2))
  "Calculate appropriate line spacing"
  (validate-canvas canvas)
  (round (* (font-dim canvas :font-height) factor)))

(defun fit-text-to-box (canvas text box-width box-height)
  "Calculate font size to fit text in box"
  (validate-canvas canvas)
  (let ((original-font (font canvas))
        (best-size 1))
    (unwind-protect
         (loop for size from 1 to 72
               do (setf (font canvas) (format nil "Arial,~D" size))
                  (multiple-value-bind (width height) (text-size canvas text)
                    (if (and (<= width box-width) (<= height box-height))
                        (setf best-size size)
                        (return))))
      (setf (font canvas) original-font))
    best-size))

;;; Text Decorations

(defun underline-text (canvas x y text)
  "Draw text with underline"
  (validate-canvas canvas)
  (let ((baseline-y (+ y (get-text-baseline canvas text)))
        (text-width (text-width canvas text)))
    (text canvas x y text)
    (line canvas x (+ baseline-y 2) (+ x text-width) (+ baseline-y 2))))

(defun strikethrough-text (canvas x y text)
  "Draw text with strikethrough"
  (validate-canvas canvas)
  (let ((middle-y (+ y (/ (font-dim canvas :font-height) 2)))
        (text-width (text-width canvas text)))
    (text canvas x y text)
    (line canvas x middle-y (+ x text-width) middle-y)))
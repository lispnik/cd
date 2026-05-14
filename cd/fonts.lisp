(in-package #:cd)

;;; Advanced Font Management

(export '(font
          font-typeface
          font-style
          font-size
          font-dimensions
          text-extent
          text-metrics
          native-font
          available-fonts))

(defun (setf font) (font-spec canvas)
  "Set font using typeface, style, and size.
   Font-spec can be:
   - (typeface style size) - list of three elements
   - string - native font specification"
  (with-validation 'set-font
    (validate-canvas canvas 'set-font))

  (etypecase font-spec
    (string
     ;; Native font specification
     (with-cd-error-checking ('set-font :canvas canvas)
       (cd-cffi::%cd-canvas-native-font canvas font-spec)))
    (list
     ;; (typeface style size) specification
     (unless (= (length font-spec) 3)
       (error 'cd-parameter-error
              :parameter 'font-spec
              :value font-spec
              :operation 'set-font
              :expected "(typeface style size) or native font string"))
     (destructuring-bind (typeface style size) font-spec
       (with-validation 'set-font
         (validate-string typeface 'typeface 'set-font)
         (validate-coordinate size 'size 'set-font :min 1))
       (with-cd-error-checking ('set-font :canvas canvas)
         (cd-cffi::%cd-canvas-font canvas typeface style (round size))))))
  font-spec)

(defun font (canvas)
  "Get current font specification as (typeface style size)."
  (with-validation 'get-font
    (validate-canvas canvas 'get-font))

  (with-cd-error-checking ('get-font :canvas canvas)
    (cffi:with-foreign-objects ((typeface-ptr :pointer)
                               (style-ptr 'cd-cffi::font-style)
                               (size-ptr :int))
      (cd-cffi::%cd-canvas-get-font canvas typeface-ptr style-ptr size-ptr)
      (list (cffi:mem-ref typeface-ptr :string)
            (cffi:mem-ref style-ptr 'cd-cffi::font-style)
            (cffi:mem-ref size-ptr :int)))))

(defun font-typeface (canvas)
  "Get current font typeface."
  (first (font canvas)))

(defun font-style (canvas)
  "Get current font style."
  (second (font canvas)))

(defun font-size (canvas)
  "Get current font size."
  (third (font canvas)))

(defun (setf font-typeface) (typeface canvas)
  "Set font typeface, preserving style and size."
  (let ((current-font (font canvas)))
    (setf (font canvas) (list typeface (second current-font) (third current-font)))
    typeface))

(defun (setf font-style) (style canvas)
  "Set font style, preserving typeface and size."
  (let ((current-font (font canvas)))
    (setf (font canvas) (list (first current-font) style (third current-font)))
    style))

(defun (setf font-size) (size canvas)
  "Set font size, preserving typeface and style."
  (let ((current-font (font canvas)))
    (setf (font canvas) (list (first current-font) (second current-font) size))
    size))

(defun native-font (canvas &optional font-string)
  "Get or set native font specification."
  (with-validation 'native-font
    (validate-canvas canvas 'native-font)
    (when font-string
      (validate-string font-string 'font-string 'native-font)))

  (with-cd-error-checking ('native-font :canvas canvas)
    (if font-string
        (progn
          (cd-cffi::%cd-canvas-native-font canvas font-string)
          font-string)
        (cd-cffi::%cd-canvas-native-font canvas (cffi:null-pointer)))))

(defun font-dimensions (canvas)
  "Get font dimensions: max-width, height, ascent, descent."
  (with-validation 'font-dimensions
    (validate-canvas canvas 'font-dimensions))

  (with-cd-error-checking ('font-dimensions :canvas canvas)
    (cffi:with-foreign-objects ((max-width-ptr :int)
                               (height-ptr :int)
                               (ascent-ptr :int)
                               (descent-ptr :int))
      (cd-cffi::%cd-canvas-get-font-dimensions canvas max-width-ptr height-ptr
                                               ascent-ptr descent-ptr)
      (values (cffi:mem-ref max-width-ptr :int)
              (cffi:mem-ref height-ptr :int)
              (cffi:mem-ref ascent-ptr :int)
              (cffi:mem-ref descent-ptr :int)))))

(defun text-extent (canvas text)
  "Get text extent as width and height."
  (with-validation 'text-extent
    (validate-canvas canvas 'text-extent)
    (validate-string text 'text 'text-extent))

  (with-cd-error-checking ('text-extent :canvas canvas)
    (cffi:with-foreign-objects ((width-ptr :int)
                               (height-ptr :int))
      (cd-cffi::%cd-canvas-get-text-size canvas text width-ptr height-ptr)
      (values (cffi:mem-ref width-ptr :int)
              (cffi:mem-ref height-ptr :int)))))

(defun text-metrics (canvas text x y)
  "Get comprehensive text metrics including bounding box.
   Returns: width, height, xmin, xmax, ymin, ymax, baseline-y."
  (with-validation 'text-metrics
    (validate-canvas canvas 'text-metrics)
    (validate-string text 'text 'text-metrics)
    (validate-coordinate x 'x 'text-metrics)
    (validate-coordinate y 'y 'text-metrics))

  (with-cd-error-checking ('text-metrics :canvas canvas)
    (multiple-value-bind (width height) (text-extent canvas text)
      (cffi:with-foreign-objects ((xmin-ptr :int)
                                 (xmax-ptr :int)
                                 (ymin-ptr :int)
                                 (ymax-ptr :int))
        (cd-cffi::%cd-canvas-get-text-box canvas x y text
                                         xmin-ptr xmax-ptr ymin-ptr ymax-ptr)
        (multiple-value-bind (max-width font-height ascent descent)
            (font-dimensions canvas)
          (declare (ignore max-width))
          (values width height
                  (cffi:mem-ref xmin-ptr :int)
                  (cffi:mem-ref xmax-ptr :int)
                  (cffi:mem-ref ymin-ptr :int)
                  (cffi:mem-ref ymax-ptr :int)
                  (- y descent)))))))

;; World coordinate versions
(defun (setf wd:font) (font-spec canvas)
  "Set font using world coordinates for size."
  (etypecase font-spec
    (list
     (destructuring-bind (typeface style size) font-spec
       (with-cd-error-checking ('set-wd-font :canvas canvas)
         (cd-cffi::%wd-canvas-font canvas typeface style (coerce size 'double-float)))))
    (string
     (setf (font canvas) font-spec)))
  font-spec)

(defun wd:text-extent (canvas text)
  "Get text extent in world coordinates."
  (with-validation 'wd-text-extent
    (validate-canvas canvas 'wd-text-extent)
    (validate-string text 'text 'wd-text-extent))

  (with-cd-error-checking ('wd-text-extent :canvas canvas)
    (cffi:with-foreign-objects ((width-ptr :double)
                               (height-ptr :double))
      (cd-cffi::%wd-canvas-get-text-size canvas text width-ptr height-ptr)
      (values (cffi:mem-ref width-ptr :double)
              (cffi:mem-ref height-ptr :double)))))

(defun wd:font-dimensions (canvas)
  "Get font dimensions in world coordinates."
  (with-validation 'wd-font-dimensions
    (validate-canvas canvas 'wd-font-dimensions))

  (with-cd-error-checking ('wd-font-dimensions :canvas canvas)
    (cffi:with-foreign-objects ((max-width-ptr :double)
                               (height-ptr :double)
                               (ascent-ptr :double)
                               (descent-ptr :double))
      (cd-cffi::%wd-canvas-get-font-dimensions canvas max-width-ptr height-ptr
                                               ascent-ptr descent-ptr)
      (values (cffi:mem-ref max-width-ptr :double)
              (cffi:mem-ref height-ptr :double)
              (cffi:mem-ref ascent-ptr :double)
              (cffi:mem-ref descent-ptr :double)))))

;; Export to WD package
(export '(font text-extent font-dimensions) (find-package "WD"))
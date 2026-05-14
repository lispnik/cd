(in-package #:cd)

;;; Advanced Image Operations

;;; Image Filtering

(deftype kernel ()
  "A convolution kernel for image filtering"
  '(simple-array single-float (* *)))

(defun make-kernel (size values)
  "Create a convolution kernel"
  (let ((kernel (make-array (list size size) :element-type 'single-float)))
    (dotimes (i size)
      (dotimes (j size)
        (setf (aref kernel i j) (coerce (aref values (+ (* i size) j)) 'single-float))))
    kernel))

(defun blur-kernel (size sigma)
  "Create a Gaussian blur kernel"
  (let ((kernel (make-array (list size size) :element-type 'single-float))
        (center (floor size 2))
        (sum 0.0))
    ;; Generate Gaussian kernel
    (dotimes (i size)
      (dotimes (j size)
        (let* ((x (- i center))
               (y (- j center))
               (value (exp (- (/ (+ (* x x) (* y y)) (* 2 sigma sigma))))))
          (setf (aref kernel i j) value)
          (incf sum value))))
    ;; Normalize kernel
    (dotimes (i size)
      (dotimes (j size)
        (setf (aref kernel i j) (/ (aref kernel i j) sum))))
    kernel))

(defun sharpen-kernel ()
  "Create a sharpening kernel"
  (make-kernel 3 #(0 -1 0 -1 5 -1 0 -1 0)))

(defun edge-detection-kernel ()
  "Create an edge detection kernel"
  (make-kernel 3 #(-1 -1 -1 -1 8 -1 -1 -1 -1)))

(defun emboss-kernel ()
  "Create an emboss kernel"
  (make-kernel 3 #(-2 -1 0 -1 1 1 0 1 2)))

(defun apply-convolution-filter (r-in g-in b-in width height kernel)
  "Apply convolution filter to RGB image data"
  (let* ((size (array-dimension kernel 0))
         (offset (floor size 2))
         (r-out (make-array (* width height) :element-type '(unsigned-byte 8)))
         (g-out (make-array (* width height) :element-type '(unsigned-byte 8)))
         (b-out (make-array (* width height) :element-type '(unsigned-byte 8))))

    (dotimes (y height)
      (dotimes (x width)
        (let ((r-sum 0.0) (g-sum 0.0) (b-sum 0.0))
          ;; Apply kernel
          (dotimes (ky size)
            (dotimes (kx size)
              (let ((sy (+ y (- ky offset)))
                    (sx (+ x (- kx offset))))
                ;; Handle edge cases with clamping
                (setf sy (max 0 (min (1- height) sy)))
                (setf sx (max 0 (min (1- width) sx)))
                (let ((index (+ (* sy width) sx))
                      (kernel-val (aref kernel ky kx)))
                  (incf r-sum (* (aref r-in index) kernel-val))
                  (incf g-sum (* (aref g-in index) kernel-val))
                  (incf b-sum (* (aref b-in index) kernel-val))))))

          ;; Clamp results and store
          (let ((index (+ (* y width) x)))
            (setf (aref r-out index) (max 0 (min 255 (round r-sum))))
            (setf (aref g-out index) (max 0 (min 255 (round g-sum))))
            (setf (aref b-out index) (max 0 (min 255 (round b-sum))))))))

    (values r-out g-out b-out)))

(defun filter-image-rgb (canvas x y width height filter-type &optional (param 1.0))
  "Apply filter to image region and put it back"
  (validate-canvas canvas)
  (multiple-value-bind (r-in g-in b-in) (get-image-rgb canvas x y width height)
    (let ((kernel (ecase filter-type
                    (:blur (blur-kernel 5 param))
                    (:sharpen (sharpen-kernel))
                    (:edge-detect (edge-detection-kernel))
                    (:emboss (emboss-kernel)))))
      (multiple-value-bind (r-out g-out b-out)
          (apply-convolution-filter r-in g-in b-in width height kernel)
        (put-image-rgb canvas width height r-out g-out b-out x y 0 0 0 0)))))

;;; Image Compositing

(deftype blend-mode ()
  "Image blending modes"
  '(member :normal :multiply :screen :overlay :soft-light :hard-light
           :color-dodge :color-burn :darken :lighten :difference :exclusion))

(defun blend-colors (c1 c2 mode alpha)
  "Blend two colors using specified blend mode"
  (declare (type (integer 0 255) c1 c2)
           (type (real 0 1) alpha)
           (type blend-mode mode))
  (let ((result (ecase mode
                  (:normal c2)
                  (:multiply (round (/ (* c1 c2) 255)))
                  (:screen (- 255 (round (/ (* (- 255 c1) (- 255 c2)) 255))))
                  (:overlay (if (< c1 128)
                                (round (/ (* 2 c1 c2) 255))
                                (- 255 (round (/ (* 2 (- 255 c1) (- 255 c2)) 255)))))
                  (:darken (min c1 c2))
                  (:lighten (max c1 c2))
                  (:difference (abs (- c1 c2)))
                  (:exclusion (round (- 255 (/ (* 2 c1 c2) 255)))))))
    (round (+ (* c1 (- 1 alpha)) (* result alpha)))))

(defun composite-images (r1 g1 b1 r2 g2 b2 width height mode alpha)
  "Composite two images using specified blend mode"
  (declare (type blend-mode mode)
           (type (real 0 1) alpha))
  (let ((r-out (make-array (* width height) :element-type '(unsigned-byte 8)))
        (g-out (make-array (* width height) :element-type '(unsigned-byte 8)))
        (b-out (make-array (* width height) :element-type '(unsigned-byte 8))))

    (dotimes (i (* width height))
      (setf (aref r-out i) (blend-colors (aref r1 i) (aref r2 i) mode alpha))
      (setf (aref g-out i) (blend-colors (aref g1 i) (aref g2 i) mode alpha))
      (setf (aref b-out i) (blend-colors (aref b1 i) (aref b2 i) mode alpha)))

    (values r-out g-out b-out)))

(defun alpha-blend-images (r1 g1 b1 a1 r2 g2 b2 a2 width height)
  "Alpha blend two RGBA images"
  (let ((r-out (make-array (* width height) :element-type '(unsigned-byte 8)))
        (g-out (make-array (* width height) :element-type '(unsigned-byte 8)))
        (b-out (make-array (* width height) :element-type '(unsigned-byte 8)))
        (a-out (make-array (* width height) :element-type '(unsigned-byte 8))))

    (dotimes (i (* width height))
      (let* ((alpha1 (/ (aref a1 i) 255.0))
             (alpha2 (/ (aref a2 i) 255.0))
             (alpha-out (+ alpha2 (* alpha1 (- 1 alpha2))))
             (inv-alpha-out (if (> alpha-out 0) (/ 1.0 alpha-out) 0)))

        (if (> alpha-out 0)
            (progn
              (setf (aref r-out i) (round (* inv-alpha-out
                                             (+ (* (aref r2 i) alpha2)
                                                (* (aref r1 i) alpha1 (- 1 alpha2))))))
              (setf (aref g-out i) (round (* inv-alpha-out
                                             (+ (* (aref g2 i) alpha2)
                                                (* (aref g1 i) alpha1 (- 1 alpha2))))))
              (setf (aref b-out i) (round (* inv-alpha-out
                                             (+ (* (aref b2 i) alpha2)
                                                (* (aref b1 i) alpha1 (- 1 alpha2))))))
              (setf (aref a-out i) (round (* alpha-out 255))))
            (progn
              (setf (aref r-out i) 0)
              (setf (aref g-out i) 0)
              (setf (aref b-out i) 0)
              (setf (aref a-out i) 0)))))

    (values r-out g-out b-out a-out)))

;;; Color Space Transformations

(defun rgb-to-grayscale (r-in g-in b-in)
  "Convert RGB image to grayscale"
  (let ((gray (make-array (array-total-size r-in) :element-type '(unsigned-byte 8))))
    (dotimes (i (array-total-size r-in))
      (setf (aref gray i)
            (round (+ (* 0.299 (aref r-in i))
                      (* 0.587 (aref g-in i))
                      (* 0.114 (aref b-in i))))))
    gray))

(defun adjust-brightness (r-in g-in b-in adjustment)
  "Adjust image brightness"
  (declare (type (real -1 1) adjustment))
  (let ((factor (+ 1 adjustment))
        (r-out (make-array (array-total-size r-in) :element-type '(unsigned-byte 8)))
        (g-out (make-array (array-total-size g-in) :element-type '(unsigned-byte 8)))
        (b-out (make-array (array-total-size b-in) :element-type '(unsigned-byte 8))))

    (dotimes (i (array-total-size r-in))
      (setf (aref r-out i) (max 0 (min 255 (round (* (aref r-in i) factor)))))
      (setf (aref g-out i) (max 0 (min 255 (round (* (aref g-in i) factor)))))
      (setf (aref b-out i) (max 0 (min 255 (round (* (aref b-in i) factor))))))

    (values r-out g-out b-out)))

(defun adjust-contrast (r-in g-in b-in adjustment)
  "Adjust image contrast"
  (declare (type (real -1 1) adjustment))
  (let ((factor (+ 1 adjustment))
        (r-out (make-array (array-total-size r-in) :element-type '(unsigned-byte 8)))
        (g-out (make-array (array-total-size g-in) :element-type '(unsigned-byte 8)))
        (b-out (make-array (array-total-size b-in) :element-type '(unsigned-byte 8))))

    (dotimes (i (array-total-size r-in))
      (setf (aref r-out i) (max 0 (min 255 (round (+ (* (- (aref r-in i) 128) factor) 128)))))
      (setf (aref g-out i) (max 0 (min 255 (round (+ (* (- (aref g-in i) 128) factor) 128)))))
      (setf (aref b-out i) (max 0 (min 255 (round (+ (* (- (aref b-in i) 128) factor) 128))))))

    (values r-out g-out b-out)))

(defun adjust-gamma (r-in g-in b-in gamma)
  "Adjust image gamma"
  (declare (type (real 0.1 3.0) gamma))
  (let ((inv-gamma (/ 1.0 gamma))
        (r-out (make-array (array-total-size r-in) :element-type '(unsigned-byte 8)))
        (g-out (make-array (array-total-size g-in) :element-type '(unsigned-byte 8)))
        (b-out (make-array (array-total-size b-in) :element-type '(unsigned-byte 8))))

    (dotimes (i (array-total-size r-in))
      (setf (aref r-out i) (round (* 255 (expt (/ (aref r-in i) 255.0) inv-gamma))))
      (setf (aref g-out i) (round (* 255 (expt (/ (aref g-in i) 255.0) inv-gamma))))
      (setf (aref b-out i) (round (* 255 (expt (/ (aref b-in i) 255.0) inv-gamma)))))

    (values r-out g-out b-out)))

;;; Image Scaling and Resampling

(defun scale-image-bilinear (r-in g-in b-in width height new-width new-height)
  "Scale image using bilinear interpolation"
  (let ((r-out (make-array (* new-width new-height) :element-type '(unsigned-byte 8)))
        (g-out (make-array (* new-width new-height) :element-type '(unsigned-byte 8)))
        (b-out (make-array (* new-width new-height) :element-type '(unsigned-byte 8)))
        (x-ratio (/ width new-width))
        (y-ratio (/ height new-height)))

    (dotimes (y new-height)
      (dotimes (x new-width)
        (let* ((src-x (* x x-ratio))
               (src-y (* y y-ratio))
               (x1 (floor src-x))
               (y1 (floor src-y))
               (x2 (min (1- width) (1+ x1)))
               (y2 (min (1- height) (1+ y1)))
               (dx (- src-x x1))
               (dy (- src-y y1)))

          ;; Bilinear interpolation
          (flet ((interpolate (data)
                   (let ((p11 (aref data (+ (* y1 width) x1)))
                         (p12 (aref data (+ (* y1 width) x2)))
                         (p21 (aref data (+ (* y2 width) x1)))
                         (p22 (aref data (+ (* y2 width) x2))))
                     (round (+ (* p11 (- 1 dx) (- 1 dy))
                               (* p12 dx (- 1 dy))
                               (* p21 (- 1 dx) dy)
                               (* p22 dx dy))))))

            (let ((out-index (+ (* y new-width) x)))
              (setf (aref r-out out-index) (interpolate r-in))
              (setf (aref g-out out-index) (interpolate g-in))
              (setf (aref b-out out-index) (interpolate b-in)))))))

    (values r-out g-out b-out)))

;;; Image Analysis

(defun image-histogram (data)
  "Calculate histogram of image data"
  (let ((histogram (make-array 256 :element-type 'integer :initial-element 0)))
    (dotimes (i (array-total-size data))
      (incf (aref histogram (aref data i))))
    histogram))

(defun image-statistics (data)
  "Calculate basic statistics of image data"
  (let ((sum 0) (min-val 255) (max-val 0) (count (array-total-size data)))
    (dotimes (i count)
      (let ((val (aref data i)))
        (incf sum val)
        (setf min-val (min min-val val))
        (setf max-val (max max-val val))))
    (let ((mean (/ sum count)))
      (list :mean mean :min min-val :max max-val :count count))))

;;; High-level Image Processing Functions

(defun blur-image (canvas x y width height &optional (sigma 1.0))
  "Apply blur filter to image region"
  (validate-canvas canvas)
  (filter-image-rgb canvas x y width height :blur sigma))

(defun sharpen-image (canvas x y width height)
  "Apply sharpen filter to image region"
  (validate-canvas canvas)
  (filter-image-rgb canvas x y width height :sharpen))

(defun edge-detect-image (canvas x y width height)
  "Apply edge detection to image region"
  (validate-canvas canvas)
  (filter-image-rgb canvas x y width height :edge-detect))

(defun emboss-image (canvas x y width height)
  "Apply emboss effect to image region"
  (validate-canvas canvas)
  (filter-image-rgb canvas x y width height :emboss))

(defun composite-image-region (canvas src-x src-y dest-x dest-y width height mode &optional (alpha 0.5))
  "Composite one image region onto another"
  (validate-canvas canvas)
  (multiple-value-bind (r1 g1 b1) (get-image-rgb canvas dest-x dest-y width height)
    (multiple-value-bind (r2 g2 b2) (get-image-rgb canvas src-x src-y width height)
      (multiple-value-bind (r-out g-out b-out)
          (composite-images r1 g1 b1 r2 g2 b2 width height mode alpha)
        (put-image-rgb canvas width height r-out g-out b-out dest-x dest-y 0 0 0 0)))))

(defun adjust-image-levels (canvas x y width height brightness contrast gamma)
  "Adjust image levels (brightness, contrast, gamma)"
  (validate-canvas canvas)
  (multiple-value-bind (r-in g-in b-in) (get-image-rgb canvas x y width height)
    (multiple-value-bind (r-out g-out b-out)
        (adjust-gamma
         (multiple-value-list
          (adjust-contrast
           (multiple-value-list (adjust-brightness r-in g-in b-in brightness))
           contrast))
         gamma)
      (put-image-rgb canvas width height r-out g-out b-out x y 0 0 0 0))))

;;; Image I/O Support (placeholder - would require external libraries)

(defun save-canvas-as-png (canvas filename &optional (x 0) (y 0) (width nil) (height nil))
  "Save canvas region as PNG file (placeholder)"
  (validate-canvas canvas)
  (multiple-value-bind (canvas-width canvas-height) (size canvas)
    (let ((save-width (or width canvas-width))
          (save-height (or height canvas-height)))
      (multiple-value-bind (r g b) (get-image-rgb canvas x y save-width save-height)
        ;; This would require integration with a PNG library
        (format t "Would save ~Dx~D PNG to ~A~%" save-width save-height filename)
        (values r g b)))))

(defun load-png-to-canvas (canvas filename x y)
  "Load PNG file to canvas (placeholder)"
  (validate-canvas canvas)
  ;; This would require integration with a PNG library
  (format t "Would load PNG from ~A to (~D,~D)~%" filename x y)
  (values 0 0))
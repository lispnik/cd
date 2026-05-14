(in-package #:cd)

;;; Advanced Drawing Primitives

;;; Path Operations

(defvar *current-path* nil
  "Current path being constructed")

(defclass path ()
  ((actions :initform '() :accessor path-actions)
   (points :initform '() :accessor path-points))
  (:documentation "Represents a drawing path"))

(defun begin-path ()
  "Begin a new path"
  (setf *current-path* (make-instance 'path)))

(defun end-path (canvas mode)
  "End the current path and render it"
  (declare (type (member :fill :stroke :fill-stroke :clip) mode))
  (validate-canvas canvas)
  (when *current-path*
    (render-path canvas *current-path* mode)
    (setf *current-path* nil)))

(defun path-move-to (x y)
  "Move to point without drawing"
  (declare (type integer x y))
  (when *current-path*
    (push :move-to (path-actions *current-path*))
    (push (list x y) (path-points *current-path*))))

(defun path-line-to (x y)
  "Draw line to point"
  (declare (type integer x y))
  (when *current-path*
    (push :line-to (path-actions *current-path*))
    (push (list x y) (path-points *current-path*))))

(defun path-curve-to (x1 y1 x2 y2 x3 y3)
  "Draw cubic bezier curve"
  (declare (type integer x1 y1 x2 y2 x3 y3))
  (when *current-path*
    (push :curve-to (path-actions *current-path*))
    (push (list x1 y1 x2 y2 x3 y3) (path-points *current-path*))))

(defun path-arc-to (xc yc w h a1 a2)
  "Add arc to path"
  (declare (type integer xc yc w h a1 a2))
  (when *current-path*
    (push :arc-to (path-actions *current-path*))
    (push (list xc yc w h a1 a2) (path-points *current-path*))))

(defun path-close ()
  "Close the current path"
  (when *current-path*
    (push :close (path-actions *current-path*))))

(defun render-path (canvas path mode)
  "Render a path to the canvas"
  (declare (type path path))
  (validate-canvas canvas)
  (cd-cffi::%cd-canvas-path-set canvas (ecase mode
                                         (:fill (cffi:foreign-enum-value 'cd-cffi:path-action :path-new))
                                         (:stroke (cffi:foreign-enum-value 'cd-cffi:path-action :path-new))
                                         (:fill-stroke (cffi:foreign-enum-value 'cd-cffi:path-action :path-new))
                                         (:clip (cffi:foreign-enum-value 'cd-cffi:path-action :path-new))))

  ;; Execute path actions
  (loop for action in (reverse (path-actions path))
        for points in (reverse (path-points path))
        do (case action
             (:move-to
              (cd-cffi::%cd-canvas-path-set canvas (cffi:foreign-enum-value 'cd-cffi:path-action :path-move-to)))
             (:line-to
              (cd-cffi::%cd-canvas-path-set canvas (cffi:foreign-enum-value 'cd-cffi:path-action :path-line-to)))
             (:curve-to
              (cd-cffi::%cd-canvas-path-set canvas (cffi:foreign-enum-value 'cd-cffi:path-action :path-curve-to)))
             (:arc-to
              (cd-cffi::%cd-canvas-path-set canvas (cffi:foreign-enum-value 'cd-cffi:path-action :path-arc)))
             (:close
              (cd-cffi::%cd-canvas-path-set canvas (cffi:foreign-enum-value 'cd-cffi:path-action :path-close)))))

  ;; Finish path
  (cd-cffi::%cd-canvas-path-set canvas (cffi:foreign-enum-value 'cd-cffi:path-action
                                                                (ecase mode
                                                                  (:fill :path-fill)
                                                                  (:stroke :path-stroke)
                                                                  (:fill-stroke :path-fill-stroke)
                                                                  (:clip :path-clip)))))

(defmacro with-path ((canvas mode) &body body)
  "Execute body building a path, then render it"
  `(progn
     (begin-path)
     (unwind-protect
          (progn ,@body)
       (end-path ,canvas ,mode))))

;;; Spline Functions

(defun spline (canvas points)
  "Draw a spline through the given points"
  (validate-canvas canvas)
  (validate-parameter 'points points 'arrayp)
  (let ((n (/ (length points) 2)))
    (cffi:with-foreign-object (foreign-points :int (length points))
      (dotimes (i (length points))
        (setf (cffi:mem-aref foreign-points :int i) (aref points i)))
      (cd-cffi::%cd-canvas-spline canvas foreign-points n))))

(defun smooth-curve (canvas points &optional (tension 0.5))
  "Draw a smooth curve through points using cardinal splines"
  (declare (type (real 0 1) tension))
  (validate-canvas canvas)
  (when (< (length points) 6) ; Need at least 3 points (6 coordinates)
    (error 'cd-parameter-error :message "Smooth curve requires at least 3 points"))

  (let ((spline-points (make-array (+ (length points) 4) :fill-pointer 0)))
    ;; Add control points for smooth curve
    (vector-push (aref points 0) spline-points) ; Duplicate first point
    (vector-push (aref points 1) spline-points)
    (loop for i from 0 below (length points)
          do (vector-push (aref points i) spline-points))
    (vector-push (aref points (- (length points) 2)) spline-points) ; Duplicate last point
    (vector-push (aref points (- (length points) 1)) spline-points)

    (spline canvas spline-points)))

;;; Advanced Bezier Functions

(defun bezier-curve (canvas points)
  "Draw bezier curve through control points"
  (validate-canvas canvas)
  (validate-parameter 'points points 'arrayp)
  (let ((n (/ (length points) 2)))
    (cffi:with-foreign-object (foreign-points :int (length points))
      (dotimes (i (length points))
        (setf (cffi:mem-aref foreign-points :int i) (aref points i)))
      (cd-cffi::%cd-canvas-bezier canvas foreign-points n))))

(defun cubic-bezier (canvas x1 y1 x2 y2 x3 y3 x4 y4)
  "Draw cubic bezier curve with explicit control points"
  (declare (type integer x1 y1 x2 y2 x3 y3 x4 y4))
  (validate-canvas canvas)
  (let ((points (vector x1 y1 x2 y2 x3 y3 x4 y4)))
    (bezier-curve canvas points)))

(defun quadratic-bezier (canvas x1 y1 x2 y2 x3 y3)
  "Draw quadratic bezier curve (converted to cubic)"
  (declare (type integer x1 y1 x2 y2 x3 y3))
  ;; Convert quadratic to cubic bezier
  (let ((cx1 (+ x1 (* 2/3 (- x2 x1))))
        (cy1 (+ y1 (* 2/3 (- y2 y1))))
        (cx2 (+ x3 (* 2/3 (- x2 x3))))
        (cy2 (+ y3 (* 2/3 (- y2 y3)))))
    (cubic-bezier canvas x1 y1 (round cx1) (round cy1) (round cx2) (round cy2) x3 y3)))

;;; Gradient Support (simulated for backends that don't support it natively)

(defclass gradient ()
  ((type :initarg :type :accessor gradient-type)
   (colors :initarg :colors :accessor gradient-colors)
   (stops :initarg :stops :accessor gradient-stops)
   (start-point :initarg :start-point :accessor gradient-start-point)
   (end-point :initarg :end-point :accessor gradient-end-point)
   (center :initarg :center :accessor gradient-center)
   (radius :initarg :radius :accessor gradient-radius))
  (:documentation "Gradient definition"))

(defun make-linear-gradient (x1 y1 x2 y2 colors &optional (stops nil))
  "Create a linear gradient"
  (make-instance 'gradient
                 :type :linear
                 :start-point (list x1 y1)
                 :end-point (list x2 y2)
                 :colors colors
                 :stops (or stops (loop for i from 0 below (length colors)
                                        collect (/ i (1- (length colors)))))))

(defun make-radial-gradient (cx cy radius colors &optional (stops nil))
  "Create a radial gradient"
  (make-instance 'gradient
                 :type :radial
                 :center (list cx cy)
                 :radius radius
                 :colors colors
                 :stops (or stops (loop for i from 0 below (length colors)
                                        collect (/ i (1- (length colors)))))))

(defun gradient-fill-rectangle (canvas gradient x y width height &optional (steps 20))
  "Fill rectangle with gradient (simulated)"
  (validate-canvas canvas)
  (let ((saved-style (interior-style canvas)))
    (unwind-protect
         (progn
           (setf (interior-style canvas) :interior-solid)
           (ecase (gradient-type gradient)
             (:linear (simulate-linear-gradient canvas gradient x y width height steps))
             (:radial (simulate-radial-gradient canvas gradient x y width height steps))))
      (setf (interior-style canvas) saved-style))))

(defun simulate-linear-gradient (canvas gradient x y width height steps)
  "Simulate linear gradient by drawing multiple rectangles"
  (let* ((colors (gradient-colors gradient))
         (stops (gradient-stops gradient))
         (step-height (/ height steps)))
    (dotimes (i steps)
      (let* ((t-val (/ i steps))
             (color (interpolate-gradient-color colors stops t-val))
             (rect-y (+ y (* i step-height))))
        (setf (foreground canvas) color)
        (box canvas x (round rect-y) width (ceiling step-height))))))

(defun simulate-radial-gradient (canvas gradient x y width height steps)
  "Simulate radial gradient by drawing concentric shapes"
  (let* ((colors (gradient-colors gradient))
         (stops (gradient-stops gradient))
         (center-x (+ x (/ width 2)))
         (center-y (+ y (/ height 2)))
         (max-radius (sqrt (+ (* width width) (* height height)))))
    (loop for i from steps downto 0
          for t-val = (/ i steps)
          for radius = (* t-val max-radius)
          for color = (interpolate-gradient-color colors stops t-val)
          do (setf (foreground canvas) color)
             (sector canvas center-x center-y (* 2 (round radius)) (* 2 (round radius)) 0 360))))

(defun interpolate-gradient-color (colors stops t-val)
  "Interpolate color at position t-val in gradient"
  (cond
    ((= (length colors) 1) (first colors))
    ((<= t-val (first stops)) (first colors))
    ((>= t-val (first (last stops))) (first (last colors)))
    (t (loop for i from 0 below (1- (length stops))
             when (and (<= (nth i stops) t-val) (< t-val (nth (1+ i) stops)))
               do (let* ((local-t (/ (- t-val (nth i stops))
                                    (- (nth (1+ i) stops) (nth i stops))))
                         (color1 (nth i colors))
                         (color2 (nth (1+ i) colors)))
                    (return (interpolate-colors color1 color2 local-t)))
             finally (return (first (last colors)))))))

(defun interpolate-colors (color1 color2 t-val)
  "Interpolate between two colors"
  (multiple-value-bind (r1 g1 b1) (decode-color color1)
    (multiple-value-bind (r2 g2 b2) (decode-color color2)
      (encode-color (round (+ r1 (* t-val (- r2 r1))))
                    (round (+ g1 (* t-val (- g2 g1))))
                    (round (+ b1 (* t-val (- b2 b1))))))))

;;; Geometric Shape Helpers

(defun draw-rounded-rectangle (canvas x y width height radius)
  "Draw a rounded rectangle"
  (validate-canvas canvas)
  (with-path (canvas :stroke)
    (path-move-to (+ x radius) y)
    (path-line-to (+ x width (- radius)) y)
    (path-arc-to (+ x width (- radius)) (+ y radius) (* radius 2) (* radius 2) 270 360)
    (path-line-to (+ x width) (+ y height (- radius)))
    (path-arc-to (+ x width (- radius)) (+ y height (- radius)) (* radius 2) (* radius 2) 0 90)
    (path-line-to (+ x radius) (+ y height))
    (path-arc-to (+ x radius) (+ y height (- radius)) (* radius 2) (* radius 2) 90 180)
    (path-line-to x (+ y radius))
    (path-arc-to (+ x radius) (+ y radius) (* radius 2) (* radius 2) 180 270)
    (path-close)))

(defun draw-star (canvas cx cy outer-radius inner-radius points)
  "Draw a star shape"
  (validate-canvas canvas)
  (with-path (canvas :stroke)
    (dotimes (i (* points 2))
      (let* ((angle (* i pi (/ 1.0 points)))
             (radius (if (evenp i) outer-radius inner-radius))
             (x (+ cx (* radius (cos angle))))
             (y (+ cy (* radius (sin angle)))))
        (if (= i 0)
            (path-move-to (round x) (round y))
            (path-line-to (round x) (round y)))))
    (path-close)))

(defun draw-regular-polygon (canvas cx cy radius sides)
  "Draw a regular polygon"
  (validate-canvas canvas)
  (with-path (canvas :stroke)
    (dotimes (i sides)
      (let* ((angle (* i 2 pi (/ 1.0 sides)))
             (x (+ cx (* radius (cos angle))))
             (y (+ cy (* radius (sin angle)))))
        (if (= i 0)
            (path-move-to (round x) (round y))
            (path-line-to (round x) (round y)))))
    (path-close)))
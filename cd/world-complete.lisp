(in-package #:wd)

;;; Complete World Coordinate API Implementation

;;; Advanced Drawing Functions in World Coordinates

(defun spline (canvas points)
  "Draw spline in world coordinates"
  (cd:validate-canvas canvas)
  (let ((canvas-points (make-array (length points))))
    (loop for i from 0 below (length points) by 2
          do (multiple-value-bind (cx cy) (world-to-canvas canvas (aref points i) (aref points (1+ i)))
               (setf (aref canvas-points i) cx)
               (setf (aref canvas-points (1+ i)) cy)))
    (cd:spline canvas canvas-points)))

(defun cubic-bezier (canvas x1 y1 x2 y2 x3 y3 x4 y4)
  "Draw cubic bezier in world coordinates"
  (cd:validate-canvas canvas)
  (multiple-value-bind (cx1 cy1) (world-to-canvas canvas x1 y1)
    (multiple-value-bind (cx2 cy2) (world-to-canvas canvas x2 y2)
      (multiple-value-bind (cx3 cy3) (world-to-canvas canvas x3 y3)
        (multiple-value-bind (cx4 cy4) (world-to-canvas canvas x4 y4)
          (cd:cubic-bezier canvas cx1 cy1 cx2 cy2 cx3 cy3 cx4 cy4))))))

(defun quadratic-bezier (canvas x1 y1 x2 y2 x3 y3)
  "Draw quadratic bezier in world coordinates"
  (cd:validate-canvas canvas)
  (multiple-value-bind (cx1 cy1) (world-to-canvas canvas x1 y1)
    (multiple-value-bind (cx2 cy2) (world-to-canvas canvas x2 y2)
      (multiple-value-bind (cx3 cy3) (world-to-canvas canvas x3 y3)
        (cd:quadratic-bezier canvas cx1 cy1 cx2 cy2 cx3 cy3)))))

;;; Geometric Helpers in World Coordinates

(defun draw-rounded-rectangle (canvas x y width height radius)
  "Draw rounded rectangle in world coordinates"
  (cd:validate-canvas canvas)
  (multiple-value-bind (cx cy) (world-to-canvas canvas x y)
    (multiple-value-bind (cx2 cy2) (world-to-canvas canvas (+ x width) (+ y height))
      (let ((cwidth (- cx2 cx))
            (cheight (- cy2 cy)))
        (multiple-value-bind (cr1 cr2) (world-to-canvas canvas radius radius)
          (declare (ignore cr2))
          (cd:draw-rounded-rectangle canvas cx cy cwidth cheight cr1))))))

(defun draw-star (canvas cx cy outer-radius inner-radius points)
  "Draw star in world coordinates"
  (cd:validate-canvas canvas)
  (multiple-value-bind (ccx ccy) (world-to-canvas canvas cx cy)
    (multiple-value-bind (cr1 cr2) (world-to-canvas canvas outer-radius inner-radius)
      (declare (ignore cr2))
      (cd:draw-star canvas ccx ccy cr1 inner-radius points))))

(defun draw-regular-polygon (canvas cx cy radius sides)
  "Draw regular polygon in world coordinates"
  (cd:validate-canvas canvas)
  (multiple-value-bind (ccx ccy) (world-to-canvas canvas cx cy)
    (multiple-value-bind (cr cr2) (world-to-canvas canvas radius radius)
      (declare (ignore cr2))
      (cd:draw-regular-polygon canvas ccx ccy cr sides))))

;;; Image Operations in World Coordinates

(defun put-image-rgb (canvas width height r g b x y sx sy sw sh)
  "Put RGB image at world coordinates"
  (cd:validate-canvas canvas)
  (multiple-value-bind (cx cy) (world-to-canvas canvas x y)
    (cd:put-image-rgb canvas width height r g b cx cy sx sy sw sh)))

(defun put-image-rgba (canvas width height r g b a x y sx sy sw sh)
  "Put RGBA image at world coordinates"
  (cd:validate-canvas canvas)
  (multiple-value-bind (cx cy) (world-to-canvas canvas x y)
    (cd:put-image-rgba canvas width height r g b a cx cy sx sy sw sh)))

(defun get-image-rgb (canvas x y width height)
  "Get RGB image from world coordinates"
  (cd:validate-canvas canvas)
  (multiple-value-bind (cx cy) (world-to-canvas canvas x y)
    (multiple-value-bind (cx2 cy2) (world-to-canvas canvas (+ x width) (+ y height))
      (let ((cwidth (abs (- cx2 cx)))
            (cheight (abs (- cy2 cy))))
        (cd:get-image-rgb canvas cx cy cwidth cheight)))))

(defun put-image-stretch (canvas image x y width height sx sy sw sh)
  "Put server image at world coordinates with scaling"
  (cd:validate-canvas canvas)
  (multiple-value-bind (cx cy) (world-to-canvas canvas x y)
    (multiple-value-bind (cx2 cy2) (world-to-canvas canvas (+ x width) (+ y height))
      (let ((cwidth (abs (- cx2 cx)))
            (cheight (abs (- cy2 cy))))
        (cd:put-image-stretch canvas image cx cy cwidth cheight sx sy sw sh)))))

;;; Advanced Text in World Coordinates

(defun text-multiline (canvas x y text &key (max-width nil) (alignment :left) (line-height 1.2))
  "Draw multiline text at world coordinates"
  (cd:validate-canvas canvas)
  (multiple-value-bind (cx cy) (world-to-canvas canvas x y)
    ;; Convert max-width from world to canvas coordinates if provided
    (let ((canvas-max-width
            (when max-width
              (multiple-value-bind (cx2 cy2) (world-to-canvas canvas (+ x max-width) y)
                (declare (ignore cy2))
                (abs (- cx2 cx))))))
      (cd:text-multiline canvas cx cy text
                         :max-width canvas-max-width
                         :alignment alignment
                         :line-height line-height))))

(defun text-bounds (canvas x y text)
  "Get text bounds in world coordinates"
  (cd:validate-canvas canvas)
  (multiple-value-bind (cx cy) (world-to-canvas canvas x y)
    (multiple-value-bind (xmin xmax ymin ymax) (cd:text-bounds canvas cx cy text)
      ;; Convert back to world coordinates
      (multiple-value-bind (wx1 wy1) (canvas-to-world canvas xmin ymin)
        (multiple-value-bind (wx2 wy2) (canvas-to-world canvas xmax ymax)
          (values wx1 wx2 wy1 wy2))))))

;;; Path Operations in World Coordinates
;;; Note: These would need to integrate with the path system

(defun path-move-to (x y)
  "Move to point in world coordinates (adds to current path)"
  (when cd:*current-path*
    ;; This would need access to current canvas context
    ;; For now, store world coordinates and convert during rendering
    (cd:path-move-to x y))) ; Placeholder - needs integration

;;; Vector Text Extensions

(defun vector-text-direction (canvas x1 y1 x2 y2)
  "Set vector text direction using world coordinates"
  (cd:validate-canvas canvas)
  (multiple-value-bind (cx1 cy1) (world-to-canvas canvas x1 y1)
    (multiple-value-bind (cx2 cy2) (world-to-canvas canvas x2 y2)
      (cd:vector-text-direction canvas cx1 cy1 cx2 cy2))))

;;; Clipping in World Coordinates

(defun clip (canvas xmin xmax ymin ymax)
  "Set clipping region in world coordinates"
  (cd:validate-canvas canvas)
  (multiple-value-bind (cxmin cymin) (world-to-canvas canvas xmin ymin)
    (multiple-value-bind (cxmax cymax) (world-to-canvas canvas xmax ymax)
      (let ((cx (min cxmin cxmax))
            (cy (min cymin cymax))
            (cwidth (abs (- cxmax cxmin)))
            (cheight (abs (- cymax cymin))))
        (cd:clip canvas cx cy cwidth cheight)))))

;;; Coordinate System Utilities

(defun set-world-bounds (canvas xmin xmax ymin ymax)
  "Set world coordinate bounds (alias for world-set)"
  (world-set canvas xmin xmax ymin ymax))

(defun get-world-bounds (canvas)
  "Get world coordinate bounds (alias for world-get)"
  (world-get canvas))

(defun world-width (canvas)
  "Get world coordinate system width"
  (multiple-value-bind (xmin xmax ymin ymax) (world-get canvas)
    (declare (ignore ymin ymax))
    (- xmax xmin)))

(defun world-height (canvas)
  "Get world coordinate system height"
  (multiple-value-bind (xmin xmax ymin ymax) (world-get canvas)
    (declare (ignore xmin xmax))
    (- ymax ymin)))

;;; Measurement Functions in World Coordinates

(defun distance (x1 y1 x2 y2)
  "Calculate distance between two world coordinate points"
  (sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1)))))

(defun point-in-world-rect-p (x y rect-x rect-y rect-width rect-height)
  "Test if point is inside world coordinate rectangle"
  (and (>= x rect-x)
       (<= x (+ rect-x rect-width))
       (>= y rect-y)
       (<= y (+ rect-y rect-height))))

;;; Animation Support in World Coordinates

(defun animate-world-point (canvas start-x start-y end-x end-y duration current-time)
  "Animate a point in world coordinates"
  (let ((t-val (min 1.0 (/ current-time duration))))
    (values (cd:linear-interpolation start-x end-x t-val)
            (cd:linear-interpolation start-y end-y t-val))))
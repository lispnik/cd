(in-package #:cd)

(export '(clip region-combine-mode))

(defun (setf clip) (new-mode canvas)
  "Activates or deactivates clipping. Returns the previous status. Values:

:CLIP-AREA
:CLIP-POLYGON
:CLIP-REGION
:CLIP-OFF (default)

The value :CLIP-AREA activates a rectangular area as the clipping region.

The value :CLIP-POLYGON activates a polygon as a clipping region, but works only
in some drivers (please refer to the notes of each driver). The clipping polygon
must be defined before activating the polygon clipping; if it is not defined,
the current clipping state remains unchanged. See the documentation of
CD:BEGIN/CD:VERTEX/CD:END to create a polygon.

The value :CLIP-REGION activates a complex clipping region. See the
documentation of regions.

The defined clipping area, polygon and complex regions are stored internally, so
you may define them independently and switch between area, polygon and complex
region without having to define them again. Also if the active clipping region
is re-defined it immediately becomes the current clipping region."
  (cd-cffi::%cd-canvas-clip canvas new-mode)
  new-mode)

(defun clip (canvas)
  "Returns the current clip mode."
  (cd-cffi::%cd-canvas-clip canvas cd-cffi::+cd-query+))

(defwrappers ("canvas-clip-area" "clip-area")
  (defun (setf ^function-name) (canvas xmin xmax ymin ymax)
    "Defines the current rectangle for clipping. Only the points in the interval
xmin <= x <= xmax and ymin <= y <= ymax will be printed. Default region: (0,
w-1, 0, h-1)."
    (^cffi-function-name
     canvas
     (coerce xmin ^type)
     (coerce xmax ^type)
     (coerce ymin ^type)
     (coerce ymax ^type))))

(defwrappers ("canvas-get-clip-area" "clip-area")
  (defun ^function-name (canvas)
    "Returns the rectangle and the clipping status as values xmin, xmax, ymin
and ymax."
    (cffi:with-foreign-objects
	((xmin-ptr ^cffi-type)
	 (xmax-ptr ^cffi-type)
	 (ymin-ptr ^cffi-type)
	 (ymax-ptr ^cffi-type))
      (^cffi-function-name canvas xmin-ptr xmax-ptr ymin-ptr ymax-ptr)
      (values (cffi:mem-ref xmin-ptr ^cffi-type)
	      (cffi:mem-ref xmax-ptr ^cffi-type)
	      (cffi:mem-ref ymin-ptr ^cffi-type)
	      (cffi:mem-ref ymax-ptr ^cffi-type)))))

(defun (setf region-combine-mode) (new-mode canvas)
  "Changes the way regions are combined when created. Values:

:COMBINE-UNION (default)
:COMBINE-INTERSECT
:COMBINE-DIFFERENCE
:COMBINE-NOT-INTERSECT

FIXME include nice picture http://webserver2.tecgraf.puc-rio.br/cd/img/regions.gif"
  (cd-cffi::%cd-canvas-region-combine-mode canvas new-mode)
  new-mode)

(defun region-combine-mode (canvas)
  "Returns the current region combine mode."
  (cd-cffi::%cd-canvas-region-combine-mode canvas :query))

(defwrappers ("canvas-is-point-in-region" "point-in-region-p" "cd")
  (defun ^function-name (canvas x y)
    "Returns T if the point is contained inside the current region."
    (^cffi-function-name canvas (coerce x ^type) (coerce y ^type))))

(defwrappers ("canvas-offset-region" "offset-region" "cd")
  (define-cd-setf-expander ^function-name ^cffi-function-name 2 ^type
    "Moves the current region by the given offset values x and y.

In X-Windows, if the region moves to outside the canvas border, the part moved
outside will be lost, the region will need to be reconstructed."))

(defwrappers ("canvas-get-region-box" "region-box" "cd")
  (defun ^function-name (canvas)
    "Returns the rectangle of the bounding box of the current region as values
xmin, xmax, ymin, ymax"
    (cffi:with-foreign-objects
	((xmin-ptr ^cffi-type)
	 (xmax-ptr ^cffi-type)
	 (ymin-ptr ^cffi-type)
	 (ymax-ptr ^cffi-type))
      (^cffi-function-name canvas xmin-ptr xmax-ptr ymin-ptr ymax-ptr)
      (values (cffi:mem-ref xmin-ptr ^cffi-type)
	      (cffi:mem-ref xmax-ptr ^cffi-type)
	      (cffi:mem-ref ymin-ptr ^cffi-type)
	      (cffi:mem-ref ymax-ptr ^cffi-type)))))

(in-package #:cd)

(export '(mark-type
          line-style
          line-style-dashes
          line-join
          line-cap
          background-opacity
          fill-mode
          interior-style
          hatch
          stipple
          pattern
          begin
          end
          vertex
          path
          call-with-vertexes
          call-with-vertices
          with-vertexes
          with-vertices))

(defwrappers ("canvas-pixel" "pixel")
  (defun ^function-name (canvas x y color)
    "Configures the pixel (x,y) with the color defined by color. It is the
smallest element of the canvas. It depends only on global attributes of the
canvas. It can be very slow on some drivers. Sometimes it is implemented as a
rectangle with size 1x1."
    (^cffi-function-name canvas (coerce x ^type) (coerce y ^type) color)))

(defwrappers ("canvas-mark" "mark")
  (defun ^function-name (canvas x y)
    "Draws a mark in (x,y) using the current foreground color. It is not
possible to use this function between a call to functions cdCanvasBegin and
cdCanvasEnd if the type of mark is set to CD_DIAMOND. If the active driver does
not include this primitive, it will be simulated using other primitives from the
library, such as LINE.

If you will call this function several times in a sequence, then it is
recommended that the application changes the filling and line attributes to
those used by this code:

(setf (interior-style canvas) :interior-solid
      (line-style canvas) :line-continuous
      (line-width canvas) 1)"
    (^cffi-function-name canvas (coerce x ^type) (coerce y ^type))))

(defun (setf mark-type) (new-type canvas)
  "Configures the current mark type for:

:MARK-PLUS
:MARK-STAR (default)
:MARK-CIRCLE
:MARK-X
:MARK-BOX
:MARK-DIAMOND
:MARK-HOLLOW-CIRCLE
:MARK-HOLLOW-BOX
:MARK-HOLLOW-DIAMOND"
  (cd-cffi::%cd-canvas-mark-type canvas new-type))

(defun mark-type (canvas)
  "Returns the current mark type."
  (cd-cffi::%cd-canvas-mark-type canvas :query))

(defwrappers ("canvas-mark-size" "mark-size" "cd")
  (defun (setf ^function-name) (new-size canvas)
    "Configures the mark size in pixels. Returns the previous value. Default
value: 10. Valid width interval: >= 1."
    (^cffi-function-name canvas (coerce new-size ^type)))

  (defun ^function-name (canvas)
    "Returns the current size."
    (^cffi-function-name canvas :query)))

(defwrappers ("canvas-line" "line")
  (defun ^function-name (canvas x1 y1 x2 y2)
    "Draws a line from (x1,y1) to (x2,y2) using the current foreground color and
line width and style. Both points are included in the line."
    (^cffi-function-name canvas
			 (coerce x1 ^type)
			 (coerce y1 ^type)
			 (coerce x2 ^type)
			 (coerce y2 ^type))))

(defwrappers ("canvas-rect" "rect")
  (defun ^function-name (canvas xmin xmax ymin ymax)
    "Draws a rectangle with no filling. All points in the limits of interval
x_min<=x<=x_max, y_min<=y<=y_max will be painted. It is affected by line
attributes and the foreground color. If the active driver does not include this
primitive, it will be simulated using the LINE primitive."
    (^cffi-function-name canvas
			 (coerce xmin ^type)
			 (coerce xmax ^type)
			 (coerce ymin ^type)
			 (coerce ymax ^type))))

(defwrappers ("canvas-arc" "arc")
  (defun ^function-name (canvas xc yc w h angle1 angle2)
    "Draws the arc of an ellipse aligned with the axis, using the current
foreground color and line width and style.

The coordinate (xc,yc) defines the center of the ellipse. Dimensions w and h
define the elliptic axes X and Y, respectively.

Angles angle1 and angle2 are in degrees and oriented counter-clockwise. They
define the arc start and end, but they are not the angle relative to the center,
except when w==h and the ellipse is reduced to a circle. The arc starts at the
point (xc+(w/2)*cos(angle1), yc+(h/2)*sin(angle1)) and ends
at (xc+(w/2)*cos(angle2), yc+(h/2)*sin(angle2)). A complete ellipse can be drawn
using 0 and 360 as the angles. If angle2 is less than angle1 it will be
increased by 360 until it is greater than angle1.

The angles are specified so if the size of the ellipse (w x h) is changed, its
shape is preserved. So the angles relative to the center are dependent from the
ellipse size. The actual angle can be obtained using rangle =
atan2((h/2)*sin(angle), (w/2)*cos(angle)).

To specify the angle in radians, you can use the definition +RADIANS-TO-DEGREES+
to multiply the value in radians before passing the angle to CD.

FIXME include lovely picture http://webserver2.tecgraf.puc-rio.br/cd/img/arc.gif"
    (^cffi-function-name canvas
			 (coerce xc ^type)
			 (coerce yc ^type)
			 (coerce w ^type)
			 (coerce h ^type)
			 (coerce angle1 ^type)
			 (coerce angle2 ^type))))

(defun (setf line-style) (new-style canvas)
  "Configures the line style for: 

:LINE-CONTINUOUS (default)
:LINE-DASHED
:LINE-DOTTED
:LINE-DASH-DOT
:LINE-DASH-DOT-DOT
:LINE-CUSTOM

When :LINE-CUSTOM is used, the LINE-STYLE-DASHES function must be called before
to initialize the custom dashes. The spaces are drawn with the background color,
except when back opacity is transparent then the background is left unchanged.

FIXME include this lovely thing too http://webserver2.tecgraf.puc-rio.br/cd/img/lines.gif"
  (cd-cffi::%cd-canvas-line-style canvas new-style))

(defun line-style (canvas)
  "Returns the current line style."
  (cd-cffi::%cd-canvas-line-style canvas :query))

(defun (setf line-style-dashes) (new-dashes canvas)
  "Defines the custom line style dashes. The first value is the length of the
first dash, the second value is the length of the first space, and so on. For
example: #(10 2 5 2) means dash size 10, space size 2, dash size 5, space size
2, and repeats the pattern. Sizes are in pixels."
  (check-type new-dashes (simple-array (real) 1))
  (let ((dashes-count (length new-dashes)))
    (cffi:with-foreign-object (dashes-ptr :int dashes-count)
      (loop for i below dashes-count
            do (setf (cffi:mem-aref dashes-ptr :int i)
                     (coerce (aref new-dashes i) 'integer)))
      (cd-cffi::%cd-canvas-line-style-dashes canvas dashes-ptr dashes-count))))

(defwrappers ("canvas-line-width" "line-width" "cd") 
  (defun (setf ^function-name) (new-width canvas)
    "Configures the width of the current line (in pixels). Default value:
1. Valid width interval: >= 1. In WC, it configures the current line width in
millimeters."
    (^cffi-function-name canvas (coerce new-width ^type))
    new-width)
  
  (defun ^function-name (canvas)
    "Return the current line width."
    (^cffi-function-name canvas :query)))

(defun (setf line-join) (new-join canvas)
  "Configures the current line style. Values: 

:JOIN-MITER (default)
:JOIN-BEVEL
:JOIN-ROUND

FIXME include lovely picture http://webserver2.tecgraf.puc-rio.br/cd/img/linejoin.gif"
  (cd-cffi::%cd-canvas-line-join canvas new-join)
  new-join)

(defun line-join (canvas)
  "Returns the current line join."
  (cd-cffi::%cd-canvas-line-join canvas :query))

(defun (setf line-cap) (new-cap canvas)
  "Configures the current line style for:

:CAP-FLAT (default)
:CAP-SQUARE
:CAP-ROUND

FIXME include http://webserver2.tecgraf.puc-rio.br/cd/img/linecap.gif"
  (cd-cffi::%cd-canvas-line-cap canvas new-cap)
  new-cap)

(defun line-cap (canvas)
  "Returns the current line cap."
  (cd-cffi::%cd-canvas-line-cap canvas :query))

(defwrappers ("canvas-box" "box")
  (defun ^function-name (canvas xmin xmax ymin ymax)
    "Fills a rectangle according to the current interior style. All points in
the interval x_min<=x<=x_max, y_min<=y<=y_max will be painted. When the interior
style :INTERIOR-HOLLOW is defined, the function behaves like its equivalent
CD:RECT."
    (^cffi-function-name canvas
			 (coerce xmin ^type)
			 (coerce xmax ^type)
			 (coerce ymin ^type)
			 (coerce ymax ^type))))

(defwrappers ("canvas-sector" "sector")
  (defun ^function-name (canvas xc yc w h angle1 angle2)
    "Fills the arc of an ellipse aligned with the axis, according to the current
interior style, in the shape of a pie.

The coordinate (xc,yc) defines the center of the ellipse. Dimensions w and h
define the elliptic axes X and Y, respectively.

Angles angle1 and angle2 are in degrees and oriented counter-clockwise. They
define the arc start and end, but they are not the angle relative to the center,
except when w==h and the ellipse is reduced to a circle. The arc starts at the
point (xc+(w/2)*cos(angle1), yc+(h/2)*sin(angle1)) and ends
at (xc+(w/2)*cos(angle2), yc+(h/2)*sin(angle2)). A complete ellipse can be drawn
using 0 and 360 as the angles.  If angle2 is less than angle1 it will be
increased by 360 until it is greater than angle1.

The angles are specified so if the size of the ellipse (w x h) is changed, its
shape is preserved. So the angles relative to the center are dependent from the
ellipse size. The actual angle can be obtained using rangle =
atan2((h/2)*sin(angle), (w/2)*cos(angle)).

To specify the angle in radians, you can use the definition
CD:RADIANS-TO-DEGREES to multiply the value in radians before passing the angle
to CD.

When the interior style :INTERIOR-HOLLOW is defined, the function behaves like
its equivalent CD:ARC, plus two lines connecting to the center.

;; FIXME include nice picture fro mhttp://webserver2.tecgraf.puc-rio.br/cd/img/sector.gif"
    (^cffi-function-name canvas
			 (coerce xc ^type)
			 (coerce yc ^type)
			 (coerce w ^type)
			 (coerce h ^type)
			 (coerce angle1 ^type)
			 (coerce angle2 ^type))))

(defwrappers ("canvas-chord" "chord")
  (defun ^function-name (canvas xc yc w h angle1 angle2)
    "Fills the arc of an ellipse aligned with the axis, according to the current
interior style, the start and end points of the arc are connected. The
parameters are the same as the CD:SECTOR.

When the interior style :INTERIOR-HOLLOW is defined, the function behaves like
its equivalent CD:ARC, plus a line connecting the arc start and end points.

FIXME picture from http://webserver2.tecgraf.puc-rio.br/cd/img/chord.gif"
    (^cffi-function-name canvas
			 (coerce xc ^type)
			 (coerce yc ^type)
			 (coerce w ^type)
			 (coerce h ^type)
			 (coerce angle1 ^type)
			 (coerce angle2 ^type))))

(defun (setf background-opacity) (new-opacity canvas)
  "Configures the background opacity to filling primitives based on the
foreground and background colors. Note that only when interior style is
:INTERIOR-HATCH or :INTERIOR-STIPPLE that backopacity is used. Values:

:OPACITY-TRANSPARENT (default)
:OPACITY-OPAQUE

If it is opaque the primitive will erase whatever is in the background with the
background color. If it is transparent, only the foreground color is painted. In
some drivers it is always opaque.

FIXME http://webserver2.tecgraf.puc-rio.br/cd/img/opacity.gif"
  (cd-cffi::%cd-canvas-back-opacity canvas new-opacity)
  new-opacity)

(defun background-opacity (canvas)
  "Returns the current opacity."
  (cd-cffi::%cd-canvas-back-opacity canvas :query))

(defun (setf fill-mode) (new-fill-mode canvas)
  "Selects a predefined polygon fill rule. Values: 

:FILL-EVEN-ODD (default)
:FILL-WINDING

FIXME http://webserver2.tecgraf.puc-rio.br/cd/img/fillmode.gif"
  (cd-cffi::%cd-canvas-fill-mode canvas new-fill-mode)
  new-fill-mode)

(defun fill-mode (canvas)
  "Returns the current fill mode."
  (cd-cffi::%cd-canvas-fill-mode canvas :query))

(defun (setf interior-style) (new-interior-style canvas)
  "Configures the current style for the area filling primitives. Values:

:INTERIOR-SOLID (default)
:INTERIOR-HOLLOW
:INTERIOR-HATCH
:INTERIOR-STIPPLE
:INTERIOR-PATTERN 

Note that only :INTERIOR-HATCH and :INTERIOR-STIPPLE are affected by the
backopacity.

If a stipple or a pattern were not defined, when they are selected the state of
the attribute is not changed.

When the style :INTERIOR-HOLLOW is defined, functions BOX and SECTOR behave as
their equivalent RECT and ARC w/ lines and the polygons with style :POLYGON-FILL
behave like :POLYGON-CLOSED-LINES."
  (cd-cffi::%cd-canvas-interior-style canvas new-interior-style)
  new-interior-style)

(defun interior-style (canvas)
  "Returns the current interior style."
  (cd-cffi::%cd-canvas-interior-style canvas :query))

(defun (setf hatch) (new-hatch canvas)
  "Selects a predefined hatch style and sets the interior style to
:INTERIOR-HATCH. Values:

:HATCH-HORIZONTAL (default)
:HATCH-VERTICAL
:HATCH-FORWARD-DIAGONAL
:HATCH-BACKWARD-DIAGONAL
:HATCH-CROSS
:HATCH-DIAGONAL-CROSS

The lines are drawn with the foreground color, and the background is drawn with
the background color if back opacity is opaque. The foreground and background
colors must be set before setting the style. In some drivers it is always
opaque.

FIXME http://webserver2.tecgraf.puc-rio.br/cd/img/hatch.gif"
  (cd-cffi::%cd-canvas-hatch canvas new-hatch)
  new-hatch)

(defun hatch (canvas)
  "Returns the current hatch style."
  (cd-cffi::%cd-canvas-hatch canvas :query))

(defun (setf stipple) (new-stipple canvas)
  "Defines a wxh matrix of zeros (0) and ones (1). The zeros are mapped to the
background color or are transparent, according to the background opacity
attribute. The ones are mapped to the foreground color. The function sets the
interior style to CD_STIPPLE. To avoid having to deal with matrices in C, the
element (i,j) of fgbg is stored as fgbg[j*w+i]. The origin is the left bottom
corner of the image. It does not need to be stored by the application, as it is
internally replicated by the library.  In some drivers is always opaque. The
foreground and background colors must be set before setting the style."
  (check-type new-stipple (simple-array * 2))
  (destructuring-bind (w h)
      (array-dimensions new-stipple)
    (let ((stipple-ptr (cffi:foreign-alloc :unsigned-char :count (* w h))))
      (loop for j below h
	    do (loop for i below w
		     do (setf (cffi:mem-aref stipple-ptr :unsigned-char (+ (* j w) i))
			      (coerce (aref new-stipple i j) 'bit))))
      (cd-cffi::%cd-canvas-stipple canvas w h stipple-ptr))))

(defun stipple (canvas)
  "Returns 2D array representing the stipple or NIL if no stipple is
defined."
  (cffi:with-foreign-objects
      ((w-ptr :int)
       (h-ptr :int))
    (let ((stipple-ptr (cd-cffi::%cd-canvas-get-stipple canvas w-ptr h-ptr)))
      (unless (cffi:null-pointer-p stipple-ptr)
	(let* ((w (cffi:mem-ref w-ptr :int))
	       (h (cffi:mem-ref h-ptr :int))
	       (stipple (make-array (list w h) :element-type 'bit)))
	  (loop for j below w
		do (loop for i below h
			 do (setf (aref stipple i j)
                                  (cffi:mem-aref stipple-ptr :unsigned-char (+ (* w j) i))))))))))

;;; FIXME
;;; implemnent wd:stipple
;;; implement wd:[pattern
;; FIXME TODO
;; pattern
;; maybe some lisp magic like for lua

(defun begin (canvas path-mode)
  "Starts defining a polygon to be drawn (or filled) according to the mode:

:PATH-MODE-CLOSED-LINES
:PATH-MODE-OPEN-LINES
:PATH-MODE-FILL
:PATH-MODE-CLIP
:PATH-MODE-REGION
:PATH-MODE-BEZIER

Do not create embedded polygons, that is, do not call function cdBegin twice
without a call to cdEnd in between.

:PATH-MODE-OPEN-LINES: connects all the points at END. Depends on line width and
line style attributes.

:PATH-MODE-CLOSED-LINES: connects all the points at END and connects the last
point to the first. Depends on line width and line style attributes.

:PATH-MODE-FILL: connects the last point to the first and fills the resulting
polygon according to the current interior style. When the interior style
:INTERIOR-HOLLOW is defined the it behaves as if the mode were
:PATH-MODE-CLOSED-LINES.

:PATH-MODE-CLIP: instead of creating a polygon to be drawn, creates a polygon to
define a polygonal clipping region.

:PATH-MODE-BEZIER: defines the points of a bezier curve. There must be at least
4 points: start, control, control and end. To specify a sequence of curves use 3
more points for each curve: control, control, end, control, control, end,
... The end point is used as start point for the next curve.

:PATH-MODE-REGION: starts the creation of a complex region for clipping. All
calls to BOX, SECTOR, CHORD, filled polygons and TEXT will be composed in a
region for clipping. See regions documentation.

:PATH-MODE-PATH: creates a path composed of several primitives that can be line
draw, filled or used as clipping. Must call (SETF PATH) to configure the action
between sequences of VERTEX"
  (cd-cffi::%cd-canvas-begin canvas path-mode))

(defun end (canvas)
  "Ends the polygon's definition and draws it."
  (cd-cffi::%cd-canvas-end canvas))

(defun (setf path) (new-path-action canvas)
  "Configures the action between sequences of calls to VERTEX. Values can be:

:PATH-ACTION-NEW - creates a new empty path. Useful if more than one path is
configured. BEGIN with :PATH-MODE-PATH already creates a new path.

:PATH-ACTION-MOVETO - moves the current position to the given coordinates. Must
be followed by 1 call to VERTEX or WD:VERTEX.

:PATH-ACTION-LINETO - adds a line to the path from the current position to the
given coordinates. The current position is updated to the given coordinates. If
there is no current position, nothing is connected and only the current position
is updated. Must be followed by 1 call to VERTEX or WD:VERTEX

:PATH-ACTION-ARC - adds an arc to the path. If there is a current position adds
also a line from the current position to the start of the arc. The end of the
arc becomes the current position. Must be followed by 3 calls to VERTEX or
WD:VERTEX. One for the center of the arc (xc,yc), one for the bounding rectangle
size (w,h), and one for the start and end angles (angle1,angle2). Angles are in
degrees and oriented counter-clockwise, but angle2 can be smaller than angle1 to
describe a clockwise arc.

:PATH-ACTION-CURVETO - adds a bezier curve to the path. If there is no current
position, the first point will be used twice. The end point becomes the current
position. Must be followed by 3 calls to VERTEX or WD:VERTEX. Must be first
control point (x1,y1) + second control point (x2,y2) + end point (x3,y3).

:PATH-ACTION-CLOSE - adds a line to the path that connects the last point with
the first point of the path, closing it.

:PATH-ACTION-FILL - fills the path with the current fill attributes, then the
path is discarded.

:PATH-ACTION-STROKE - strokes the path with the current line attributes, then
the path is discarded.

:PATH-ACTION-FILLSTROKE - fills the path with the current fill attributes,
strokes the path with the current line attributes, then the path is discarded.

:PATH-ACTION-CLIP - use the path as a clipping area to be intersected with the
current clipping area, then the path is discarded."
  (cd-cffi::%cd-canvas-path-set canvas new-path-action))

(defwrappers ("canvas-vertex" "vertex" "cdf")
  (defun ^function-name (canvas x y)
    "Adds a vertex to the polygon definition."
    (^cffi-function-name canvas (coerce x ^type) (coerce y ^type))))

(defun call-with-vertices (canvas path-mode func)
  (unwind-protect
       (progn
         (begin canvas path-mode)
         (funcall func canvas))
    (end canvas)))

(setf (fdefinition 'call-with-vertexes) #'call-with-vertices)

(defmacro with-vertices ((canvas path-mode) &body body)
  (check-type canvas symbol)
  `(call-with-vertices  ,canvas ,path-mode #'(lambda (,canvas) ,@body)))

(setf (macro-function 'with-vertexes) (macro-function 'with-vertices))

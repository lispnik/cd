(defpackage #:cd
  (:use #:common-lisp
	#:cffi
	#:alexandria
	#:serapeum)
  (:export #:canvas #:context)
  (:shadow #:box))

(defpackage #:wd
  (:use #:common-lisp
        #:cd-cffi)
  (:export)
  (:shadow #:box)
  (:documentation "Allows the use of a World Coordinate System. In this system you can
attribute coordinates to any unit you want. After you define a window (rectangular region)
in your world, each given coordinate is then mapped to canvas coordinates to draw the
primitives. You can define a viewport in your canvas to change the coordinate mapping from
world to canvas. The image below shows the relation between Window and Viewport.

Window x Viewport

FIXME Insert diagram here

If you want to map coordinates from one system to another, use the WORLD-TO-CANVAS and
CANVAS-TO-WORLD functions.

The quality of the picture depends on the conversion from World to Canvas, so if the
canvas has a small size the picture quality will be poor. To increase picture quality
create a canvas with a larger size, if possible.

All World Coordinate drawing in all drivers are simulated using other CD primitives and do
NOT depend or use the CD:TRANSFORM transformation matrix."))

(in-package #:cd)

(export '(foreground background write-mode))

(defun (setf foreground) (new-foreground canvas)
  "Configures a new current foreground color. This color is used in all
primitives (lines, areas, marks and text). Default value: +BLACK+."
  (cd-cffi::%cd-canvas-set-foreground canvas new-foreground)
  new-foreground)

(defun foreground (canvas)
  "Return the current foreground color."
  (cd-cffi::%cd-canvas-foreground canvas cd-cffi::+cd-query+))

(defun (setf background) (new-background canvas)
  "Configures the new current background color and returns the previous
one. However, it does not automatically change the background of a canvas. For
such, it is necessary to call the CLEAR function. The background color only
makes sense for CLEAR and for primitives affected by the background opacity
attribute. Default value: +WHITE+."
  (cd-cffi::%cd-canvas-set-background canvas new-background)
  new-background)

(defun background (canvas)
  "Returns the current background color."
  (cd-cffi::%cd-canvas-background canvas cd-cffi::+cd-query+))

(defun (setf write-mode) (new-write-mode canvas)
  "Defines the writing type for all drawing primitives:

:WRITE-REPLACE (default)
:WRITE-XOR
:WRITE-NOT-XOR

Note: operation :WRITE-XOR is very useful, because, using white as the
foreground color and drawing the same image twice, you can go back to the
original color, before the drawing. This is commonly used for mouse selection
feedback."
  (cd-cffi::%cd-canvas-write-mode
   canvas new-write-mode)
  new-write-mode)

(defun write-mode (canvas)
  "Returns the current write mode."
  (cd-cffi::%cd-canvas-write-mode canvas :query))

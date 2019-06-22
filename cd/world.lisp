(in-package #:wd)

(export '(window
          viewport
          world-to-canvas
          canvas-to-world
          transform
          translate
          scale
          hardcopy))

(cd::define-cd-setf-expander window cd-cffi::%wd-canvas-window 4 'double-float
  "Configures a window in the world coordinate system to be used to convert
world coordinates (with values in real numbers) into canvas coordinates (with
values in integers). The default window is the size in millimeters of the whole
canvas.")

(defun window (canvas)
  "Queries the current window in the world coordinate system being used to
convert world coordinates into canvas coordinates (and the other way
round). Returns values xmin, xmax, ymin and ymax."
  (cffi:with-foreign-objects
      ((xmin-ptr :double)
       (xmax-ptr :double)
       (ymin-ptr :double)
       (ymax-ptr :double))
    (cd-cffi::%wd-canvas-get-window canvas xmin-ptr xmax-ptr ymin-ptr ymax-ptr)
    (values (cffi:mem-ref xmin-ptr :double)
            (cffi:mem-ref xmax-ptr :double)
            (cffi:mem-ref ymin-ptr :double)
            (cffi:mem-ref ymax-ptr :double))))

(cd::define-cd-setf-expander viewport cd-cffi::%wd-canvas-viewport 4 'integer
  "Configures a viewport in the canvas coordinate system to be used to convert
world coordinates (with values in real numbers) into canvas coordinates (with
values in integers). The default viewport is the whole canvas (0, w-1, 0,
h-1). If the canvas size is changed, the viewport will not be automatically
updated.")

(defun viewport (canvas)
  "Queries the current viewport in the world coordinate system being used to
convert world coordinates into canvas coordinates (and the other way
round). Returns values xmin, xmax, ymin and ymax."
  (cffi:with-foreign-objects
      ((xmin-ptr :int)
       (xmax-ptr :int)
       (ymin-ptr :int)
       (ymax-ptr :int))
    (cd-cffi::%wd-canvas-get-viewport canvas xmin-ptr xmax-ptr ymin-ptr ymax-ptr)
    (values (cffi:mem-ref xmin-ptr :int)
            (cffi:mem-ref xmax-ptr :int)
            (cffi:mem-ref ymin-ptr :int)
            (cffi:mem-ref ymax-ptr :int))))

(defun world-to-canvas (canvas x y)
  "Converts world coordinates into canvas coordinates. Returns x and y
coordinate values for canvas."
  (cffi:with-foreign-objects
      ((x-ptr :int)
       (y-ptr :int))
    (cd-cffi::%wd-canvas-world-to-canvas
     canvas (coerce x 'double-float) (coerce y 'double-float) x-ptr y-ptr)
    (values (cffi:mem-ref x-ptr :int)
            (cffi:mem-ref x-ptr :int))))

(defun canvas-to-world (canvas x y)
  "Converts canvas coordinates into world coordinates. Returns x and y
coordinate values for world."
  (cffi:with-foreign-objects
      ((x-ptr :double)
       (y-ptr :double))
    (cd-cffi::%wd-canvas-canvas-to-world
     canvas (coerce x 'integer) (coerce y 'integer) x-ptr y-ptr)
    (values (cffi:mem-ref x-ptr :double)
            (cffi:mem-ref x-ptr :double))))

(defun (setf transform) ()
  "Configures the world coordinate system transformation to be used to convert
world coordinates (with values in real numbers) into canvas coordinates (with
values in integers). The transformation is automatically set by wdCanvasWindow
and wdCanvasViewport. This has NO relation with cdCanvasTransform.")

(defun transform (canvas x y)
  "Queries the current transformation being used to convert world coordinates
into canvas coordinates (and the other way round).  Returns x and y transform
values for canvas → world."
  (cffi:with-foreign-objects
      ((x-ptr :double)
       (y-ptr :double))
    (cd-cffi::%wd-canvas-get-transform
     canvas (coerce x 'double-float) (coerce y 'double-float) x-ptr y-ptr)
    (values (cffi:mem-ref x-ptr :double)
            (cffi:mem-ref y-ptr :double))))

(defun translate (canvas dtx dty)
  "Translates the transformation by a delta, by adding the given values to the
current tx and ty values."
  (cd-cffi::%wd-canvas-translate
   canvas
   (coerce dtx 'double-float)
   (coerce dty 'double-float)))

(defun scale (canvas dsx dsy)
  "Scales the transformation by a delta, by multiplying the given values by the
current sx and sy values."
  (cd-cffi::%wd-canvas-scale
   canvas
   (coerce dsx 'double-float)
   (coerce dsy 'double-float)))

(defun hardcopy ()
  "Creates a new canvas, prepares Window and Viewport according to the provided
canvas, maintaining the aspect ratio and making the drawing occupy the largest
possible area of the new canvas, calls the drawing function (which must use
routines in WC) and, finally, removes the new canvas.

It is usually used for 'hard copies' of drawings (print equivalent copy). The
most common used contexts are Printer, PS and PDF."
  ;; FIXME
  )

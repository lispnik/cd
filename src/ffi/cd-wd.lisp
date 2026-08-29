;;;; src/ffi/cd-wd.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: wd.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcfun ("wdCanvasPlay" %wd-canvas-play) :int
  (canvas cd-canvas)
  (context cd-context)
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double)
  (data :pointer))

(cffi:defcfun ("wdCanvasWindow" %wd-canvas-window) :void
  (canvas cd-canvas)
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double))

(cffi:defcfun ("wdCanvasGetWindow" %wd-canvas-get-window) :void
  (canvas cd-canvas)
  (xmin :pointer)
  (xmax :pointer)
  (ymin :pointer)
  (ymax :pointer))

(cffi:defcfun ("wdCanvasViewport" %wd-canvas-viewport) :void
  (canvas cd-canvas)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int))

(cffi:defcfun ("wdCanvasGetViewport" %wd-canvas-get-viewport) :void
  (canvas cd-canvas)
  (xmin :pointer)
  (xmax :pointer)
  (ymin :pointer)
  (ymax :pointer))

(cffi:defcfun ("wdCanvasWorld2Canvas" %wd-canvas-world2-canvas) :void
  (canvas cd-canvas)
  (xw :double)
  (yw :double)
  (xv :pointer)
  (yv :pointer))

(cffi:defcfun ("wdCanvasWorld2CanvasSize" %wd-canvas-world2-canvas-size) :void
  (canvas cd-canvas)
  (hw :double)
  (vw :double)
  (hv :pointer)
  (vv :pointer))

(cffi:defcfun ("wdCanvasCanvas2World" %wd-canvas-canvas2-world) :void
  (canvas cd-canvas)
  (xv :int)
  (yv :int)
  (xw :pointer)
  (yw :pointer))

(cffi:defcfun ("wdCanvasSetTransform" %wd-canvas-set-transform) :void
  (canvas cd-canvas)
  (sx :double)
  (sy :double)
  (tx :double)
  (ty :double))

(cffi:defcfun ("wdCanvasGetTransform" %wd-canvas-get-transform) :void
  (canvas cd-canvas)
  (sx :pointer)
  (sy :pointer)
  (tx :pointer)
  (ty :pointer))

(cffi:defcfun ("wdCanvasTranslate" %wd-canvas-translate) :void
  (canvas cd-canvas)
  (dtx :double)
  (dty :double))

(cffi:defcfun ("wdCanvasScale" %wd-canvas-scale) :void
  (canvas cd-canvas)
  (dsx :double)
  (dsy :double))

(cffi:defcfun ("wdCanvasClipArea" %wd-canvas-clip-area) :void
  (canvas cd-canvas)
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double))

(cffi:defcfun ("wdCanvasGetClipArea" %wd-canvas-get-clip-area) :int
  (canvas cd-canvas)
  (xmin :pointer)
  (xmax :pointer)
  (ymin :pointer)
  (ymax :pointer))

(cffi:defcfun ("wdCanvasIsPointInRegion" %wd-canvas-is-point-in-region) :int
  (canvas cd-canvas)
  (x :double)
  (y :double))

(cffi:defcfun ("wdCanvasOffsetRegion" %wd-canvas-offset-region) :void
  (canvas cd-canvas)
  (x :double)
  (y :double))

(cffi:defcfun ("wdCanvasGetRegionBox" %wd-canvas-get-region-box) :void
  (canvas cd-canvas)
  (xmin :pointer)
  (xmax :pointer)
  (ymin :pointer)
  (ymax :pointer))

(cffi:defcfun ("wdCanvasHardcopy" %wd-canvas-hardcopy) :void
  (canvas cd-canvas)
  (ctx cd-context)
  (data :pointer)
  (arg3 :pointer))

(cffi:defcfun ("wdCanvasPixel" %wd-canvas-pixel) :void
  (canvas cd-canvas)
  (x :double)
  (y :double)
  (color :long))

(cffi:defcfun ("wdCanvasMark" %wd-canvas-mark) :void
  (canvas cd-canvas)
  (x :double)
  (y :double))

(cffi:defcfun ("wdCanvasLine" %wd-canvas-line) :void
  (canvas cd-canvas)
  (x1 :double)
  (y1 :double)
  (x2 :double)
  (y2 :double))

(cffi:defcfun ("wdCanvasVertex" %wd-canvas-vertex) :void
  (canvas cd-canvas)
  (x :double)
  (y :double))

(cffi:defcfun ("wdCanvasRect" %wd-canvas-rect) :void
  (canvas cd-canvas)
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double))

(cffi:defcfun ("wdCanvasBox" %wd-canvas-box) :void
  (canvas cd-canvas)
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double))

(cffi:defcfun ("wdCanvasArc" %wd-canvas-arc) :void
  (canvas cd-canvas)
  (xc :double)
  (yc :double)
  (w :double)
  (h :double)
  (angle1 :double)
  (angle2 :double))

(cffi:defcfun ("wdCanvasSector" %wd-canvas-sector) :void
  (canvas cd-canvas)
  (xc :double)
  (yc :double)
  (w :double)
  (h :double)
  (angle1 :double)
  (angle2 :double))

(cffi:defcfun ("wdCanvasChord" %wd-canvas-chord) :void
  (canvas cd-canvas)
  (xc :double)
  (yc :double)
  (w :double)
  (h :double)
  (angle1 :double)
  (angle2 :double))

(cffi:defcfun ("wdCanvasText" %wd-canvas-text) :void
  (canvas cd-canvas)
  (x :double)
  (y :double)
  (s :string))

(cffi:defcfun ("wdCanvasGetImageRGB" %wd-canvas-get-image-rgb) :void
  (canvas cd-canvas)
  (r :pointer)
  (g :pointer)
  (b :pointer)
  (x :double)
  (y :double)
  (iw :int)
  (ih :int))

(cffi:defcfun ("wdCanvasPutImageRectRGB" %wd-canvas-put-image-rect-rgb) :void
  (canvas cd-canvas)
  (iw :int)
  (ih :int)
  (r :pointer)
  (g :pointer)
  (b :pointer)
  (x :double)
  (y :double)
  (w :double)
  (h :double)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int))

(cffi:defcfun ("wdCanvasPutImageRectRGBA" %wd-canvas-put-image-rect-rgba) :void
  (canvas cd-canvas)
  (iw :int)
  (ih :int)
  (r :pointer)
  (g :pointer)
  (b :pointer)
  (a :pointer)
  (x :double)
  (y :double)
  (w :double)
  (h :double)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int))

(cffi:defcfun ("wdCanvasPutImageRectMap" %wd-canvas-put-image-rect-map) :void
  (canvas cd-canvas)
  (iw :int)
  (ih :int)
  (index :pointer)
  (colors :pointer)
  (x :double)
  (y :double)
  (w :double)
  (h :double)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int))

(cffi:defcfun ("wdCanvasPutImageRect" %wd-canvas-put-image-rect) :void
  (canvas cd-canvas)
  (image cd-image)
  (x :double)
  (y :double)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int))

(cffi:defcfun ("wdCanvasPutBitmap" %wd-canvas-put-bitmap) :void
  (canvas cd-canvas)
  (bitmap cd-bitmap)
  (x :double)
  (y :double)
  (w :double)
  (h :double))

(cffi:defcfun ("wdCanvasLineWidth" %wd-canvas-line-width) :double
  (canvas cd-canvas)
  (width :double))

(cffi:defcfun ("wdCanvasFont" %wd-canvas-font) :int
  (canvas cd-canvas)
  (type-face :string)
  (style :int)
  (size :double))

(cffi:defcfun ("wdCanvasGetFont" %wd-canvas-get-font) :void
  (canvas cd-canvas)
  (type-face :pointer)
  (style :pointer)
  (size :pointer))

(cffi:defcfun ("wdCanvasMarkSize" %wd-canvas-mark-size) :double
  (canvas cd-canvas)
  (size :double))

(cffi:defcfun ("wdCanvasGetFontDim" %wd-canvas-get-font-dim) :void
  (canvas cd-canvas)
  (max-width :pointer)
  (height :pointer)
  (ascent :pointer)
  (descent :pointer))

(cffi:defcfun ("wdCanvasGetTextSize" %wd-canvas-get-text-size) :void
  (canvas cd-canvas)
  (s :string)
  (width :pointer)
  (height :pointer))

(cffi:defcfun ("wdCanvasGetTextBox" %wd-canvas-get-text-box) :void
  (canvas cd-canvas)
  (x :double)
  (y :double)
  (s :string)
  (xmin :pointer)
  (xmax :pointer)
  (ymin :pointer)
  (ymax :pointer))

(cffi:defcfun ("wdCanvasGetTextBounds" %wd-canvas-get-text-bounds) :void
  (canvas cd-canvas)
  (x :double)
  (y :double)
  (s :string)
  (rect :pointer))

(cffi:defcfun ("wdCanvasStipple" %wd-canvas-stipple) :void
  (canvas cd-canvas)
  (w :int)
  (h :int)
  (fgbg :pointer)
  (w-mm :double)
  (h-mm :double))

(cffi:defcfun ("wdCanvasPattern" %wd-canvas-pattern) :void
  (canvas cd-canvas)
  (w :int)
  (h :int)
  (color :pointer)
  (w-mm :double)
  (h-mm :double))

(cffi:defcfun ("wdCanvasVectorTextDirection" %wd-canvas-vector-text-direction) :void
  (canvas cd-canvas)
  (x1 :double)
  (y1 :double)
  (x2 :double)
  (y2 :double))

(cffi:defcfun ("wdCanvasVectorTextSize" %wd-canvas-vector-text-size) :void
  (canvas cd-canvas)
  (size-x :double)
  (size-y :double)
  (s :string))

(cffi:defcfun ("wdCanvasGetVectorTextSize" %wd-canvas-get-vector-text-size) :void
  (canvas cd-canvas)
  (s :string)
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("wdCanvasVectorCharSize" %wd-canvas-vector-char-size) :double
  (canvas cd-canvas)
  (size :double))

(cffi:defcfun ("wdCanvasVectorText" %wd-canvas-vector-text) :void
  (canvas cd-canvas)
  (x :double)
  (y :double)
  (s :string))

(cffi:defcfun ("wdCanvasMultiLineVectorText" %wd-canvas-multi-line-vector-text) :void
  (canvas cd-canvas)
  (x :double)
  (y :double)
  (s :string))

(cffi:defcfun ("wdCanvasGetVectorTextBounds" %wd-canvas-get-vector-text-bounds) :void
  (canvas cd-canvas)
  (s :string)
  (x :double)
  (y :double)
  (rect :pointer))

(cffi:defcfun ("wdCanvasGetVectorTextBox" %wd-canvas-get-vector-text-box) :void
  (canvas cd-canvas)
  (x :double)
  (y :double)
  (s :string)
  (xmin :pointer)
  (xmax :pointer)
  (ymin :pointer)
  (ymax :pointer))

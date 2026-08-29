;;;; src/ffi/cd.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: cd.h
;;;; Hand corrections below are expected and are kept; re-run the
;;;; generator into a clean tree and diff.

(in-package #:cd.ffi)

(cffi:defcenum bitmap-type
  :bitmap-type-rgb
  :bitmap-type-map
  (:bitmap-type-rgba #x100))

(cffi:defcenum bitmap-data
  :bitmap-data-ired
  :bitmap-data-igreen
  :bitmap-data-iblue
  :bitmap-data-ialpha
  :bitmap-data-index
  :bitmap-data-colors)

(cffi:defcenum status-report
  (:status-report-error -1)
  (:status-report-ok 0))

(cffi:defcenum clip-mode
  :clip-mode-clipoff
  :clip-mode-cliparea
  :clip-mode-clippolygon
  :clip-mode-clipregion
  :clip-mode-clippath)

(cffi:defcenum region-combine-mode
  :region-combine-mode-union
  :region-combine-mode-intersect
  :region-combine-mode-difference
  :region-combine-mode-notintersect)

(cffi:defcenum polygon-mode
  :polygon-mode-fill
  :polygon-mode-open-lines
  :polygon-mode-closed-lines
  :polygon-mode-clip
  :polygon-mode-bezier
  :polygon-mode-region
  :polygon-mode-path)

(cffi:defcenum path-actions
  :path-actions-new
  :path-actions-moveto
  :path-actions-lineto
  :path-actions-arc
  :path-actions-curveto
  :path-actions-close
  :path-actions-fill
  :path-actions-stroke
  :path-actions-fillstroke
  :path-actions-clip)

(cffi:defcenum fill-mode
  :fill-mode-evenodd
  :fill-mode-winding)

(cffi:defcenum line-join
  :line-join-miter
  :line-join-bevel
  :line-join-round)

(cffi:defcenum line-cap
  :line-cap-capflat
  :line-cap-capsquare
  :line-cap-capround)

(cffi:defcenum background-opacity-mode
  :background-opacity-mode-opaque
  :background-opacity-mode-transparent)

(cffi:defcenum write-mode
  :write-mode-replace
  :write-mode-xor
  :write-mode-not-xor)

(cffi:defcenum color-allocation-mode
  :color-allocation-mode-polite
  :color-allocation-mode-force)

(cffi:defcenum line-style
  :line-style-continuous
  :line-style-dashed
  :line-style-dotted
  :line-style-dash-dot
  :line-style-dash-dot-dot
  :line-style-custom)

(cffi:defcenum marker-type
  :marker-type-plus
  :marker-type-star
  :marker-type-circle
  :marker-type-x
  :marker-type-box
  :marker-type-diamond
  :marker-type-hollow-circle
  :marker-type-hollow-box
  :marker-type-hollow-diamond)

(cffi:defcenum hatch-type
  :hatch-type-horizontal
  :hatch-type-vertical
  :hatch-type-fdiagonal
  :hatch-type-bdiagonal
  :hatch-type-cross
  :hatch-type-diagcross)

(cffi:defcenum interior-style
  :interior-style-solid
  :interior-style-hatch
  :interior-style-stipple
  :interior-style-pattern
  :interior-style-hollow
  :interior-style-custompattern)

(cffi:defcenum text-alignment
  :text-alignment-north
  :text-alignment-south
  :text-alignment-east
  :text-alignment-west
  :text-alignment-north-east
  :text-alignment-north-west
  :text-alignment-south-east
  :text-alignment-south-west
  :text-alignment-center
  :text-alignment-base-left
  :text-alignment-base-center
  :text-alignment-base-right)

(cffi:defcenum style
  (:style-plain 0)
  (:style-bold 1)
  (:style-italic 2)
  (:style-underline 4)
  (:style-strikeout 8))

(cffi:defcenum some-font-sizes
  (:some-font-sizes-small 8)
  (:some-font-sizes-standard 12)
  (:some-font-sizes-large 18))

(cffi:defcenum context-types
  :context-types-window
  :context-types-device
  :context-types-image
  :context-types-file)

(cffi:defcenum paper-sizes
  :paper-sizes-a0
  :paper-sizes-a1
  :paper-sizes-a2
  :paper-sizes-a3
  :paper-sizes-a4
  :paper-sizes-a5
  :paper-sizes-letter
  :paper-sizes-legal)

(cffi:defcfun ("cdVersion" %cd-version) :string)

(cffi:defcfun ("cdVersionDate" %cd-version-date) :string)

(cffi:defcfun ("cdVersionNumber" %cd-version-number) :int)

(cffi:defcfun ("cdCreateCanvas" %cd-create-canvas) cd-canvas
  (context cd-context)
  (data :pointer))

(cffi:defcfun ("cdCreateCanvasf" %cd-create-canvasf) cd-canvas
  (context cd-context)
  (format :string)
  &rest)

(cffi:defcfun ("cdKillCanvas" %cd-kill-canvas) :void
  (canvas cd-canvas))

(cffi:defcfun ("cdCanvasGetContext" %cd-canvas-get-context) cd-context
  (canvas cd-canvas))

(cffi:defcfun ("cdCanvasActivate" %cd-canvas-activate) :int
  (canvas cd-canvas))

(cffi:defcfun ("cdCanvasDeactivate" %cd-canvas-deactivate) :void
  (canvas cd-canvas))

(cffi:defcfun ("cdUseContextPlus" %cd-use-context-plus) :int
  (use :int))

(cffi:defcfun ("cdInitContextPlus" %cd-init-context-plus) :void)

(cffi:defcfun ("cdFinishContextPlus" %cd-finish-context-plus) :void)

(cffi:defcfun ("cdContextRegisterCallback" %cd-context-register-callback) :int
  (context cd-context)
  (cb :int)
  (func :pointer))

(cffi:defcfun ("cdContextCaps" %cd-context-caps) :unsigned-long
  (context cd-context))

(cffi:defcfun ("cdContextIsPlus" %cd-context-is-plus) :int
  (context cd-context))

(cffi:defcfun ("cdContextType" %cd-context-type) :int
  (context cd-context))

(cffi:defcfun ("cdCanvasSimulate" %cd-canvas-simulate) :int
  (canvas cd-canvas)
  (mode :int))

(cffi:defcfun ("cdCanvasFlush" %cd-canvas-flush) :void
  (canvas cd-canvas))

(cffi:defcfun ("cdCanvasClear" %cd-canvas-clear) :void
  (canvas cd-canvas))

(cffi:defcfun ("cdCanvasSaveState" %cd-canvas-save-state) cd-state
  (canvas cd-canvas))

(cffi:defcfun ("cdCanvasRestoreState" %cd-canvas-restore-state) :void
  (canvas cd-canvas)
  (state cd-state))

(cffi:defcfun ("cdReleaseState" %cd-release-state) :void
  (state cd-state))

(cffi:defcfun ("cdCanvasSetAttribute" %cd-canvas-set-attribute) :void
  (canvas cd-canvas)
  (name :string)
  (data :pointer))

(cffi:defcfun ("cdCanvasSetfAttribute" %cd-canvas-setf-attribute) :void
  (canvas cd-canvas)
  (name :string)
  (format :string)
  &rest)

(cffi:defcfun ("cdCanvasGetAttribute" %cd-canvas-get-attribute) :string
  (canvas cd-canvas)
  (name :string))

(cffi:defcfun ("cdCanvasPlay" %cd-canvas-play) :int
  (canvas cd-canvas)
  (context cd-context)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int)
  (data :pointer))

(cffi:defcfun ("cdCanvasGetSize" %cd-canvas-get-size) :void
  (canvas cd-canvas)
  (width :pointer)
  (height :pointer)
  (width-mm :pointer)
  (height-mm :pointer))

(cffi:defcfun ("cdCanvasUpdateYAxis" %cd-canvas-update-yaxis) :int
  (canvas cd-canvas)
  (y :pointer))

(cffi:defcfun ("cdfCanvasUpdateYAxis" %cdf-canvas-update-yaxis) :double
  (canvas cd-canvas)
  (y :pointer))

(cffi:defcfun ("cdCanvasInvertYAxis" %cd-canvas-invert-yaxis) :int
  (canvas cd-canvas)
  (y :int))

(cffi:defcfun ("cdfCanvasInvertYAxis" %cdf-canvas-invert-yaxis) :double
  (canvas cd-canvas)
  (y :double))

(cffi:defcfun ("cdCanvasMM2Pixel" %cd-canvas-mm2-pixel) :void
  (canvas cd-canvas)
  (mm-dx :double)
  (mm-dy :double)
  (dx :pointer)
  (dy :pointer))

(cffi:defcfun ("cdCanvasPixel2MM" %cd-canvas-pixel2-mm) :void
  (canvas cd-canvas)
  (dx :int)
  (dy :int)
  (mm-dx :pointer)
  (mm-dy :pointer))

(cffi:defcfun ("cdfCanvasMM2Pixel" %cdf-canvas-mm2-pixel) :void
  (canvas cd-canvas)
  (mm-dx :double)
  (mm-dy :double)
  (dx :pointer)
  (dy :pointer))

(cffi:defcfun ("cdfCanvasPixel2MM" %cdf-canvas-pixel2-mm) :void
  (canvas cd-canvas)
  (dx :double)
  (dy :double)
  (mm-dx :pointer)
  (mm-dy :pointer))

(cffi:defcfun ("cdCanvasOrigin" %cd-canvas-origin) :void
  (canvas cd-canvas)
  (x :int)
  (y :int))

(cffi:defcfun ("cdfCanvasOrigin" %cdf-canvas-origin) :void
  (canvas cd-canvas)
  (x :double)
  (y :double))

(cffi:defcfun ("cdCanvasGetOrigin" %cd-canvas-get-origin) :void
  (canvas cd-canvas)
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("cdfCanvasGetOrigin" %cdf-canvas-get-origin) :void
  (canvas cd-canvas)
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("cdCanvasTransform" %cd-canvas-transform) :void
  (canvas cd-canvas)
  (matrix :pointer))

(cffi:defcfun ("cdCanvasGetTransform" %cd-canvas-get-transform) :pointer
  (canvas cd-canvas))

(cffi:defcfun ("cdCanvasTransformMultiply" %cd-canvas-transform-multiply) :void
  (canvas cd-canvas)
  (matrix :pointer))

(cffi:defcfun ("cdCanvasTransformRotate" %cd-canvas-transform-rotate) :void
  (canvas cd-canvas)
  (angle :double))

(cffi:defcfun ("cdCanvasTransformScale" %cd-canvas-transform-scale) :void
  (canvas cd-canvas)
  (sx :double)
  (sy :double))

(cffi:defcfun ("cdCanvasTransformTranslate" %cd-canvas-transform-translate) :void
  (canvas cd-canvas)
  (dx :double)
  (dy :double))

(cffi:defcfun ("cdCanvasTransformPoint" %cd-canvas-transform-point) :void
  (canvas cd-canvas)
  (x :int)
  (y :int)
  (tx :pointer)
  (ty :pointer))

(cffi:defcfun ("cdfCanvasTransformPoint" %cdf-canvas-transform-point) :void
  (canvas cd-canvas)
  (x :double)
  (y :double)
  (tx :pointer)
  (ty :pointer))

(cffi:defcfun ("cdCanvasClip" %cd-canvas-clip) :int
  (canvas cd-canvas)
  (mode :int))

(cffi:defcfun ("cdCanvasClipArea" %cd-canvas-clip-area) :void
  (canvas cd-canvas)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int))

(cffi:defcfun ("cdCanvasGetClipArea" %cd-canvas-get-clip-area) :int
  (canvas cd-canvas)
  (xmin :pointer)
  (xmax :pointer)
  (ymin :pointer)
  (ymax :pointer))

(cffi:defcfun ("cdfCanvasClipArea" %cdf-canvas-clip-area) :void
  (canvas cd-canvas)
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double))

(cffi:defcfun ("cdfCanvasGetClipArea" %cdf-canvas-get-clip-area) :int
  (canvas cd-canvas)
  (xmin :pointer)
  (xmax :pointer)
  (ymin :pointer)
  (ymax :pointer))

(cffi:defcfun ("cdCanvasIsPointInRegion" %cd-canvas-is-point-in-region) :int
  (canvas cd-canvas)
  (x :int)
  (y :int))

(cffi:defcfun ("cdCanvasOffsetRegion" %cd-canvas-offset-region) :void
  (canvas cd-canvas)
  (x :int)
  (y :int))

(cffi:defcfun ("cdCanvasGetRegionBox" %cd-canvas-get-region-box) :void
  (canvas cd-canvas)
  (xmin :pointer)
  (xmax :pointer)
  (ymin :pointer)
  (ymax :pointer))

(cffi:defcfun ("cdCanvasRegionCombineMode" %cd-canvas-region-combine-mode) :int
  (canvas cd-canvas)
  (mode :int))

(cffi:defcfun ("cdCanvasPixel" %cd-canvas-pixel) :void
  (canvas cd-canvas)
  (x :int)
  (y :int)
  (color :long))

(cffi:defcfun ("cdCanvasMark" %cd-canvas-mark) :void
  (canvas cd-canvas)
  (x :int)
  (y :int))

(cffi:defcfun ("cdfCanvasPixel" %cdf-canvas-pixel) :void
  (canvas cd-canvas)
  (x :double)
  (y :double)
  (color :long))

(cffi:defcfun ("cdfCanvasMark" %cdf-canvas-mark) :void
  (canvas cd-canvas)
  (x :double)
  (y :double))

(cffi:defcfun ("cdCanvasBegin" %cd-canvas-begin) :void
  (canvas cd-canvas)
  (mode :int))

(cffi:defcfun ("cdCanvasPathSet" %cd-canvas-path-set) :void
  (canvas cd-canvas)
  (action :int))

(cffi:defcfun ("cdCanvasEnd" %cd-canvas-end) :void
  (canvas cd-canvas))

(cffi:defcfun ("cdCanvasLine" %cd-canvas-line) :void
  (canvas cd-canvas)
  (x1 :int)
  (y1 :int)
  (x2 :int)
  (y2 :int))

(cffi:defcfun ("cdCanvasVertex" %cd-canvas-vertex) :void
  (canvas cd-canvas)
  (x :int)
  (y :int))

(cffi:defcfun ("cdCanvasRect" %cd-canvas-rect) :void
  (canvas cd-canvas)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int))

(cffi:defcfun ("cdCanvasBox" %cd-canvas-box) :void
  (canvas cd-canvas)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int))

(cffi:defcfun ("cdCanvasArc" %cd-canvas-arc) :void
  (canvas cd-canvas)
  (xc :int)
  (yc :int)
  (w :int)
  (h :int)
  (angle1 :double)
  (angle2 :double))

(cffi:defcfun ("cdCanvasSector" %cd-canvas-sector) :void
  (canvas cd-canvas)
  (xc :int)
  (yc :int)
  (w :int)
  (h :int)
  (angle1 :double)
  (angle2 :double))

(cffi:defcfun ("cdCanvasChord" %cd-canvas-chord) :void
  (canvas cd-canvas)
  (xc :int)
  (yc :int)
  (w :int)
  (h :int)
  (angle1 :double)
  (angle2 :double))

(cffi:defcfun ("cdCanvasText" %cd-canvas-text) :void
  (canvas cd-canvas)
  (x :int)
  (y :int)
  (s :string))

(cffi:defcfun ("cdfCanvasLine" %cdf-canvas-line) :void
  (canvas cd-canvas)
  (x1 :double)
  (y1 :double)
  (x2 :double)
  (y2 :double))

(cffi:defcfun ("cdfCanvasVertex" %cdf-canvas-vertex) :void
  (canvas cd-canvas)
  (x :double)
  (y :double))

(cffi:defcfun ("cdfCanvasRect" %cdf-canvas-rect) :void
  (canvas cd-canvas)
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double))

(cffi:defcfun ("cdfCanvasBox" %cdf-canvas-box) :void
  (canvas cd-canvas)
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double))

(cffi:defcfun ("cdfCanvasArc" %cdf-canvas-arc) :void
  (canvas cd-canvas)
  (xc :double)
  (yc :double)
  (w :double)
  (h :double)
  (angle1 :double)
  (angle2 :double))

(cffi:defcfun ("cdfCanvasSector" %cdf-canvas-sector) :void
  (canvas cd-canvas)
  (xc :double)
  (yc :double)
  (w :double)
  (h :double)
  (angle1 :double)
  (angle2 :double))

(cffi:defcfun ("cdfCanvasChord" %cdf-canvas-chord) :void
  (canvas cd-canvas)
  (xc :double)
  (yc :double)
  (w :double)
  (h :double)
  (angle1 :double)
  (angle2 :double))

(cffi:defcfun ("cdfCanvasText" %cdf-canvas-text) :void
  (canvas cd-canvas)
  (x :double)
  (y :double)
  (s :string))

(cffi:defcfun ("cdCanvasSetBackground" %cd-canvas-set-background) :void
  (canvas cd-canvas)
  (color :long))

(cffi:defcfun ("cdCanvasSetForeground" %cd-canvas-set-foreground) :void
  (canvas cd-canvas)
  (color :long))

(cffi:defcfun ("cdCanvasBackground" %cd-canvas-background) :long
  (canvas cd-canvas)
  (color :long))

(cffi:defcfun ("cdCanvasForeground" %cd-canvas-foreground) :long
  (canvas cd-canvas)
  (color :long))

(cffi:defcfun ("cdCanvasBackOpacity" %cd-canvas-back-opacity) :int
  (canvas cd-canvas)
  (opacity :int))

(cffi:defcfun ("cdCanvasWriteMode" %cd-canvas-write-mode) :int
  (canvas cd-canvas)
  (mode :int))

(cffi:defcfun ("cdCanvasLineStyle" %cd-canvas-line-style) :int
  (canvas cd-canvas)
  (style :int))

(cffi:defcfun ("cdCanvasLineStyleDashes" %cd-canvas-line-style-dashes) :void
  (canvas cd-canvas)
  (dashes :pointer)
  (count :int))

(cffi:defcfun ("cdCanvasLineWidth" %cd-canvas-line-width) :int
  (canvas cd-canvas)
  (width :int))

(cffi:defcfun ("cdCanvasLineJoin" %cd-canvas-line-join) :int
  (canvas cd-canvas)
  (join :int))

(cffi:defcfun ("cdCanvasLineCap" %cd-canvas-line-cap) :int
  (canvas cd-canvas)
  (cap :int))

(cffi:defcfun ("cdCanvasInteriorStyle" %cd-canvas-interior-style) :int
  (canvas cd-canvas)
  (style :int))

(cffi:defcfun ("cdCanvasHatch" %cd-canvas-hatch) :int
  (canvas cd-canvas)
  (style :int))

(cffi:defcfun ("cdCanvasStipple" %cd-canvas-stipple) :void
  (canvas cd-canvas)
  (w :int)
  (h :int)
  (stipple :pointer))

(cffi:defcfun ("cdCanvasGetStipple" %cd-canvas-get-stipple) :pointer
  (canvas cd-canvas)
  (n :pointer)
  (m :pointer))

(cffi:defcfun ("cdCanvasPattern" %cd-canvas-pattern) :void
  (canvas cd-canvas)
  (w :int)
  (h :int)
  (pattern :pointer))

(cffi:defcfun ("cdCanvasGetPattern" %cd-canvas-get-pattern) :pointer
  (canvas cd-canvas)
  (n :pointer)
  (m :pointer))

(cffi:defcfun ("cdCanvasFillMode" %cd-canvas-fill-mode) :int
  (canvas cd-canvas)
  (mode :int))

(cffi:defcfun ("cdCanvasFont" %cd-canvas-font) :int
  (canvas cd-canvas)
  (type-face :string)
  (style :int)
  (size :int))

(cffi:defcfun ("cdCanvasGetFont" %cd-canvas-get-font) :void
  (canvas cd-canvas)
  (type-face :pointer)
  (style :pointer)
  (size :pointer))

(cffi:defcfun ("cdCanvasNativeFont" %cd-canvas-native-font) :string
  (canvas cd-canvas)
  (font :string))

(cffi:defcfun ("cdCanvasTextAlignment" %cd-canvas-text-alignment) :int
  (canvas cd-canvas)
  (alignment :int))

(cffi:defcfun ("cdCanvasTextOrientation" %cd-canvas-text-orientation) :double
  (canvas cd-canvas)
  (angle :double))

(cffi:defcfun ("cdCanvasMarkType" %cd-canvas-mark-type) :int
  (canvas cd-canvas)
  (type :int))

(cffi:defcfun ("cdCanvasMarkSize" %cd-canvas-mark-size) :int
  (canvas cd-canvas)
  (size :int))

(cffi:defcfun ("cdCanvasVectorText" %cd-canvas-vector-text) :void
  (canvas cd-canvas)
  (x :int)
  (y :int)
  (s :string))

(cffi:defcfun ("cdCanvasMultiLineVectorText" %cd-canvas-multi-line-vector-text) :void
  (canvas cd-canvas)
  (x :int)
  (y :int)
  (s :string))

(cffi:defcfun ("cdCanvasVectorFont" %cd-canvas-vector-font) :string
  (canvas cd-canvas)
  (filename :string))

(cffi:defcfun ("cdCanvasVectorTextDirection" %cd-canvas-vector-text-direction) :void
  (canvas cd-canvas)
  (x1 :int)
  (y1 :int)
  (x2 :int)
  (y2 :int))

(cffi:defcfun ("cdCanvasVectorTextTransform" %cd-canvas-vector-text-transform) :pointer
  (canvas cd-canvas)
  (matrix :pointer))

(cffi:defcfun ("cdCanvasVectorTextSize" %cd-canvas-vector-text-size) :void
  (canvas cd-canvas)
  (size-x :int)
  (size-y :int)
  (s :string))

(cffi:defcfun ("cdCanvasVectorCharSize" %cd-canvas-vector-char-size) :int
  (canvas cd-canvas)
  (size :int))

(cffi:defcfun ("cdCanvasVectorFontSize" %cd-canvas-vector-font-size) :void
  (canvas cd-canvas)
  (size-x :double)
  (size-y :double))

(cffi:defcfun ("cdCanvasGetVectorFontSize" %cd-canvas-get-vector-font-size) :void
  (canvas cd-canvas)
  (size-x :pointer)
  (size-y :pointer))

(cffi:defcfun ("cdCanvasGetVectorTextSize" %cd-canvas-get-vector-text-size) :void
  (canvas cd-canvas)
  (s :string)
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("cdCanvasGetVectorTextBounds" %cd-canvas-get-vector-text-bounds) :void
  (canvas cd-canvas)
  (s :string)
  (x :int)
  (y :int)
  (rect :pointer))

(cffi:defcfun ("cdCanvasGetVectorTextBox" %cd-canvas-get-vector-text-box) :void
  (canvas cd-canvas)
  (x :int)
  (y :int)
  (s :string)
  (xmin :pointer)
  (xmax :pointer)
  (ymin :pointer)
  (ymax :pointer))

(cffi:defcfun ("cdfCanvasVectorTextDirection" %cdf-canvas-vector-text-direction) :void
  (canvas cd-canvas)
  (x1 :double)
  (y1 :double)
  (x2 :double)
  (y2 :double))

(cffi:defcfun ("cdfCanvasVectorTextSize" %cdf-canvas-vector-text-size) :void
  (canvas cd-canvas)
  (size-x :double)
  (size-y :double)
  (s :string))

(cffi:defcfun ("cdfCanvasGetVectorTextSize" %cdf-canvas-get-vector-text-size) :void
  (canvas cd-canvas)
  (s :string)
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("cdfCanvasVectorCharSize" %cdf-canvas-vector-char-size) :double
  (canvas cd-canvas)
  (size :double))

(cffi:defcfun ("cdfCanvasVectorText" %cdf-canvas-vector-text) :void
  (canvas cd-canvas)
  (x :double)
  (y :double)
  (s :string))

(cffi:defcfun ("cdfCanvasMultiLineVectorText" %cdf-canvas-multi-line-vector-text) :void
  (canvas cd-canvas)
  (x :double)
  (y :double)
  (s :string))

(cffi:defcfun ("cdfCanvasGetVectorTextBounds" %cdf-canvas-get-vector-text-bounds) :void
  (canvas cd-canvas)
  (s :string)
  (x :double)
  (y :double)
  (rect :pointer))

(cffi:defcfun ("cdfCanvasGetVectorTextBox" %cdf-canvas-get-vector-text-box) :void
  (canvas cd-canvas)
  (x :double)
  (y :double)
  (s :string)
  (xmin :pointer)
  (xmax :pointer)
  (ymin :pointer)
  (ymax :pointer))

(cffi:defcfun ("cdCanvasGetFontDim" %cd-canvas-get-font-dim) :void
  (canvas cd-canvas)
  (max-width :pointer)
  (height :pointer)
  (ascent :pointer)
  (descent :pointer))

(cffi:defcfun ("cdCanvasGetTextSize" %cd-canvas-get-text-size) :void
  (canvas cd-canvas)
  (s :string)
  (width :pointer)
  (height :pointer))

(cffi:defcfun ("cdCanvasGetTextBox" %cd-canvas-get-text-box) :void
  (canvas cd-canvas)
  (x :int)
  (y :int)
  (s :string)
  (xmin :pointer)
  (xmax :pointer)
  (ymin :pointer)
  (ymax :pointer))

(cffi:defcfun ("cdfCanvasGetTextBox" %cdf-canvas-get-text-box) :void
  (canvas cd-canvas)
  (x :double)
  (y :double)
  (s :string)
  (xmin :pointer)
  (xmax :pointer)
  (ymin :pointer)
  (ymax :pointer))

(cffi:defcfun ("cdCanvasGetTextBounds" %cd-canvas-get-text-bounds) :void
  (canvas cd-canvas)
  (x :int)
  (y :int)
  (s :string)
  (rect :pointer))

(cffi:defcfun ("cdfCanvasGetTextBounds" %cdf-canvas-get-text-bounds) :void
  (canvas cd-canvas)
  (x :double)
  (y :double)
  (s :string)
  (rect :pointer))

(cffi:defcfun ("cdCanvasGetColorPlanes" %cd-canvas-get-color-planes) :int
  (canvas cd-canvas))

(cffi:defcfun ("cdCanvasPalette" %cd-canvas-palette) :void
  (canvas cd-canvas)
  (n :int)
  (palette :pointer)
  (mode :int))

(cffi:defcfun ("cdCanvasGetImageRGB" %cd-canvas-get-image-rgb) :void
  (canvas cd-canvas)
  (r :pointer)
  (g :pointer)
  (b :pointer)
  (x :int)
  (y :int)
  (iw :int)
  (ih :int))

(cffi:defcfun ("cdCanvasPutImageRectRGB" %cd-canvas-put-image-rect-rgb) :void
  (canvas cd-canvas)
  (iw :int)
  (ih :int)
  (r :pointer)
  (g :pointer)
  (b :pointer)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int))

(cffi:defcfun ("cdCanvasPutImageRectRGBA" %cd-canvas-put-image-rect-rgba) :void
  (canvas cd-canvas)
  (iw :int)
  (ih :int)
  (r :pointer)
  (g :pointer)
  (b :pointer)
  (a :pointer)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int))

(cffi:defcfun ("cdCanvasPutImageRectMap" %cd-canvas-put-image-rect-map) :void
  (canvas cd-canvas)
  (iw :int)
  (ih :int)
  (index :pointer)
  (colors :pointer)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int))

(cffi:defcfun ("cdfCanvasPutImageRectRGB" %cdf-canvas-put-image-rect-rgb) :void
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

(cffi:defcfun ("cdfCanvasPutImageRectRGBA" %cdf-canvas-put-image-rect-rgba) :void
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

(cffi:defcfun ("cdfCanvasPutImageRectMap" %cdf-canvas-put-image-rect-map) :void
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

(cffi:defcfun ("cdCanvasCreateImage" %cd-canvas-create-image) cd-image
  (canvas cd-canvas)
  (w :int)
  (h :int))

(cffi:defcfun ("cdKillImage" %cd-kill-image) :void
  (image cd-image))

(cffi:defcfun ("cdCanvasGetImage" %cd-canvas-get-image) :void
  (canvas cd-canvas)
  (image cd-image)
  (x :int)
  (y :int))

(cffi:defcfun ("cdCanvasPutImageRect" %cd-canvas-put-image-rect) :void
  (canvas cd-canvas)
  (image cd-image)
  (x :int)
  (y :int)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int))

(cffi:defcfun ("cdCanvasScrollArea" %cd-canvas-scroll-area) :void
  (canvas cd-canvas)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int)
  (dx :int)
  (dy :int))

(cffi:defcfun ("cdCreateBitmap" %cd-create-bitmap) cd-bitmap
  (w :int)
  (h :int)
  (type :int))

(cffi:defcfun ("cdInitBitmap" %cd-init-bitmap) cd-bitmap
  (w :int)
  (h :int)
  (type :int)
  &rest)

(cffi:defcfun ("cdKillBitmap" %cd-kill-bitmap) :void
  (bitmap cd-bitmap))

(cffi:defcfun ("cdBitmapGetData" %cd-bitmap-get-data) :pointer
  (bitmap cd-bitmap)
  (dataptr :int))

(cffi:defcfun ("cdBitmapSetRect" %cd-bitmap-set-rect) :void
  (bitmap cd-bitmap)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int))

(cffi:defcfun ("cdCanvasPutBitmap" %cd-canvas-put-bitmap) :void
  (canvas cd-canvas)
  (bitmap cd-bitmap)
  (x :int)
  (y :int)
  (w :int)
  (h :int))

(cffi:defcfun ("cdCanvasGetBitmap" %cd-canvas-get-bitmap) :void
  (canvas cd-canvas)
  (bitmap cd-bitmap)
  (x :int)
  (y :int))

(cffi:defcfun ("cdBitmapRGB2Map" %cd-bitmap-rgb2-map) :void
  (bitmap-rgb cd-bitmap)
  (bitmap-map cd-bitmap))

(cffi:defcfun ("cdEncodeColor" %cd-encode-color) :long
  (red :unsigned-char)
  (green :unsigned-char)
  (blue :unsigned-char))

(cffi:defcfun ("cdEncodeColorAlpha" %cd-encode-color-alpha) :long
  (red :unsigned-char)
  (green :unsigned-char)
  (blue :unsigned-char)
  (alpha :unsigned-char))

(cffi:defcfun ("cdEncodeAlpha" %cd-encode-alpha) :long
  (color :long)
  (alpha :unsigned-char))

(cffi:defcfun ("cdDecodeColor" %cd-decode-color) :void
  (color :long)
  (red :pointer)
  (green :pointer)
  (blue :pointer))

(cffi:defcfun ("cdDecodeColorAlpha" %cd-decode-color-alpha) :void
  (color :long)
  (red :pointer)
  (green :pointer)
  (blue :pointer)
  (alpha :pointer))

(cffi:defcfun ("cdDecodeAlpha" %cd-decode-alpha) :unsigned-char
  (color :long))

(cffi:defcfun ("cdRGB2Map" %cd-rgb2-map) :void
  (width :int)
  (height :int)
  (red :pointer)
  (green :pointer)
  (blue :pointer)
  (index :pointer)
  (pal-size :int)
  (color :pointer))

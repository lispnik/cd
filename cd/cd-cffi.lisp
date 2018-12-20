(defpackage #:cd-cffi
  (:use #:common-lisp)
  (:export #:cd-context
           #:cd-canvas))

(in-package #:cd-cffi)

(cffi:define-foreign-library lib-cd
  (:unix "libcd.so")
  (:windows "cd.dll")
  (t (:default "cd")))

(cffi:use-foreign-library lib-cd)

(cffi:defctype cd-context :pointer)
(cffi:defctype cd-canvas :pointer)
(cffi:defctype cd-state :pointer)
(cffi:defctype attr-name :string)

(defconstant +cd-query+ -1)

(cffi:defcfun (%cd-version "cdVersion") :string)
(cffi:defcfun (%cd-version-date "cdVersionDate") :string)
(cffi:defcfun (%cd-version-number "cdVersionNumber") :int)

(cffi:defcfun (%cd-create-canvas "cdCreateCanvas") cd-canvas
  (context cd-context)
  (data :pointer))

(cffi:defcfun (%cd-kill-canvas "cdKillCanvas") :void
  (canvas cd-canvas))

(cffi:defcfun (%cd-canvas-get-context "cdCanvasGetContext") cd-context
  (canvas cd-canvas))

(cffi:defcenum status-report
  (:error -1)
  :ok)

(cffi:defcfun (%cd-canvas-activate "cdCanvasActivate") status-report
  (canvas cd-canvas))

(cffi:defcfun (%cd-canvas-deactivate "cdCanvasDeactivate") :void
  (canvas cd-canvas))

(cffi:defcfun (%cd-use-context-plus "cdUseContextPlus") :boolean
  (use :boolean))

(cffi:defcfun (%cd-context-register-callback "cdContextRegisterCallback") status-report
  (context cd-context)
  (cb :int)
  (callback :pointer))

(cffi:defbitfield capabilities
  (:none 0)
  :flush
  :clear
  :play
  :yaxis
  :cliparea
  :clippoly
  :region
  :rect
  :chord
  :imagergb
  :imagergba
  :imagemap
  :getimagergb
  :imagesrv
  :background
  :backopacity
  :writemode
  :linestyle
  :linewith
  :fprimtives
  :hatch
  :stipple
  :pattern
  :font
  :fontdim
  :textsize
  :textorientation
  :palette
  :linecap
  :linejoin
  :path
  :bezier
  (:all #xffffffff))

(cffi:defcfun (%cd-context-caps "cdContextCaps") capabilities
  (context cd-context))

(cffi:defcfun (%cd-context-is-plus "cdContextIsPlus") :boolean
  (context cd-context))

(cffi:defcenum context-type
  :window
  :device
  :image
  :file)

(cffi:defcfun (%cd-context-type "cdContextType") context-type
  (context cd-context))

;; typedef int (*cdCallback)(cdCanvas* canvas, ...);

(cffi:defbitfield simulation-mode
  (:none 0)
  :line
  :rect
  :box
  :arc
  :sector
  :chord
  :polyline
  :polygon
  :text
  (:all #xffff)
  (:lines #x4b)
  (:fills #xb4))

(cffi:defcfun (%cd-canvas-simulate "cdCanvasSimulate") simulation-mode
  (canvas cd-canvas)
  (mode simulation-mode))

(cffi:defcfun (%cd-canvas-flush "cdCanvasFlush") :void
  (canvas cd-canvas))

(cffi:defcfun (%cd-canvas-clear "cdCanvasClear") :void
  (canvas cd-canvas))

(cffi:defcfun (%cd-canvas-save-state "cdCanvasSaveState") cd-state
  (canvas cd-canvas))

(cffi:defcfun (%cd-canvas-restore-state "cdCanvasRestoreState") :void
  (canvas cd-canvas)
  (state cd-state))

(cffi:defcfun (%cd-release-state "cdReleaseState") :void
  (state cd-state))

(cffi:defcfun (%cd-canvas-set-attribute "cdCanvasSetAttribute") :void
  (canvas cd-canvas)
  (name attr-name)
  (data :string))

(cffi:defcfun (%cd-canvas-get-attribute "cdCanvasGetAttribute") :string
  (canvas cd-canvas)
  (name attr-name))

(defmacro defcfun-with-type (target replacement (symbol name) return-type &rest args)
  `(cffi:defcfun (,symbol ,name) ,(subst replacement target return-type)
     ,@(subst replacement target args)))

;;; FIXME remove the duplicated code below

(defmacro defcfun-cd/cdf (target (symbol-name name) return-type &body args)
  `(progn
     (defcfun-with-type
         ,target :int ,(list (intern (format nil "~:@(%cd-~A~)" symbol-name))
                             (concatenate 'string "cd" name))
         ,return-type ,@args)
     (defcfun-with-type
         ,target :double ,(list (intern (format nil "~:@(%cdf-~A~)" symbol-name))
                                (concatenate 'string "cdf" name))
         ,return-type ,@args)))

(defmacro defcfun-cd/wd (target (symbol-name name) return-type &body args)
  `(progn
     (defcfun-with-type
         ,target :int ,(list (intern (format nil "~:@(%cd-~A~)" symbol-name))
                             (concatenate 'string "cd" name))
         ,return-type ,@args)
     (defcfun-with-type
         ,target :double ,(list (intern (format nil "~:@(%wd-~A~)" symbol-name))
                                (concatenate 'string "wd" name))
         ,return-type ,@args)))

(defmacro defcfun-cd/cdf/wd (target (symbol-name name) return-type &body args)
  `(progn
     (defcfun-cd/wd ,target (,symbol-name ,name) ,return-type ,@args)
     (defcfun-with-type
         ,target :double ,(list (intern (format nil "~:@(%cdf-~A~)" symbol-name))
                                (concatenate 'string "cdf" name))
         ,return-type ,@args)))

(defcfun-cd/wd target ("canvas-play" "CanvasPlay") status-report
  (canvas cd-canvas)
  (context cd-context)
  (xmin target)
  (xmax target)
  (ymin target)
  (ymax target)
  (data :pointer))

;; /* client images using bitmap structure */
;; typedef struct _cdBitmap {
;; int w;
;; int h;
;; int type;
;; void *data;
;; } cdBitmap;

(cffi:defcfun (%cd-canvas-get-size "cdCanvasGetSize") :void
  (canvas cd-canvas)
  (width (:pointer :int))
  (height (:pointer :int))
  (width-mm (:pointer :double))
  (height-mm (:pointer :double)))

(defcfun-cd/cdf target ("canvas-update-y-axis" "CanvasUpdateYAxis") target
  (canvas cd-canvas)
  (y (:pointer target)))

(defcfun-cd/cdf target ("canvas-invert-y-axis" "CanvasInvertYAxis") target
  (canvas cd-canvas)
  (y target))

(defcfun-cd/cdf target ("canvas-mm-2-pixel" "CanvasMM2Pixel") :void
  (canvas cd-canvas)
  (mm-dx :double)
  (mm-dy :double)
  (dx (:pointer target))
  (dy (:pointer target)))

(defcfun-cd/cdf target ("canvas-pixel-2-mm" "CanvasPixel2MM") :void
  (canvas cd-canvas)
  (dx target)
  (dy target)
  (mm-dx (:pointer :double))
  (mm-dy (:pointer :double)))

(defcfun-cd/cdf target ("canvas-origin" "CanvasOrigin") :void
  (canvas cd-canvas)
  (x target)
  (y target))

(defcfun-cd/cdf target ("canvas-get-origin" "CanvasGetOrigin") :void
  (canvas cd-canvas)
  (x (:pointer target))
  (y (:pointer target)))

(cffi:defcfun (%cd-canvas-transform "cdCanvasTransform") :void
  (canvas cd-canvas)
  (matrix (:pointer :double)))

(cffi:defcfun (%cd-canvas-get-transform "cdCanvasGetTransform") (:pointer :double)
  (canvas cd-canvas))

(cffi:defcfun (%cd-canvas-transform-multiply "cdCanvasTransformMultiply") :void
  (canvas cd-canvas)
  (matrix (:pointer :double)))

(cffi:defcfun (%cd-canvas-transform-rotate "cdCanvasTransformRotate") :void
  (canvas cd-canvas)
  (angle :double))

(cffi:defcfun (%cd-canvas-transform-scale "cdCanvasTransformScale") :void
  (canvas cd-canvas)
  (sx :double)
  (sy :double))

(cffi:defcfun (%cd-canvas-transform-translate "cdCanvasTransformTranslate") :void
  (canvas cd-canvas)
  (dx :double)
  (dy :double))

(defcfun-cd/cdf target ("canvas-transform-point" "CanvasTransformPoint") :void
  (canvas cd-canvas)
  (x target)
  (y target)
  (tx (:pointer target))
  (ty (:pointer target)))

(cffi:defcenum clip-mode
  (:clip-query -1)
  :clip-off
  :clip-area
  :clip-polygon
  :clip-region)

(cffi:defcfun (%cd-canvas-clip "cdCanvasClip") clip-mode
  (canvas cd-canvas)
  (mode clip-mode))

(defcfun-cd/cdf/wd target ("canvas-clip-area" "CanvasClipArea") :void
  (canvas cd-canvas)
  (xmin target)
  (xmax target)
  (ymin target)
  (ymax target))

(defcfun-cd/cdf/wd target ("canvas-get-clip-area" "CanvasGetClipArea") :int ;FIXME translate return value to enum etc.
  (canvas cd-canvas)
  (xmin (:pointer target))
  (xmax (:pointer target))
  (ymin (:pointer target))
  (ymax (:pointer target)))

(defcfun-cd/wd target ("canvas-is-point-in-region" "CanvasIsPointInRegion") :boolean
  (canvas cd-canvas)
  (x target)
  (y target))

(defcfun-cd/wd target ("canvas-offset-region" "CanvasOffsetRegion") :void
  (canvas cd-canvas)
  (x target)
  (y target))

(defcfun-cd/wd target ("canvas-get-region-box" "CanvasGetRegionBox") :void
  (canvas cd-canvas)
  (xmin (:pointer target))
  (xmax (:pointer target))
  (ymin (:pointer target))
  (ymax (:pointer target)))

(cffi:defcenum region-combine-mode
  (:query -1)
  :combine-union
  :combine-intersect
  :combine-difference
  :combine-not-intersect)

(cffi:defcfun (%cd-canvas-region-combine-mode "cdCanvasRegionCombineMode") region-combine-mode
  (canvas cd-canvas)
  (mode region-combine-mode))

(cffi:defctype encoded-color :long)

(defcfun-cd/cdf/wd target ("canvas-pixel" "CanvasPixel") :void
  (canvas cd-canvas)
  (x target)
  (y target)
  (color encoded-color))

(defcfun-cd/cdf/wd target ("canvas-mark" "CanvasMark") :void
  (canvas cd-canvas)
  (x target)
  (y target))

(cffi:defcenum path-mode
  :path-mode-fill
  :path-mode-open-lines
  :path-mode-closed-lines
  :path-mode-clip
  :path-mode-bezier
  :path-mode-region
  :path-mode-path)

(cffi:defcfun (%cd-canvas-begin "cdCanvasBegin") :void
  (canvas cd-canvas)
  (mode path-mode))

(cffi:defcenum path-action
  :path-action-new
  :path-action-moveto
  :path-action-lineto
  :path-action-arc
  :path-action-curveto
  :path-action-close
  :path-action-fill
  :path-action-stroke
  :path-action-fillstroke
  :path-action-clip)

(cffi:defcfun (%cd-canvas-path-set "cdCanvasPathSet") :void
  (canvas cd-canvas)
  (action path-action))

(cffi:defcfun (%cd-canvas-end "cdCanvasEnd") :void
  (canvas cd-canvas))

(defcfun-cd/cdf/wd target ("canvas-line" "CanvasLine") :void
  (canvas cd-canvas)
  (x1 target)
  (y1 target)
  (x2 target)
  (y2 target))

(defcfun-cd/cdf/wd target ("canvas-vertex" "CanvasVertex") :void
  (canvas cd-canvas)
  (x target)
  (y target))

(defcfun-cd/cdf/wd target ("canvas-rect" "CanvasRect") :void
  (canvas cd-canvas)
  (xmin target)
  (xmax target)
  (ymin target)
  (ymax target))

(defcfun-cd/cdf/wd target ("canvas-box" "CanvasBox") :void
  (canvas cd-canvas)
  (xmin target)
  (xmax target)
  (ymin target)
  (ymax target))

(defcfun-cd/cdf/wd target ("canvas-arc" "CanvasArc") :void
  (canvas cd-canvas)
  (xc target)
  (yc target)
  (w target)
  (h target)
  (angle1 :double)
  (angle2 :double))

(defcfun-cd/cdf/wd target ("canvas-sector" "CanvasSector") :void
  (canvas cd-canvas)
  (xc target)
  (yc target)
  (w target)
  (h target)
  (angle1 :double)
  (angle2 :double))

(defcfun-cd/cdf/wd target ("canvas-chord" "CanvasChord") :void
  (canvas cd-canvas)
  (xc target)
  (yc target)
  (w target)
  (h target)
  (angle1 :double)
  (angle2 :double))

(defcfun-cd/cdf/wd target ("canvas-text" "CanvasText") :void
  (canvas cd-canvas)
  (x target)
  (y target)
  (text :string))

(cffi:defcfun (%cd-canvas-set-background "cdCanvasSetBackground") :void
  (canvas cd-canvas)
  (color encoded-color))

(cffi:defcfun (%cd-canvas-set-foreground "cdCanvasSetForeground") :void
  (canvas cd-canvas)
  (color encoded-color))

(cffi:defcfun (%cd-canvas-background "cdCanvasBackground") encoded-color
  (canvas cd-canvas)
  (color encoded-color))

(cffi:defcfun (%cd-canvas-foreground "cdCanvasForeground") encoded-color
  (canvas cd-canvas)
  (color encoded-color))

(cffi:defcenum background-opacity
  (:query -1)
  :opacity-opaque
  :opacity-transparent)

(cffi:defcfun (%cd-canvas-back-opacity "cdCanvasBackOpacity") background-opacity
  (canvas cd-canvas)
  (opacity background-opacity))

(cffi:defcenum write-mode
  (:query -1)
  :write-replace
  :write-xor
  :write-not-xor)

(cffi:defcfun (%cd-canvas-write-mode "cdCanvasWriteMode") write-mode
  (canvas cd-canvas)
  (mode write-mode))

(cffi:defcenum line-style
  (:query -1)
  :line-continuous
  :line-dashed
  :line-dotted
  :line-dash-dot
  :line-dash-dot-dot
  :line-custom)

(cffi:defcfun (%cd-canvas-line-style  "cdCanvasLineStyle") line-style
  (canvas cd-canvas)
  (style line-style))

(cffi:defcfun (%cd-canvas-line-style-dashes "cdCanvasLineStyleDashes") :void
  (canvas cd-canvas)
  (dashes (:pointer :int))
  (count :int))

(defcfun-cd/wd target ("canvas-line-width" "CanvasLineWidth") :int
  (canvas cd-canvas)
  (width target))

(cffi:defcenum line-join
  (:query -1)
  :join-miter
  :join-bevel
  :join-round)

(cffi:defcfun (%cd-canvas-line-join "cdCanvasLineJoin") line-join
  (canvas cd-canvas)
  (join line-join))

(cffi:defcenum line-cap
  (:query -1)
  :cap-flat
  :cap-square
  :cap-round)

(cffi:defcfun (%cd-canvas-line-cap "cdCanvasLineCap") line-cap
  (canvas cd-canvas)
  (cap line-cap))

(cffi:defcenum interior-style
  (:query -1)
  :interior-solid
  :interior-hatch
  :interior-stipple
  :interior-pattern
  :interior-hollow)

(cffi:defcfun (%cd-canvas-interior-style "cdCanvasInteriorStyle") interior-style
  (canvas cd-canvas)
  (style interior-style))

(cffi:defcenum hatch-type
  (:query -1)
  :hatch-horizontal
  :hatch-vertical
  :hatch-forward-diagonal
  :hatch-backward-diagonal
  :hatch-cross
  :hatch-diagonal-cross)

(cffi:defcfun (%cd-canvas-hatch "cdCanvasHatch") hatch-type
  (canvas cd-canvas)
  (hatch hatch-type))

(cffi:defcfun (%cd-canvas-stipple "cdCanvasStipple") :void
  (canvas cd-canvas)
  (w :int)
  (h :int)
  (stipple (:pointer :unsigned-char)))

(cffi:defcfun (%cd-canvas-get-stipple "cdCanvasGetStipple") (:pointer :unsigned-char)
  (canvas cd-canvas)
  (n (:pointer :int))
  (m (:pointer :int)))

(cffi:defcfun (%cd-canvas-pattern "cdCanvasPattern") :void
  (canvas cd-canvas)
  (w :int)
  (h :int)
  (pattern (:pointer :long)))

(cffi:defcfun (%cd-canvas-get-pattern "cdCanvasGetPattern") (:pointer :long)
  (canvas cd-canvas)
  (n (:pointer :int))
  (m (:pointer :int)))

(cffi:defcenum fill-mode
  (:query -1)
  :fill-even-odd
  :fill-winding)

(cffi:defcfun (%cd-canvas-fill-mode "cdCanvasFillMode") fill-mode
  (canvas cd-canvas)
  (mode fill-mode))

(cffi:defbitfield font-style
  (:query -1)
  :font-style-plain
  :font-style-bold
  :font-style-italic
  :font-style-underline
  :font-style-strikeout)

(defcfun-cd/wd target ("canvas-font" "CanvasFont") :void
  (canvas cd-canvas)
  (typeface :string)
  (style font-style)
  (size target))

(defcfun-cd/wd target ("canvas-get-font" "CanvasGetFont") :void
  (canvas cd-canvas)
  (typeface :pointer)                   ;FIXME pass what in here?
  (style (:pointer font-style))
  (size (:pointer target)))

(cffi:defcfun (%cd-canvas-native-font "cdCanvasNativeFont") :string
  (canvas cd-canvas)
  (font :string))

;;; FIXME ^^^
;; Using "NULL" as a parameter, it only returns the previous string and does not change the font. The value returned is the last attributed value, which may not correspond exactly to the font selected by the driver.
;; Using "(char*)CD_QUERY" as a parameter, it returns the current selected font in the common format definition.

(cffi:defcenum text-alignment
  (:query -1)
  :alignment-north
  :alignment-south
  :alignment-east
  :alignment-west
  :alignment-north-east
  :alignment-north-west
  :alignment-south-east
  :alignment-south-west
  :alignment-center
  :alignment-base-left
  :alignment-base-center
  :alignment-base-right)

(cffi:defcfun (%cd-canvas-text-alignment "cdCanvasTextAlignment") text-alignment
  (canvas cd-canvas)
  (alignment text-alignment))

(cffi:defcfun (%cd-canvas-text-orientation "cdCanvasTextOrientation") :double
  (canvas cd-canvas)
  (orientation :double))

;;; FIXME create another function current-text-orientation that returns the current by passing CD_QUERY (-1)

(cffi:defcenum mark-type
  (:query -1)
  :mark-plus
  :mark-star
  :mark-circle
  :mark-x
  :mark-box
  :mark-diamond
  :mark-hollow-circle
  :mark-hollow-box
  :mark-hollow-diamond)

(cffi:defcfun (%cd-canvas-mark-type "cdCanvasMarkType") mark-type
  (canvas cd-canvas)
  (mark mark-type))

(defcfun-cd/wd target ("canvas-mark-size" "CanvasMarkSize") target
  (canvas cd-canvas)
  (size target))

;;; FIXME create another function for the current mark size ^^^

(defcfun-cd/cdf/wd target ("canvas-vector-text" "CanvasVectorText") target
  (canvas cd-canvas)
  (x target)
  (y target)
  (text :string))

(defcfun-cd/cdf/wd target ("canvas-multiline-vector-text" "CanvasMultiLineVectorText") target
  (canvas cd-canvas)
  (x target)
  (y target)
  (text :string))

(cffi:defcfun (%cd-canvas-vector-font "cdCanvasVectorFont") :string
  (canvas cd-canvas)
  (filename :string))

(defcfun-cd/cdf/wd target ("canvas-vector-text-direction" "CanvasVectorTextDirection") :void
  (canvas cd-canvas)
  (x1 target)
  (y1 target)
  (x2 target)
  (y2 target))

(cffi:defcfun (%cd-canvas-vector-text-transform "cdCanvasVectorTextTransform") (:pointer :double)
  (canvas cd-canvas)
  (matrix (:pointer :double)))

(defcfun-cd/cdf/wd target ("canvas-vector-text-size" "CanvasVectorTextSize") :void
  (canvas cd-canvas)
  (size-x target)
  (size-y target)
  (text :string))

(defcfun-cd/cdf/wd target ("canvas-vector-char-size" "CanvasVectorCharSize") target
  (canvas cd-canvas)
  (size target))

(cffi:defcfun (%cd-canvas-vector-font-size "cdCanvasVectorFontSize") :void
  (canvas cd-canvas)
  (size-x :double)
  (size-y :double))

(cffi:defcfun (%cd-canvas-get-vector-font-size "cdCanvasGetVectorFontSize") :void
  (canvas cd-canvas)
  (size-x (:pointer :double))
  (size-y (:pointer :double)))

(defcfun-cd/cdf/wd target ("canvas-get-vector-text-size" "CanvasGetVectorTextSize") :void
  (canvas cd-canvas)
  (text :string)
  (x (:pointer target))
  (y (:pointer target)))

(defcfun-cd/cdf/wd target ("canvas-get-vector-text-bounds" "CanvasGetVectorTextBounds") :void
  (canvas cd-canvas)
  (text :string)
  (x target)
  (y target)
  (rect (:pointer target)))

(defcfun-cd/cdf/wd target ("canvas-get-vector-text-box" "CanvasGetVectorTextBox") :void
  (canvas cd-canvas)
  (x target)
  (y target)
  (text :string)
  (xmin (:pointer target))
  (xmax (:pointer target))
  (ymin (:pointer target))
  (ymax (:pointer target)))

(defcfun-cd/wd target ("canvas-get-font-dimensions" "CanvasGetFontDim") :void
  (canvas cd-canvas)
  (max-width (:pointer target))
  (height (:pointer target))
  (ascent (:pointer target))
  (descent (:pointer target)))

(defcfun-cd/wd target ("canvas-get-text-size" "CanvasGetTextSize") :void
  (canvas cd-canvas)
  (text :string)
  (width (:pointer target))
  (height (:pointer target)))

(defcfun-cd/cdf/wd target ("canvas-get-text-box" "CanvasGetTextBox") :void
  (canvas cd-canvas)
  (x target)
  (y target)
  (text :string)
  (xmin (:pointer target))
  (xmax (:pointer target))
  (ymin (:pointer target))
  (ymax (:pointer target)))

(defcfun-cd/cdf/wd target ("canvas-get-text-bounds" "CanvasGetTextBounds") :void
  (canvas cd-canvas)
  (x target)
  (y target)
  (text :string)
  (rect (:pointer target)))

(cffi:defcfun (%cd-canvas-get-color-planes "cdCanvasGetColorPlanes") :int
  (canvas cd-canvas))

(cffi:defcenum color-allocation-mode
  :palette-polite
  :palette-force)

(cffi:defcfun (%cd-canvas-palette "cdCanvasPalette") :void
  (canvas cd-canvas)
  (n :int)
  (palette (:pointer :long))
  (mode color-allocation-mode))

(defcfun-cd/wd target ("canvas-get-image-rgb" "CanvasGetImageRGB") :void
  (canvas cd-canvas)
  (red (:pointer :unsigned-char))
  (green (:pointer :unsigned-char))
  (blue (:pointer :unsigned-char))
  (x target)
  (y target)
  (iw :int)
  (ih :int))

(defcfun-cd/cdf/wd target ("canvas-put-image-rect-rgb" "CanvasPutImageRectRGB") :void
  (canvas cd-canvas)
  (iw :int)
  (ih :int)
  (red (:pointer :unsigned-char))
  (green (:pointer :unsigned-char))
  (blue (:pointer :unsigned-char))
  (x target)
  (y target)
  (w target)
  (h target)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int))

(defcfun-cd/cdf/wd target ("canvas-put-image-rect-rgba" "CanvasPutImageRectRGBA") :void
  (canvas cd-canvas)
  (iw :int)
  (ih :int)
  (red (:pointer :unsigned-char))
  (green (:pointer :unsigned-char))
  (blue (:pointer :unsigned-char))
  (alpha (:pointer :unsigned-char))
  (x target)
  (y target)
  (w target)
  (h target)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int))

(defcfun-cd/cdf/wd target ("canvas-put-image-rect-map" "CanvasPutImageRectMap") :void
  (canvas cd-canvas)
  (iw :int)
  (ih :int)
  (index (:pointer :unsigned-char))
  (colors (:pointer encoded-color))
  (x target)
  (y target)
  (w target)
  (h target)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int))

;; /* server images - deprecated (use double buffer drivers) */
;; cdImage* cdCanvasCreateImage(cdCanvas* canvas, int w, int h);
;; void cdKillImage(cdImage* image);
;; void cdCanvasGetImage(cdCanvas* canvas, cdImage* image, int x, int y);
;; void cdCanvasPutImageRect(cdCanvas* canvas, cdImage* image, int x, int y, int xmin, int xmax, int ymin, int ymax);
;; void cdCanvasScrollArea(cdCanvas* canvas, int xmin, int xmax, int ymin, int ymax, int dx, int dy);

;; /* bitmap - deprecated (use imImage) */
;; cdBitmap* cdCreateBitmap(int w, int h, int type);
;; cdBitmap* cdInitBitmap(int w, int h, int type, ...);
;; void cdKillBitmap(cdBitmap* bitmap);
;; unsigned char* cdBitmapGetData(cdBitmap* bitmap, int dataptr);
;; void cdBitmapSetRect(cdBitmap* bitmap, int xmin, int xmax, int ymin, int ymax);
;; void cdCanvasPutBitmap(cdCanvas* canvas, cdBitmap* bitmap, int x, int y, int w, int h);
;; void cdCanvasGetBitmap(cdCanvas* canvas, cdBitmap* bitmap, int x, int y);
;; void cdBitmapRGB2Map(cdBitmap* bitmap_rgb, cdBitmap* bitmap_map);

(cffi:defcfun (%cd-encode-color "cdEncodeColor") encoded-color
  (red :unsigned-char)
  (green :unsigned-char)
  (blue :unsigned-char))

(cffi:defcfun (%cd-encode-color-alpha "cdEncodeColorAlpha") encoded-color
  (red :unsigned-char)
  (green :unsigned-char)
  (blue :unsigned-char)
  (alpha :unsigned-char))

(cffi:defcfun (%cd-encode-alpha "cdEncodeAlpha") encoded-color
  (color encoded-color)
  (alpha :unsigned-char))

(cffi:defcfun (%cd-decode-color "cdDecodeColor") :void
  (color encoded-color)
  (red (:pointer :unsigned-char))
  (green (:pointer :unsigned-char))
  (blue (:pointer :unsigned-char)))

(cffi:defcfun (%cd-decode-color-alpha "cdDecodeColorAlpha") :void
  (color encoded-color)
  (red (:pointer :unsigned-char))
  (green (:pointer :unsigned-char))
  (blue (:pointer :unsigned-char))
  (alpha (:pointer :unsigned-char)))

(cffi:defcfun (%cd-decode-alpha "cdDecodeAlpha") :unsigned-char
  (color encoded-color))

;; #define cdAlpha(_)    (unsigned char)(~(((_) >> 24) & 0xFF))
;; #define cdReserved(_) (unsigned char)(((_) >> 24) & 0xFF)
;; #define cdRed(_)      (unsigned char)(((_) >> 16) & 0xFF)
;; #define cdGreen(_)    (unsigned char)(((_) >>  8) & 0xFF)
;; #define cdBlue(_)     (unsigned char)(((_) >>  0) & 0xFF)

(cffi:defcfun (%cd-rgb-to-map "cdRGB2Map") :void
  (width :int)
  (height :int)
  (red (:pointer :unsigned-char))
  (green (:pointer :unsigned-char))
  (blue (:pointer :unsigned-char))
  (index (:pointer :unsigned-char))
  (palette-size :int)
  (color (:pointer :int)))

;; /* cdPlay definitions */
;; #define CD_SIZECB 0        /* size callback */
;; typedef int(*cdSizeCB)(cdCanvas *canvas, int w, int h, double w_mm, double h_mm);
;; #define CD_ABORT 1
;; #define CD_CONTINUE 0

(cffi:defcenum paper-size
  :a0
  :a1
  :a2
  :a3
  :a4
  :a5
  :letter
  :legal)

(cffi:defcfun (%wd-canvas-window "wdCanvasWindow") :void
  (canvas cd-canvas)
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double))

(cffi:defcfun (%wd-canvas-get-window "wdCanvasGetWindow") :void
  (canvas cd-canvas)
  (xmin (:pointer :double))
  (xmax (:pointer :double))
  (ymin (:pointer :double))
  (ymax (:pointer :double)))

(cffi:defcfun (%wd-canvas-viewport "wdCanvasViewport") :void
  (canvas cd-canvas)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int))

(cffi:defcfun (%wd-canvas-get-viewport "wdCanvasGetViewport") :void
  (canvas cd-canvas)
  (xmin (:pointer :double))
  (xmax (:pointer :double))
  (ymin (:pointer :double))
  (ymax (:pointer :double)))

(cffi:defcfun (%wd-canvas-world-to-canvas "wdCanvasWorld2Canvas") :void
  (canvas cd-canvas)
  (xw :double)
  (yw :double)
  (xv (:pointer :int))
  (yv (:pointer :int)))

(cffi:defcfun (%wd-canvas-world-to-canvas-size "wdCanvasWorld2CanvasSize") :void
  (canvas cd-canvas)
  (hw :double)
  (vw :double)
  (hv (:pointer :int))
  (vv (:pointer :int)))

(cffi:defcfun (%wd-canvas-canvas-to-world "wdCanvasCanvas2World") :void
  (canvas cd-canvas)
  (xv :int)
  (yv :int)
  (xw (:pointer :double))
  (yw (:pointer :double)))

(cffi:defcfun (%wd-canvas-set-transform "wdCanvasSetTransform") :void
  (canvas cd-canvas)
  (sx :double)
  (sy :double)
  (tx :double)
  (ty :double))

(cffi:defcfun (%wd-canvas-get-transform "wdCanvasGetTransform") :void
  (canvas cd-canvas)
  (sx (:pointer :double))
  (sy (:pointer :double))
  (tx (:pointer :double))
  (ty (:pointer :double)))

(cffi:defcfun (%wd-canvas-translate "wdCanvasTranslate") :void
  (canvas cd-canvas)
  (dtx :double)
  (dty :double))

(cffi:defcfun (%wd-canvas-scale "wdCanvasScale") :void
  (canvas cd-canvas)
  (dsx :double)
  (dsy :double))

(cffi:defcfun (%wd-canvas-hardcopy "wdCanvasHardcopy") :void
  (canvas cd-canvas)
  (context cd-context)
  (data :pointer)
  (draw-func :pointer))                 ;void(*draw_func)(cdCanvas *canvas_copy)

(cffi:defcfun (%wd-canvas-stipple "wdCanvasStipple") :void
  (canvas cd-canvas)
  (w :int)
  (h :int)
  (fgbg (:pointer :unsigned-char))
  (w-mm :double)
  (h-mm :double))

(cffi:defcfun (%wd-canvas-pattern "wdCanvasPattern") :void
  (canvas cd-canvas)
  (w :int)
  (h :int)
  (color (:pointer :long))
  (w-mm :double)
  (h-mm :double))

(defpackage #:cd
  (:use #:common-lisp
	#:cffi
	#:alexandria)
  (:export
   ;; Core types and constants
   #:canvas #:context

   ;; Error handling
   #:cd-error #:cd-context-error #:cd-canvas-error #:cd-parameter-error
   #:cd-resource-error #:cd-operation-error
   #:*cd-validation-enabled*
   #:validate-parameter #:validate-canvas #:validate-context
   #:with-cd-error-checking #:with-canvas-restarts

   ;; Context and canvas management
   #:context-debug #:context-image-rgb #:context-svg #:context-picture
   #:context-postscript #:context-pdf #:context-printer
   #:context-capabilities #:context-type #:context-plus-p
   #:create-canvas #:kill #:activate #:deactivate
   #:clear #:flush #:size

   ;; Basic drawing primitives
   #:pixel #:mark #:line #:rect #:box #:arc #:sector #:chord
   #:polygon #:polyline #:bezier

   ;; Advanced drawing
   #:begin-path #:end-path #:path-move-to #:path-line-to #:path-curve-to
   #:path-arc-to #:path-close #:with-path
   #:spline #:smooth-curve #:bezier-curve #:cubic-bezier #:quadratic-bezier
   #:draw-rounded-rectangle #:draw-star #:draw-regular-polygon

   ;; Gradients
   #:make-linear-gradient #:make-radial-gradient #:gradient-fill-rectangle

   ;; Transformations
   #:transformation-matrix #:make-identity-matrix #:make-translation-matrix
   #:make-rotation-matrix #:make-scaling-matrix #:multiply-matrices
   #:transform #:transform-multiply #:transform-translate #:transform-rotate
   #:transform-scale #:transform-point #:reset-transform
   #:with-transform #:with-translation #:with-rotation #:with-scaling
   #:translate-coordinate-system #:rotate-coordinate-system #:scale-coordinate-system

   ;; Attributes
   #:foreground #:background #:write-mode #:background-opacity
   #:line-style #:line-width #:line-cap #:line-join
   #:interior-style #:hatch #:fill-mode
   #:mark-type #:mark-size

   ;; Pattern and stipple support
   #:pattern #:pattern-size #:stipple #:stipple-size

   ;; Text rendering
   #:text #:font #:text-alignment #:text-size #:text-bounds #:text-box
   #:font-dim

   ;; Advanced text
   #:text-multiline #:text-width #:text-height
   #:make-text-style #:make-rich-text-segment #:draw-rich-text
   #:draw-outlined-text #:draw-shadow-text #:draw-3d-text
   #:text-along-path #:underline-text #:strikethrough-text
   #:justify-text #:fit-text-to-box #:get-font-metrics

   ;; Vector text
   #:vector-text #:vector-font #:vector-text-direction #:vector-text-alignment
   #:vector-text-size #:vector-char-size #:vector-text-bounds

   ;; Color management
   #:+black+ #:+white+ #:+red+ #:+green+ #:+blue+ #:+yellow+ #:+magenta+ #:+cyan+ #:+gray+
   #:+query+
   #:encode-color #:encode-color-alpha #:encode-alpha
   #:decode-color #:decode-color-alpha #:decode-alpha
   #:red #:green #:blue #:alpha #:reserved
   #:rgb-to-hsv #:hsv-to-rgb
   #:palette #:palette-size
   #:colors-equal-p

   ;; Image operations
   #:get-image-rgb #:put-image-rgb #:get-image-rgba #:put-image-rgba
   #:get-image-map #:put-image-map

   ;; Server images
   #:create-image-rgb #:create-image-rgba #:create-image-map
   #:get-image #:put-image-stretch #:kill-image #:image-size
   #:get-image-rgb-server #:get-image-rgba-server

   ;; Advanced image processing
   #:blur-kernel #:sharpen-kernel #:edge-detection-kernel #:emboss-kernel
   #:filter-image-rgb #:blur-image #:sharpen-image #:edge-detect-image #:emboss-image
   #:composite-images #:alpha-blend-images #:composite-image-region
   #:rgb-to-grayscale #:adjust-brightness #:adjust-contrast #:adjust-gamma
   #:adjust-image-levels #:scale-image-bilinear
   #:image-histogram #:image-statistics

   ;; Coordinate systems
   #:world-to-canvas #:canvas-to-world #:invert-y-axis
   #:mm-to-pixel #:pixel-to-mm
   #:world-set #:world-get

   ;; Clipping
   #:clip #:clip-off

   ;; State management
   #:save-state #:restore-state #:release-state

   ;; Animation
   #:make-animation #:add-frame #:play-animation-frame #:animate-canvas
   #:make-animation-player #:play-animation #:pause-animation #:stop-animation
   #:update-animation-player
   #:linear-interpolation #:ease-in-quad #:ease-out-quad #:ease-in-out-quad
   #:ease-in-cubic #:ease-out-cubic #:ease-in-out-cubic
   #:animate-property #:animate-color #:animate-along-path
   #:make-particle-system #:update-particles #:draw-particles
   #:with-animation-frame #:create-bounce-animation #:create-rotation-animation #:create-fade-animation

   ;; Backend extensions
   #:make-ps-options #:create-postscript-canvas
   #:make-pdf-options #:create-pdf-canvas #:pdf-add-bookmark #:pdf-set-metadata
   #:make-print-options #:create-printer-canvas #:print-document #:list-printers
   #:export-to-svg #:export-to-eps #:export-multiple-formats
   #:detect-backend-capabilities #:recommend-backend-for-task
   #:optimize-for-backend #:test-backend-features

   ;; Utilities
   #:version #:version-date #:version-number
   #:with-enhanced-canvas)
  (:shadow #:box))

(defpackage #:wd
  (:use #:common-lisp
        #:cd-cffi)
  (:export
   ;; World coordinate functions
   #:world-to-canvas #:canvas-to-world
   #:world-set #:world-get #:set-world-bounds #:get-world-bounds
   #:world-width #:world-height

   ;; Basic drawing primitives in world coordinates
   #:line #:rect #:box #:arc #:sector #:chord
   #:polygon #:polyline #:bezier
   #:pixel #:mark #:text

   ;; Advanced drawing in world coordinates
   #:spline #:cubic-bezier #:quadratic-bezier
   #:draw-rounded-rectangle #:draw-star #:draw-regular-polygon

   ;; Image operations in world coordinates
   #:put-image-rgb #:put-image-rgba #:get-image-rgb #:put-image-stretch

   ;; Advanced text in world coordinates
   #:text-multiline #:text-bounds
   #:vector-text #:vector-text-bounds #:vector-text-direction

   ;; Clipping in world coordinates
   #:clip

   ;; Path operations in world coordinates
   #:path-move-to #:path-line-to #:path-curve-to

   ;; Utility functions
   #:distance #:point-in-world-rect-p
   #:animate-world-point)
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

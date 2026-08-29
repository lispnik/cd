;;;; tests/drawing.lisp — the high-level drawing API.
;;;;
;;;; Assertions lean on the SVG driver, because a driver that writes a file is
;;;; unusually easy to check: what CD emitted is right there to read, and a
;;;; primitive that silently did nothing leaves no element behind.

(in-package #:cd.tests)

(def-suite color-suite :in cd-suite :description "Colour designators.")
(in-suite color-suite)

(test color-accepts-three-spellings
  "Packed, (r g b) and :name are interchangeable everywhere.

The previous binding's main colour hazard: a list handed where a long belonged
became an alien type error rather than anything legible."
  (is (= (cd:color '(255 0 0)) (cd:color :red)))
  (is (= (cd:color (cd:encode-color 255 0 0)) (cd:color :red)))
  (is (= (cd:color 12345) 12345)))

(test color-round-trips
  (multiple-value-bind (r g b) (cd:decode-color (cd:encode-color 1 2 3))
    (is (= 1 r)) (is (= 2 g)) (is (= 3 b)))
  (is (= 255 (cd:color-red (cd:color :red))))
  (is (zerop (cd:color-green (cd:color :red)))))

(test alpha-survives-encoding
  (multiple-value-bind (r g b a) (cd:decode-color (cd:encode-color 10 20 30 40))
    (is (= 10 r)) (is (= 20 g)) (is (= 30 b)) (is (= 40 a))))

(test unknown-colour-name-is-reported
  (signals cd:cd-error (cd:color :chartreuse)))

;;; ---------------------------------------------------------------------------

(def-suite drawing-suite :in cd-suite :description "Primitives and attributes.")
(in-suite drawing-suite)

(defmacro with-svg ((canvas path) &body body)
  "Draw into an SVG canvas, then bind the resulting file's text to TEXT."
  `(let ((path (tmp-file ,path)))
     (cd:with-canvas (,canvas (cd:svg-canvas path :width-mm 100 :height-mm 100))
       ,@body)
     (uiop:read-file-string path)))

(test primitives-emit-elements
  (let ((svg (with-svg (c "prim.svg")
               (cd:line c 0 0 100 100)
               (cd:box c 10 60 10 60)
               (cd:rect c 70 90 70 90)
               (cd:arc c 150 150 60 40 0 180)
               (cd:sector c 200 100 50 50 45 270)
               (cd:mark c 250 250)
               (cd:text c 50 300 "hello"))))
    (is (search "<line" svg))
    (is (search "<rect" svg))
    (is (search "<path" svg) "arc and sector become paths")
    (is (search "<text" svg))
    (is (search "hello" svg))))

(test integer-and-double-paths-both-draw
  "Integers use CD's integer entry point, anything else the cdf* double one."
  (let ((svg (with-svg (c "dispatch.svg")
               (cd:line c 0 0 100 100)
               (cd:line c 0.5d0 0.5d0 99.5d0 20.0d0))))
    ;; The double path keeps its fraction; the integer path cannot have one.
    (is (search "x1=\"0.5\"" svg) "the double entry point must preserve 0.5")
    (is (search "x1=\"0\"" svg) "the integer entry point must emit whole numbers")))

(test attributes-round-trip-as-keywords
  (cd:with-canvas (c (cd:svg-canvas (tmp-file "attr.svg")))
    (setf (cd:line-width c) 3)          (is (= 3 (cd:line-width c)))
    (setf (cd:line-style c) :dashed)    (is (eq :dashed (cd:line-style c)))
    (setf (cd:line-join c) :bevel)      (is (eq :bevel (cd:line-join c)))
    (setf (cd:line-cap c) :round)       (is (eq :round (cd:line-cap c)))
    (setf (cd:interior-style c) :hatch) (is (eq :hatch (cd:interior-style c)))
    (setf (cd:hatch-style c) :cross)    (is (eq :cross (cd:hatch-style c)))
    (setf (cd:back-opacity c) :opaque)  (is (eq :opaque (cd:back-opacity c)))
    (setf (cd:text-alignment c) :center) (is (eq :center (cd:text-alignment c)))))

(test colour-attributes-round-trip
  (cd:with-canvas (c (cd:svg-canvas (tmp-file "col.svg")))
    (setf (cd:foreground c) :red)
    (is (= (cd:color :red) (cd:foreground c)))
    (setf (cd:background c) '(0 128 255))
    (is (= (cd:color '(0 128 255)) (cd:background c)))))

(test invalid-attribute-keyword-is-reported
  (cd:with-canvas (c (cd:svg-canvas (tmp-file "bad.svg")))
    (signals cd:cd-error (setf (cd:line-style c) :squiggly))
    (signals cd:cd-error (setf (cd:interior-style c) :nonsense))))

(test with-shape-emits-a-polygon
  (let ((svg (with-svg (c "shape.svg")
               (cd:with-shape (c :fill)
                 (cd:vertex c 10 10) (cd:vertex c 90 10) (cd:vertex c 50 80)))))
    (is (or (search "<polygon" svg) (search "<path" svg))
        "a filled vertex sequence must emit something")))

(test with-shape-ends-on-unwind
  "A canvas left mid-shape has CD accumulating vertices, and the next
unrelated drawing call joins the polygon."
  (cd:with-canvas (c (cd:svg-canvas (tmp-file "unwound.svg")))
    (ignore-errors
     (cd:with-shape (c :fill)
       (cd:vertex c 0 0)
       (cl:error "abandon mid-shape")))
    ;; If the shape were still open this would be swallowed into it.
    (finishes (cd:line c 0 0 10 10))))

(test font-round-trips
  (cd:with-canvas (c (cd:svg-canvas (tmp-file "font.svg")))
    (cd:font c :face "Helvetica" :style :bold :size 12)
    (multiple-value-bind (face style size) (cd:font c)
      (is (stringp face))
      (is (eq :bold style))
      (is (= 12 size)))
    (multiple-value-bind (w h) (cd:text-size c "Hello")
      (is (plusp w)) (is (plusp h)))
    (multiple-value-bind (mw h) (cd:font-dimensions c)
      (is (plusp mw)) (is (plusp h)))))

;;; ---------------------------------------------------------------------------

(def-suite world-suite :in cd-suite :description "World coordinates.")
(in-suite world-suite)

(test world-window-round-trips
  (cd:with-canvas (c (cd:svg-canvas (tmp-file "wd.svg") :width-mm 100 :height-mm 100))
    (setf (cd:wd-window c) '(-1.0 1.0 -1.0 1.0))
    (multiple-value-bind (xmin xmax ymin ymax) (cd:wd-window c)
      (is (< -1.01d0 xmin -0.99d0))
      (is (< 0.99d0 xmax 1.01d0))
      (is (< -1.01d0 ymin -0.99d0))
      (is (< 0.99d0 ymax 1.01d0)))))

(test world-and-canvas-coordinates-invert
  (cd:with-canvas (c (cd:svg-canvas (tmp-file "wd2.svg") :width-mm 100 :height-mm 100))
    (setf (cd:wd-window c) '(-1.0 1.0 -1.0 1.0))
    ;; The centre of the window is the centre of the canvas.
    (multiple-value-bind (px py) (cd:wd-world-to-canvas c 0.0 0.0)
      (multiple-value-bind (w h) (cd:canvas-size c)
        (is (< (abs (- px (/ w 2))) 2))
        (is (< (abs (- py (/ h 2))) 2))))
    (multiple-value-bind (wx wy) (cd:wd-canvas-to-world c 0 0)
      (is (< -1.01d0 wx -0.98d0))
      (is (< -1.01d0 wy -0.98d0)))))

(test with-wd-window-restores
  (cd:with-canvas (c (cd:svg-canvas (tmp-file "wd3.svg")))
    (setf (cd:wd-window c) '(-1.0 1.0 -1.0 1.0))
    (cd:with-wd-window (c 0.0 10.0 0.0 10.0)
      (is (< 9.99d0 (nth-value 1 (cd:wd-window c)) 10.01d0)))
    (is (< 0.99d0 (nth-value 1 (cd:wd-window c)) 1.01d0))))

(test world-primitives-draw
  (let ((svg (with-svg (c "wdprim.svg")
               (setf (cd:wd-window c) '(0.0 10.0 0.0 10.0))
               (cd:wd-line c 1.0 1.0 9.0 9.0)
               (cd:wd-box c 2.0 8.0 2.0 4.0)
               (cd:wd-text c 5.0 5.0 "world"))))
    (is (search "<line" svg))
    (is (search "world" svg))))

;;; ---------------------------------------------------------------------------

(def-suite clip-suite :in cd-suite :description "Clipping and transforms.")
(in-suite clip-suite)

(test clip-mode-round-trips
  (cd:with-canvas (c (cd:svg-canvas (tmp-file "clip.svg")))
    (is (eq :off (cd:clip c)))
    (setf (cd:clip c) :area)
    (is (eq :area (cd:clip c)))
    (setf (cd:clip c) :off)
    (is (eq :off (cd:clip c)))
    (signals cd:cd-error (setf (cd:clip c) :sideways))))

(test with-clip-area-sets-and-restores
  "Setting the rectangle alone changes nothing until the mode is :area; the
macro does both, and puts the mode back."
  (cd:with-canvas (c (cd:svg-canvas (tmp-file "clip2.svg")))
    (cd:with-clip-area (c 10 100 10 100)
      (is (eq :area (cd:clip c)))
      (multiple-value-bind (xmin xmax) (cd:clip-area c)
        (is (= 10 xmin)) (is (= 100 xmax))))
    (is (eq :off (cd:clip c)))))

(test transform-composes-and-applies
  (cd:with-canvas (c (cd:svg-canvas (tmp-file "xform.svg")))
    (cd:with-transform (c)
      (cd:transform-scale c 2 2)
      ;; Scaling by two doubles the coordinate.
      (multiple-value-bind (x y) (cd:transform-point c 10 20)
        (is (= 20 x)) (is (= 40 y))))))

(test with-transform-restores
  "CD's transform is canvas state; a function that rotates and does not undo
it leaves every later caller drawing at an angle."
  (cd:with-canvas (c (cd:svg-canvas (tmp-file "xform2.svg")))
    (cd:with-transform (c)
      (cd:transform-rotate c 45))
    (multiple-value-bind (x y) (cd:transform-point c 10 0)
      (is (= 10 x)) (is (= 0 y)))))

;;; ---------------------------------------------------------------------------

(def-suite play-suite :in cd-suite :description "Recording and replay.")
(in-suite play-suite)

(test metafile-replays-into-another-driver
  "CD's answer to vector-format conversion: record once, replay anywhere."
  (let ((mf (tmp-file "rec.cdm"))
        (svg (tmp-file "replayed.svg")))
    (cd:with-canvas (c (cd:metafile-canvas mf :width-mm 100 :height-mm 100))
      (cd:line c 0 0 200 200)
      (cd:text c 50 150 "recorded"))
    (is (plusp (file-size mf)))
    (cd:with-canvas (out (cd:svg-canvas svg :width-mm 100 :height-mm 100))
      (cd:play-file out mf))
    (let ((text (uiop:read-file-string svg)))
      (is (search "<line" text) "the recorded line must be replayed")
      (is (search "recorded" text) "the recorded text must be replayed"))))

(test replaying-a-missing-file-signals
  (cd:with-canvas (out (cd:svg-canvas (tmp-file "noplay.svg")))
    (signals cd:cd-error (cd:play-file out (tmp-file "does-not-exist.cdm")))))

;;; ---------------------------------------------------------------------------

(def-suite im-bridge-suite :in cd-suite :description "The CD <-> IM bridge.")
(in-suite im-bridge-suite)

(test bridge-availability-is-reported-honestly
  "IM-BRIDGE-AVAILABLE-P must agree with whether the entry point is really there.

Either answer is legitimate -- CD compiles the bridge in only when
CD_ENABLE_IM was on. What would not be legitimate is claiming the bridge is
present when cdCanvasPutImImage is absent, because then every bridge call
goes to a null pointer and crashes instead of signalling."
  (is (eq (and (cffi:foreign-symbol-pointer "cdCanvasPutImImage") t)
          (cd:im-bridge-available-p))))

(test image-drawn-onto-a-canvas-can-be-read-back
  "The round trip both bindings have to agree on: an IM image drawn onto a CD
canvas, then captured back into a new IM image."
  (with-im-bridge
   (im:with-image (source (im:create 64 48 :color-space-rgb :data-type-byte))
    ;; Fill every plane so the captured mean is unambiguous.
    (dotimes (plane 3)
      (dotimes (i (im:pixel-count source))
        (setf (cffi:mem-aref (im:plane-pointer source plane) :unsigned-char i) 200)))
    (cd:with-canvas (c (cd:image-rgb-canvas 200 200))
      (finishes (cd:put-image c source :x 0 :y 0))
      (im:with-image (grabbed (cd:capture-image c :width 32 :height 32))
        (is (= 32 (im:width grabbed)))
        (is (= 32 (im:height grabbed)))
        (is (eq :color-space-rgb (im:color-space grabbed)))
        ;; The region captured is inside where the image was drawn, so it must
        ;; carry the value written above rather than the blank background.
        (is (< 190d0 (getf (im:statistics grabbed) :mean) 210d0)))))))

(test put-image-scales-when-asked
  (with-im-bridge
   (im:with-image (source (im:create 16 16 :color-space-rgb :data-type-byte))
     (cd:with-canvas (c (cd:image-rgb-canvas 100 100))
       (finishes (cd:put-image c source :x 0 :y 0 :width 64 :height 64))))))

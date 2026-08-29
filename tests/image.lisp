;;;; tests/image.lisp — raster images, palettes, patterns and replay filters.

(in-package #:cd.tests)

(def-suite raster-suite :in cd-suite
  :description "Putting and getting pixels without going through IM.")
(in-suite raster-suite)

(defun constant-plane (count value)
  (make-array count :element-type '(unsigned-byte 8) :initial-element value))

(test rgb-image-round-trips-through-a-canvas
  "Put three planes in, read them back. The image-rgb driver holds a real
raster, so what comes out is what went in."
  (let* ((w 16) (h 16) (n (* w h)))
    (cd:with-canvas (c (cd:image-rgb-canvas 64 64))
      (cd:put-image-rgb c w h
                        (constant-plane n 10)
                        (constant-plane n 20)
                        (constant-plane n 30)
                        :x 0 :y 0)
      (multiple-value-bind (r g b) (cd:get-image-rgb c 8 8 :x 2 :y 2)
        (is (= 64 (length r)))
        (is (= 64 (length g)))
        (is (= 64 (length b)))
        (is (= 10 (aref r 0)) "red plane must survive the round trip")
        (is (= 20 (aref g 0)))
        (is (= 30 (aref b 0)))))))

(test rgb-image-scales-when-asked
  (let* ((w 8) (n (* w w)))
    (cd:with-canvas (c (cd:image-rgb-canvas 64 64))
      (finishes (cd:put-image-rgb c w w
                                  (constant-plane n 255)
                                  (constant-plane n 0)
                                  (constant-plane n 0)
                                  :draw-width 32 :draw-height 32))
      ;; Scaled up by four, so a point well inside the drawn area is red.
      (multiple-value-bind (r g) (cd:get-image-rgb c 1 1 :x 16 :y 16)
        (is (= 255 (aref r 0)))
        (is (zerop (aref g 0)))))))

(test short-plane-is-reported-not-read-past
  "A plane shorter than width*height would otherwise be read off the end."
  (cd:with-canvas (c (cd:image-rgb-canvas 32 32))
    (signals cd:cd-error
      (cd:put-image-rgb c 16 16
                        (constant-plane 4 0)      ; far too short
                        (constant-plane 256 0)
                        (constant-plane 256 0)))))

(test mapped-image-draws-through-its-palette
  (let* ((w 8) (n (* w w))
         (indices (make-array n :element-type '(unsigned-byte 8)
                                :initial-element 1)))
    (cd:with-canvas (c (cd:image-rgb-canvas 64 64))
      ;; Index 1 is red, so the drawn region must come back red.
      (cd:put-image-map c w w indices (vector :black :red) :x 0 :y 0)
      (multiple-value-bind (r g) (cd:get-image-rgb c 1 1 :x 2 :y 2)
        (is (= 255 (aref r 0)))
        (is (zerop (aref g 0)))))))

(test palette-accepts-colour-designators
  (cd:with-canvas (c (cd:image-rgb-canvas 32 32))
    (finishes (cd:palette c (vector :red :green '(0 0 255) 16777215)))
    (signals cd:cd-error (cd:palette c (vector :red) :mode :insistently))))

(test server-image-round-trips
  "Deprecated by CD, but a canvas that allocates one still has to release it."
  (cd:with-canvas (c (cd:image-rgb-canvas 64 64))
    (setf (cd:foreground c) :red)
    (cd:box c 0 32 0 32)
    (cd:with-server-image (image c 16 16)
      (finishes (cd:capture-server-image c image :x 0 :y 0))
      (finishes (cd:put-server-image c image :x 40 :y 40)))))

;;; ---------------------------------------------------------------------------

(def-suite pattern-suite :in cd-suite
  :description "Filling with an IM image as pattern or stipple.")
(in-suite pattern-suite)

(test pattern-and-stipple-accept-im-images
  "Both take an IM:IMAGE and change how later filled shapes are painted.
CD reports nothing back, so the assertion is that the fill still happens and
the canvas survives -- a wrong pointer here would crash rather than fail."
  (im:with-image (tile (im:create 8 8 :color-space-rgb :data-type-byte))
    (dotimes (plane 3)
      (dotimes (i (im:pixel-count tile))
        (setf (cffi:mem-aref (im:plane-pointer tile plane) :unsigned-char i) 128)))
    (cd:with-canvas (c (cd:image-rgb-canvas 64 64))
      (finishes (cd:pattern-image c tile))
      (is (eq :pattern (cd:interior-style c))
          "CD switches interior style to :pattern as a side effect")
      (finishes (cd:box c 0 32 0 32))
      (finishes (cd:stipple-image c tile))
      (finishes (cd:box c 32 64 32 64)))))

;;; ---------------------------------------------------------------------------

(def-suite callback-suite :in cd-suite
  :description "Driver callbacks during replay.")
(in-suite callback-suite)

(test unsupported-callback-is-reported-not-ignored
  "cdContextRegisterCallback answers CD_OK or CD_ERROR, and ignoring it is how
a callback comes to be silently not installed.

Callback ids are numbered per driver -- 1 is CD_CGMCOUNTERCB to the CGM driver
and nothing at all to the metafile one -- so registering against the wrong
driver must fail loudly."
  (signals cd:unsupported-operation
    (cd:register-callback "METAFILE" :cgm-counter
                          (lambda (canvas) (declare (ignore canvas)) t))))

(test unknown-callback-name-is-reported
  (signals cd:cd-error
    (cd:register-callback "CGM" :no-such-callback
                          (lambda (canvas) (declare (ignore canvas)) t))))

(test cgm-callback-registers-and-unregisters
  (when (cd:driver-available-p "CGM")
    (finishes
     (cd:with-callback ("CGM" :cgm-counter
                        (lambda (canvas) (declare (ignore canvas)) t))
       ;; Registration alone is the assertion: whether the callback fires
       ;; depends on there being a CGM file to replay.
       t))))

;;; ---------------------------------------------------------------------------

(def-suite driver-suite :in cd-suite
  :description "Every file driver this build offers, actually writing a file.")
(in-suite driver-suite)

(test each-file-driver-writes-something
  "One canvas per driver, drawn on and killed, then the file checked.

Constructors that build CD's data strings are only trustworthy if the driver
accepts what they produce -- and CD signals a malformed string by returning
NULL, which CANVAS-CREATION-ERROR would surface."
  (dolist (spec '(("SVG"      "d.svg"  svg-canvas)
                  ("PS"       "d.ps"   postscript-canvas)
                  ("PDF"      "d.pdf"  pdf-canvas)
                  ("METAFILE" "d.cdm"  metafile-canvas)
                  ("CGM"      "d.cgm"  cgm-canvas)
                  ("DXF"      "d.dxf"  dxf-canvas)
                  ("DGN"      "d.dgn"  dgn-canvas)))
    (destructuring-bind (driver filename constructor) spec
      (when (cd:driver-available-p driver)
        (let ((path (tmp-file filename)))
          (cd:with-canvas (c (funcall (find-symbol (symbol-name constructor) :cd)
                                      path :width-mm 50 :height-mm 50))
            (cd:line c 0 0 100 100))
          (is (probe-file path) "~A must produce a file" driver)
          (is (plusp (file-size path)) "~A must write something" driver))))))

(test picture-canvas-needs-no-file
  (when (cd:driver-available-p "PICTURE")
    (cd:with-canvas (c (cd:picture-canvas :width-mm 50 :height-mm 50))
      (finishes (cd:line c 0 0 100 100)))))

(test debug-canvas-logs-the-calls
  (when (cd:driver-available-p "DEBUG")
    (let ((path (tmp-file "trace.log")))
      (cd:with-canvas (c (cd:debug-canvas path))
        (cd:line c 0 0 10 10))
      (is (probe-file path))
      (is (plusp (file-size path)) "the debug driver records what it was asked"))))

(test unavailable-driver-names-itself
  (signals cd:driver-not-available (cd:make-canvas "NOSUCHDRIVER" "x")))

;;; ---------------------------------------------------------------------------

(def-suite canvas-query-suite :in cd-suite
  :description "Canvas queries and saved state.")
(in-suite canvas-query-suite)

(test resolution-relates-pixels-to-millimetres
  (cd:with-canvas (c (cd:svg-canvas (tmp-file "res.svg")
                                    :width-mm 100 :height-mm 100))
    (multiple-value-bind (w h) (cd:canvas-size c)
      (declare (ignore h))
      (multiple-value-bind (wmm hmm) (cd:canvas-size-mm c)
        (declare (ignore hmm))
        (is (plusp (cd:canvas-resolution c)))
        (is (< (abs (- (cd:canvas-resolution c) (/ w wmm))) 0.01d0))))))

(test state-can-be-saved-and-restored
  (cd:with-canvas (c (cd:image-rgb-canvas 32 32))
    (setf (cd:foreground c) :red
          (cd:line-width c) 5)
    (let ((state (cd:save-state c)))
      (setf (cd:foreground c) :blue
            (cd:line-width c) 1)
      (is (= (cd:color :blue) (cd:foreground c)))
      (cd:restore-state c state)
      (is (= (cd:color :red) (cd:foreground c))
          "restoring must bring the colour back")
      (is (= 5 (cd:line-width c))
          "and the line width with it"))))

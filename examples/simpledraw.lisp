(defpackage #:cd-examples.simple-draw
  (:use #:common-lisp)
  (:export #:simple-draw))

(in-package #:cd-examples.simple-draw)

(defun simple-draw (canvas)
  (multiple-value-bind (w h w-mm h-mm)
      (cd:size canvas)
    (declare (ignore w-mm h-mm))
    ;; Draw a reactangle and a polyline at the bottom-left area, using a thick
    ;; line with transparency. Notice that transparency is only supported in a
    ;; few drivers, and line join is not supported in the IMAGERGB driver.
    (setf (cd:background canvas) cd:+white+
          (cd:foreground canvas) (cd:encode-alpha cd:+dark-magenta+ 128)
          (cd:line-width canvas) 3
          (cd:line-style canvas) :line-continuous
          (cd:interior-style canvas) :interior-solid)
    (cd:clear canvas)
    (cd:rect canvas 100 200 100 200)
    (cd:with-vertices (canvas :path-mode-open-lines)
      (cd:vertex canvas 300 250)
      (cd:vertex canvas 320 270)
      (cd:vertex canvas 350 260)
      (cd:vertex canvas 340 200)
      (cd:vertex canvas 310 210))
    ;; Draw the red diagonal line with a custom line style.  Notice that line
    ;; styles are not supported in the IMAGERGB driver.
    (setf (cd:foreground canvas) cd:+red+
          (cd:line-style-dashes canvas) #(20 15 5 5)
          (cd:line-style canvas) :line-custom)
    (cd:line canvas 0 0 (1- w) (1- h))
    ;; Draw the blue diagonal line with a pre-defined line style. Notice that
    ;; the pre-defined line style is dependent on the driver.
    (setf (cd:foreground canvas) cd:+blue+
          (cd:line-width canvas) 10
          (cd:line-style canvas) :line-dotted)
    (cd:line canvas 0 (1- h) (1- w) 0)
    ;; Reset line style and width
    (setf (cd:line-style canvas) :line-continuous
          (cd:line-width canvas) 1
          (cd:interior-style canvas) :interior-solid
          (cd:foreground canvas) cd:+magenta+)
    (cd:sector canvas (- w 100) 100 100 100 50 180)
    (setf (cd:foreground canvas) cd:+red+)
    (cd:arc canvas 100 100 100 100 50 180)
    ;; Draw a solid filled rectangle at center.
    (setf (cd:foreground canvas) cd:+yellow+)
    (cd:box canvas (- (/ w 2) 100) (+ (/ w 2) 100) (- (/ h 2) 100) (+ (/ h 2) 100))
    ;;  Prepare font for text
    (setf (cd:text-alignment canvas) :alignment-center
          (cd:text-orientation canvas) 70
          ;; FIXME no setf defined for font??
          ;; (cd:font canvas) "Times" :font-style-bold 24)
          )
    ;; Draw text at center, with orientation, and draw its bounding box. Notice
    ;; that in some drivers the bounding box is not precise
    (let ((text (format nil "CD Version~%~A" (cd:version)))
          (x (/ w 2))
          (y (/ h 2)))
      (multiple-value-bind
            (x0 y0 x1 y1 x2 y2 x3 y3)
          (cd:text-bounds canvas x y text)
        (setf (cd:foreground canvas) cd:+red+)
        (cd:with-vertices (canvas :path-mode-closed-lines)
          (cd:vertex canvas x0 y0)
          (cd:vertex canvas x1 y1)
          (cd:vertex canvas x2 y2)
          (cd:vertex canvas x3 y3))
        (setf (cd:foreground canvas) cd:+blue+)
        (cd:text canvas x y text)))
    ;; Prepare World Coordinates
    (setf (wd:viewport canvas) (values 0 (1- w) 0 (1- h)))
    (setf (wd:window canvas)
          (if (> w h)
              (values 0 (/ w h) 0 1)
              (values 0 1 0 (/ h w))))
    ;; Draw a filled blue rectangle in WC
    (wd:box canvas 0.2 0.3 0.4 0.5)
    ;;  Draw the diagonal of that rectangle in WC
    (setf (cd:foreground canvas) cd:+red+)
    (wd:line canvas 0.2 0.4 0.3 0.5)
    ;; Prepare Vector Text in WC
    (setf (cd:vector-character-size canvas) 0.07)
    ;; Draw vector text, and draw its bounding box. We also use this text to
    ;; show when we are using a contextplus driver.
    (setf (cd:foreground canvas) cd:+red+)
    (let ((text (format nil "~A ~A" (lisp-implementation-type) (lisp-implementation-version)))
          (x 0.25)
          (y 0.35))
      (multiple-value-bind
            (x0 y0 x1 y1 x2 y2 x3 y3)
          (wd::vector-text-bounds canvas text 0.25 0.35)
        (cd:with-vertices (canvas :path-mode-closed-lines)
          (wd:vertex canvas x0 y0)
          (wd:vertex canvas x1 y1)
          (wd:vertex canvas x2 y2)
          (wd:vertex canvas x3 y3))
        (setf (cd:line-width canvas) 2
              (cd:line-style canvas) :line-continuous)
        (wd:vector-text canvas x y text)))
    ;; Draw a filled path at center-right (looks like a weird fish). Notice that
    ;; in PDF the arc is necessarily a circle arc, and not an ellipse.
    (setf (cd:line-width canvas) 1
          (cd:foreground canvas) cd:+green+)
    (cd:with-vertices (canvas :path-mode-path)
      (setf (cd:path canvas) :path-action-moveto)
      (cd:vertex canvas (+ (/ w 2) 200) (/ h 2))
      (setf (cd:path canvas) :path-action-lineto)
      (cd:vertex canvas (+ (/ w 2) 230) (+ (/ h 2) 50))
      (setf (cd:path canvas) :path-action-lineto)
      (cd:vertex canvas (+ (/ w 2) 250) (+ (/ h 2) 50))
      (setf (cd:path canvas) :path-action-curveto)
      (cd:vertex canvas (+ (/ w 2) 150 150) (+ (/ h 2) 200 -50)) ;control point for start
      (cd:vertex canvas (+ (/ w 2) 150 180) (+ (/ h 2) 250 -50)) ;control point for end 
      (cd:vertex canvas (+ (/ w 2) 150 180) (+ (/ h 2) 200 -50)) ;end point
      (setf (cd:path canvas) :path-action-curveto)
      (cd:vertex canvas (+ (/ w 2) 150 180) (+ (/ h 2) 150 -50))
      (cd:vertex canvas (+ (/ w 2) 150 150) (+ (/ h 2) 100 -50))
      (cd:vertex canvas (+ (/ w 2) 150 300) (+ (/ h 2) 100 -50))
      (setf (cd:path canvas) :path-action-lineto)
      (cd:vertex canvas (+ (/ w 2) 150 300) (+ (/ h 2) -50))
      (setf (cd:path canvas) :path-action-arc)
      (cd:vertex canvas (+ (/ w 2) 300) (/ h 2)) ;center
      (cd:vertex canvas 200 100)        ;width, height
      (cd:vertex canvas (* -30 1000) (* -170 1000)) ;start angle, end angle (degrees / 1000)
      (setf (cd:path canvas) :path-action-fill))
    ;; Draw 3 pixels at center left
    (cd:pixel canvas 10 (/ h 2) cd:+red+)
    (cd:pixel canvas 11 (+ (/ h 2) 1) cd:+green+)
    (cd:pixel canvas 12 (+ (/ h 2) 2) cd:+blue+)
    ;; Draw 4 mark types, distributed near each corner
    (setf (cd:foreground canvas) cd:+red+
          (cd:mark-size canvas) 30
          (cd:mark-type canvas) :mark-plus)
    (cd:mark canvas 200 200)
    (setf (cd:mark-type canvas) :mark-circle)
    (cd:mark canvas (- w 200) 200)
    (setf (cd:mark-type canvas) :mark-hollow-circle)
    (cd:mark canvas  200 (- h 200))
    (setf (cd:mark-type canvas) :mark-diamond)
    (cd:mark canvas (- w 200) (- h 200))
    ;; Draw all the line style possibilities at bottom. Notice that they have
    ;; some small differences between drivers.
    (setf (cd:line-width canvas) 1)
    (loop for i from 10 by 10
          for style in '(:line-continuous :line-dashed :line-dotted :line-dash-dot :line-dash-dot-dot)
          do (progn
               (setf (cd:line-style canvas) style)
               (cd:line canvas 0 i w i )))
    ;; Draw all the hatch style possibilities in the top-left corner. Notice
    ;; that they have some small differences between drivers.
    (loop for i from 0 by 100
          for hatch in '(:hatch-vertical :hatch-forward-diagonal :hatch-backward-diagonal :hatch-cross :hatch-horizontal :hatch-diagonal-cross)
          do (progn
               (setf (cd:hatch canvas) hatch)
               (cd:box canvas i (+ i 100) (- h 100) h)))
    ;; Draw 4 regions, in diamond shape, at top, bottom, left, right, using
    ;; different interior styles.
    ;; At top, not filled polygon, notice that the last line style is used.
    (cd:with-vertices (canvas :path-mode-closed-lines)
      (cd:vertex canvas (/ w 2) (- h 100))
      (cd:vertex canvas (+ (/ w 2) 50) (- h 150))
      (cd:vertex canvas (/ w 2) (- h 200))
      (cd:vertex canvas (- (/ w 2) 50) (- h 150)))
    ;; At left, hatch filled polygon
    (setf (cd:hatch canvas) :hatch-diagonal-cross)
    (cd:with-vertices (canvas :path-mode-fill)
      (cd:vertex canvas 100 (/ h 2))
      (cd:vertex canvas 150 (+ (/ h 2) 50))
      (cd:vertex canvas 200 (/ h 2))
      (cd:vertex canvas 150 (- (/ h 2) 50))
      )
    ;; At right, pattern filled polygon
    ;;    FIXME (setf (cd:pattern canvas) (values :style-si))
    (cd:with-vertices (canvas :path-mode-fill)
      (cd:vertex canvas (- w 100) (/ h 2))
      (cd:vertex canvas (- w 150) (+ (/ h 2) 50))
      (cd:vertex canvas (- w 200) (/ h 2))
      (cd:vertex canvas (- w 150) (- (/ h 2) 50)))
    ;; At bottom, stipple filled polygon
    ;; FIXME stipple...
    (cd:with-vertices (canvas :path-mode-fill)
      (cd:vertex canvas (/ w 2) 100)
      (cd:vertex canvas (+ (/ w 2) 50) 150)
      (cd:vertex canvas (/ w 2) 200)
      (cd:vertex canvas (+ (/ w 2) -50) 150))
    ;; Draw two beziers at bottom-left
    (cd:with-vertices (canvas :path-mode-bezier)
      (cd:vertex canvas 100 100)
      (cd:vertex canvas 150 200)
      (cd:vertex canvas 180 250)
      (cd:vertex canvas 180 200)
      (cd:vertex canvas 180 150)
      (cd:vertex canvas 150 100)
      (cd:vertex canvas 300 100))
    ;; Draw an image
    (let* ((image-width 600)
           (image-height 400)
           (pixels (* image-width image-height)))
      (let ((channels (loop for c below 4
                            collect
                            (loop with channel = (make-array pixels :element-type '(unsigned-byte 8))
                                  for j below image-width
                                  do (loop for i below image-height
                                           do (setf (aref channel (+ (* i image-width) j))
                                                    (logand #xff
                                                            (floor (+ 128 (* 128 (sin (+ (* 0.01 j) (* 4 c)))))))))
                                  finally (return channel)))))
        (cd:transform-rotate canvas 25)
        (cd:transform-translate canvas (/ w 3) (- (/ h 3) 300))
        (cd:put-image-rect-rgba canvas image-width image-height
                                (elt channels 0) (elt channels 1) (elt channels 2) (elt channels 3)
                                0 0
                                600 400
                                0 0 0 0)))))

#+nil
(sb-int:with-float-traps-masked
    (:invalid)
  (cd:with-canvas (canvas (cd-pdf:context-pdf) "/tmp/foo.pdf -w150 -h110")
    (simple-draw canvas)))

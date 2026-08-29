;;;; src/image.lisp — raster images that do not involve IM.
;;;;
;;;; CD can put and get pixels directly, from three plane arrays for RGB or an
;;;; index array plus a colour table for mapped images. src/im.lisp is the
;;;; nicer route when the IM bindings are to hand; this is what remains when
;;;; they are not, and what a caller with pixels already in a Lisp array wants.
;;;;
;;;; A server image (CD's cdImage) is a different thing again: an offscreen
;;;; buffer belonging to a canvas, useful for saving and restoring a region.
;;;; CD marks it deprecated in favour of the double-buffer drivers, and it is
;;;; wrapped here only because a canvas that has one still has to release it.

(in-package #:cd)

(export '(put-image-rgb
          get-image-rgb
          put-image-map
          palette
          server-image
          kill-server-image
          with-server-image
          put-server-image
          capture-server-image))

(defun %plane-buffer (data count what)
  "Copy one plane of samples into freshly allocated foreign memory.

Returns the pointer; the caller frees it. CD reads the planes during the call
and keeps no reference, so stack allocation would do -- but the three planes
together can be large, and a 4-megapixel image is three 4 MB objects that have
no business on the control stack."
  (unless (>= (length data) count)
    (cl:error 'cd-error
              :detail (format nil "~A has ~D samples, need ~D"
                              what (length data) count)))
  (let ((pointer (cffi:foreign-alloc :unsigned-char :count count)))
    (dotimes (i count pointer)
      (setf (cffi:mem-aref pointer :unsigned-char i) (aref data i)))))

(defmacro %with-plane-buffers ((&rest bindings) count what &body body)
  "Bind each (VAR DATA) to a foreign copy of DATA, freeing all on exit."
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind ((var data) &rest rest) bindings
        `(let ((,var (%plane-buffer ,data ,count ,what)))
           (unwind-protect (%with-plane-buffers ,rest ,count ,what ,@body)
             (cffi:foreign-free ,var))))))

(defun put-image-rgb (canvas width height red green blue
                      &key (x 0) (y 0) (draw-width 0) (draw-height 0))
  "Draw a WIDTH by HEIGHT RGB image from three sample arrays.

RED, GREEN and BLUE are sequences of (UNSIGNED-BYTE 8), each WIDTH*HEIGHT
long, in CD's bottom-up row order. DRAW-WIDTH and DRAW-HEIGHT scale the result;
zero means natural size.

Three separate planes rather than interleaved pixels, because that is how both
CD and IM store an image -- converting from RGBRGB would mean allocating and
transposing, which is the caller's decision to make, not this function's."
  (let ((count (* width height)))
    (%with-plane-buffers ((r red) (g green) (b blue)) count "an RGB plane"
      (cd.ffi::%cd-canvas-put-image-rect-rgb (handle canvas) width height
                                             r g b x y draw-width draw-height
                                             0 0 0 0)))
  canvas)

(defun get-image-rgb (canvas width height &key (x 0) (y 0))
  "Read a WIDTH by HEIGHT region back as (VALUES RED GREEN BLUE).

Each is a fresh (UNSIGNED-BYTE 8) vector of WIDTH*HEIGHT samples.

Only drivers that hold a raster can do this. PostScript and SVG have no pixels
to read, and CD answers by leaving the buffers untouched rather than by
failing -- so a result of all zeros from a vector driver is CD declining, not
a black image."
  (let ((count (* width height)))
    (cffi:with-foreign-objects ((r :unsigned-char count)
                                (g :unsigned-char count)
                                (b :unsigned-char count))
      (cd.ffi::%cd-canvas-get-image-rgb (handle canvas) r g b x y width height)
      (flet ((collect (pointer)
               (let ((out (make-array count :element-type '(unsigned-byte 8))))
                 (dotimes (i count out)
                   (setf (aref out i) (cffi:mem-aref pointer :unsigned-char i))))))
        (values (collect r) (collect g) (collect b))))))

(defun put-image-map (canvas width height indices colors
                      &key (x 0) (y 0) (draw-width 0) (draw-height 0))
  "Draw a WIDTH by HEIGHT indexed image.

INDICES holds one palette index per pixel; COLORS is the palette, a sequence
of packed colours -- so any colour designator COLOR accepts works here too."
  (let ((count (* width height))
        (palette-size (length colors)))
    (%with-plane-buffers ((index indices)) count "the index plane"
      (cffi:with-foreign-object (table :long palette-size)
        (loop for entry across (coerce colors 'vector)
              for i from 0
              do (setf (cffi:mem-aref table :long i) (color entry)))
        (cd.ffi::%cd-canvas-put-image-rect-map (handle canvas) width height
                                               index table x y
                                               draw-width draw-height
                                               0 0 0 0))))
  canvas)

(defparameter *palette-modes*
  '((:polite . 0) (:force . 1))
  "How hard CD should try to install a palette.

:POLITE shares the system palette with other windows; :FORCE takes it over.
Only meaningful on the indexed-colour displays this API was designed for, and
a no-op on everything modern.")

(defun palette (canvas colors &key (mode :polite))
  "Install COLORS as the canvas palette.

Each entry is any colour designator. Relevant only to drivers with an indexed
colour model."
  (let ((size (length colors)))
    (cffi:with-foreign-object (table :long (max size 1))
      (loop for entry across (coerce colors 'vector)
            for i from 0
            do (setf (cffi:mem-aref table :long i) (color entry)))
      (cd.ffi::%cd-canvas-palette
       (handle canvas) size table
       (or (cdr (assoc mode *palette-modes*))
           (cl:error 'cd-error
                     :detail (format nil "~S is not a palette mode; expected one of ~S"
                                     mode (mapcar #'car *palette-modes*)))))))
  canvas)

;;; Server images -------------------------------------------------------------

(defun server-image (canvas width height)
  "Allocate an offscreen buffer belonging to CANVAS.

Deprecated by CD in favour of the double-buffer drivers, and kept because a
canvas that allocates one still has to release it. Prefer WITH-SERVER-IMAGE."
  (let ((pointer (cd.ffi::%cd-canvas-create-image (handle canvas) width height)))
    (when (cffi:null-pointer-p pointer)
      (cl:error 'cd-error
                :detail (format nil "could not allocate a ~Dx~D server image"
                                width height)))
    pointer))

(defun kill-server-image (image)
  "Release a server image."
  (cd.ffi::%cd-kill-image image)
  nil)

(defmacro with-server-image ((var canvas width height) &body body)
  "Allocate a server image for the extent of BODY and release it after.

Unlike a canvas, this is a bare foreign pointer with no finalizer behind it --
CD ties its lifetime to the canvas that made it, so wrapping it in a CLOS
object with independent finalization would invite freeing it after its canvas
had gone."
  (alexandria:with-gensyms (image)
    `(let ((,image (server-image ,canvas ,width ,height)))
       (let ((,var ,image))
         (unwind-protect (progn ,@body)
           (kill-server-image ,image))))))

(defun capture-server-image (canvas image &key (x 0) (y 0))
  "Copy a region of CANVAS into a server IMAGE."
  (cd.ffi::%cd-canvas-get-image (handle canvas) image x y)
  image)

(defun put-server-image (canvas image &key (x 0) (y 0))
  "Draw a server IMAGE onto CANVAS at (X, Y)."
  (cd.ffi::%cd-canvas-put-image-rect (handle canvas) image x y 0 0 0 0)
  canvas)

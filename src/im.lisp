;;;; src/im.lisp — the bridge between CD and IM.
;;;;
;;;; CD and IM are sibling Tecgraf libraries, and libcd is built against libim
;;;; here, so cdim.h already knows what an imImage is. This file makes the two
;;;; Lisp bindings agree too: these functions take and return IM:IMAGE objects
;;;; rather than the raw pointers underneath, so an image loaded with IM can be
;;;; drawn onto a CD canvas without either side knowing about the other's
;;;; representation.
;;;;
;;;;   (im:with-image (photo (im:load #p"photo.jpg"))
;;;;     (cd:with-canvas (c (cd:svg-canvas #p"out.svg"))
;;;;       (cd:put-image c photo :x 0 :y 0)))
;;;;
;;;; The bridge exists only when libcd was compiled with CD_ENABLE_IM. The
;;;; generator drives off the built library, so on a build without it these
;;;; functions simply are not defined -- which is why IM-BRIDGE-AVAILABLE-P
;;;; asks rather than assumes.

(in-package #:cd)

(export '(im-bridge-available-p
          put-image
          capture-image
          get-image
          pattern-image
          stipple-image
          wd-put-image
          wd-get-image))

(defun im-bridge-available-p ()
  "True when this build of CD contains the IM driver.

CD compiles its drivers in per CMake option, so a libcd built with
CD_ENABLE_IM=OFF has no cdCanvasPutImImage at all."
  (and (fboundp 'cd.ffi::%cd-canvas-put-im-image)
       (cffi:foreign-symbol-pointer "cdCanvasPutImImage")
       t))

(defun %require-bridge ()
  (unless (im-bridge-available-p)
    (cl:error 'driver-not-available
              :name "IM"
              :detail "libcd was built without CD_ENABLE_IM")))

(defun put-image (canvas image &key (x 0) (y 0) width height)
  "Draw IMAGE, an IM:IMAGE, onto CANVAS with its lower-left corner at (X, Y).

WIDTH and HEIGHT scale it; omitted, the image's own dimensions are used.
Integer coordinates use CD's integer entry point, anything else the double one.

Remember both libraries store images bottom-up, so (X, Y) is measured from the
bottom-left of the canvas and no flip is involved."
  (%require-bridge)
  (let ((w (or width (im:width image)))
        (h (or height (im:height image))))
    (if (and (integerp x) (integerp y) (integerp w) (integerp h))
        (cd.ffi::%cd-canvas-put-im-image (handle canvas) (im:handle image) x y w h)
        (cd.ffi::%cdf-canvas-put-im-image (handle canvas) (im:handle image)
                                          (%d x) (%d y) (%d w) (%d h))))
  canvas)

(defun get-image (canvas image &key (x 0) (y 0))
  "Capture part of CANVAS into IMAGE, an existing IM:IMAGE.

The image's own size decides how much is read, starting at (X, Y). Returns
IMAGE.

Not every driver can do this: reading pixels back requires the driver to have
them, so it works on the image and window drivers and does nothing useful on
PostScript or SVG, which have no raster to read. CD reports that by leaving
the image untouched rather than by failing."
  (%require-bridge)
  (cd.ffi::%cd-canvas-get-im-image (handle canvas) (im:handle image) x y)
  image)

(defun capture-image (canvas &key (x 0) (y 0) width height)
  "Capture part of CANVAS into a freshly allocated IM:IMAGE.

Convenience over GET-IMAGE for the common case of not already having an image
to read into. WIDTH and HEIGHT default to the whole canvas.

The caller owns the result: use IM:WITH-IMAGE or IM:DESTROY."
  (%require-bridge)
  (multiple-value-bind (canvas-width canvas-height) (canvas-size canvas)
    (let ((image (im:create (or width canvas-width) (or height canvas-height)
                            :color-space-rgb :data-type-byte)))
      (handler-case (get-image canvas image :x x :y y)
        ;; The image is ours until it is returned; anything going wrong between
        ;; allocating it and handing it back would otherwise leak it.
        (cl:error (c) (im:destroy image) (cl:error c))))))

(defun pattern-image (canvas image)
  "Use IMAGE as the fill pattern for subsequent filled shapes.

Sets the interior style to :pattern as a side effect, which is CD's behaviour
rather than a convenience added here."
  (%require-bridge)
  (cd.ffi::%cd-canvas-pattern-im-image (handle canvas) (im:handle image))
  canvas)

(defun stipple-image (canvas image)
  "Use IMAGE as the fill stipple for subsequent filled shapes.

A stipple is a one-bit mask painted in the foreground and background colours,
where a pattern carries its own colours."
  (%require-bridge)
  (cd.ffi::%cd-canvas-stipple-im-image (handle canvas) (im:handle image))
  canvas)

;;; The world-coordinate forms ------------------------------------------------

(defun wd-put-image (canvas image &key (x 0d0) (y 0d0) width height)
  "Draw IMAGE onto CANVAS at a world coordinate, sized in world units."
  (%require-bridge)
  (cd.ffi::%wd-canvas-put-im-image (handle canvas) (im:handle image)
                                   (%d x) (%d y)
                                   (%d (or width (im:width image)))
                                   (%d (or height (im:height image))))
  canvas)

(defun wd-get-image (canvas image &key (x 0d0) (y 0d0))
  "Capture part of CANVAS into IMAGE, positioned in world coordinates."
  (%require-bridge)
  (cd.ffi::%wd-canvas-get-im-image (handle canvas) (im:handle image) (%d x) (%d y))
  image)

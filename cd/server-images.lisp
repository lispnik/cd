(in-package #:cd)

;;; Server Image Management

(export '(create-image
          kill-image
          get-image
          put-image
          put-image-rect
          scroll-area
          image-width
          image-height
          create-image-rgb
          create-image-rgba
          create-image-map))

(deftype cd-image ()
  "CD image handle type."
  'cffi:foreign-pointer)

(defun create-image (canvas width height)
  "Create a server-side image with specified dimensions."
  (with-validation 'create-image
    (validate-canvas canvas 'create-image)
    (validate-dimensions width height 'create-image))

  (with-cd-error-checking ('create-image :canvas canvas)
    (let ((image (cd-cffi::%cd-canvas-create-image canvas width height)))
      (check-null-result image 'create-image :canvas canvas :what "image"))))

(defun kill-image (image)
  "Destroy a server-side image and free its resources."
  (when image
    (with-cd-error-checking ('kill-image)
      (cd-cffi::%cd-kill-image image))))

(defun get-image (canvas image x y)
  "Copy image data from canvas to server image at specified position."
  (with-validation 'get-image
    (validate-canvas canvas 'get-image)
    (validate-coordinate x 'x 'get-image)
    (validate-coordinate y 'y 'get-image))

  (with-cd-error-checking ('get-image :canvas canvas)
    (cd-cffi::%cd-canvas-get-image canvas image x y)))

(defun put-image (canvas image x y)
  "Draw server image to canvas at specified position."
  (with-validation 'put-image
    (validate-canvas canvas 'put-image)
    (validate-coordinate x 'x 'put-image)
    (validate-coordinate y 'y 'put-image))

  (with-cd-error-checking ('put-image :canvas canvas)
    (cd-cffi::%cd-canvas-put-image-rect canvas image x y 0 -1 0 -1)))

(defun put-image-rect (canvas image x y xmin xmax ymin ymax)
  "Draw portion of server image to canvas."
  (with-validation 'put-image-rect
    (validate-canvas canvas 'put-image-rect)
    (validate-coordinate x 'x 'put-image-rect)
    (validate-coordinate y 'y 'put-image-rect)
    (validate-coordinate xmin 'xmin 'put-image-rect)
    (validate-coordinate xmax 'xmax 'put-image-rect)
    (validate-coordinate ymin 'ymin 'put-image-rect)
    (validate-coordinate ymax 'ymax 'put-image-rect))

  (with-cd-error-checking ('put-image-rect :canvas canvas)
    (cd-cffi::%cd-canvas-put-image-rect canvas image x y xmin xmax ymin ymax)))

(defun scroll-area (canvas xmin xmax ymin ymax dx dy)
  "Scroll a rectangular area of the canvas."
  (with-validation 'scroll-area
    (validate-canvas canvas 'scroll-area)
    (validate-coordinate xmin 'xmin 'scroll-area)
    (validate-coordinate xmax 'xmax 'scroll-area)
    (validate-coordinate ymin 'ymin 'scroll-area)
    (validate-coordinate ymax 'ymax 'scroll-area)
    (validate-coordinate dx 'dx 'scroll-area)
    (validate-coordinate dy 'dy 'scroll-area))

  (with-cd-error-checking ('scroll-area :canvas canvas)
    (cd-cffi::%cd-canvas-scroll-area canvas xmin xmax ymin ymax dx dy)))

;; Enhanced image creation functions

(defun create-image-rgb (canvas width height red green blue)
  "Create server image from RGB data arrays."
  (with-validation 'create-image-rgb
    (validate-canvas canvas 'create-image-rgb)
    (validate-dimensions width height 'create-image-rgb)
    (let ((expected-size (* width height)))
      (validate-array red 'red 'create-image-rgb
                     :element-type '(unsigned-byte 8) :min-size expected-size)
      (validate-array green 'green 'create-image-rgb
                     :element-type '(unsigned-byte 8) :min-size expected-size)
      (validate-array blue 'blue 'create-image-rgb
                     :element-type '(unsigned-byte 8) :min-size expected-size)))

  (with-cd-error-checking ('create-image-rgb :canvas canvas)
    (cffi:with-foreign-objects ((red-ptr :unsigned-char (* width height))
                               (green-ptr :unsigned-char (* width height))
                               (blue-ptr :unsigned-char (* width height)))
      ;; Copy data to foreign memory
      (loop for i from 0 below (* width height) do
        (setf (cffi:mem-aref red-ptr :unsigned-char i) (aref red i)
              (cffi:mem-aref green-ptr :unsigned-char i) (aref green i)
              (cffi:mem-aref blue-ptr :unsigned-char i) (aref blue i)))
      (let ((image (cd-cffi::%cd-canvas-create-image-rgb canvas width height
                                                        red-ptr green-ptr blue-ptr)))
        (check-null-result image 'create-image-rgb :canvas canvas :what "image")))))

(defun create-image-rgba (canvas width height red green blue alpha)
  "Create server image from RGBA data arrays."
  (with-validation 'create-image-rgba
    (validate-canvas canvas 'create-image-rgba)
    (validate-dimensions width height 'create-image-rgba)
    (let ((expected-size (* width height)))
      (validate-array red 'red 'create-image-rgba
                     :element-type '(unsigned-byte 8) :min-size expected-size)
      (validate-array green 'green 'create-image-rgba
                     :element-type '(unsigned-byte 8) :min-size expected-size)
      (validate-array blue 'blue 'create-image-rgba
                     :element-type '(unsigned-byte 8) :min-size expected-size)
      (validate-array alpha 'alpha 'create-image-rgba
                     :element-type '(unsigned-byte 8) :min-size expected-size)))

  (with-cd-error-checking ('create-image-rgba :canvas canvas)
    (cffi:with-foreign-objects ((red-ptr :unsigned-char (* width height))
                               (green-ptr :unsigned-char (* width height))
                               (blue-ptr :unsigned-char (* width height))
                               (alpha-ptr :unsigned-char (* width height)))
      ;; Copy data to foreign memory
      (loop for i from 0 below (* width height) do
        (setf (cffi:mem-aref red-ptr :unsigned-char i) (aref red i)
              (cffi:mem-aref green-ptr :unsigned-char i) (aref green i)
              (cffi:mem-aref blue-ptr :unsigned-char i) (aref blue i)
              (cffi:mem-aref alpha-ptr :unsigned-char i) (aref alpha i)))
      (let ((image (cd-cffi::%cd-canvas-create-image-rgba canvas width height
                                                         red-ptr green-ptr blue-ptr alpha-ptr)))
        (check-null-result image 'create-image-rgba :canvas canvas :what "image")))))

(defun create-image-map (canvas width height indices palette)
  "Create server image from indexed color data."
  (with-validation 'create-image-map
    (validate-canvas canvas 'create-image-map)
    (validate-dimensions width height 'create-image-map)
    (let ((expected-size (* width height)))
      (validate-array indices 'indices 'create-image-map
                     :element-type '(unsigned-byte 8) :min-size expected-size)
      (validate-array palette 'palette 'create-image-map
                     :element-type 'integer :min-size 1)))

  (with-cd-error-checking ('create-image-map :canvas canvas)
    (let ((palette-size (length palette)))
      (cffi:with-foreign-objects ((indices-ptr :unsigned-char (* width height))
                                 (palette-ptr :long palette-size))
        ;; Copy data to foreign memory
        (loop for i from 0 below (* width height) do
          (setf (cffi:mem-aref indices-ptr :unsigned-char i) (aref indices i)))
        (loop for i from 0 below palette-size do
          (setf (cffi:mem-aref palette-ptr :long i) (aref palette i)))
        (let ((image (cd-cffi::%cd-canvas-create-image-map canvas width height
                                                          indices-ptr palette-ptr)))
          (check-null-result image 'create-image-map :canvas canvas :what "image"))))))

;; Image introspection (these would need to be added to CFFI bindings)
(defun image-width (image)
  "Get width of a server image (placeholder - needs CFFI implementation)."
  (declare (ignore image))
  (error 'cd-backend-error
         :operation 'image-width
         :backend "server-image"
         :format-control "Image introspection not available - CD library limitation"))

(defun image-height (image)
  "Get height of a server image (placeholder - needs CFFI implementation)."
  (declare (ignore image))
  (error 'cd-backend-error
         :operation 'image-height
         :backend "server-image"
         :format-control "Image introspection not available - CD library limitation"))

;; Convenience macros
(defmacro with-image ((image-var canvas width height) &body body)
  "Create an image, execute body, and ensure image is cleaned up."
  `(let ((,image-var nil))
     (unwind-protect
          (progn
            (setf ,image-var (create-image ,canvas ,width ,height))
            ,@body)
       (when ,image-var
         (ignore-errors (kill-image ,image-var))))))

(defmacro with-image-rgb ((image-var canvas width height red green blue) &body body)
  "Create an RGB image, execute body, and ensure image is cleaned up."
  `(let ((,image-var nil))
     (unwind-protect
          (progn
            (setf ,image-var (create-image-rgb ,canvas ,width ,height ,red ,green ,blue))
            ,@body)
       (when ,image-var
         (ignore-errors (kill-image ,image-var))))))

(export '(with-image with-image-rgb cd-image))
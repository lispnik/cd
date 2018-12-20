(in-package #:cd)

(export '(rgb-to-map))

(defun %channel-ptr-array (channel-ptr length)
  (let ((channel (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length channel)
      (setf (aref channel i)
            (cffi:mem-aref channel-ptr :unsigned-char i)))))

(defwrappers ("canvas-get-image-rgb" "image-rgb" "cd")
  (defun ^function-name (canvas x y width height)
    "Returns the red, green and blue components of each pixel in a server image
as 3 arrays of octets. 

The RGB components are provided in three matrices stored as byte arrays. The (i,
j) component of these matrices is at the address (j*WIDTH+i). As occurs with all
primitives from the Canvas Draw library, the pixel (0, 0) is at the bottom left
corner, and the pixel (WIDTH-1, HEIGHT-1) is that the upper right corner of the
image rectangle."
    (check-type width integer)
    (check-type height integer)
    (let ((length (* width height)))
      (cffi:with-foreign-objects
          ((r-ptr :unsigned-char length)
           (g-ptr :unsigned-char length)
           (b-ptr :unsigned-char length))
        (^cffi-function-name
         canvas
         r-ptr g-ptr b-ptr
         (coerce x ^type)
         (coerce y ^type)
         width
         height)
        (values (%channel-ptr-array r-ptr length)
                (%channel-ptr-array g-ptr length)
                (%channel-ptr-array b-ptr length))))))

(defwrappers ("canvas-put-image-rect-rgb" "put-image-rect-rgb")
  "Puts, in a specified area of the canvas, an image with its red, green and
blue components defined in the three matrices stored in byte arrays.

The (i, j) component of these matrices is at the address (j*IMAGE-WIDTH+i). The
pixel (0,0) is at the bottom left corner, and the pixel (IMAGE-WIDTH-1,
IMAGE-HEIGHT-1) is that the upper right corner of the image rectangle.

Parameters WIDTH and HEIGHT refer to the target rectangle of the canvas, so that
it is possible to reduce or expand the image drawn. If WIDTH and HEIGHT are 0,
the size of the image is assumed (IMAGE-WIDTH and IMAGE-HEIGHT).

It also allows specifying a rectangle inside the image to be drawn, if XMIN,
XMAX, YMIN and YMAX are 0 then the whole image is assumed.

If the driver has bpp <=8 or only 256 colors or less, then the image is
converted to 256 optimal colors using the function RGB-TO-MAP and is drawn using
PUT-IMAGE-RECT-MAP."
  (defun ^function-name (canvas image-width image-height red green blue x y width height xmin xmax ymin ymax)
    (check-type image-width integer)
    (check-type image-height integer) 
    (check-type xmin integer)
    (check-type xmax integer)
    (check-type ymin integer)
    (check-type ymax integer)
    (let ((length (* image-width image-height)))
      (cffi:with-foreign-objects
          ((r-ptr :unsigned-char length)
           (g-ptr :unsigned-char length)
           (b-ptr :unsigned-char length))
        (dotimes (i length)
          (setf (cffi:mem-aref r-ptr :unsigned-char i) (aref red i)
                (cffi:mem-aref b-ptr :unsigned-char i) (aref green i)
                (cffi:mem-aref g-ptr :unsigned-char i) (aref blue i)))
        (^cffi-function-name
         canvas
         image-width image-height
         r-ptr g-ptr b-ptr
         (coerce x ^type) (coerce y ^type) (coerce width ^type) (coerce height ^type)
         xmin xmax ymin ymax)))))

(defwrappers ("canvas-put-image-rect-rgba" "put-image-rect-rgba")
  "The same as function PUT-IMAGE-RECT-RGB, except for the fact that it is
possible to specify an alpha channel. 

The resulting color is the image color weighted by the alpha value, using the
formula result=(source * alpha + destiny * (255 - alpha))/255. This means that,
if alpha is 0, the resulting color is the target color (completely transparent),
and, if alpha is 255, the resulting color is the original image
color (completely opaque).

If this function is not defined for a given driver, then the function
PUT-IMAGE-RECT-RGB is used, as long as it is defined."
  (defun ^function-name
      (canvas image-width image-height red green blue alpha x y width height xmin xmax ymin ymax)
    (check-type image-width integer)
    (check-type image-height integer) 
    (check-type xmin integer)
    (check-type xmax integer)
    (check-type ymin integer)
    (check-type ymax integer)
    (let ((length (* image-width image-height)))
      (cffi:with-foreign-objects
          ((r-ptr :unsigned-char length)
           (g-ptr :unsigned-char length)
           (b-ptr :unsigned-char length)
           (a-ptr :unsigned-char length))
        (dotimes (i length)
          (setf (cffi:mem-aref r-ptr :unsigned-char i) (aref red i)
                (cffi:mem-aref b-ptr :unsigned-char i) (aref green i)
                (cffi:mem-aref g-ptr :unsigned-char i) (aref blue i)
                (cffi:mem-aref a-ptr :unsigned-char i) (aref alpha i)))
        (^cffi-function-name
         canvas
         image-width image-height
         r-ptr g-ptr b-ptr a-ptr
         (coerce x ^type) (coerce y ^type) (coerce width ^type) (coerce height ^type)
         xmin xmax ymin ymax)))))

(defwrappers ("canvas-put-image-rect-map" "put-image-rect-map")
  "The same as function PUT-IMAGE-RECT-RGB, except for the fact that the colors
are provided by means of an index matrix (map). The color corresponding to a
given index is given in colors[index]. The map is also a matrix stored as a byte
vector. If the color vector is NIL, then a vector with 256 gray tones is
assumed."
  (defun ^function-name
      (canvas image-width image-height index colors x y width height xmin xmax ymin ymax)
    (check-type image-width integer)
    (check-type image-height integer) 
    (check-type xmin integer)
    (check-type xmax integer)
    (check-type ymin integer)
    (check-type ymax integer)
    (check-type index (array * (256)))
    (check-type colors (or null (array * (256))))
    (if colors
        (cffi:with-foreign-objects
            ((index-ptr :unsigned-char 256)
             (colors-ptr :long 256))
          (dotimes (i 256)
            (setf (cffi:mem-aref index-ptr :unsigned-char i) (aref index i)
                  (cffi:mem-aref colors-ptr :unsigned-char i) (aref colors i)))
          (^cffi-function-name
           canvas
           image-width image-height
           index-ptr colors-ptr
           (coerce x ^type) (coerce y ^type) (coerce width ^type) (coerce height ^type)
           xmin xmax ymin ymax))
        (cffi:with-foreign-object
            (index-ptr :unsigned-char 256)
          (dotimes (i 256)
            (setf (cffi:mem-aref index-ptr :unsigned-char i) (aref index i)))
          (^cffi-function-name
           canvas
           image-width image-height
           index-ptr (cffi:null-pointer)
           (coerce x ^type) (coerce y ^type) (coerce width ^type) (coerce height ^type)
           xmin xmax ymin ymax)))))


(defwrappers ("canvas-get-im-image" "get-im-image" "cd")
  (defun ^function-name (canvas image x y)
    "The same as the GET-IMAGE-RGB functions except for the fact that use an
IM:IM-IMAGE. Image must be a display RGB image (color_space=IM_RGB and
data_type=IM_BYTE). See the CD IM-IMAGE context documentation. (since 5.9)"
    (^cffi-function-name canvas image (coerce x ^type) (coerce y ^type))))

(defwrappers ("canvas-put-im-image" "put-im-image")
  (defun ^function-name ()
    "The same as the above functions except for the fact that use an imImage
structure. Image must be a displayable image (imImageIsBitmap), and it can has
an alpha channel. See the CD IM-IMAGE documentation. (since 5.9)"
    (^cffi-function-name
     canvas
     image
     (coerce x ^type)
     (coerce y ^type)
     (coerce width ^type)
     (coerce type ^type))))

(defun rgb-to-map (image-width image-height red green blue &optional (palette-size 256))
  "Converts an RGB image into an image with 256 indexed colors, returning the
INDEX and COLORS as values. The resulting image must have the same
size (IMAGE-WIDTH x IMAGE-HEIGHT) as the RGB image. This is the same algorithm
used in the IM library - in fact, the same code."
  (check-type image-width integer)
  (check-type image-height integer)
  (check-type palette-size (integer 0 256))
  (let ((length (* image-width image-height)))
    (cffi:with-foreign-objects
        ((r-ptr :unsigned-char length)
         (g-ptr :unsigned-char length)
         (b-ptr :unsigned-char length)
         (index-ptr :unsigned-char palette-size)
         (colors-ptr :long palette-size))
      (dotimes (i length)
        (setf (cffi:mem-aref r-ptr :unsigned-char i) (aref red i)
              (cffi:mem-aref g-ptr :unsigned-char i) (aref green i)
              (cffi:mem-aref b-ptr :unsigned-char i) (aref blue i)))
      (cd-cffi::%cd-rgb-to-map 
       image-width image-height
       r-ptr g-ptr b-ptr
       index-ptr
       palette-size
       colors-ptr)
      (let ((index (make-array palette-size :element-type '(unsigned-byte 8)))
            (colors (make-array palette-size)))
        (dotimes (i palette-size (values index colors))
          (setf (aref index i) (cffi:mem-aref index-ptr :unsigned-char i)
                (aref colors i) (cffi:mem-aref colors-ptr 'cd-cffi::encoded-color)))))))


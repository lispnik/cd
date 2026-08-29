;;;; src/color.lisp — colours.
;;;;
;;;; CD packs a colour into a long: 8 bits each of red, green, blue and,
;;;; optionally, alpha. Every attribute and primitive that takes a colour takes
;;;; that long, so the packed form is what crosses the FFI boundary.
;;;;
;;;; The wrappers accept either. Anywhere a colour is wanted you may pass the
;;;; packed integer CD itself uses, or a list (R G B) / (R G B A) with
;;;; components 0-255, or one of the named colours below. COLOR normalises all
;;;; three, so callers never have to remember which a particular function
;;;; expects -- which was the previous binding's main colour hazard, since a
;;;; list handed where a long belonged became a type error at the alien
;;;; boundary rather than anything legible.

(in-package #:cd)

(export '(color
          encode-color
          decode-color
          color-red
          color-green
          color-blue
          color-alpha
          *named-colors*
          +black+ +white+ +red+ +green+ +blue+
          +yellow+ +cyan+ +magenta+ +gray+ +dark-gray+ +transparent+))

(defun encode-color (red green blue &optional alpha)
  "Pack 0-255 components into the long CD uses for a colour.

With ALPHA the result carries an opacity CD's alpha-aware drivers honour: 255
is opaque and 0 fully transparent. Note the direction -- CD stores alpha, not
transparency, despite cdEncodeAlpha's name suggesting otherwise in places."
  (if alpha
      (cd.ffi::%cd-encode-color-alpha red green blue alpha)
      (cd.ffi::%cd-encode-color red green blue)))

(defun decode-color (color)
  "(VALUES RED GREEN BLUE ALPHA) from a packed colour, each 0-255."
  (cffi:with-foreign-objects ((r :unsigned-char) (g :unsigned-char)
                              (b :unsigned-char) (a :unsigned-char))
    (cd.ffi::%cd-decode-color-alpha color r g b a)
    (values (cffi:mem-ref r :unsigned-char)
            (cffi:mem-ref g :unsigned-char)
            (cffi:mem-ref b :unsigned-char)
            (cffi:mem-ref a :unsigned-char))))

(defun color-red   (color) (nth-value 0 (decode-color color)))
(defun color-green (color) (nth-value 1 (decode-color color)))
(defun color-blue  (color) (nth-value 2 (decode-color color)))
(defun color-alpha (color) (nth-value 3 (decode-color color)))

(defparameter *named-colors*
  ;; CD's own palette, from the constants in cd.h. Kept as a table rather than
  ;; as constants alone so COLOR can accept the keyword spelling.
  (let ((table (make-hash-table :test #'eq)))
    (loop for (name r g b) in '((:black 0 0 0)
                                (:white 255 255 255)
                                (:red 255 0 0)
                                (:green 0 255 0)
                                (:blue 0 0 255)
                                (:yellow 255 255 0)
                                (:cyan 0 255 255)
                                (:magenta 255 0 255)
                                (:dark-red 128 0 0)
                                (:dark-green 0 128 0)
                                (:dark-blue 0 0 128)
                                (:dark-yellow 128 128 0)
                                (:dark-cyan 0 128 128)
                                (:dark-magenta 128 0 128)
                                (:gray 128 128 128)
                                (:dark-gray 64 64 64)
                                (:light-gray 192 192 192))
          do (setf (gethash name table) (encode-color r g b)))
    table)
  "CD's named colours, keyword to packed value.")

(defun color (designator)
  "Normalise a colour designator to the packed long CD expects.

Accepts what a caller is likely to have to hand:

  an integer      already packed; passed through
  (r g b)         components 0-255
  (r g b a)       with alpha, 255 opaque
  :red, :black    a name from *NAMED-COLORS*

Every wrapper that takes a colour runs its argument through this, so the three
spellings are interchangeable everywhere rather than in whichever functions
remembered to allow them."
  (etypecase designator
    (integer designator)
    (keyword (or (gethash designator *named-colors*)
                 (cl:error 'cd-error
                           :detail (format nil "~S is not a known colour name; ~
                                                try one of ~S"
                                           designator
                                           (sort (loop for k being the hash-keys
                                                         of *named-colors*
                                                       collect k)
                                                 #'string<)))))
    (cons (destructuring-bind (r g b &optional a) designator
            (encode-color r g b a)))))

(defmacro %defcolor (name key)
  `(defparameter ,name (gethash ,key *named-colors*)
     ,(format nil "CD's ~(~A~), packed." key)))

(%defcolor +black+ :black)
(%defcolor +white+ :white)
(%defcolor +red+ :red)
(%defcolor +green+ :green)
(%defcolor +blue+ :blue)
(%defcolor +yellow+ :yellow)
(%defcolor +cyan+ :cyan)
(%defcolor +magenta+ :magenta)
(%defcolor +gray+ :gray)
(%defcolor +dark-gray+ :dark-gray)

(defparameter +transparent+ (encode-color 0 0 0 0)
  "Fully transparent black.

Only meaningful on drivers that honour alpha; the rest treat it as black.")

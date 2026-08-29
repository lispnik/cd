;;;; src/canvas.lisp — the CANVAS class, its lifetime, and the drivers.
;;;;
;;;; CD creates a canvas from a context and a driver-specific "data" string:
;;;;
;;;;   cdCreateCanvas(cdContextPS(), "out.ps -pA4 -s300 -e")
;;;;   cdCreateCanvas(cdContextSVG(), "out.svg 200x150 3.5")
;;;;
;;;; Each driver invents its own syntax for that string, documents it in its
;;;; own HTML page, and reports a malformed one by returning NULL with no
;;;; explanation. Making callers assemble those by hand is the single worst
;;;; part of using CD from another language, so this file does not: every
;;;; driver gets a constructor with named parameters that builds the string.
;;;;
;;;; MAKE-CANVAS remains for drivers not wrapped here, and for anyone who
;;;; would rather write the string themselves.

(in-package #:cd)

(export '(canvas
          canvasp
          handle
          kill
          killed-p
          with-canvas
          with-canvases
          make-canvas
          drivers
          driver-available-p
          svg-canvas
          postscript-canvas
          pdf-canvas
          metafile-canvas
          picture-canvas
          image-rgb-canvas
          cgm-canvas
          dxf-canvas
          dgn-canvas
          debug-canvas
          canvas-size
          canvas-size-mm
          canvas-resolution
          simulate
          activate-state
          save-state
          restore-state))

(defclass canvas ()
  ((handle :initarg :handle :accessor %handle
           :documentation "The cdCanvas*, or NIL once killed.")
   (driver :initarg :driver :initform nil :reader canvas-driver
           :documentation "The driver name, for reports.")
   (finalizer-key :initform nil :accessor %finalizer-key
                  :documentation
                  "A cons whose CAR is the pointer the finalizer releases.

Indirection so KILL can disarm the finalizer without racing it: setting the
CAR to NIL is a single write, and a finalizer finding NIL does nothing."))
  (:documentation
   "A CD drawing surface.

Created against a driver -- SVG, PostScript, PDF, an in-memory image -- and
released by KILL, by WITH-CANVAS on unwind, or as a last resort by a
finalizer. For the file drivers the release is not merely tidiness: CD writes
the file's trailer in cdKillCanvas, so a canvas that is never killed leaves a
truncated and usually unreadable file."))

(defun canvasp (object) (typep object 'canvas))

(defmethod print-object ((canvas canvas) stream)
  (print-unreadable-object (canvas stream :type t :identity nil)
    (if (killed-p canvas)
        (format stream "killed")
        (multiple-value-bind (width height) (canvas-size canvas)
          (format stream "~A ~Dx~D" (or (canvas-driver canvas) "?") width height)))))

;;; Lifetime ------------------------------------------------------------------

(defun %release (key)
  "Release the pointer in KEY, once. Used by both KILL and the finalizer."
  (let ((pointer (car key)))
    (when pointer
      (setf (car key) nil)
      (cd.ffi::%cd-kill-canvas pointer))))

(defun wrap-handle (pointer &key driver detail)
  "Wrap a fresh cdCanvas* in a CANVAS, arming its finalizer."
  (when (or (null pointer) (cffi:null-pointer-p pointer))
    (cl:error 'canvas-creation-error
              :detail (or detail (format nil "~A driver" (or driver "unknown")))))
  (let* ((key (list pointer))
         (canvas (make-instance 'canvas :handle pointer :driver driver)))
    (setf (%finalizer-key canvas) key)
    ;; Closes over KEY, never over CANVAS: a finalizer that references the
    ;; object whose collection triggers it keeps that object alive forever.
    (tg:finalize canvas (lambda () (%release key)))
    canvas))

(defun handle (canvas)
  "The live cdCanvas* behind CANVAS, or signal INVALID-CANVAS.

Every operation goes through this, which is what turns drawing into freed
memory into a condition with a name."
  (or (%handle canvas)
      (cl:error 'invalid-canvas :canvas canvas)))

(defun killed-p (canvas) (null (%handle canvas)))

(defun kill (canvas)
  "Release CANVAS. Safe to call more than once.

For a file driver this is what writes the trailer and closes the file, so it
is not optional: an unkilled PostScript or PDF canvas leaves a file no reader
will accept."
  (when (%handle canvas)
    (setf (%handle canvas) nil)
    (tg:cancel-finalization canvas)
    (%release (%finalizer-key canvas)))
  nil)

(defmacro with-canvas ((var form) &body body)
  "Evaluate BODY with VAR bound to the canvas FORM returns, killing it after.

The canvas is killed however BODY leaves -- return, error or throw -- which
for a file driver is the difference between a complete file and a truncated
one."
  `(let ((,var ,form))
     (unwind-protect (progn ,@body)
       (when ,var (kill ,var)))))

(defmacro with-canvases (bindings &body body)
  "WITH-CANVAS over several bindings, released in reverse order."
  (if (null bindings)
      `(progn ,@body)
      `(with-canvas ,(first bindings)
         (with-canvases ,(rest bindings) ,@body))))

;;; Drivers -------------------------------------------------------------------

(defparameter *driver-contexts*
  '(("SVG"        . cd.ffi::%cd-context-svg)
    ("PS"         . cd.ffi::%cd-context-ps)
    ("PDF"        . cd.ffi::%cd-context-pdf)
    ("METAFILE"   . cd.ffi::%cd-context-metafile)
    ("PICTURE"    . cd.ffi::%cd-context-picture)
    ("IMAGE"      . cd.ffi::%cd-context-image)
    ("IMAGERGB"   . cd.ffi::%cd-context-image-rgb)
    ("DBUFFER"    . cd.ffi::%cd-context-d-buffer)
    ("DBUFFERRGB" . cd.ffi::%cd-context-d-buffer-rgb)
    ("CGM"        . cd.ffi::%cd-context-cgm)
    ("DXF"        . cd.ffi::%cd-context-dxf)
    ("DGN"        . cd.ffi::%cd-context-dgn)
    ("DEBUG"      . cd.ffi::%cd-context-debug)
    ("CLIPBOARD"  . cd.ffi::%cd-context-clipboard)
    ("NATIVEWINDOW" . cd.ffi::%cd-context-native-window)
    ("GL"         . cd.ffi::%cd-context-gl)
    ("PRINTER"    . cd.ffi::%cd-context-printer)
    ("IMIMAGE"    . cd.ffi::%cd-context-im-image))
  "Driver name to the C function returning its context.

Which of these exist is decided when libcd is compiled: the drivers are
built into the one library according to CMake options, so a build configured
with CD_ENABLE_PDF=OFF genuinely has no PDF context and the symbol is absent.
DRIVER-AVAILABLE-P asks the loaded library rather than assuming.")

(defun driver-available-p (name)
  "True when this build of CD contains the named driver."
  (let ((fn (cdr (assoc (string-upcase name) *driver-contexts* :test #'string=))))
    (and fn (fboundp fn)
         (let ((context (ignore-errors (funcall fn))))
           (and context (not (cffi:null-pointer-p context)))))))

(defun drivers ()
  "The drivers this build of CD actually contains, as a list of names.

Worth calling before assuming: the same version of CD compiled twice can offer
different drivers, and the failure mode otherwise is a NULL context and a
canvas that will not create."
  (loop for (name . fn) in *driver-contexts*
        when (and (fboundp fn)
                  (let ((context (ignore-errors (funcall fn))))
                    (and context (not (cffi:null-pointer-p context)))))
          collect name))

(defun %context (name)
  "The cdContext* for NAME, or signal DRIVER-NOT-AVAILABLE."
  (let ((fn (cdr (assoc (string-upcase name) *driver-contexts* :test #'string=))))
    (unless (and fn (fboundp fn))
      (cl:error 'driver-not-available :name name))
    (let ((context (funcall fn)))
      (when (or (null context) (cffi:null-pointer-p context))
        (cl:error 'driver-not-available :name name))
      context)))

(defun make-canvas (driver data)
  "Create a canvas on DRIVER from the raw CD data string DATA.

The escape hatch. Prefer the per-driver constructors below, which build DATA
from named parameters -- CD reports a malformed string by returning NULL and
saying nothing about what it disliked."
  ;; cdCreateCanvas types its second argument void*, not char*, because a few
  ;; drivers are handed a struct rather than a string -- the window drivers
  ;; take a native handle. Every driver wrapped here takes a string, so it has
  ;; to be marshalled explicitly; passing the Lisp string straight through is
  ;; a type error at the alien boundary.
  ;;
  ;; Freeing it on the way out is safe: the file drivers parse the string
  ;; inside cdCreateCanvas -- opening the file and writing its header before
  ;; returning -- and keep no reference to it.
  (cffi:with-foreign-string (foreign data)
    (wrap-handle (cd.ffi::%cd-create-canvas (%context driver) foreign)
                 :driver (string-upcase driver)
                 :detail (format nil "~A with data ~S" (string-upcase driver) data))))

;;; Data-string construction --------------------------------------------------

(defun %number (value)
  "Format a number the way CD's parsers expect: no exponent, no trailing dot.

CD scans these with sscanf(\"%g\"), which does accept exponent notation --
but Lisp prints a double as 1.5d2, and the d is not something %g knows."
  (cond ((integerp value) (format nil "~D" value))
        ((rationalp value) (format nil "~,4F" (cl:float value 1.0d0)))
        (t (let ((s (format nil "~,4F" value)))
             ;; Trim the trailing zeros %g would not have printed.
             (if (find #\. s)
                 (string-right-trim "." (string-right-trim "0" s))
                 s)))))

(defun %quote-filename (filename)
  "CD requires a filename containing spaces to be double-quoted."
  (let ((name (namestring (translate-logical-pathname filename))))
    (if (find #\Space name) (format nil "\"~A\"" name) name)))

;;; The file drivers ----------------------------------------------------------

(defun svg-canvas (filename &key width-mm height-mm resolution)
  "An SVG canvas writing to FILENAME.

WIDTH-MM and HEIGHT-MM are the canvas size in millimetres; RESOLUTION is
pixels per millimetre. CD's own defaults apply when they are omitted, and its
default size is INT_MAX in both directions, which is rarely what anyone wants
-- pass them.

  (cd:with-canvas (c (cd:svg-canvas \"plot.svg\" :width-mm 200 :height-mm 150))
    (cd:line c 0 0 100 100))"
  (make-canvas "SVG"
               (format nil "~A~@[ ~Ax~A~]~@[ ~A~]"
                       (%quote-filename filename)
                       (when (and width-mm height-mm) (%number width-mm))
                       (when (and width-mm height-mm) (%number height-mm))
                       (when resolution (%number resolution)))))

(defun metafile-canvas (filename &key width-mm height-mm resolution)
  "A CD metafile canvas writing to FILENAME.

A metafile records the drawing rather than rasterising it, so it can be
replayed into any other driver later with PLAY."
  (make-canvas "METAFILE"
               (format nil "~A~@[ ~Ax~A~]~@[ ~A~]"
                       (%quote-filename filename)
                       (when (and width-mm height-mm) (%number width-mm))
                       (when (and width-mm height-mm) (%number height-mm))
                       (when resolution (%number resolution)))))

(defparameter *paper-sizes*
  '((:a0 . 0) (:a1 . 1) (:a2 . 2) (:a3 . 3) (:a4 . 4) (:a5 . 5)
    (:letter . 6) (:legal . 7))
  "CD's predefined paper sizes, in the order cd.h's anonymous enum declares
them. Passed to the PostScript and PDF drivers as -p<n>.")

(defun %paper-code (paper)
  (or (cdr (assoc paper *paper-sizes*))
      (cl:error 'cd-error
                :detail (format nil "~S is not a known paper size; expected one of ~S"
                                paper (mapcar #'car *paper-sizes*)))))

(defun %page-options (&key paper width-mm height-mm left right bottom top
                           resolution landscape debug-p margin)
  "The -p/-w/-h/-l/-r/-b/-t/-s/-o/-g flags PostScript and PDF share."
  (with-output-to-string (s)
    (when paper (format s " -p~D" (%paper-code paper)))
    (when width-mm (format s " -w~A" (%number width-mm)))
    (when height-mm (format s " -h~A" (%number height-mm)))
    (when left (format s " -l~A" (%number left)))
    (when right (format s " -r~A" (%number right)))
    (when bottom (format s " -b~A" (%number bottom)))
    (when top (format s " -t~A" (%number top)))
    (when resolution (format s " -s~D" (round resolution)))
    (when landscape (write-string " -o" s))
    (when debug-p (write-string " -g" s))
    (when margin (format s " -d~A" (%number margin)))))

(defun postscript-canvas (filename &key paper width-mm height-mm
                                        left right bottom top
                                        resolution landscape
                                        encapsulated level-1 debug-p margin)
  "A PostScript canvas writing to FILENAME.

PAPER is :A0 through :A5, :LETTER or :LEGAL; or give WIDTH-MM and HEIGHT-MM.
ENCAPSULATED writes EPS, LEVEL-1 restricts output to PostScript Level 1.
RESOLUTION is in DPI.

  (cd:with-canvas (c (cd:postscript-canvas \"fig.eps\"
                                           :paper :a4 :encapsulated t))
    ...)"
  (make-canvas "PS"
               (concatenate 'string
                            (%quote-filename filename)
                            (%page-options :paper paper :width-mm width-mm
                                           :height-mm height-mm
                                           :left left :right right
                                           :bottom bottom :top top
                                           :resolution resolution
                                           :landscape landscape :debug-p debug-p
                                           :margin margin)
                            (if encapsulated " -e" "")
                            (if level-1 " -1" ""))))

(defun pdf-canvas (filename &key paper width-mm height-mm
                                 left right bottom top resolution landscape margin)
  "A PDF canvas writing to FILENAME. Parameters as for POSTSCRIPT-CANVAS."
  (make-canvas "PDF"
               (format nil "~A~A"
                       (%quote-filename filename)
                       (%page-options :paper paper :width-mm width-mm
                                      :height-mm height-mm :left left :right right
                                      :bottom bottom :top top :resolution resolution
                                      :landscape landscape :margin margin))))

(defun cgm-canvas (filename &key width-mm height-mm resolution binary)
  "A Computer Graphics Metafile canvas. BINARY selects the binary encoding."
  (make-canvas "CGM"
               (format nil "~A~@[ ~Ax~A~]~@[ ~A~]~@[ -b~*~]"
                       (%quote-filename filename)
                       (when (and width-mm height-mm) (%number width-mm))
                       (when (and width-mm height-mm) (%number height-mm))
                       (when resolution (%number resolution))
                       binary)))

(defun dxf-canvas (filename &key width-mm height-mm resolution)
  "An AutoCAD DXF canvas."
  (make-canvas "DXF"
               (format nil "~A~@[ ~Ax~A~]~@[ ~A~]"
                       (%quote-filename filename)
                       (when (and width-mm height-mm) (%number width-mm))
                       (when (and width-mm height-mm) (%number height-mm))
                       (when resolution (%number resolution)))))

(defun dgn-canvas (filename &key width-mm height-mm resolution)
  "A MicroStation DGN canvas."
  (make-canvas "DGN"
               (format nil "~A~@[ ~Ax~A~]~@[ ~A~]"
                       (%quote-filename filename)
                       (when (and width-mm height-mm) (%number width-mm))
                       (when (and width-mm height-mm) (%number height-mm))
                       (when resolution (%number resolution)))))

(defun debug-canvas (filename)
  "A canvas that writes a log of every CD call to FILENAME.

Not a drawing driver: it records what was asked of it, which is the fastest
way to see what a piece of drawing code actually emits."
  (make-canvas "DEBUG" (%quote-filename filename)))

;;; In-memory drivers ---------------------------------------------------------

(defun picture-canvas (&key width-mm height-mm resolution)
  "A canvas that records drawing in memory for later replay with PLAY."
  (make-canvas "PICTURE"
               (format nil "~@[~Ax~A~]~@[ ~A~]"
                       (when (and width-mm height-mm) (%number width-mm))
                       (when (and width-mm height-mm) (%number height-mm))
                       (when resolution (%number resolution)))))

(defun image-rgb-canvas (width height &key resolution)
  "A canvas drawing into an in-memory RGB buffer WIDTH by HEIGHT pixels."
  (make-canvas "IMAGERGB"
               (format nil "~Dx~D~@[ -r~A~]" width height
                       (when resolution (%number resolution)))))

;;; Canvas queries ------------------------------------------------------------

(defun canvas-size (canvas)
  "(VALUES WIDTH HEIGHT) in pixels."
  (cffi:with-foreign-objects ((w :int) (h :int) (wmm :double) (hmm :double))
    (cd.ffi::%cd-canvas-get-size (handle canvas) w h wmm hmm)
    (values (cffi:mem-ref w :int) (cffi:mem-ref h :int))))

(defun canvas-size-mm (canvas)
  "(VALUES WIDTH-MM HEIGHT-MM) in millimetres."
  (cffi:with-foreign-objects ((w :int) (h :int) (wmm :double) (hmm :double))
    (cd.ffi::%cd-canvas-get-size (handle canvas) w h wmm hmm)
    (values (cffi:mem-ref wmm :double) (cffi:mem-ref hmm :double))))

(defun canvas-resolution (canvas)
  "Pixels per millimetre, derived from the canvas's pixel and physical size."
  (multiple-value-bind (width height) (canvas-size canvas)
    (declare (ignore height))
    (multiple-value-bind (width-mm height-mm) (canvas-size-mm canvas)
      (declare (ignore height-mm))
      (if (zerop width-mm) 0d0 (/ width width-mm)))))

;;; Saved state ---------------------------------------------------------------

(defun save-state (canvas)
  "Capture CANVAS's current attributes as an opaque state object.

The result is foreign memory owned by the caller; release it with
RESTORE-STATE's counterpart cdReleaseState, which WITH-SAVED-STATE does."
  (cd.ffi::%cd-canvas-save-state (handle canvas)))

(defun restore-state (canvas state)
  "Restore attributes previously captured by SAVE-STATE."
  (cd.ffi::%cd-canvas-restore-state (handle canvas) state)
  canvas)

(defun activate-state (canvas)
  "Make CANVAS the target of CD's global API.

Exposed only because a few CD entry points -- and any C code sharing the
canvas -- still read the active canvas. Nothing in this binding needs it: the
drawing operations all take their canvas explicitly."
  (cd.ffi::%cd-canvas-activate (handle canvas))
  canvas)

(defun simulate (canvas mode)
  "Force CD to simulate primitives rather than use the driver's own.

MODE is a bitmask from CD's simulation flags. Returns the previous value."
  (cd.ffi::%cd-canvas-simulate (handle canvas) mode))

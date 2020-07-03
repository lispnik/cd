(in-package #:cd)

(export '(context-cairo-image-rgb
          context-cairo-pdf
          context-cairo-ps
          context-cairo-svg
          context-cgm
          context-clipboard
          context-dbuffer
          context-dbuffer-rgb
          context-debug
          context-dgn
          context-dxf
          context-emf
          context-image
          context-image-rgb
          context-metafile
          context-native-window
          context-picture
          context-pptx
          context-printer
          context-ps
          context-svg
          context-wmf
          initialization-error
          create-canvas
          call-with-canvas
          with-canvas))

(setf (fdefinition 'context-cairo-image-rgb) #'cd-cffi::%cd-context-cairo-image-rgb)
(setf (fdefinition 'context-cairo-pdf) #'cd-cffi::%cd-context-cairo-pdf)
(setf (fdefinition 'context-cairo-ps) #'cd-cffi::%cd-context-cairo-ps)
(setf (fdefinition 'context-cairo-svg) #'cd-cffi::%cd-context-cairo-svg)
(setf (fdefinition 'context-cgm) #'cd-cffi::%cd-context-cgm)
(setf (fdefinition 'context-clipboard) #'cd-cffi::%cd-context-clipboard)
(setf (fdefinition 'context-dbuffer) #'cd-cffi::%cd-context-dbuffer)
(setf (fdefinition 'context-dbuffer-rgb) #'cd-cffi::%cd-context-dbuffer-rgb)
(setf (fdefinition 'context-debug) #'cd-cffi::%cd-context-debug)
(setf (fdefinition 'context-dgn) #'cd-cffi::%cd-context-dgn)
(setf (fdefinition 'context-dxf) #'cd-cffi::%cd-context-dxf)
(setf (fdefinition 'context-emf) #'cd-cffi::%cd-context-emf)
(setf (fdefinition 'context-image) #'cd-cffi::%cd-context-image)
(setf (fdefinition 'context-image-rgb) #'cd-cffi::%cd-context-image-rgb)
(setf (fdefinition 'context-metafile) #'cd-cffi::%cd-context-metafile)
(setf (fdefinition 'context-native-window) #'cd-cffi::%cd-context-native-window)
(setf (fdefinition 'context-picture) #'cd-cffi::%cd-context-picture)
(setf (fdefinition 'context-pptx) #'cd-cffi::%cd-context-pptx)
(setf (fdefinition 'context-printer) #'cd-cffi::%cd-context-printer)
(setf (fdefinition 'context-ps) #'cd-cffi::%cd-context-ps)
(setf (fdefinition 'context-svg) #'cd-cffi::%cd-context-svg)
(setf (fdefinition 'context-wmf) #'cd-cffi::%cd-context-wmf)

(define-condition initialization-error (error)
  ((spec :initarg :spec))
  (:report (lambda (condition stream)
	     (format stream "Error creating canvas from spec ~S"
		     (slot-value condition 'spec)))
   )
  (:documentation
   "Signaled when a canvas cannot be created."))

(defun create-canvas (context &optional spec)
  "Creates a CD canvas for a virtual visualization surface (VVS). A VVS may be
the canvas of a user-interface window, the page of a document sent to a printer,
an offscreen image, the clipboard, a metafile, and so on. To create the canvas,
it is necessary to specify the driver in which each canvas is implemented.

The driver is set by the driver variable with additional information provided in
the data parameter. Even though it is possible to create more than one canvas
with the same driver/data pair, this is not recommended, and its behavior is not
specified. Each canvas maintains its own features.

In case of failure, a condition of type CD:INITIALIZATION-ERROR is raised.

The following predefined drivers are available:

Window-Base Drivers

FIXME convert to Lisp documenation:
 
    CD_IUP = IUP Canvas (cdiup.h).
    CD_NATIVEWINDOW = Native Window (cdnative.h).
    CD_GL = Native Window (cdgl.h).

Device-Based Drivers

    CD_CLIPBOARD = Clipboard (cdclipbd.h).
    CD_PRINTER = Printer (cdprint.h).
    CD_PICTURE = Picture in memory (cdpicture.h).

Image-Based Drivers 

    CD_IMAGE = Server-Image Drawing (cdimage.h).
    CD_IMAGERGB = Client-Image Drawing (cdirgb.h).
    CD_DBUFFER = Offscreen Drawing (cddbuf.h).
    CD_DBUFFERRGB = Client Offscreen Drawing (cddbuf.h).

File-Based Drivers 

    CD_PDF = Adobe Portable Document Format (cdpdf.h).
    CD_PS = PostScript File (cdps.h).
    CD_SVG = Scalable Vector Graphics (cdsvg.h).
    CD_METAFILE = Internal CD Metafile (cdmf.h).
    CD_DEBUG = Internal CD Debug Log (cddebug.h).
    CD_CGM = Computer Graphics Metafile ISO (cdcgm.h).
    CD_DGN = MicroStation Design File (cddgn.h).
    CD_DXF = AutoCad Drawing Interchange File (cddxf.h).
    CD_EMF = Microsoft Windows Enhanced Metafile (cdemf.h). Works only in MS Windows systems.
    CD_WMF = Microsoft Windows Metafile (cdwmf.h). Works only in MS Windows systems."
  (let* ((cffi-spec (or spec (cffi:null-pointer)))
	 (canvas
           (etypecase cffi-spec
             (string
              (with-foreign-string (spec-ptr cffi-spec)
                (cd-cffi::%cd-create-canvas context spec-ptr)))
             (tecgraf-base:ihandle
              (cd-cffi::%cd-create-canvas context (pffft:pointer cffi-spec)))
             (cffi:foreign-pointer
              (progn
                (cd-cffi::%cd-create-canvas context cffi-spec))))))
    (if (cffi:null-pointer-p canvas)
	(error 'initialization-error :spec spec)
	canvas)))

(defun call-with-canvas (context spec func)
  (let ((canvas (create-canvas context spec)))
    (unwind-protect
         (funcall func canvas)
      (kill canvas))))

(defmacro with-canvas ((canvas context spec) &body body)
  `(call-with-canvas ,context ,spec #'(lambda (,canvas) ,@body)))


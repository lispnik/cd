(in-package #:cd)

(export '(kill
          activation-error
          activate
          deactivate
          use-context-plus
          context
          context-capabilities
          context-type
          context-plus-p
          simulate))

(defun kill (canvas)
  "Destroys a previously created canvas."
  (cd-cffi::%cd-kill-canvas canvas))

(define-condition activation-error (error) ()
  (:documentation "Signaled when a canvas cannot be activated."))

(defun activate (canvas)
  "Activates a canvas for drawing. This is used only for a few drivers. Native
Window and IUP drivers will update the canvas size if the window size has
changed. Double Buffer driver will recreate the image buffer if the window
canvas size has changed. In these cases the function MUST be called, for other
drivers is useless.

Condition ACTIVATION-ERROR is signaled when activation fails."
  (when (eq :error (cd-cffi::%cd-canvas-activate canvas))
    (error 'activation-error)))

(defun deactivate (canvas)
  "Called when the application has finished drawing in the canvas. It is
optional, but if used for the Native Window driver in Windows when the handle
can not be retained, the drawing can only be done again after am ACTIVATE. On
some drivers will simply call FLUSH."
  (cd-cffi::%cd-canvas-deactivate canvas))

(defun use-context-plus (use-p)
  "Activates or deactivates the use of an external context for the next calls of
the create canvas functions. Returns the previous activation state."
  (cd-cffi::%cd-use-context-plus use-p))

(defun context (canvas)
  "Returns the context of a given canvas, which can be compared with the
predefined contexts, such as CD_PS."
  (cd-cffi::%cd-canvas-get-context canvas))

(defun context-capabilities (context)
  "Returns the list of resources available for that context:

:FLUSH
:CLEAR 
:PLAY 
:YAXIS - The native Y axis orientation is bottom-up.
:CLIPAREA 
:CLIPPOLY - Usually is not implemented.
:REGION - Usually is not implemented. 

:RECT - Rectangles are implemented directly in the driver (they are usually 
simulated).

:CHORD
:IMAGERGB 

:IMAGERGBA - If this is not implemented, but GET-IMAGE-RGB is, then it is
simulated using GET-IMAGE-RGB and PUT-IMAGE-RGB.

:IMAGEMAP 
:GETIMAGERGB 

:IMAGESRV - Usually is only implemented in contexts of window graphics
systems (Native Window and IUP).

:BACKGROUND 
:BACKOPACITY 
:WRITEMODE 
:LINESTYLE 
:LINEWITH 

:FPRIMTIVES - Primitives using floating point coordinates are implemented
directly in the driver (they are usually simulated).

:HATCH 
:STIPPLE 
:PATTERN 
:FONT 

:FONTDIM - If not defined, the function is implemented using an internal
heuristics of the library.

:TEXTSIZE - If not defined, the function is implemented using an internal
heuristics of the library.

:TEXTORIENTATION - Usually is not implemented.

:PALETTE - Usually is only implemented in contexts of window graphics
systems (Native Window and IUP).

:LINECAP   
:LINEJOIN
:PATH  
:BEZIER"
  (cd-cffi::%cd-context-caps context))

(defun context-type (context)
  "Returns the type of the context:

:WINDOW - GUI window based
:DEVICE - device based (clipboard, printer, picture)
:IMAGE - server or client image based, including double buffer based
:FILE - metafile based"
  (cd-cffi::%cd-context-type context))

(defun context-plus-p (context)
  "Returns true if it is a context plus."
  (cd-cffi::%cd-context-is-plus context))

(defun simulate (canvas mode-list)
  "Activates the simulation of one or more primitives. It is ignored for the
canvas in the ImageRGB context, because in this case everything is already
simulated. It also has no effect for primitives that are usually simulated. It
returns the previous simulation, but does not include primitives that are
usually simulated. The simulation can be activated at any moment. For instance,
if a line simulation is required only for a situation, the simulation can be
activated for the line to be drawn, and then deactivated.

If simulation is activated the driver transformation matrix is disabled.

See in the Simulation sub-driver the information on how each simulation is
performed.

:NONE - Deactivates all kinds of simulation.
:LINE 
:RECT 
:BOX 
:ARC 
:SECTOR 
:CHORD 
:POLYLINE 
:POLYGON 
:TEXT 
:ALL - Activates all simulation options. 
:LINES - Combination of :LINE, :RECT, :ARC and :POLYLINE.
:FILLS - Combination of :BOX, :SECTOR, :CHORD and :POLYGON."
  (cffi:foreign-bitfield-symbols
   'cd-cffi::simulation-mode
   (cd-cffi::%cd-canvas-simulate
    canvas
    (cffi:foreign-bitfield-value 'cd-cffi::simulation-mode (if mode-list mode-list '(:none))))))

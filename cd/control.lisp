(in-package #:cd)

(export '(clear
          flush
          save-state
          restore-state
          release-state
          call-with-state-saved
          with-state-saved
          attribute))

(defun clear (canvas)
  "Cleans the active canvas using the current background color.

This action is interpreted very differently by each driver. Many drivers simply
draw a rectangle with the current background color. It is NOT necessary to call
CLEAR when the canvas has just been created, as at this moment it is already
clean. Most file-based drivers do not implement this function."
  (cd-cffi::%cd-canvas-clear canvas))

(defun flush (canvas)
  "Has a different meaning for each driver. It is useful to send information to
buffered devices and to move to a new page or layer. In all cases, the current
canvas attributes are preserved."
  (cd-cffi::%cd-canvas-flush canvas))

(defun save-state (canvas)
  "Saves the state of attributes of the active canvas. It does not save PLAY callbacks,
polygon creation states (begin/vertex/vertex/...), the palette, complex clipping
regions and driver internal attributes."
  (cd-cffi::%cd-canvas-save-state canvas))

(defun restore-state (canvas state)
  "Restores the attribute state of the active canvas. It can be used between
canvases of different contexts. It can be used several times for the same
state."
  (cd-cffi::%cd-canvas-restore-state canvas state))

(defun release-state (state)
  "Releases the memory allocated by the SAVE-STATE function."
  (cd-cffi::%cd-release-state state))

(defun call-with-state-saved (canvas func)
  "Functional version of WITH-STATE-SAVED. Call FUNC with argument CANVAS,
saving the canvas state before and and restoring the canvas state and releasing
state memory after."
  (let ((state (save-state canvas)))
    (unwind-protect
	 (funcall func canvas)
      (progn
	(restore-state canvas state)
	(release-state state)))))

(defmacro with-state-saved ((canvas) &body body)
  "Save the state of CANVAS, perform BODY and then restore the state of CANVAS."
  (with-gensyms (canvas-var)
    `(let ((,canvas-var ,canvas))
       (call-with-state-saved ,canvas-var #'(lambda (canvas) ,@body)))))

(defun attribute-name (name)
  (check-type name (or string keyword))
  (if (stringp name)
      name
      (symbol-name name)))

(defun (setf attribute) (new-value canvas name)
  "Modifies a custom attribute directly in the driver of the active canvas. If
the driver does not have this attribute, the call is ignored. All drivers have
the USERDATA attribute (since 5.9)."
  (cd-cffi::%cd-canvas-set-attribute
   canvas
   (attribute-name name)
   new-value-ptr))

(defun attribute (canvas name)
  "Returns a custom attribute from the driver of the active canvas. If the
driver does not have this attribute, it returns NIL."
  (cd-cffi::%cd-canvas-get-attribute canvas (attribute-name name)))


;;;; src/play.lisp — replaying recorded drawing.
;;;;
;;;; A metafile or picture canvas records drawing rather than rasterising it.
;;;; PLAY interprets such a recording onto another canvas, which is how CD
;;;; converts between its output formats: record once, replay into SVG, into
;;;; PostScript, into an image buffer.

(in-package #:cd)

(export '(play play-file register-callback with-callback *callback-ids*))

(defconstant +continue+ 0 "A callback's answer to let PLAY carry on.")
(defconstant +abort+ 1 "A callback's answer to stop PLAY.")

(defparameter *callback-ids*
  '((:size . 0)                ; CD_SIZECB, cd.h
    (:cgm-counter . 1)         ; CD_CGMCOUNTERCB and the rest, cdcgm.h
    (:cgm-scale-mode . 2)
    (:cgm-vdc-extent . 3)
    (:cgm-begin-picture . 4)
    (:cgm-begin-picture-body . 5)
    (:cgm-begin-metafile . 6))
  "Callback identifiers, by driver.

These are NOT interchangeable. CD numbers callbacks per driver, so 1 means
CD_CGMCOUNTERCB to the CGM driver and nothing at all to the metafile one, and
each callback has its own argument list -- cdCallback is declared variadic
precisely because they do not agree.

There is no generic \"called for every primitive\" callback. An earlier version
of this file offered one, registering id 0 against the metafile driver on the
assumption that ids were universal; cdContextRegisterCallback answered CD_ERROR
and the replay then failed, which is how the assumption was caught.")

(defvar *callbacks* (make-hash-table :test #'equal)
  "Registered Lisp callbacks, keyed by (DRIVER . CALLBACK-ID).")

(defvar *current-callback-key* nil
  "Which registration the trampoline is serving.

Set around a registration rather than passed in, because the trampoline's
signature is fixed by C and carries nowhere to put it.")

;;; The trampoline ------------------------------------------------------------
;;;
;;; The same rule as IM's progress counter, and for the same reason: a Lisp
;;; condition must not unwind through C. CD is mid-interpretation with its own
;;; allocations, and a non-local exit here would leave them. Catch everything
;;; and answer in the only vocabulary C has -- which for CD is CD_CONTINUE or
;;; CD_ABORT, not the zero-and-nonzero most callbacks use.
;;;
;;; One trampoline serves every callback because CFFI:CALLBACK yields a
;;; per-symbol pointer, so a distinct Lisp function per registration would need
;;; a distinct DEFCALLBACK. The registration key tells the trampoline which
;;; Lisp function to reach; the extra arguments each callback carries are not
;;; forwarded, since their number and type differ per callback and reading them
;;; wrongly is a crash rather than a wrong answer.

(cffi:defcallback %callback-trampoline :int ((canvas :pointer))
  (let ((handler (gethash *current-callback-key* *callbacks*)))
    (if (null handler)
        +continue+
        (handler-case
            (if (funcall handler canvas) +continue+ +abort+)
          (cl:error () +abort+)))))

(defun register-callback (driver callback-id function)
  "Register FUNCTION as the CALLBACK-ID callback of DRIVER.

CALLBACK-ID is a key from *CALLBACK-IDS* or a raw integer. FUNCTION is called
with the canvas and returns true to continue or NIL to abort the replay.

Signals if the driver does not support that callback -- cdContextRegisterCallback
answers CD_OK or CD_ERROR, and ignoring it is how a callback comes to be
silently not installed."
  (let* ((id (if (keywordp callback-id)
                 (or (cdr (assoc callback-id *callback-ids*))
                     (cl:error 'cd-error
                               :detail (format nil "~S is not a known callback; expected one of ~S"
                                               callback-id (mapcar #'car *callback-ids*))))
                 callback-id))
         (key (cons (string-upcase driver) id)))
    (setf (gethash key *callbacks*) function
          *current-callback-key* key)
    (let ((status (cd.ffi::%cd-context-register-callback
                   (%context driver) id
                   (if function
                       (cffi:callback %callback-trampoline)
                       (cffi:null-pointer)))))
      (when (minusp status)
        (remhash key *callbacks*)
        (cl:error 'unsupported-operation
                  :detail (format nil "the ~A driver has no callback ~A"
                                  (string-upcase driver) callback-id)))
      function)))

(defmacro with-callback ((driver callback-id function) &body body)
  "Register FUNCTION for the extent of BODY, then remove it.

  (cd:with-callback (\"CGM\" :cgm-counter
                     (lambda (canvas) (declare (ignore canvas)) t))
    (cd:play-file target #p\"drawing.cgm\" :driver \"CGM\"))

Removal matters beyond tidiness: a callback pointer left registered into a
Lisp image that is later dumped and restored is a crash with no useful
backtrace."
  (alexandria:with-gensyms (d id)
    `(let ((,d ,driver) (,id ,callback-id))
       (register-callback ,d ,id ,function)
       (unwind-protect (progn ,@body)
         (ignore-errors (register-callback ,d ,id nil))))))

;;; Replay --------------------------------------------------------------------

(defun play (canvas driver data &key (xmin 0) (xmax 0) (ymin 0) (ymax 0))
  "Replay the recording named by DATA onto CANVAS, through DRIVER.

The bounds scale the result; all four zero means natural size. DRIVER is the
name of the driver that produced the recording -- \"METAFILE\" for a .cdm
file, \"PICTURE\" for one held in memory.

Returns CANVAS. CD reports failure with CD_ERROR, which becomes an error here
rather than a return value nobody checks."
  (let ((result (cffi:with-foreign-string (foreign data)
                  (cd.ffi::%cd-canvas-play (handle canvas) (%context driver)
                                           xmin xmax ymin ymax foreign))))
    (when (minusp result)
      (cl:error 'cd-error
                :detail (format nil "replaying ~A through the ~A driver"
                                data (string-upcase driver))))
    canvas))

(defun play-file (canvas pathname &key (driver "METAFILE")
                                       (xmin 0) (xmax 0) (ymin 0) (ymax 0))
  "Replay a recorded drawing file onto CANVAS.

  (cd:with-canvas (out (cd:svg-canvas #p\"out.svg\"))
    (cd:play-file out #p\"recorded.cdm\"))

which is CD's answer to converting between vector formats: whatever was
recorded is re-emitted through whichever driver the target canvas uses."
  (play canvas driver (namestring (translate-logical-pathname pathname))
        :xmin xmin :xmax xmax :ymin ymin :ymax ymax))

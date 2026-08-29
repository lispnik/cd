;;;; tests/package.lisp — suite definition.
;;;;
;;;; Suite definitions and the helpers shared across the test files: a
;;;; temporary directory, and WITH-IM-BRIDGE for the tests that need a libcd
;;;; built with CD_ENABLE_IM.

(defpackage #:cd.tests
  (:use #:common-lisp #:fiveam)
  (:export #:cd-suite #:run-all))

(in-package #:cd.tests)

(def-suite cd-suite
  :description "The CD Common Lisp bindings.")

(defun run-all ()
  "Run the whole suite. Returns NIL if anything failed."
  (run! 'cd-suite))

(defparameter *tmp-dir*
  (uiop:ensure-directory-pathname
   (uiop:merge-pathnames* "cd-tests/" (uiop:temporary-directory))))

(defun tmp-file (name)
  (ensure-directories-exist *tmp-dir*)
  (uiop:merge-pathnames* name *tmp-dir*))

(defun file-size (path)
  (with-open-file (s path :element-type '(unsigned-byte 8)) (file-length s)))

(defmacro with-im-bridge (&body body)
  "Run BODY only where libcd provides the IM bridge, and skip otherwise.

CD compiles its drivers in per CMake option, so a libcd built with
CD_ENABLE_IM=OFF has no cdCanvasPutImImage at all -- which is why the binding
offers IM-BRIDGE-AVAILABLE-P rather than assuming. A test that failed instead
of skipping would be asserting how libcd happened to be configured, not
anything about this binding.

A skip is not as good as a pass: where these tests skip, the bridge is
genuinely uncovered."
  `(if (cd:im-bridge-available-p)
       (progn ,@body)
       (skip "libcd was built without CD_ENABLE_IM; the IM bridge is absent.")))

(defun optional-binding-p (name)
  "True for a binding a legitimate build of libcd may simply not contain.

Only the IM bridge so far: its entry points all carry ImImage in the name.
Everything else this binding declares is unconditional in CD's sources."
  (and (search "ImImage" name) t))

;;; ---------------------------------------------------------------------------

(def-suite library-suite :in cd-suite
  :description "Loading libcd and reporting what it contains.")
(in-suite library-suite)

(test library-is-loaded
  (is-true (cd:library-loaded-p))
  (is (stringp (cd:library-pathname))))

(test version-is-reported
  (is (stringp (cd:version)))
  (is (plusp (length (cd:version)))))

(test every-binding-resolves
  "Every C function this binding declares exists in the loaded libcd.

The check the previous binding lacked, and the reason ten dead entries
survived in it: cdCanvasBezier, cdCanvasSpline and cdCanvasTextBounds among
them were declared in headers that no library implements."
  (let* ((missing (remove-if #'cffi:foreign-symbol-pointer cd.ffi::*bindings*))
         (optional (remove-if-not #'optional-binding-p missing))
         (required (remove-if #'optional-binding-p missing)))
    ;; Absent optional entry points are reported, not asserted on: this test
    ;; exists to catch bindings to functions no build of CD provides, which is
    ;; a different thing from a driver this build was configured without.
    (when optional
      (format t "~&; ~D optional binding~:P absent from this libcd: ~{~A~^, ~}~%"
              (length optional) optional))
    (is (null required)
        "~D bound function~:P do not exist in the loaded libcd: ~{~A~^, ~}"
        (length required) required)))

(test drivers-are-reported
  "DRIVERS asks the library rather than assuming: CD compiles its drivers in
per CMake option, so the same version built twice can offer different ones."
  (let ((drivers (cd:drivers)))
    (is (listp drivers))
    (is (plusp (length drivers)))
    ;; SVG and metafile are pure-C drivers with no external dependency, so a
    ;; build without them would be a very unusual one.
    (is (member "SVG" drivers :test #'string=))
    (is-true (cd:driver-available-p "SVG"))
    (is-false (cd:driver-available-p "NOSUCHDRIVER"))))

;;; ---------------------------------------------------------------------------

(def-suite canvas-suite :in cd-suite
  :description "Canvas creation, lifetime and the driver constructors.")
(in-suite canvas-suite)

(test svg-canvas-honours-its-size
  (cd:with-canvas (c (cd:svg-canvas (tmp-file "size.svg")
                                    :width-mm 200 :height-mm 150))
    (multiple-value-bind (w h) (cd:canvas-size c)
      (is (plusp w)) (is (plusp h)))
    (multiple-value-bind (wmm hmm) (cd:canvas-size-mm c)
      (is (< 199d0 wmm 201d0))
      (is (< 149d0 hmm 151d0)))))

(test kill-writes-the-file-trailer
  "A file canvas must be killed, not merely dropped: CD writes the trailer in
cdKillCanvas, so an unkilled canvas leaves a file no reader will accept."
  (let ((path (tmp-file "trailer.svg")))
    (cd:with-canvas (c (cd:svg-canvas path :width-mm 50 :height-mm 50))
      (cd.ffi::%cd-canvas-line (cd:handle c) 0 0 10 10))
    (is (probe-file path))
    (is (plusp (file-size path)))
    (let ((text (uiop:read-file-string path)))
      (is (search "</svg>" text) "the trailer must have been written"))))

(test postscript-canvas-writes-eps
  (let ((path (tmp-file "fig.eps")))
    (cd:with-canvas (c (cd:postscript-canvas path :paper :a4 :encapsulated t))
      (cd.ffi::%cd-canvas-line (cd:handle c) 0 0 100 100))
    (with-open-file (s path)
      (is (search "EPSF" (read-line s)) "an -e canvas must produce EPS"))))

(test kill-is-idempotent
  (let ((c (cd:svg-canvas (tmp-file "idem.svg"))))
    (is-false (cd:killed-p c))
    (cd:kill c)
    (is-true (cd:killed-p c))
    (finishes (cd:kill c))
    (finishes (cd:kill c))))

(test use-after-kill-signals
  (let ((c (cd:svg-canvas (tmp-file "uak.svg"))))
    (cd:kill c)
    (signals cd:invalid-canvas (cd:handle c))
    (signals cd:invalid-canvas (cd:canvas-size c))))

(test with-canvas-releases-on-error
  (let (captured)
    (ignore-errors
     (cd:with-canvas (c (cd:svg-canvas (tmp-file "unwind.svg")))
       (setf captured c)
       (cl:error "unwind")))
    (is-true (cd:killed-p captured))))

(test finalizer-releases-escaped-canvases
  "Canvases that never reach a WITH-CANVAS are still released, at GC."
  (dotimes (i 50)
    (cd:svg-canvas (tmp-file (format nil "escape-~D.svg" i))))
  (finishes (tg:gc :full t)))

(test image-rgb-canvas-has-the-requested-pixels
  (cd:with-canvas (c (cd:image-rgb-canvas 320 240))
    (multiple-value-bind (w h) (cd:canvas-size c)
      (is (= 320 w))
      (is (= 240 h)))))

;;; ---------------------------------------------------------------------------

(def-suite condition-suite :in cd-suite
  :description "What failures look like.")
(in-suite condition-suite)

(test every-condition-is-an-error
  (dolist (name '(cd:cd-error cd:driver-error cd:driver-not-available
                  cd:canvas-creation-error cd:invalid-canvas
                  cd:unsupported-operation cd:library-not-found))
    (is (subtypep name 'cl:error) "~A must be a subtype of CL:ERROR" name)
    (is (subtypep name 'cd:cd-condition) "~A must be a CD:CD-CONDITION" name)))

(test missing-driver-is-reported-by-name
  (signals cd:driver-not-available (cd:make-canvas "NOSUCHDRIVER" "x"))
  (handler-case (cd:make-canvas "NOSUCHDRIVER" "x")
    (cd:driver-not-available (c)
      (is (string= "NOSUCHDRIVER" (cd:driver-not-available-name c))))))

(test unwritable-path-is-a-creation-error
  "cdCreateCanvas returns NULL and says nothing; the binding says which
driver and what data string it was given, that being the only evidence."
  (signals cd:canvas-creation-error
    (cd:svg-canvas "/nonexistent-directory-xyz/out.svg"))
  (handler-case (cd:svg-canvas "/nonexistent-directory-xyz/out.svg")
    (cd:canvas-creation-error (c)
      (is-true (cd:error-detail c)))))

(test conditions-report-usefully
  (dolist (form (list (make-condition 'cd:driver-not-available :name "PDF")
                      (make-condition 'cd:invalid-canvas)
                      (make-condition 'cd:canvas-creation-error :detail "SVG")
                      (make-condition 'cd:library-not-found
                                      :detail "libcd" :candidates '("a" "b"))))
    (let ((text (princ-to-string form)))
      (is (plusp (length text)))
      (is (not (search "#<" text)) "~A printed as an unreadable object"
          (type-of form)))))

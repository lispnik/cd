(defpackage #:cd-examples.sierpinski
  (:use #:common-lisp)
  (:export #:sierpinski))

(in-package #:cd-examples.sierpinski)

;;; This is adapted from Edi's LTK example in Common Lisp Recipes: A
;;; Problem-Solution Approach

(defun sierpinski (canvas level)
  (multiple-value-bind
        (w h)
      (cd:size canvas)
    (labels ((square (x y x-size y-size)
               (cd:box canvas x (+ x x-size) y (+ y y-size)))
             (recurse (x y x-size y-size level)
               (let ((x-step (/ x-size 3))
                     (y-step (/ y-size 3)))
                 (square (+ x x-step) (+ y y-step) x-step y-step)
                 (when (plusp level)
                   (dolist (x-next (list x (+ x x-step) (+ x x-step x-step)))
                     (dolist (y-next (list y (+ y y-step) (+ y y-step y-step)))
                       (recurse x-next y-next x-step y-step (1- level))))))))
      (recurse 0 0 w h level))))

#+nil
(let ((canvas #+linux (cd:create-canvas (cd:context-ps) "/tmp/sierpinski.ps -w1000 -h1000")
	      #+windows (cd:create-canvas (cd-pdf:context-pdf) "c:/users/matthew/projects/foo.pdf")))
  (unwind-protect
       (progn
         (setf (cd:foreground canvas) cd:+black+)
         (sierpinski canvas 6))
    (cd:kill canvas)))

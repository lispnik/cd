(in-package #:cd)

(export '(+mm-to-pt+
          +pt-to-mm+
          +radians-to-degrees+
          +degrees-to-radians+
          +font-small+
          +font-standard+
          +font-large+))

(defconstant +mm-to-pt+ 2.834645669
  "Number of points per millimeter.")

(defconstant +pt-to-mm+ (/ 1 +mm-to-pt+)
  "Number of millimeters per point.")

(defconstant +radians-to-degrees+ (/ 180 pi)
  "Number of degrees per radian.")

(defconstant +degrees-to-radians+ (/ pi 180)
  "Number of radians per degree.")

(defconstant +font-small+ 8 "Small font size in points.")
(defconstant +font-standard+ 12 "Standard font size in points.")
(defconstant +font-large+ 18 "Large font size in points.")

(defun degrees (radians)
  "Convert RADIANS to degrees"
  (* +radians-to-degrees+ radians))

(defun radians (degrees)
  "Convert DEGREES to radians"
  (* +degrees-to-radians+ degrees))

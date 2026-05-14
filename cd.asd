(defsystem #:cd
  :description "CFFI bindings to CD, a 2D Graphics Library"
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :homepage "https://github.com/lispnik/cd"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "cd"
  :components ((:file "package")
	       (:file "utils")
               (:file "errors")
               (:file "validation")
               (:file "error-checking")
               (:file "recovery")
	       (:file "context")
               (:file "constants")
               (:file "init")
               (:file "control")
               (:file "coord")
               (:file "world")
               (:file "world-complete")
               (:file "attributes")
               (:file "wrapper")
               (:file "clipping")
               (:file "primitives")
               (:file "text")
	       (:file "vector")
               (:file "images")
               (:file "patterns")
               (:file "fonts")
               (:file "server-images")
               (:file "color-management")
               (:file "transforms")
               (:file "advanced-drawing")
               (:file "advanced-text")
               (:file "advanced-images")
               (:file "animation")
               (:file "backend-extensions")
               (:file "other")
	       (:file "cd"))
  :depends-on (#:cd-cffi
               #:tecgraf-base
               #:cffi
               #:alexandria
               #:split-sequence))

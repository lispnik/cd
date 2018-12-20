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
	       (:file "context")
               (:file "constants")
               (:file "init")
               (:file "control")                 
               (:file "coord")
               (:file "world")
               (:file "attributes")
               (:file "wrapper")
               (:file "clipping")
               (:file "primitives")
               (:file "text")
	       (:file "vector")
               (:file "images")
               (:file "other")
	       (:file "cd"))
  :depends-on (#:cd-cffi
               #:cffi
               #:trivial-garbage
               #:alexandria
               #:serapeum))

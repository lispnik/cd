(defsystem #:cd-all
  :description "CFFI bindings to CD, a 2D Graphics Library"
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :homepage "https://github.com/lispnik/cd"
  :licence "MIT"
  :depends-on (#:cd
	       #:cd-context-plus
	       #:cd-gl
	       #:cd-im
	       #:cd-pdf))
	       

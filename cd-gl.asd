(defsystem #:cd-gl
  :serial t
  :pathname "gl"
  :components ((:file "gl"))
  :depends-on (#:cd-gl-cffi
	       #:cffi
	       #:alexandria))

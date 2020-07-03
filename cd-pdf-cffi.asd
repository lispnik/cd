(defsystem #:cd-pdf-cffi
  :serial t
  :pathname "pdf"
  :components ((:file "pdf-cffi"))
  :depends-on (#:cffi
	       #:cd-cffi))

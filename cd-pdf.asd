(defsystem #:cd-pdf
  :serial t
  :pathname "pdf"
  :components ((:file "pdf"))
  :depends-on (#:cd-pdf-cffi
	       #:cd
	       #:cffi
	       #:serapeum))

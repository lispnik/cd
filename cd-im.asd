(defsystem #:cd-im
  :serial t
  :pathname "im"
  :components ((:file "im"))
  :depends-on (#:cd-im-cffi
	       #:cffi
	       #:alexandria
	       #:serapeum))

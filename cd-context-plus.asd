(defsystem #:cd-context-plus
  :serial t
  :pathname "context-plus"
  :components ((:file "context-plus"))
  :depends-on (#:cd-context-plus-cffi
	       #:cffi
	       #:alexandria
	       #:serapeum))

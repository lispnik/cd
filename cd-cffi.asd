(defsystem #:cd-cffi
  :serial t
  :pathname "cd"
  :components ((:file "cd-cffi")
	       (:file "context-cffi"))
  :depends-on (#:cffi))

(defsystem #:cd-context-plus-cffi
  :serial t
  :pathname "context-plus"
  :components ((:file "context-plus-cffi"))
  :depends-on (#:cffi))

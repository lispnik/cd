(defsystem #:cd-gl-cffi
  :serial t
  :pathname "gl"
  :components ((:file "gl-cffi"))
  :depends-on (#:cffi
               #:cd-cffi))

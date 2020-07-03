(defsystem #:cd-im-cffi
  :serial t
  :pathname "im"
  :components ((:file "im-cffi"))
  :depends-on (#:im-cffi
               #:cffi
               #:cd-cffi))

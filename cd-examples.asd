(defsystem #:cd-examples
  :serial t
  :pathname "examples"
  :components ((:file "simpledraw")
               (:file "sierpinski"))
  :depends-on (#:cd
               #:babel))

(defsystem #:cd-tests
  :description "Test suite for CD graphics library bindings"
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  :depends-on (#:cd
               #:cd-all
               #:fiveam
               #:alexandria)
  :serial t
  :pathname "tests"
  :components ((:file "package")
               (:file "test-utils")
               (:file "basic-tests")
               (:file "context-tests")
               (:file "canvas-tests")
               (:file "drawing-tests")
               (:file "text-tests")
               (:file "font-tests")
               (:file "image-tests")
               (:file "server-image-tests")
               (:file "color-tests")
               (:file "pattern-tests")
               (:file "transformation-tests")
               (:file "clipping-tests")
               (:file "vector-tests")
               (:file "world-coordinate-tests")
               (:file "world-complete-tests")
               (:file "error-handling-tests")
               (:file "backend-tests")
               (:file "integration-tests")
               (:file "performance-tests")
               (:file "transform-tests")
               (:file "advanced-drawing-tests")
               (:file "advanced-text-tests")
               (:file "advanced-image-tests")
               (:file "animation-tests")
               (:file "backend-extension-tests")
               (:file "run-tests"))
  :perform (test-op (o c)
                    (symbol-call :cd-tests :run-all-tests)))
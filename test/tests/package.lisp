(defpackage #:cd-tests
  (:use #:common-lisp #:cd #:fiveam #:alexandria)
  (:export #:run-all-tests
           #:run-basic-tests
           #:run-drawing-tests
           #:run-text-tests
           #:run-image-tests
           #:run-error-tests
           #:run-performance-tests))

(in-package #:cd-tests)

(def-suite cd-test-suite
  :description "Complete test suite for CD graphics library bindings")

(def-suite basic-tests :in cd-test-suite)
(def-suite context-tests :in cd-test-suite)
(def-suite canvas-tests :in cd-test-suite)
(def-suite drawing-tests :in cd-test-suite)
(def-suite text-tests :in cd-test-suite)
(def-suite font-tests :in cd-test-suite)
(def-suite image-tests :in cd-test-suite)
(def-suite server-image-tests :in cd-test-suite)
(def-suite color-tests :in cd-test-suite)
(def-suite pattern-tests :in cd-test-suite)
(def-suite transformation-tests :in cd-test-suite)
(def-suite clipping-tests :in cd-test-suite)
(def-suite vector-tests :in cd-test-suite)
(def-suite world-coordinate-tests :in cd-test-suite)
(def-suite error-handling-tests :in cd-test-suite)
(def-suite backend-tests :in cd-test-suite)
(def-suite integration-tests :in cd-test-suite)
(def-suite performance-tests :in cd-test-suite)
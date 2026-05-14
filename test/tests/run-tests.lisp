(in-package #:cd-tests)

;;; Test Runner and Main Entry Points

(defun run-all-tests ()
  "Run all CD library tests"
  (format t "~&~%=== Running Complete CD Library Test Suite ===~%")
  (ensure-test-directory)
  (let ((results (run! 'cd-test-suite)))
    (format t "~&~%=== Test Suite Complete ===~%")
    results))

(defun run-basic-tests ()
  "Run basic functionality tests"
  (format t "~&~%=== Running Basic Tests ===~%")
  (ensure-test-directory)
  (run! 'basic-tests))

(defun run-context-tests ()
  "Run context management tests"
  (format t "~&~%=== Running Context Tests ===~%")
  (run! 'context-tests))

(defun run-canvas-tests ()
  "Run canvas management tests"
  (format t "~&~%=== Running Canvas Tests ===~%")
  (run! 'canvas-tests))

(defun run-drawing-tests ()
  "Run drawing primitive tests"
  (format t "~&~%=== Running Drawing Tests ===~%")
  (ensure-test-directory)
  (run! 'drawing-tests))

(defun run-text-tests ()
  "Run text rendering tests"
  (format t "~&~%=== Running Text Tests ===~%")
  (ensure-test-directory)
  (run! 'text-tests))

(defun run-font-tests ()
  "Run font management tests"
  (format t "~&~%=== Running Font Tests ===~%")
  (run! 'font-tests))

(defun run-image-tests ()
  "Run image handling tests"
  (format t "~&~%=== Running Image Tests ===~%")
  (run! 'image-tests))

(defun run-server-image-tests ()
  "Run server image tests"
  (format t "~&~%=== Running Server Image Tests ===~%")
  (run! 'server-image-tests))

(defun run-color-tests ()
  "Run color management tests"
  (format t "~&~%=== Running Color Tests ===~%")
  (run! 'color-tests))

(defun run-pattern-tests ()
  "Run pattern and stipple tests"
  (format t "~&~%=== Running Pattern Tests ===~%")
  (ensure-test-directory)
  (run! 'pattern-tests))

(defun run-transformation-tests ()
  "Run coordinate transformation tests"
  (format t "~&~%=== Running Transformation Tests ===~%")
  (run! 'transformation-tests))

(defun run-clipping-tests ()
  "Run clipping tests"
  (format t "~&~%=== Running Clipping Tests ===~%")
  (ensure-test-directory)
  (run! 'clipping-tests))

(defun run-vector-tests ()
  "Run vector text tests"
  (format t "~&~%=== Running Vector Tests ===~%")
  (ensure-test-directory)
  (run! 'vector-tests))

(defun run-world-coordinate-tests ()
  "Run world coordinate tests"
  (format t "~&~%=== Running World Coordinate Tests ===~%")
  (ensure-test-directory)
  (run! 'world-coordinate-tests))

(defun run-world-complete-tests ()
  "Run complete world coordinate system tests"
  (format t "~&~%=== Running World Complete Tests ===~%")
  (ensure-test-directory)
  (run! 'world-complete-tests))

(defun run-error-tests ()
  "Run error handling tests"
  (format t "~&~%=== Running Error Handling Tests ===~%")
  (run! 'error-handling-tests))

(defun run-backend-tests ()
  "Run backend-specific tests"
  (format t "~&~%=== Running Backend Tests ===~%")
  (ensure-test-directory)
  (run! 'backend-tests))

(defun run-integration-tests ()
  "Run integration tests"
  (format t "~&~%=== Running Integration Tests ===~%")
  (ensure-test-directory)
  (run! 'integration-tests))

(defun run-performance-tests ()
  "Run performance tests"
  (format t "~&~%=== Running Performance Tests ===~%")
  (run! 'performance-tests))

(defun run-transform-tests ()
  "Run transformation matrix tests"
  (format t "~&~%=== Running Transform Tests ===~%")
  (run! 'transformation-tests)) ; Uses existing suite

(defun run-advanced-drawing-tests ()
  "Run advanced drawing tests"
  (format t "~&~%=== Running Advanced Drawing Tests ===~%")
  (run! 'advanced-drawing-tests))

(defun run-advanced-text-tests ()
  "Run advanced text tests"
  (format t "~&~%=== Running Advanced Text Tests ===~%")
  (run! 'advanced-text-tests))

(defun run-advanced-image-tests ()
  "Run advanced image tests"
  (format t "~&~%=== Running Advanced Image Tests ===~%")
  (run! 'advanced-image-tests))

(defun run-animation-tests ()
  "Run animation tests"
  (format t "~&~%=== Running Animation Tests ===~%")
  (run! 'animation-tests))

(defun run-backend-extension-tests ()
  "Run backend extension tests"
  (format t "~&~%=== Running Backend Extension Tests ===~%")
  (run! 'backend-extension-tests))

(defun run-quick-tests ()
  "Run a subset of tests for quick validation"
  (format t "~&~%=== Running Quick Test Suite ===~%")
  (ensure-test-directory)
  (run! 'basic-tests)
  (run! 'canvas-tests)
  (run! 'drawing-tests)
  (run! 'color-tests))

(defun run-comprehensive-tests ()
  "Run comprehensive test suite excluding performance tests"
  (format t "~&~%=== Running Comprehensive Test Suite ===~%")
  (ensure-test-directory)
  (run! 'basic-tests)
  (run! 'context-tests)
  (run! 'canvas-tests)
  (run! 'drawing-tests)
  (run! 'text-tests)
  (run! 'font-tests)
  (run! 'image-tests)
  (run! 'server-image-tests)
  (run! 'color-tests)
  (run! 'pattern-tests)
  (run! 'transformation-tests)
  (run! 'clipping-tests)
  (run! 'vector-tests)
  (run! 'world-coordinate-tests)
  (run! 'world-complete-tests)
  (run! 'error-handling-tests)
  (run! 'backend-tests)
  (run! 'integration-tests)
  (run! 'advanced-drawing-tests)
  (run! 'advanced-text-tests)
  (run! 'advanced-image-tests)
  (run! 'animation-tests)
  (run! 'backend-extension-tests))

(defun test-summary ()
  "Print a summary of available test functions"
  (format t "~&~%=== CD Library Test Suite ===~%")
  (format t "~&Available test functions:~%")
  (format t "~&  (run-all-tests)           - Run complete test suite~%")
  (format t "~&  (run-quick-tests)         - Run quick validation tests~%")
  (format t "~&  (run-comprehensive-tests) - Run all tests except performance~%")
  (format t "~&~%Individual test suites:~%")
  (format t "~&  (run-basic-tests)         - Basic functionality~%")
  (format t "~&  (run-context-tests)       - Context management~%")
  (format t "~&  (run-canvas-tests)        - Canvas operations~%")
  (format t "~&  (run-drawing-tests)       - Drawing primitives~%")
  (format t "~&  (run-text-tests)          - Text rendering~%")
  (format t "~&  (run-font-tests)          - Font management~%")
  (format t "~&  (run-image-tests)         - Image operations~%")
  (format t "~&  (run-server-image-tests)  - Server image management~%")
  (format t "~&  (run-color-tests)         - Color operations~%")
  (format t "~&  (run-pattern-tests)       - Patterns and stipples~%")
  (format t "~&  (run-transformation-tests) - Coordinate transformations~%")
  (format t "~&  (run-clipping-tests)      - Clipping operations~%")
  (format t "~&  (run-vector-tests)        - Vector text~%")
  (format t "~&  (run-world-coordinate-tests) - World coordinates~%")
  (format t "~&  (run-world-complete-tests) - Complete world coordinate API~%")
  (format t "~&  (run-error-tests)         - Error handling~%")
  (format t "~&  (run-backend-tests)       - Backend-specific functionality~%")
  (format t "~&  (run-integration-tests)   - Cross-component integration~%")
  (format t "~&  (run-performance-tests)   - Performance benchmarks~%")
  (format t "~&~%Advanced feature test suites:~%")
  (format t "~&  (run-transform-tests)     - Transformation matrices~%")
  (format t "~&  (run-advanced-drawing-tests) - Paths, splines, gradients~%")
  (format t "~&  (run-advanced-text-tests) - Multi-line text, effects, typography~%")
  (format t "~&  (run-advanced-image-tests) - Image filtering, compositing, processing~%")
  (format t "~&  (run-animation-tests)     - Animation and particle systems~%")
  (format t "~&  (run-backend-extension-tests) - PostScript, PDF, printer support~%")
  (format t "~&~%Test output files are written to: ~A~%" *test-output-dir*)
  (format t "~&~%To run tests: (in-package :cd-tests) then call test functions~%"))

;; Print summary when file is loaded
(eval-when (:load-toplevel :execute)
  (test-summary))
# Enhanced Error Handling for CD Library Bindings

This document describes the enhanced error handling system for the Common Lisp CD (Canvas Draw) library bindings.

## Overview

The enhanced error handling system provides:

1. **Comprehensive condition hierarchy** for different types of errors
2. **Parameter validation** with detailed error messages
3. **Return value checking** with context-aware error reporting
4. **Backend capability checking** to prevent unsupported operations
5. **Error recovery and restarts** for graceful error handling
6. **Debug support** with operation tracing and verbose error reporting

## Quick Start

```lisp
;; Enable enhanced error handling
(setf *cd-validation-enabled* t
      *cd-debug-mode* t
      *enable-auto-recovery* t)

;; Use enhanced canvas creation with automatic recovery
(with-enhanced-canvas (canvas (context-svg) "output.svg")
  (line canvas 0 0 100 100)
  (box canvas 20 80 20 80))
```

## Error Condition Hierarchy

### Base Conditions

- `cd-error` - Base condition for all CD library errors
- `cd-resource-error` - Resource allocation/management errors
- `cd-parameter-error` - Invalid parameter values
- `cd-backend-error` - Backend-specific functionality errors
- `cd-drawing-error` - Errors during drawing operations
- `cd-file-error` - File-related errors
- `cd-memory-error` - Memory allocation errors

### Specific Conditions

- `cd-canvas-error` - Canvas-related errors
- `cd-context-error` - Context-related errors
- `initialization-error` - Canvas creation failures (enhanced)
- `activation-error` - Canvas activation failures (enhanced)

## Parameter Validation

### Enabling/Disabling Validation

```lisp
;; Enable validation (default)
(setf *cd-validation-enabled* t)

;; Disable for performance-critical sections
(setf *cd-validation-enabled* nil)

;; Temporary disable
(let ((*cd-validation-enabled* nil))
  ;; Fast operations without validation
  )
```

### Validation Functions

```lisp
;; Validate specific parameter types
(validate-canvas canvas 'my-operation)
(validate-coordinate x 'x-coordinate 'my-operation :min 0 :max 1000)
(validate-color color 'my-operation)
(validate-dimensions width height 'my-operation)
```

### Custom Validation

```lisp
(with-validation 'my-operation
  (validate-canvas canvas 'my-operation)
  (validate-coordinate x 'x 'my-operation :min 0)
  ;; ... rest of operation
  )
```

## Enhanced Canvas Operations

### Safe Canvas Creation

```lisp
;; Basic enhanced creation
(defparameter *canvas* (create-canvas (context-svg) "output.svg"))

;; With comprehensive error checking
(defparameter *canvas* (enhanced-create-canvas (context-svg) "output.svg"))
```

### Safe Drawing Operations

```lisp
;; Use safe wrappers for drawing
(safe-line canvas 0 0 100 100)
(safe-put-image-rgba canvas width height r g b a x y w h xmin xmax ymin ymax)

;; Or use generic safe operation wrapper
(safe-canvas-operation 'line canvas 0 0 100 100)
```

## Error Recovery and Restarts

### Canvas Creation with Restarts

```lisp
(with-canvas-restarts (canvas (context-pdf) "report.pdf")
  (generate-report canvas))
;; Available restarts:
;;   - retry-canvas-creation
;;   - use-debug-canvas
;;   - use-image-canvas
;;   - skip-canvas-operation
```

### Drawing Operations with Restarts

```lisp
(with-drawing-restarts draw-complex-shape
  (complex-shape-drawing-code))
;; Available restarts:
;;   - skip-drawing-operation
;;   - retry-drawing-operation
;;   - use-fallback-method
```

### File Operations with Restarts

```lisp
(with-file-restarts ("output.svg")
  (with-enhanced-canvas (canvas (context-svg) "output.svg")
    (draw-something canvas)))
;; Available restarts:
;;   - retry-with-new-path
;;   - use-temporary-file
;;   - skip-file-operation
```

### Automatic Recovery

```lisp
;; Enable automatic error recovery
(setf *enable-auto-recovery* t
      *default-fallback-context* (context-debug))

;; Operations will automatically try fallback options
(with-automatic-recovery
  (with-enhanced-canvas (canvas (context-cairo-pdf) "report.pdf")
    (complex-drawing-operations canvas)))
```

## Backend Capability Checking

```lisp
;; Check if backend supports specific capability
(handler-case
    (check-canvas-capability canvas :imagergba 'put-image-rgba)
  (cd-backend-error (e)
    (format t "Backend ~A doesn't support RGBA images~%" 
            (error-backend e))))

;; Safe functions automatically check capabilities
(safe-put-image-rgba canvas ...)  ; Checks :imagergba capability
```

## Debug Mode and Tracing

### Enable Debug Mode

```lisp
(setf *cd-debug-mode* t)
;; Now all operations will be traced and errors will be verbose
```

### Operation Context

```lisp
(with-cd-context "complex drawing operation"
  ;; All errors in this context will include the description
  (multiple-drawing-operations))
```

### Error Context Example

```lisp
(with-cd-error-checking ('draw-logo :canvas canvas)
  (draw-logo-components canvas))
```

## Batch Processing with Error Handling

```lisp
(defun process-multiple-files (file-specs)
  (let ((successful 0) (failed 0))
    (dolist (spec file-specs)
      (handler-case
          (with-enhanced-canvas (canvas (spec-context spec) (spec-filename spec))
            (process-file canvas spec)
            (incf successful))
        (cd-error (e)
          (format t "Failed to process ~A: ~A~%" (spec-filename spec) e)
          (incf failed))))
    (values successful failed)))
```

## Best Practices

### 1. Always Use Enhanced Functions

```lisp
;; Good
(create-canvas (context-svg) "output.svg")  ; Uses enhanced-create-canvas
(activate canvas)                          ; Uses enhanced-activate

;; Avoid direct CFFI calls
(cd-cffi::%cd-create-canvas ...)           ; No error checking
```

### 2. Handle Expected Errors

```lisp
(handler-case
    (with-enhanced-canvas (canvas (context-pdf) filename)
      (generate-report canvas))
  (cd-file-error (e)
    ;; Handle file-related errors specifically
    (log-error "File error: ~A" e)
    (generate-fallback-report))
  (cd-backend-error (e)
    ;; Handle backend errors
    (try-alternative-backend e)))
```

### 3. Use Restarts for Interactive Applications

```lisp
(defun interactive-draw ()
  (restart-case
      (with-enhanced-canvas (canvas (get-user-context) (get-user-filename))
        (interactive-drawing-loop canvas))
    (retry-with-new-settings ()
      :report "Retry with different canvas settings"
      (interactive-draw))
    (use-safe-defaults ()
      :report "Use safe default settings"
      (with-enhanced-canvas (canvas (context-debug) nil)
        (interactive-drawing-loop canvas)))))
```

### 4. Disable Validation in Performance-Critical Code

```lisp
(defun high-performance-drawing (canvas points)
  (let ((*cd-validation-enabled* nil))  ; Disable for speed
    (loop for point in points do
      (pixel canvas (point-x point) (point-y point) (point-color point)))))
```

### 5. Set Up Global Error Handling for Applications

```lisp
;; At application startup
(setup-application-error-handling)

;; Your application code here
;; All CD errors will be handled consistently

;; At application shutdown
(teardown-application-error-handling)
```

## Configuration Variables

- `*cd-validation-enabled*` - Enable/disable parameter validation
- `*cd-debug-mode*` - Enable verbose error reporting and tracing
- `*enable-auto-recovery*` - Enable automatic error recovery
- `*default-fallback-context*` - Default context for recovery attempts

## Migration from Basic Error Handling

### Before (Basic)

```lisp
(let ((canvas (create-canvas context spec)))
  (when (null canvas)
    (error "Canvas creation failed"))
  (unwind-protect
       (progn
         (when (eq :error (activate canvas))
           (error "Activation failed"))
         (drawing-operations canvas))
    (kill canvas)))
```

### After (Enhanced)

```lisp
(with-enhanced-canvas (canvas context spec)
  (drawing-operations canvas))
;; Automatic validation, error checking, cleanup, and recovery!
```

## Performance Considerations

- Parameter validation adds overhead - disable in tight loops
- Debug mode adds tracing overhead - disable in production
- Error recovery attempts multiple backends - may be slower
- Enhanced error messages require more memory and computation

## Example Applications

See `examples/enhanced-error-handling.lisp` for complete examples demonstrating:

- Parameter validation errors
- Backend capability checking
- Automatic error recovery
- File operation restarts
- Interactive error handling
- Batch processing with error handling
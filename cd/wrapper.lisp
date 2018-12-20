(in-package #:cd)

;;; nothing to see here, move along.

(defmacro defwrapper ((name lisp-name prefix cffi-type type) &body body)
  "Understands the following substitutions:

^FUNCTION-NAME
^CFFI-FUNCTION_NAME
^TYPE
^CFFI-TYPE"
  (let ((cffi-function-name (intern (format nil "%~:@(~A-~A~)" prefix name) "CD-CFFI"))
	(function-name (intern (string-upcase lisp-name)
			       (string-upcase (if (string= "cdf" prefix) "cd" prefix)))))
    `(progn
       ,@ (subst cffi-type '^cffi-type
		 (subst function-name '^function-name
			(subst type '^type 
			       (subst cffi-function-name '^cffi-function-name body)))))))

(defmacro defwrappers ((name lisp-name &optional (prefix "cdf")) &body body)
  (let ((cffi-type (if (string= prefix "cd") :int :double))
	(type (if (string= prefix "cd") 'integer 'double-float)))
    `(progn
       (defwrapper (,name ,lisp-name ,prefix ,cffi-type ',type)
	 ,@body)
       (defwrapper (,name ,lisp-name "wd" :double 'double-float)
	 ,@body)
       (export (intern (string-upcase ,lisp-name) (find-package "CD")))
       (export (intern (String-upcase ,lisp-name) (find-package "WD")) (find-package "WD")))))

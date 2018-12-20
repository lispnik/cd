(in-package #:cd)

(defmacro define-cd-setf-expander (name func value-count coerce-type docstring)
  `(define-setf-expander ,name (canvas &environment env)
     ,docstring
     (multiple-value-bind
           (temp-vars temp-forms store-vars setter-form getter-form)
         (get-setf-expansion canvas env)
       (declare (ignore store-vars setter-form))
       (let* ((new-value-vars (loop repeat ,value-count collect (gensym)))
              (new-value-vars-double (mapcar (lambda (x)
                                               `(coerce ,x ,',coerce-type))
                                             new-value-vars)))
         (values `(,@temp-vars)
                 `(,@temp-forms)
                 `,new-value-vars
                 `(progn
                    (,',func
                     ,getter-form
                     ,@new-value-vars-double)
                    (values ,@new-value-vars))
                 `(,',name canvas))))))

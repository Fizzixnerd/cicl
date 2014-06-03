(in-package :cicl-sys)

(defun pair-sequences (s1 s2)
    (iter
      (for e1 in s1)
      (for e2 in s2)
      (appending (list e1 e2))))

(defmacro define-make-function (type-name &rest arguments)
  (let ((keyword-args (mapcar #'(lambda (arg) (make-keyword (string-upcase arg)))
			      arguments))
	(function-name (intern (concatenate 'string "MAKE-" (symbol-name type-name)))))
  `(defun ,function-name (,@arguments)
     (make-instance ',type-name
		    ,@(pair-sequences keyword-args arguments)))))

(defmacro define-make-functions (&rest define-make-function-args)
  `(progn
     ,@(mapcar #'(lambda (dmfa) `(define-make-function ,@dmfa)) define-make-function-args)))

(define-make-functions 
  (c-type base-name qualifiers pointer-level)
  (c-scope statements)
  (c-expression-statement expression)
  (c-subvariable-description name ctype)
  (c-variable-decl name ctype value)
  (c-variable-decl-list decls)
  (c-function-decl name ctype arg-list)
  (c-function-defn name ctype arg-list body)
  (c-enum-decl     name ctype enum-list)
  (c-struct-decl   name ctype member-list)
  (c-typedef-decl  name ctype)
  (c-if-statement test-expression then-body else-body)
  (c-return-statement expression)
  (c-empty-expression)
;; TODO
;;  (c-switch-statement test-expression
;;  (c-do-loop          test-expression
  (c-while-loop   test-expression body)
  (c-for-loop     test-expression body initializer incrementer)

  (c-literal value)
  (c-identifier value)
  (c-function-call function-name subexpressions)
  (c-array-reference subexpressions)
  (c-operator-expression op subexpressions)

  (c-program statements)

  (cpp-include filename)
  
  (c-int value)
  (c-long value)
  (c-llong value)

  (c-float value)
  (c-double value)
  (c-quad value)
  
  (c-char value)
  
  (c-string value)
  
  (c-struct-literal subexpressions)
  (c-array-literal  subexpressions))


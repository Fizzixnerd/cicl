(in-package :cicl-sys)

(let ((expr (make-instance 'c-function-call
			   :function-name "fopen"
			   :subexpressions (list
					    (make-instance 'c-string
							   :value "/usr/bin/emacs")
					    (make-instance 'c-string
							   :value "r")))))
  (format nil "~A" expr))

(let ((expr (make-instance 'c-array-reference
			   :subexpressions (list
					    (make-instance 'c-identifier
							   :value "arr")
					    (make-instance 'c-long
							   :value 256)))))
  (format nil "~A" expr))

(let ((expr (make-instance 'c-operator-expression
			   :op tern
			   :subexpressions (list
					    (make-instance 'c-identifier
							   :value "booler")
					    (make-instance 'c-identifier
							   :value "NULL")
					    (make-instance 'c-string
							   :value "Well alrighty then.")))))
  (format t "~A" expr))

(let ((stmt (make-instance 'c-expression-statement
			   :expression (make-instance 'c-operator-expression
						      :op tern
						      :subexpressions (list
								       (make-instance 'c-identifier
										      :value "booler")
								       (make-instance 'c-identifier
										      :value "NULL")
								       (make-instance 'c-string
										      :value "Well alrighty then."))))))
  (format t "~A" stmt))

(let ((stmt (make-instance 'c-variable-decl
			   :name "derp"
			   :ctype "herp"
			   :value "lerp")))
  (format t "~A" stmt))

(let ((stmt (make-instance 'c-function-decl
			   :name (make-instance 'c-identifier
						:value "fopen")
			   :ctype (make-instance 'c-type
						 :base-name "FILE"
						 :qualifiers nil
						 :pointer-level 1)
			   :arg-list (list
				      (make-instance 'c-subvariable-description
						     :name (make-instance 'c-identifier
									  :value "path")
						     :ctype (make-instance 'c-type
									   :base-name "char"
									   :qualifiers (list "const")
									   :pointer-level 1))
				      (make-instance 'c-subvariable-description
						     :name (make-instance 'c-identifier
									  :value "mode")
						     :ctype (make-instance 'c-type
									   :base-name "char"
									   :qualifiers (list "const")
									   :pointer-level 1))))))
  (format t "~A" stmt))

(let* ((cscp (make-instance 'c-scope
			    :statements (list
					 (make-instance 'c-variable-decl
							:name "j"
							:ctype (make-instance 'c-type
									      :base-name "int"
									      :qualifiers nil
									      :pointer-level 0)
							:value (make-instance 'c-identifier
									      :value "i")))))
       (cfrlp (make-instance 'c-for-loop
			     :body cscp
			     :initializer
			     (make-instance 'c-expression-statement
					    :expression
					    (make-instance 'c-operator-expression
							   :op assign
							   :subexpressions
							   (list
							    (make-instance 'c-identifier
									   :value "i")
							    (make-instance 'c-int
									   :value 0))))
			     :test-expression
			     (make-instance 'c-expression-statement
					    :expression
					    (make-instance 'c-operator-expression
							   :op lt
							   :subexpressions
							   (list
							    (make-instance 'c-identifier
									   :value "i")
							    (make-instance 'c-int
									   :value 10))))
			     :incrementer
			     (make-instance 'c-operator-expression
					    :op pre1+
					    :subexpressions
					    (list
					     (make-instance 'c-identifier
							    :value "i"))))))
  (format t "~A" cfrlp))


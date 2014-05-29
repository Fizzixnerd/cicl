(in-package :cicl)

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

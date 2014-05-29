(in-package :cicl)

(defclass c-literal (literal) ())
(defclass c-identifier (identifier) ())
(defclass c-function-call (function-call) ())
(defclass c-array-reference (array-reference) ())
(defclass c-aggregate-expression (aggregate-expression) ())
(defclass c-operator-expression (operator-expression) ())

(defclass c-int (c-literal)
  ((value
    :type 'integer)))
(defclass c-long (c-int) ())
(defclass c-llong (c-long) ())
(defclass c-char (c-literal)
  ((value
    :type 'character)))
(defclass c-string (c-literal)
  ((value
    :type 'string)))

(defclass c-struct-literal (c-aggregate-expression) ())
(defclass c-array-literal (c-aggregate-expression) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((cl c-long) stream)
  (format stream "~AL" (value cl)))

(defmethod print-object ((cll c-llong) stream)
  (format stream "~ALL" (value cll)))

(defmethod print-object ((cc c-char) stream)
  (format stream "'~C'" (value cc)))

(defmethod print-object ((cs c-string) stream)
  (format stream "\"~A\"" (value cs)))

(defmethod print-object ((cae c-aggregate-expression) stream)
  (format stream "{~{~A~^, ~}}" (subexpressions cae)))

(defmethod print-object ((cfc c-function-call) stream)
  (format stream "~A(~{~A~^, ~})" (function-name cfc) (subexpressions cfc)))

(defmethod print-object ((ca-ref c-array-reference) stream)
  (with-accessors ((subexpressions subexpressions)) ca-ref
    (assert (length= 2 subexpressions))
    (let* ((array-expr (first subexpressions))
	   (index-expr (second subexpressions))
	   (format-string (if (type= (type-of array-expr) 'c-identifier)
			      "~A[~A]"
			      "(~A)[~A]")))
      (format stream format-string array-expr index-expr))))

(defmethod print-object ((coe c-operator-expression) stream)
  (format stream "~A" (get-representation (op coe) (subexpressions coe))))

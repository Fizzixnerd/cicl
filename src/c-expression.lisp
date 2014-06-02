(in-package :cicl-sys)

(defclass c-literal (literal) ())
(defclass c-identifier (identifier) ())
(defclass c-function-call (function-call) ())
(defclass c-array-reference (array-reference) ())
(defclass c-aggregate-expression (aggregate-expression) ())
(defclass c-operator-expression (operator-expression) ())
(defclass c-empty-expression (expression) ())

(defclass c-int (c-literal)
  ((value
    :type 'integer)))
(defclass c-long (c-int) ())
(defclass c-llong (c-long) ())

(defclass c-float (c-literal)
  ((value
    :type float)))
(defclass c-double (c-float) ())
(defclass c-quad (c-double) ())

(defclass c-char (c-literal)
  ((value
    :type 'character)))

(defclass c-string (c-literal)
  ((value
    :type 'string)))

(defclass c-struct-literal (c-aggregate-expression) ())
(defclass c-array-literal (c-aggregate-expression) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-print-literal (type-name format-string)
  `(defmethod print-object ((v ,type-name) stream)
     (format stream ,format-string (value v))))

(define-print-literal c-int "~A")
(define-print-literal c-long "~AL")
(define-print-literal c-llong "~ALL")
(define-print-literal c-float "~Af")
(define-print-literal c-double "~A")
(define-print-literal c-quad "~A")
(define-print-literal c-char "'~C'")
(define-print-literal c-string "\"~A\"")
(define-print-literal c-literal "~A") ; Just in case!

(define-print-literal c-identifier "~A")

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

(defmethod print-object ((cee c-empty-expression) stream)
  (format stream ""))

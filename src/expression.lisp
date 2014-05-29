(in-package :cicl)

(defclass expression () ())

(defclass atomic-expression (expression)
  ((value
    :accessor value
    :initarg :value
    :initform (error "Must provide a value for the literal."))))

(defclass literal (atomic-expression) ())
(defclass identifier (atomic-expression) ())

(defclass compound-expression (expression)
  ((subexpressions
    :type 'list
    :reader subexpressions
    :initarg :subexpressions
    :initform (error "Must provide a subexpressions list."))))

(defclass function-call (compound-expression)
  ((function-name
    :type 'string
    :reader function-name
    :initarg :function-name
    :initform (error "Must provide a function name."))))
(defclass array-reference (compound-expression) ())
(defclass aggregate-expression (compound-expression) ())
(defclass operator-expression (compound-expression)
  ((op
    :type 'operator
    :reader op
    :initarg :op
    :initform (error "Must provide an operator"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((ae atomic-expression) stream)
  (format stream "~A" (value ae)))

;; (defmethod representation ((expression literal))
;;   (let ((repr (mkrepr)))
;;     (with-accessors ((format-str format-str) (format-args format-args)) repr
;;       (setf format-str "~A" expression)
;;     repr))

;; (defmethod representation ((expression compound-expression))
;;   (let ((args (mapcar #'representation (subexps expression)))
;; 	(repr (mkrepr)))
;;     (get-representation (op expression) args)))
      


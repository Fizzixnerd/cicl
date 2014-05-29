(in-package :cicl)

(define-operator post+1 2 "post" "left" 1)
(define-operator post-1 2 "post" "left" 1)
(define-operator call 2 "pre" "left" 2)
(define-operator arref 2 "post" "left" 2)
(define-operator dot 2 "in" "left" 2)
(define-operator point 2 "in" "left" 2)

(define-operator pre+1 3 "pre" "right" 1)
(define-operator pre-1 3 "pre" "right" 1)
(define-operator uplus 3 "pre" "right" 1)
(define-operator uminus 3 "pre" "right" 1)
(define-operator lnot 3 "pre" "right" 1)
(define-operator bnot 3 "pre" "right" 1)
(define-operator cast 3 "pre" "right" 2)
(define-operator deref 3 "pre" "right" 1)
(define-operator addr 3 "pre" "right" 1)
(define-operator sizeof 3 "pre" "right" 1)

(define-operator mul 5 "in" "left" 2)
(define-operator div 5 "in" "left" 2)
(define-operator remain 5 "in" "left" 2)

(define-operator bplus 6 "in" "left" 2)
(define-operator bminus 6 "in" "left" 2)

(define-operator lash 7 "in" "left" 2)
(define-operator rash 7 "in" "left" 2)

(define-operator lt 8 "in" "left" 2)
(define-operator lte 8 "in" "left" 2)
(define-operator gt 8 "in" "left" 2)
(define-operator gte 8 "in" "left" 2)

(define-operator leql 9 "in" "left" 2)
(define-operator lneq 9 "in" "left" 2)

(define-operator band 10 "in" "left" 2)

(define-operator bxor 11 "in" "left" 2)

(define-operator bor 12 "in" "left" 2)

(define-operator land 13 "in" "left" 2)

(define-operator lor 14 "in" "left" 2)

(define-operator tern 15 "in" "right" 3)
(define-operator assign 15 "in" "right" 2)
(define-operator plusr 15 "in" "right" 2)
(define-operator minusr 15 "in" "right" 2)
(define-operator mulr 15 "in" "right" 2)
(define-operator divr 15 "in" "right" 2)
(define-operator remainr 15 "in" "right" 2)
(define-operator lashr 15 "in" "right" 2)
(define-operator rashr 15 "in" "right" 2)
(define-operator bandr 15 "in" "right" 2)
(define-operator bnotr 15 "in" "right" 2)
(define-operator borr 15 "in" "right" 2)

(define-operator comma 17 "in" "left" 2)

(defmethod names ((operator assign) args) (list "="))
(defmethod names ((operator tern) args) (list "?" ":"))
(defmethod names ((operator call) args)
  (let ((name-open-paren (concatenate 'string (car args) "(")))
    (values (list name-open-paren ")")
	    (cdr args))))
(defmethod names


(defmethod refine-representation ((operator unary) arguments (repr representation))
  (with-accessors ((format-str format-str)) repr
    (setf format-str "~A~A"))
  (call-if-next-method))

(defmethod refine-representation ((operator binary) arguments (repr representation))
  (with-accessors ((format-str format-str)) repr
    (setf format-str "~A~A~A"))
  (call-if-next-method))

(defmethod refine-representation ((operator ternary) arguments (repr representation))
  (with-accessors ((format-str format-str)) repr
    (setf format-str "~A~A~A~A~A"))
  (call-if-next-method))

(defmethod refine-representation ((operator infix) arguments (repr representation))
  (with-accessors ((format-args format-args)) repr
    (multiple-value-bind (names args) (names operator arguments)
      (setf format-args (iter (for arg in args)
			      (for name in names)
			      (appending (list arg name))))))
  (call-if-next-method))

(defmethod refine-representation ((operator prefix) arguments (repr representation))
  (with-accessors ((format-args format-args)) repr
    (multiple-value-bind (names args) (names operator arguments)
      (setf format-args (append (list (car names))
				args
				(list (cadr names))))))
  (call-if-next-method))

(defmethod refine-representation ((operator postfix) arguments (repr representation))
  (with-accessors ((format-args format-args)) repr
    (setf format-args (append arguments (names operator arguments))))
  (call-if-next-method))


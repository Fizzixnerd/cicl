(in-package :cicl)

(define-operator post+1 2 "post" "left" 1)
(define-operator post-1 2 "post" "left" 1)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-names post+1 "++")
(define-simple-names post-1 "--")
(define-simple-names dot ".")
(define-simple-names point "->")

(define-simple-names pre+1 "++")
(define-simple-names pre-1 "--")
(define-simple-names uplus "+")
(define-simple-names uminus "-")
(define-simple-names lnot "!")
(define-simple-names bnot "~")
(defmethod names ((operator cast) args) (error "implement this."))
(define-simple-names deref "*")
(define-simple-names addr "&")
(defmethod names ((operator sizeof) args) (error "implement this."))

(define-simple-names mul " * ")
(define-simple-names div " / ")
(define-simple-names remain " % ")

(define-simple-names bplus " + ")
(define-simple-names bminus " - ")

(define-simple-names lash " << ")
(define-simple-names rash " >> ")

(define-simple-names lt " < ")
(define-simple-names lte " <= ")
(define-simple-names gt " > ")
(define-simple-names gte " >= ")

(define-simple-names leql " == ")
(define-simple-names lneq " != ")

(define-simple-names band " & ")

(define-simple-names bxor " ^ ")

(define-simple-names bor " | ")

(define-simple-names land " && ")

(define-simple-names lor " || ")

(defmethod names ((operator tern) args)
  (values (list " ? " " : ")
	  args))
(define-simple-names assign " = ")
(define-simple-names plusr " += ")
(define-simple-names minusr " -= ")
(define-simple-names mulr " *= ")
(define-simple-names divr " /= ")
(define-simple-names remainr " %= ")
(define-simple-names lashr " <<= ")
(define-simple-names rashr " >>= ")
(define-simple-names bandr " &= ")
(define-simple-names bnotr " ~= ")
(define-simple-names borr " |= ")

(define-simple-names comma ", ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unless-mkrepr (repr)
  (if (null repr)
      (mkrepr)
      repr))

(defmethod get-representation ((operator unary) arguments &optional (repr nil))
  (let ((rep (unless-mkrepr repr)))
    (with-accessors ((format-str format-str)) rep
      (setf format-str "~A~A"))
    (call-if-next-method operator arguments rep)
    rep))

(defmethod get-representation ((operator binary) arguments &optional (repr nil))
  (let ((rep (unless-mkrepr repr)))
    (with-accessors ((format-str format-str)) rep
      (setf format-str "~A~A~A"))
    (call-if-next-method operator arguments rep)
    rep))

(defmethod get-representation ((operator ternary) arguments &optional (repr nil))
  (let ((rep (unless-mkrepr repr)))
    (with-accessors ((format-str format-str)) rep
      (setf format-str "~A~A~A~A~A"))
    (call-if-next-method operator arguments rep)
    rep))

(defmethod get-representation ((operator infix) arguments &optional (repr nil))
  (let ((rep (unless-mkrepr repr)))
    (with-accessors ((format-args format-args)) rep
      (multiple-value-bind (names args) (names operator arguments)
	(setf format-args (append (iter (for arg in args)
					(for name in names)
					(appending (list arg name)))
				  (last args)))))
    (call-if-next-method operator arguments rep)
    rep))

(defmethod get-representation ((operator prefix) arguments &optional (repr nil))
  (let ((rep (unless-mkrepr repr)))
    (with-accessors ((format-args format-args)) rep
      (multiple-value-bind (names args) (names operator arguments)
	(setf format-args (append (list (car names))
				  args
				  (cdr names)))))
    (call-if-next-method operator arguments rep)
    rep))

(defmethod get-representation ((operator postfix) arguments &optional (repr nil))
  (let ((rep (unless-mkrepr repr)))
    (with-accessors ((format-args format-args)) rep
      (multiple-value-bind (names args) (names operator arguments)
	(setf format-args (append args names))))
    (call-if-next-method operator arguments rep)
    rep))

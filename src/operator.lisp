(in-package :cicl)

(defclass operator ()
   ((precedence
    :type 'fixnum
    :reader precedence
    :initform (error "Must provide a precedence.")
    :initarg :precedence)))

(defclass fixity ()
  ((fixity
   :type 'string
   :reader fixity
   :initform (error "Must provide a fixity."))))

(defclass associativity ()
  ((associativity
   :type 'string
   :reader associativity
   :initform (error "Must provide an associativity"))))

(defclass arity ()
  ((arity
   :type 'fixnum
   :reader arity
   :initform (error "Must provide an arity."))))

(defclass prefix (fixity)
  ((fixity
    :initform "pre")))

(defclass infix (fixity)
  ((fixity
    :initform "in")))

(defclass postfix (fixity)
  ((fixity
    :initform "post")))

(defclass right-associative (associativity)
  ((associativity
    :initform "right")))

(defclass left-associative (associativity)
  ((associativity
    :initform "right")))

(defclass unary (arity)
  ((arity
    :initform 1)))

(defclass binary (arity)
  ((arity
    :initform 2)))

(defclass ternary (arity)
  ((arity
    :initform 3)))

(defmacro define-operator (class-name precedence fixity associativity arity)
  (let ((fixity-class (cond ((string= fixity "pre") 'prefix)
			    ((string= fixity "in") 'infix)
			    ((string= fixity "post") 'postfix)
			    (t (error "Invalid fixity given: ~A." fixity))))
	(associativity-class (cond ((string= associativity "right") 'right-associative)
				   ((string= associativity "left") 'left-associative)
				   (t (error "Invalid associativity given: ~A." associativity))))
	(arity-class (cond ((= arity 1) 'unary)
			   ((= arity 2) 'binary)
			   ((= arity 3) 'ternary)
			   (t (error "Invalid arity given: ~A." arity)))))
    `(progn
       (defclass ,class-name (operator ,arity-class ,fixity-class ,associativity-class) ())
       (defvar ,class-name (make-instance ',class-name :precedence ,precedence)))))

(defgeneric operation-string (operator &rest arguments)
  (:documentation
   "Return the string representation of operator operating on arguments"))

(defgeneric refine-representation (operator arguments string)
  (:documentation
   "Refine the string representation of operator operating on
   arguments based on the type of operator."))

(defgeneric names (operator arguments)
  (:documentation
   "Return a list of the string names of `operator'."))

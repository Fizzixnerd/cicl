(in-package :cicl-sys)

;; struct and array literals are considered operators
(setf *binary-operators* (list '+ '- '* '/ '% '^ 'band 'bor '^ '<<
				 '>> 'and 'or '== '!= '= '+= '-= '*=
				 '/= '%= 'band= 'bor= '~= '<<= '>>=
				 'dot '-> 'cast 'strcat))
(setf *unary-operators* (list '+ '- '* '~ 'not 'sizeof 'addr
				'deref '++ '-- 'post1+ 'post1-))
(setf *ternary-operators* (list 'tern))
(setf *overloaded-operators* (intersection *binary-operators* *unary-operators*))
(setf *aggregate-operators* (list 'structl 'arrayl))
(setf *operators* (append *binary-operators* *unary-operators*
			    *aggregate-operators* *ternary-operators*))

(defun contains (lst ele)
  (some #'(lambda (lst-ele) (equal ele lst-ele))
	lst))

(defun symbol-downcase (sym)
  (cond
    ;; ORDER MATTERS, keywords are symbols too!
    ((keywordp sym) (make-keyword (string-downcase (symbol-name sym))))
    ((symbolp sym) (intern (string-downcase (symbol-name sym))))
    (t (error "~A is not a symbol." sym))))

(defun parse-c-expression (expression)
  (when (null expression) (return (make-c-empty-expression)))
  (if (listp expression)
      ;; Then this is a compound expression
      (progn
	(let ((operator-or-function (car expression)))
	  (if (contains *operators* operator-or-function)
	      (parse-c-operator-expression expression)
	      ;; else it's a function
	      (parse-c-function-call expression))))
      ;; else
      (parse-c-atomic-expression expression)))

(defun parse-c-function-call (expression)
  (let ((function-name (parse-c-identifier (car expression)))
	(args (mapcar #'parse-c-expression (cdr expression))))
    (make-c-function-call function-name args)))

(defun parse-c-operator-expression (expression)
  (let ((op (car expression)))
    (cond
      ((contains *overloaded-operators* op) (parse-c-overloaded-operator-expression expression))
      ((contains *binary-operators* op) (parse-c-binary-operator-expression expression))
      ((contains *unary-operators* op) (parse-c-unary-operator-expression expression))
      ((contains *ternary-operators* op) (parse-c-ternary-operator-expression expression))
      ((contains *aggregate-operators* op) (parse-c-aggregate-opereator-expression expression))
      (t (error "~A isn't an operator." op)))))

(defun parse-c-overloaded-operator-expression (expression)
  (assert (> (length expression) 1))
  (let ((op (car expression))
	(subexps (cdr expression)))
    (if (length= 1 subexps)
	(parse-c-unary-operator-expression expression)
	(parse-c-binary-operator-expression expression))))

(defun parse-c-atomic-expression (expression)
  (cond ((integerp expression) (make-c-llong expression))
	((stringp expression) (make-c-string expression))
	((characterp expression) (make-c-char expression))
	((floatp expression) (make-c-quad expression))
	((symbolp expression) (parse-c-identifier expression))
	(t (error "Don't know how to parse ~A as an atomic expression." (type-of expression)))))

(defun parse-c-identifier (expression)
  (check-type expression (or symbol string keyword))
  (make-c-identifier (unlispify-name expression)))

(defun unlispify-character (c)
  (cond
    ((char= c #\-) #\_)
    ((char= c #\?) #\P)
    ((char= c #\!) #\M)
    ((char= c #\%) #\_)
    (t c)))

(defun unlispify-name (symbol)
  (let ((unlispified-name
	 (map 'string #'unlispify-character (string-downcase (symbol-name symbol)))))
    (if (keywordp symbol)
	(make-keyword unlispified-name)
	(intern unlispified-name))))

;; (+ 1 2 3 4) -> (+ (+ 1 2) 3 4) -> (+ (+ (+ 1 2) 3) 4)
(defun nest-binary-expression (expression)
  (let* ((op (car expression))
	 (subexps (cdr expression))
	 (first-exp (car subexps))
	 (second-exp (cadr subexps))
	 (rest-subexps (cddr subexps)))
    (if rest-subexps
	(nest-binary-expression `(,op (,op ,first-exp ,second-exp) ,@rest-subexps))
	`(,op ,first-exp ,second-exp))))

(defun parse-c-unary-operator-expression (expression)
  (assert (length= 2 expression))
  (let ((op (car expression))
	(subexp (mapcar #'parse-c-expression (cdr expression))))
    (cond
      ((string= op '+) (make-c-operator-expression uplus subexp))
      ((string= op '-) (make-c-operator-expression uminus subexp))
      ((string= op '*) (make-c-operator-expression deref subexp))
      ((string= op 'deref) (make-c-operator-expression deref subexp))
      ((string= op 'not) (make-c-operator-expression lnot subexp))
      ((string= op 'sizeof) (make-c-operator-expression sizeof subexp))
      ((string= op 'addr) (make-c-operator-expression addr subexp))
      ((string= op '++) (make-c-operator-expression pre1+ subexp))
      ((string= op '--) (make-c-operator-expression pre1- subexp))
      ((string= op 'post1+) (make-c-operator-expression post1+ subexp))
      ((string= op 'post1-) (make-c-operator-expression post1- subexp))
      (t (error "~A isn't a unary operator" op)))))

(defun parse-c-binary-operator-expression (expression)
  (let ((nested-expression (nest-binary-expression expression)))
    (parse-nested-c-binary-operator-expression nested-expression)))

(defun parse-c-ternary-operator-expression (expression)
  (assert (length= 4 expression))
  (let ((op (car expression))
	(subexps (mapcar #'parse-c-expression (cdr expression))))
    (cond
      ((string= op 'tern) (make-c-operator-expression tern subexps))
      (t (error "~A isn't a ternary operator." op)))))

;; (+ (+ 1 (functo 2 3)) 4)
(defun parse-nested-c-binary-operator-expression (expression)
  (assert (length= 3 expression))
  (let* ((op (car expression))
	 (subexps (mapcar #'parse-c-expression (cdr expression))))
    (cond ((string= op '+) (make-c-operator-expression bplus subexps))
	  ((string= op '-) (make-c-operator-expression bminus subexps))
	  ((string= op '*) (make-c-operator-expression mul subexps))
	  ((string= op '/) (make-c-operator-expression div subexps))
	  ((string= op '%) (make-c-operator-expression remain subexps))
	  ((string= op 'band) (make-c-operator-expression band subexps))
	  ((string= op 'bor) (make-c-operator-expression bor subexps))
	  ((string= op '^) (make-c-operator-expression bxor subexps))
	  ((string= op '<<) (make-c-operator-expression lash subexps))
	  ((string= op '>>) (make-c-operator-expression rash subexps))
	  ((string= op 'and) (make-c-operator-expression land subexps))
	  ((string= op 'or) (make-c-operator-expression lor subexps))
	  ((string= op '==) (make-c-operator-expression leql subexps))
	  ((string= op '!=) (make-c-operator-expression lneq subexps))
	  ((string= op '=) (make-c-operator-expression assign subexps))
	  ((string= op '+=) (make-c-operator-expression plusr subexps))
	  ((string= op '-=) (make-c-operator-expression minusr subexps))
	  ((string= op '*=) (make-c-operator-expression mulr subexps))
	  ((string= op '/=) (make-c-operator-expression divr subexps))
	  ((string= op '%=) (make-c-operator-expression remainr subexps))
	  ((string= op 'band=) (make-c-operator-expression bandr subexps))
	  ((string= op 'bor=) (make-c-operator-expression borr subexps))
	  ((string= op '~=) (make-c-operator-expression bnotr subexps))
	  ((string= op '<<=) (make-c-operator-expression lashr subexps))
	  ((string= op '>>=) (make-c-operator-expression rashr subexps))
	  ((string= op 'dot) (make-c-operator-expression dot subexps))
	  ((string= op '->) (make-c-operator-expression point subexps))
	  
	  ((string= op 'cast) (let ((subexps (cons (parse-c-type (cadr expression)) (cdr subexps))))
	   (make-c-operator-expression cast subexps)))
	  
	  ((string= op 'strcat) (make-c-operator-expression cast subexps))
	  (t (error "~A isn't a binary operator." op)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-c-subvariable-description (description)
  (let ((variable-name (car description))
	(type-specifier (cdr description)))
    (make-c-subvariable-description (parse-c-atomic-expression variable-name)
				    (parse-c-type type-specifier))))

(defun %parse-c-type (type-specifier &optional qualifiers
					   type-name pointer-level)
  (let ((sym (car type-specifier)))
    (cond
      ((some #'(lambda (s) (string= sym s))
	     (list "const" "volatile" "long" "struct"))
       (appendf qualifiers (list sym)))
      ((every #'(lambda (c) (char= c #\*))
	      (symbol-name sym))
       (setf pointer-level (count #\* (symbol-name sym))))
      ((keywordp sym)
       (setf type-name sym))
      (t (error "malformed type-specifier ~A with sym ~A" type-specifier sym)))
    (if (> (length type-specifier) 1)
	(%parse-c-type (cdr type-specifier)
		       qualifiers type-name pointer-level)
	;; else
	(progn
	  (make-c-type type-name qualifiers pointer-level)))))

(defun parse-c-type (type-specifier)
  (let ((type-spec (mapcar #'unlispify-name type-specifier)))
    (%parse-c-type type-spec)))

(defun parse-c-statement (statement)
  (let ((ckeyword (car statement)))
    (cond
      ((string= ckeyword 'defun) (parse-c-function-defn statement))
      ((string= ckeyword 'defvar) (parse-c-variable-decl statement))
      ((string= ckeyword 'defstruct) (parse-c-struct-decl statement))
      ((string= ckeyword 'return) (parse-c-return-statement statement))
      ((string= ckeyword 'for) (parse-c-for-loop statement))
      ((string= ckeyword 'typedef) (parse-c-typedef-decl statement))
      (t (parse-c-expression-statement statement)))))

(defun parse-c-scope (scope)
  (let ((statements (mapcar #'parse-c-statement scope)))
    (make-c-scope statements)))

(defun parse-c-expression-statement (statement)
  (make-c-expression-statement (parse-c-expression statement)))

(defun parse-c-variable-decl (statement)
  (assert (string= 'defvar (car statement)))
  (let ((decl-lists (cdr statement)))
    (make-c-variable-decl-list (iter
				 (for decl-list in decl-lists)
				 (let* ((identifier (make-c-identifier (unlispify-name (car decl-list))))
					(type-specifier (cadr decl-list))
					(init-expression (caddr decl-list))
					(parsed-type-spec (parse-c-type type-specifier))
					(parsed-init-expr (when init-expression
							    (parse-c-expression init-expression))))
				   (collect (make-c-variable-decl identifier parsed-type-spec parsed-init-expr)))))))

(defun parse-c-function-defn (statement)
  (assert (string= 'defun (car statement)))
  (let ((function-name (cadr statement))
	(lambda-list (caddr statement))
	(function-return-type-specifier (cadddr statement))
	(function-body (cddddr statement)))
    (if (null function-body)
	(parse-c-function-decl statement)
	;; else
	(let ((fnc-name (parse-c-atomic-expression function-name))
	      (return-type (parse-c-type function-return-type-specifier))
	      (arg-list (mapcar #'parse-c-subvariable-description lambda-list))
	      (body (parse-c-scope function-body)))
	  (make-c-function-defn fnc-name return-type arg-list body)))))

;; FIXME
(defun parse-c-function-decl (statement)
  (assert (string= 'defun (car statement))))

(defun parse-c-return-statement (statement)
  (assert (string= 'return (car statement)))
  (if (cadr statement)
      (let ((return-value (parse-c-expression (cadr statement))))
	(make-c-return-statement return-value))
      ; else
      (make-c-return-statement nil)))

(defun parse-c-enum-decl (statement))

(defun parse-c-struct-decl (statement)
  (assert (string= 'defstruct (car statement)))
  (let ((type-name (parse-c-atomic-expression (cadr statement)))
	(member-list (mapcar #'parse-c-subvariable-description (cddr statement))))
    (make-c-struct-decl type-name nil member-list)))

(defun parse-c-typedef-decl (statement)
  (assert (string= 'typedef (car statement)))
  (let ((name (parse-c-atomic-expression (car (last statement))))
	(ctype (parse-c-type (cadr statement))))
    (make-c-typedef-decl name ctype)))

(defun parse-c-if-statement (statement))

(defun parse-c-switch-statement (statement))

(defun parse-c-do-loop (statement))

(defun parse-c-while-loop (statement))

(defun parse-c-for-loop (statement)
  (assert (string= 'for (car statement)))
  (let* ((control-list (cadr statement))
	 (body (mapcar #'parse-c-scope (list (cddr statement))))
	 (initializer (parse-c-expression (car control-list)))
	 (test-expression (parse-c-expression (cadr control-list)))
	 (incrementer (parse-c-expression (caddr control-list))))
    (make-c-for-loop test-expression (car body) initializer incrementer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-cpp-directive (directive))

(defun parse-cpp-include (directive))

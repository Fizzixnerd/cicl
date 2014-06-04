(in-package :cicl-sys)

;; struct and array literals are considered operators
(defvar *binary-operators* (list '+ '- '* '/ '% '^ 'band 'bor '^ '<<
				 '>> 'and 'or '== '!= '= '+= '-= '*=
				 '/= '%= 'band= 'bor= '~= '<<= '>>=
				 'dot '-> 'cast 'strcat))
(defvar *unary-operators* (list '+ '- '* '~ 'not 'sizeof 'addr
				'deref '++ '-- 'post1+ 'post1-))
(defvar *ternary-operators* (list 'tern))
(defvar *overloaded-operators* (intersection *binary-operators* *unary-operators*))
(defvar *aggregate-operators* (list 'structl 'arrayl))
(defvar *operators* (append *binary-operators* *unary-operators*
			    *aggregate-operators* *ternary-operators*))
(defvar *cicl-macros* ())

(defmacro defcmacro (name args &rest body)
  `(progn
     (defun ,name ,args
       (parse-c
	 ,@body))
     (unionf *cicl-macros* (list ',name))))

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
	      ;; else it's a either a macro or function call
	      (parse-c-function-call-or-macro expression))))
      ;; else
      (parse-c-atomic-expression expression)))

(defun parse-c-function-call-or-macro (expression)
  (assert (listp expression))
  (destructuring-bind (fnc-or-macro-name &rest args) expression
    (if (contains *cicl-macros* fnc-or-macro-name)
	(parse-macro expression)
	;; else it's a function
	(parse-c-function-call expression))))

(defun parse-macro (expression)
  (destructuring-bind (macro-name &rest args) expression
    (assert (contains *cicl-macros* macro-name))
    (eval `(apply (symbol-function ',macro-name) ',args))))

(defun parse-c-function-call (expression)
  (destructuring-bind (function-name &rest args) expression
    (make-c-function-call (parse-c-identifier function-name)
			  (mapcar #'parse-c-expression args))))

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
	 (map 'string #'unlispify-character (symbol-name symbol))))
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

(defun parse-nested-c-binary-operator-expression (expression)
  (assert (length= 3 expression))
  (let ((op (car expression))
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
	  ((string= op 'cast) ;(let ((subexps
				;     (cons (parse-c-type (cadr expression)) (cdr subexps))))
	   (make-c-operator-expression cast subexps))
	  
	  ((string= op 'strcat) (make-c-operator-expression cast subexps))
	  (t (error "~A isn't a binary operator." op)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-c-subvariable-description (description)
  (let ((variable-name (car description))
	(type-specifier (cdr description)))
    (make-c-subvariable-description (parse-c-identifier variable-name)
				    (parse-c-type type-specifier))))

(defun %parse-c-type (type-specifier &optional qualifiers
					   type-name pointer-level)
  (let ((sym (car type-specifier)))
    (cond
      ((some #'(lambda (s) (string= sym s))
	     (list 'const 'volatile 'long 'struct))
       (appendf qualifiers (list sym)))
      ((every #'(lambda (c) (char= c #\*))
	      (symbol-name sym))
       (setf pointer-level (count #\* (symbol-name sym))))
      ((symbolp sym)
       (setf type-name sym))
      (t (error "malformed type-specifier ~A with sym ~A of type ~A." type-specifier sym (type-of sym))))
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
  (cond
    ((null statement) (parse-c-empty-statement statement))
    ((listp statement)
     (let ((ckeyword (car statement)))
       (cond
	 ((string= ckeyword 'defun) (parse-c-function-defn statement))
	 ((string= ckeyword 'defvar) (parse-c-variable-decl statement))
	 ((string= ckeyword 'defstruct) (parse-c-struct-decl statement))
	 ((string= ckeyword 'return) (parse-c-return-statement statement))
	 ((string= ckeyword 'if) (parse-c-if-statement statement))
	 ((string= ckeyword 'for) (parse-c-for-loop statement))
	 ((string= ckeyword 'while) (parse-c-while-loop statement))
	 ((string= ckeyword 'typedef) (parse-c-typedef-decl statement))
	 ((string= ckeyword 'scope) (parse-explicit-c-scope statement))
	 (t (parse-c-expression-statement statement)))))))

(defun parse-c-scope (scope)
  (let ((statements (mapcar #'parse-c-statement scope)))
    (make-c-scope statements)))

(defun parse-explicit-c-scope (scope)
  (assert (string= 'scope (car scope)))
  (parse-c-scope (cdr scope)))

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
	(let ((fnc-name (parse-c-identifier function-name))
	      (return-type (parse-c-type function-return-type-specifier))
	      (arg-list (mapcar #'parse-c-subvariable-description lambda-list))
	      (body (parse-c-scope function-body)))
	  (make-c-function-defn fnc-name return-type arg-list body)))))

(defun parse-c-function-decl (statement)
  (assert (string= 'defun (car statement)))
  (assert (null (cddddr statement)))
  (let* ((function-name (cadr statement))
	 (lambda-list (caddr statement))
	 (function-return-type-specifier (cadddr statement))
	 (fnc-name (parse-c-identifier function-name))
	 (return-type (parse-c-type function-return-type-specifier))
	 (arg-list (mapcar #'parse-c-subvariable-description lambda-list)))
    (make-c-function-decl fnc-name return-type arg-list)))

(defun parse-c-return-statement (statement)
  (assert (string= 'return (car statement)))
  (if (cadr statement)
      (let ((return-value (parse-c-expression (cadr statement))))
	(make-c-return-statement return-value))
      ; else
      (make-c-return-statement nil)))

(defun parse-c-continue-statement (statement))

(defun parse-c-break-statement (statement))

(defun parse-c-enum-decl (statement)
  (assert (string= 'defenum (car statement))))

(defun parse-c-struct-decl (statement)
  (assert (string= 'defstruct (car statement)))
  (let ((type-name (parse-c-identifier (cadr statement)))
	(member-list (mapcar #'parse-c-subvariable-description (cddr statement))))
    (make-c-struct-decl type-name nil member-list)))

(defun parse-c-typedef-decl (statement)
  (assert (string= 'typedef (car statement)))
  (let ((name (parse-c-identifier (car (last statement))))
	(ctype (parse-c-type (cadr statement))))
    (make-c-typedef-decl name ctype)))

(defun parse-c-if-statement (statement)
  (assert (string= 'if (car statement)))
  (let* ((test-expression (parse-c-expression (cadr statement)))
	 (then-body (parse-c-statement (caddr statement)))
	 (else-body (cdddr statement))
	 (else-scope (parse-c-scope else-body)))
    (make-c-if-statement test-expression then-body else-scope)))

(defun parse-c-empty-statement (statement)
  (assert (null statement))
  (make-c-empty-expression))

(defun parse-c-switch-statement (statement)
  (assert (string= 'switch (car statement))))

(defun parse-c-do-loop (statement)
  (assert (string= 'do (car statement))))

(defun parse-c-while-loop (statement)
  (destructuring-bind (while-symbol test-expression &rest body) statement
    (assert (string= while-symbol 'while))
    (let ((test-expr (parse-c-expression test-expression))
	  (body (parse-c-scope body)))
      (make-c-while-loop test-expr body))))

(defun parse-c-for-loop (statement)
  (assert (string= 'for (car statement)))
  (let* ((control-list (cadr statement))
	 (body (mapcar #'parse-c-scope (list (cddr statement))))
	 (initializer (parse-c-expression (car control-list)))
	 (test-expression (parse-c-expression (cadr control-list)))
	 (incrementer (parse-c-expression (caddr control-list))))
    (make-c-for-loop test-expression (car body) initializer incrementer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-cpp-directive (directive)
  (assert (cpp-directive-p directive))
  (let ((directive-name (car directive)))
    (cond
      ((string= directive-name 'include) (parse-cpp-include directive))
      (t (error "Can't parse ~A directive." directive-name)))))

(defun parse-cpp-include (directive)
  (assert (string= 'include (car directive)))
  (let* ((filename (cadr directive))
	 (filename-len (length filename)))
    (if (and (char= (elt filename 0) #\<)
	     (char= (elt filename (1- filename-len)) #\>))
	(make-cpp-include filename)
	;; else, add some quotes for when printing.
	(make-cpp-include (concatenate 'string "\"" filename "\"")))))
	

(defun parse-c-statement-or-directive (statement-or-directive)
  (if (cpp-directive-p statement-or-directive)
      (parse-cpp-directive statement-or-directive)
      ;; else
      (parse-c-statement statement-or-directive)))

(defun cpp-directive-p (directive)
  (and (listp directive) (some #'(lambda (s) (string= (car directive) s)) (list 'include 'define
									   'ifdef 'undef))))

(defun parse-c (&rest statements-or-directives)
  (make-c-program (mapcar #'(lambda (s-or-d) (parse-c-statement-or-directive s-or-d))
			  statements-or-directives)))

(defmacro cicl (&rest statements-or-directives)
  `(parse-c ,@(iter
	       (for s-or-d in statements-or-directives)
	       (collect `',s-or-d))))

(defmacro cicl-to-file (file-name &rest cicl-statements)
  `(with-open-file (out ,file-name :direction :output :if-exists :supersede
			           :if-does-not-exist :create)
     (print (cicl ,@cicl-statements) out)))

(defmacro cicl-compile (compiled-filename &rest cicl-statements)
  (let ((source-filename (concatenate 'string compiled-filename ".c")))
  `(progn
     (cicl-to-file ,source-filename ,@cicl-statements)
     (trivial-shell:shell-command (concatenate 'string "gcc -ggdb -Wall -Wextra -O2 -std=gnu11 "
					       ,source-filename " -o " ,compiled-filename)))))

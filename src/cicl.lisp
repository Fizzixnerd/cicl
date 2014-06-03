(cl:in-package :cicl)

;; C Preprocessor Directives

(cl:defmacro literal ())
(cl:defmacro fliteral ())

(cl:defmacro wrap-in-parser (name parser-name)
  `(cl:defun ,name (cl:&rest args)
    (parse-c ',(cl:cons name 'args))))

(cl:defmacro defun (name cl:&rest args)
  `(cl:progn
     (cl:defmacro ,name (cl:&rest args)
     `(parse-c-function-call ',(cl:cons ',name args)))
     (parse-c-function-defn ',(cl:cons 'defun name args))))

(defun do-cool-stuff ((first-arg :int) (second-arg struct :int-list *))
  (:void)
  (= first-arg (-> second-arg next)))




(wrap-in-parser include parse-cpp-include)
(wrap-in-parser = parse-c-expression)
(wrap-in-parser * parse-c-expression)
(wrap-in-parser -> parse-c-expression)

(parse-c-expression
 parse-c-function-call
 parse-c-operator-expression
 parse-c-overloaded-operator-expression
 parse-c-atomic-expression
 parse-c-identifier
 parse-c-unary-operator-expression
 parse-c-binary-operator-expression
 parse-c-ternary-operator-expression
 parse-c-subvariable-description
 parse-c-type

 parse-c-statement
 parse-c-scope
 parse-explicit-c-scope
 parse-c-expression-statement
 parse-c-variable-decl
 parse-c-function-defn
 parse-c-funtion-decl
 parse-c-return-statement
 parse-c-continue-statement
 parse-c-break-statement
 parse-c-enum-decl
 parse-c-struct-decl
 parse-c-typedef-decl
 parse-c-if-statement
 parse-c-empty-statement
 parse-c-switch-statement
 parse-c-do-loop
 parse-c-while-loop
 parse-c-for-loop

 parse-cpp-directive
 parse-cpp-include

 parse-c-statement-or-directive
 parse-c)


(wrap-in-parser for parse-c-for-loop)
(wrap-in-parser while parse-c-while-loop)

(cl:defmacro ifdef ())
(cl:defmacro ifndef ())
(cl:defmacro line ())
(cl:defmacro pragma ())

;; Statements

(cl:defmacro for (cl:&rest args)
  `(parse-c-for-loop ',(cl:cons 'for args)))
(cl:defmacro while (cl:&rest args)
  `(cicl-sys::parse-c-while-loop ',(cl:cons 'while args)))
(cl:defmacro do ())

(cl:defmacro if (test then-body &body else-body))
(cl:defmacro switch ())
(cl:defmacro case ())
(cl:defmacro label ())
(cl:defmacro goto ())

(cl:defmacro defstruct ())
(cl:defmacro defun ())
(cl:defmacro defvar ())
(cl:defmacro decl ())
(cl:defmacro typedef ())

;; Expressions

(cl:defmacro + ())
(cl:defmacro - ())
(cl:defmacro * ())
(cl:defmacro / ())
(cl:defmacro % ())
(cl:defmacro band ())
(cl:defmacro bor ())
(cl:defmacro ^ ())
(cl:defmacro ~ ())
(cl:defmacro << ())
(cl:defmacro >> ())

(cl:defmacro = ())
(cl:defmacro += ())
(cl:defmacro -= ())
(cl:defmacro *= ())
(cl:defmacro /= ())
(cl:defmacro %= ())
(cl:defmacro band= ())
(cl:defmacro bor= ())
(cl:defmacro ^= ())
(cl:defmacro ~= ())
(cl:defmacro <<= ())
(cl:defmacro >>= ())

(cl:defmacro and ())
(cl:defmacro or ())
(cl:defmacro not ())
(cl:defmacro == ())
(cl:defmacro != ())

(cl:defmacro -> ())
(cl:defmacro . ())

(cl:defmacro sizeof ())
(cl:defmacro cast ())

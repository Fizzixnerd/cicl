(defpackage :c-in-cl-sys
  (:nicknames :cicl-sys)
  (:use :cl
	:iterate
	:named-readtables
	:alexandria)
  (:export parse-c-expression
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
	   parse-c))

(defvar *old-readtable* (copy-readtable nil))
(defvar *old-print-case* *print-case*)
(setf (readtable-case *readtable*) :invert)
(setf *print-case* :downcase)

(defpackage :c-in-cl
  (:nicknames :cicl)
  (:use :cicl-sys
	:iterate
	:named-readtables
	:alexandria))

(in-package :cicl)

(cl:defvar *operators* (list '+ '- '* '/ '% 'band 'bor '^ '~ '<< '>> 'and
			  'or 'not '== '!= '= '+= '-= '*= '/= '%= 'band=
			  'bor= '^= '~= '<<= '>>= '. '-> 'sizeof 'cast))

;; C Preprocessor Directives
  
(cl:defmacro line-number ())

(cl:defmacro literal ())
(cl:defmacro fliteral ())

(cl:defmacro include ())
(cl:defmacro ifdef ())
(cl:defmacro ifndef ())
(cl:defmacro line ())
(cl:defmacro pragma ())

;; Statements

(cl:defmacro for ((initializer test incrementer) &body body))
(cl:defmacro while ())
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

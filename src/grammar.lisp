;;;; Here we go with the definition of the C grammar and stuff.

(defpackage :cili.grammar.token
  (:use :cl)
  (:shadow :keyword)
  (:nicknames :cgt))

(in-package :cili.grammar.token)

(defclass token ()
  ((repr
    :initarg :repr
    :initform (error "Attempt to create an uninitialized token.")
    :reader repr
    :documentation "The representation of the token in question."))
  (:documentation "Token base class, from which other tokens inherit."))

(defclass c-token (token)
  ()
  (:documentation "C11 token base class, from which other tokens for the C programming language inherit."))

(defclass keyword (c-token)
  ((valid-keywords
    :initform (vector "auto" "break" "case" "char" "const" "continue" "default" "do" "double" "else" "enum" "extern" "float" "for" "goto" "if" "inline" "int" "long" "register" "restrict" "return" "short" "signed" "sizeof" "static" "struct" "switch" "typedef" "union" "unsigned" "void" "volatile" "while" "_Alignas" "_Alignof" "_Atomic" "_Bool" "_Complex" "_Generic" "_Imaginary" "_Noreturn" "_Static_assert" "_Thread_local")
    :reader valid-keywords
    :type 'vector
    :documentation "A class allocated vector of the C11 keywords"
    :allocation :class))
  (:documentation "A C keyword."))

(defmethod initialize-instance :after ((kw keyword) &key)
  (with-slots (repr) kw
    (typecase repr
      ('string))))

(defclass identifier (c-token)
  ()
  (:documentation "A C identifier."))

(defclass constant (c-token)
  ()
  (:documentation "A C constant."))

(defclass string-literal (c-token)
  ()
  (:documentation "A C string literal."))

(defclass punctuator (c-token)
  ()
  (:documentation "A C punctuator."))

(defun lisp-keyword-->c-keyword-string (kw)
  (check-type (repr kw) cl:keyword)
  (let* ((kw-string (string kw)))
    (assert (plusp (length kw-string)))
    (values (intern (string-downcase (substitute-if #\-
						    #'(lambda (c) (char= c #\_))
						    (if (char= (elt kw-string 0) #\_)
							(subseq kw-string 1)
							kw-string)))
		    "KEYWORD"))))
  
(defun sanitize-keyword-repr (kw)




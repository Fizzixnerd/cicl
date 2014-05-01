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

(defvar *valid-c-keywords* (vector "auto" "break" "case" "char" "const" "continue" "default" "do" "double" "else" "enum" "extern" "float" "for" "goto" "if" "inline" "int" "long" "register" "restrict" "return" "short" "signed" "sizeof" "static" "struct" "switch" "typedef" "union" "unsigned" "void" "volatile" "while" "_Alignas" "_Alignof" "_Atomic" "_Bool" "_Complex" "_Generic" "_Imaginary" "_Noreturn" "_Static_assert" "_Thread_local")
  "A vector containing all the valid C11 keywords.")

(defclass keyword (c-token)
  ((valid-keywords
    :initform *valid-c-keywords*
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
  (check-type kw cl:keyword)
  (let ((kw-string (string kw)))
    (assert (plusp (length kw-string)))
    (let ((unlispified-kw-string (substitute-if #\_
						#'(lambda (c) (char= c #\-))
						(string-downcase kw-string))))
      (if (find unlispified-kw-string *valid-c-keywords*)
	  unlispified-kw-string
	  (let* ((first-char (elt unlispified-kw-string 0))
		 (underscored-capitalized-kw-string (concatenate 'string
								"_"
								(substitute (char-upcase first-char)
									    first-char
									    unlispified-kw-string
									    :count 1))))
	    (if (find underscored-capitalized-kw-string *valid-c-keywords*)
		underscored-capitalized-kw-string
		(error "~A is not a valid keyword." '(kw))))))))

(defun c-keyword-string-->lisp-keyword (kw-string)
  (check-type kw-string string)
  (assert (plusp (length kw-string)))
  (assert (find kw-string *valid-c-keywords*))
  (let ((first-char (elt kw-string 0)))
    (values (intern (string-upcase (substitute-if #\-
						  #'(lambda (c) (char= c #\_))
						  (if (char= first-char #\_)
						      (subseq kw-string 1)
						      kw-string)))
		    "KEYWORDS"))))
  
(defun sanitize-keyword-repr (kw)




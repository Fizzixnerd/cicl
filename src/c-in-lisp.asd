(asdf:defsystem :c-in-cl
  :name "c-in-cl"
  :version "0"
  :description "Write C code in Common Lisp and compile with your C compiler."
  :maintainer "Matt Walker"
  :author "Matt Walker <matt.g.d.walker@gmail.com>"
  :license "2-Clause BSD License"
  :depends-on (#:alexandria
	       #:iterate)
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "macros"
				:depends-on ("package"))
			 (:file "operator"
				:depends-on ("package"
					     "macros"))
			 (:file "representation"
				:depends-on ("package"
					     "macros"))
			 (:file "expression"
				:depends-on ("package"
					     "macros"
					     "operator"
					     "representation"))
			 (:file "c-expression"
				:depends-on ("package"
					     "macros"
					     "operator"
					     "representation"
					     "expression"))
			 (:file "c-operators"
				:depends-on ("package"
					     "macros"
					     "operator"
					     "representation"))
			 (:file "string"
				:depends-on ("package"
					     "macros"
					     "operator"
					     "representation"
					     "expression"))
			 (:file "argument-list"
				:depends-on ("package"
					     "macros"
					     "operator"
					     "representation"
					     "expression"))))))

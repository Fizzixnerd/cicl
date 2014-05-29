(asdf:defsystem :c-in-cl
  :name "c-in-cl"
  :version "0"
  :description "Write C code in Common Lisp and compile with your C compiler."
  :maintainer "Matt Walker"
  :author "Matt Walker <matt.g.d.walker@gmail.com>"
  :license "2-Clause BSD License"
  :depends-on (#:alexandria
	       #:iterate)
  :components ((:file "package")
	       (:file "operator"
		      :depends-on ("package"))
	       (:file "c-operators"
		      :depends-on ("package"
				   "operator"))

	       ))

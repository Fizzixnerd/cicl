(in-package :cicl-sys)

(defclass statement () ())

(defclass expression-statement (statement)
  ((expression
   :type 'expression
   :reader expression
   :initarg :expression
   :initform (error "Must provide an expression."))))

(defclass decl (statement) ())

(defclass compound-statement (statement)
  ())


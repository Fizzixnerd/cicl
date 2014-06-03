(in-package :cicl-sys)

(defclass c-type ()
  ((base-name
    :type 'c-identifier
    :reader base-name
    :initarg :base-name
    :initform (error "Must provide a base-name."))
   (qualifiers
    :type 'list
    :reader qualifiers
    :initarg :qualifiers
    :initform (error "Must provide qualifiers"))
   (pointer-level
    :type 'fixnum
    :reader pointer-level
    :initarg :pointer-level
    :initform (error "Must provide a pointer-level."))))

(defclass c-scope (statement)
  ((statements
    :type 'list
    :reader statements
    :initarg :statements
    :initform (error "Must provide statements."))))

(defclass c-expression-statement (expression-statement) ())
(defclass c-return-statement (c-expression-statement) ())
(defclass c-decl (decl)
  ((name
    :type 'c-identifier
    :reader name
    :initarg :name
    :initform (error "Must provide a name."))
   (ctype
    :type 'c-type
    :reader ctype
    :initarg :ctype
    :initform (error "Must provide a ctype."))))
(defclass c-compound-statement (compound-statement)
  ((test-expression
    :type 'c-expression
    :reader test-expression
    :initarg :test-expression
    :initform (error "Must provide a test-expression."))))

(defclass c-subvariable-description ()
  ((name
    :type 'c-identifier
    :reader name
    :initarg :name
    :initform (error "Must provide a name."))
   (ctype
    :type 'c-type
    :reader ctype
    :initarg :ctype
    :initform (error "Must provide a ctype."))))

(defclass c-variable-decl (c-decl)
   ((value
     :type 'c-expression
     :reader value
     :initarg :value
     :initform (error "Must provide a value expression."))))
(defclass c-variable-decl-list ()
  ((decls
    :type 'list
    :reader decls
    :initarg :decls
    :initform (error "Must provide a decls expression."))))
(defclass c-function-decl (c-decl)
  ((arg-list
    ;; list of `c-subvariable-description's.
    :type 'list
    :reader arg-list
    :initarg :arg-list
    :initform (error "Must provide an arg-list."))))
(defclass c-function-defn (c-function-decl)
  ((body
    :type :c-scope
    :reader body
    :initarg :body
    :initform (error "Must provide a body."))))
(defclass c-enum-decl (c-decl)
  ((enum-list
    :type 'list
    :reader enum-list
    :initarg :enum-list
    :initform (error "Must provide an enum-list."))))
(defclass c-struct-decl (c-decl)
  ((member-list
    ;; list of `c-variable-descriptions's
    :type 'list
    :reader member-list
    :initarg :member-list
    :initform (error "Must provide a member-list."))))
(defclass c-typedef-decl (c-decl) ())

(defclass c-if-statement (c-compound-statement)
  ((then-body
    :type 'c-scope
    :reader then-body
    :initarg :then-body
    :initform (error "Must provide a then-body."))
   (else-body
    :type 'c-scope
    :reader else-body
    :initarg :else-body
    :initform (error "Must provide an else-body."))))
;; TODO
(defclass c-switch-statement (c-compound-statement) ())
;; TODO
(defclass c-do-loop (c-compound-statement) ())
(defclass c-while-loop (c-compound-statement)
  ((body
    :type 'c-scope
    :reader body
    :initarg :body
    :initform (error "Must provide a body."))))
(defclass c-for-loop (c-compound-statement)
  ((body
    :type 'c-scope
    :reader body
    :initarg :body
    :initform (error "Must provide a body."))
   (initializer
    :type 'c-expression
    :reader initializer
    :initarg :initializer
    :initform (error "Must provide an initializer."))
   (incrementer
    :type 'c-expression
    :reader incrementer
    :initarg :incrementer
    :initform (error "Must provide an incrementer."))))

(defclass c-program ()
  ((statements
    :type 'list
    :reader statements
    :initarg :statements
    :initform (error "Must provide statements."))))

(defun make-pointer-level-list (pointer-level)
  (when pointer-level
    (if (plusp pointer-level)
	(cons "*" (make-pointer-level-list (1- pointer-level)))
	nil)))

(defmethod print-object ((ct c-type) stream)
  (with-accessors ((base-name base-name) (qualifiers qualifiers) (pointer-level pointer-level)) ct
    (let ((pl-list (make-pointer-level-list pointer-level)))
      (format stream "~{~A ~}~A~{~A~}" qualifiers base-name pl-list))))

;; FIXME: Change me so that I emit line info!
(defmethod print-object ((cs c-scope) stream)
  (with-accessors ((statements statements)) cs
    (format stream "{~%~{~A~%~}}~%" statements)))

(defmethod print-object ((ces c-expression-statement) stream)
  (format stream "~A;" (expression ces)))

(defmethod print-object ((cvd c-variable-decl) stream)
  (with-accessors ((ctype ctype) (name name) (value value)) cvd
    (let ((format-string (if value
			     "~A ~A = ~A;"
			     "~A ~A;")))
      (format stream format-string ctype name value))))

(defmethod print-object ((csd c-subvariable-description) stream)
  (with-accessors ((ctype ctype) (name name)) csd
    (format stream "~A ~A" ctype name)))

(defmethod print-object ((cfdcl c-function-decl) stream)
  (with-accessors ((ctype ctype) (name name) (arg-list arg-list)) cfdcl
    (format stream "~A ~A(~{~A~^, ~});" ctype name arg-list)))

(defmethod print-object ((cfdfn c-function-defn) stream)
  (with-accessors ((ctype ctype) (name name) (arg-list arg-list) (body body)) cfdfn
    (format stream "~A ~A(~{~A~^, ~}) ~A" ctype name arg-list body)))

(defmethod print-object ((cfl c-for-loop) stream)
  (with-accessors ((initializer initializer) (test-expression test-expression)
		   (incrementer incrementer) (body body)) cfl
    (format stream "for (~A; ~A; ~A) ~A" initializer test-expression incrementer body)))

(defmethod print-object ((cfs c-return-statement) stream)
  (with-accessors ((expression expression)) cfs
    (if expression 
	(format stream "return ~A;" expression)
	(format stream "return;"))))

(defmethod print-object ((csd c-struct-decl) stream)
  (with-accessors ((name name) (member-list member-list)) csd
    (format stream "struct ~A {~%~{~A;~%~}};" name member-list)))

(defmethod print-object ((cvdl c-variable-decl-list) stream)
  (with-accessors ((decls decls)) cvdl
    (format stream "~{~A~%~}" decls)))

(defmethod print-object ((ctd c-typedef-decl) stream)
  (with-accessors ((name name) (ctype ctype)) ctd
    (format stream "typedef ~A ~A;" ctype name)))

(defmethod print-object ((cis c-if-statement) stream)
  (with-accessors ((test-expression test-expression) (then-body then-body)
		   (else-body else-body)) cis
    (format stream "if (~A) ~Aelse ~A" test-expression then-body else-body)))

(defmethod print-object ((cp c-program) stream)
  (with-accessors ((statements statements)) cp
    (format stream "~{~A~%~}" statements)))

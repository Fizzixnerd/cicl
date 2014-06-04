(in-package :cicl-sys)

(defclass cpp-directive () ())

(defclass cpp-include (cpp-directive)
  ((filename
    :type 'string
    :reader filename
    :initarg :filename
    :initform (error "Must provide a filename."))))

(defclass cpp-define (cpp-directive)
  ((macro-name
    :type 'c-identifier
    :reader macro-name
    :initarg :macro-name
    :initform (error "Must provide a macro-name."))))

(defmethod print-object ((cppi cpp-include) stream)
  (format stream "#include ~A" (filename cppi)))

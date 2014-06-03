(in-package :cicl-sys)

(defclass cpp-directive () ())

(defclass cpp-include (cpp-directive)
  ((filename
    :type 'string
    :reader filename
    :initarg :filename
    :initform (error "Must provide a filename."))))

(defmethod print-object ((cppi cpp-include) stream)
  (format stream "#include ~A" (filename cppi)))

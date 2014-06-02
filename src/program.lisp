(in-package :cicl)

(defclass program ()
  ((statements
    :type 'list
    :reader statements
    :initarg :statements
    :initform (error "Must provide statements."))))

(defmethod print-object ((program program) stream)
  (format stream "~{~A~%~}" (statements program)))

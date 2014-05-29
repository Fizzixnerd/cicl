(in-package :cicl)

(defclass cstring ()
  ((val
    :type 'string
    :accessor val
    :initform ""
    :initarg :val)))

(defmethod print-object ((cstr cstring) stream)
  (format stream "\"~A\"" (val cstr)))

(defun mkcstring (&optional val)
  (make-instance 'cstring :val val))

(in-package :cicl)

(defclass cstring (literal)
  ((val
    :type 'string)))

(defmethod print-object ((cstr cstring) stream)
  (format stream "\"~A\"" (val cstr)))

(defun mkcstring (&optional val)
  (make-instance 'cstring :val val))

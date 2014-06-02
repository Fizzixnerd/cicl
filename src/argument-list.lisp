(in-package :cicl-sys)

(defclass call-list (compound-expression)
  ((op)
   (subexps
    :type 'list
    :accessor val
    :initarg :val
    :initform nil)))

(defmethod print-object ((cl call-list) stream)
  (format stream "~{~a~^, ~}" (val cl)))

(defun mkcall-list (&optional val)
  (make-instance 'call-list :val val))

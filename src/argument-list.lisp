(in-package :cicl)

(defclass call-list ()
  ((val
    :type 'list
    :accessor val
    :initarg :val
    :initform nil)))

(defmethod print-object ((cl call-list) stream)
  (format stream "~{~a~^, ~}" (val cl)))

(defun mkcall-list (&optional val)
  (make-instance 'call-list :val val))

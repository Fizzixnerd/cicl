(in-package :cicl)

(defclass representation ()
  ((format-str
    :type 'string
    :accessor format-str
    :initform ""
    :initarg :format-str)
   (format-args
    :type 'list
    :accessor format-args
    :initform ()
    :initarg :format-args)))

(defun mkrepr (&optional (str "") (args ()))
  (make-instance 'representation :format-str str :format-args args))

(defmethod print-object ((repr representation) stream)
  (if (format-args repr)
      (format stream
	      (concatenate 'string "~{" (format-str repr) "~}")
	      (format-args repr))
      ;; else
      (format stream (format-str repr))))
    

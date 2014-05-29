(in-package :cicl)

(defmacro call-if-next-method (&rest args)
  `(when (next-method-p) (call-next-method ,@args)))

(defpackage :c-in-cl-sys
  (:nicknames :cicl-sys)
  (:use :cl
	:iterate
	:alexandria))

(defpackage :c-in-cl
  (:nicknames :cicl)
  (:use :cicl-sys
	:iterate
	:alexandria))

(cl:in-package :cicl)

;; OMG FIXME DONT DO THIS EVER
(cl:let ((cl:*readtable* (cl:copy-readtable cl:nil)))
  (cl:setf (cl:readtable-case cl:*readtable*) :invert)
  (named-readtables:defreadtable :cicl (:case :invert)))
(cl:setf cl:*print-case* :downcase)

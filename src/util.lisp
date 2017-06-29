;;;; util.lisp
;;;; Various utilities

(in-package #:jj)

(defun regex-matches-p (regex target-string)
  "Returns whether or not TARGET-STRING as a whole matches the regex."
  (multiple-value-bind (begin end)
      (cl-ppcre:scan regex target-string)
    (cond ((not begin) nil)
          ((not (eql begin 0)) nil)
          ((not (eql end (length target-string))) nil)
          (t t))))

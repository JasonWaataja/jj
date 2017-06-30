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

(defun set= (set1 set2)
  "Checks if the sets have the same number of elements and if each element in
SET1 is in SET2."
  (and (= (cl-containers:size set1) (cl-containers:size set2))
       (cl-containers:every-item-p set1
                                   (lambda (item)
                                     (find-item set2 item)))))

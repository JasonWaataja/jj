;;;; event.lisp
;;;; Key events, etc.

(in-package #:jj)

(defclass event () ())

(defgeneric describe-event (event)
  (:documentation "Return a string describing EVENT."))

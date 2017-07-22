;;;; event.lisp
;;;; Key events, etc.

(in-package #:jj)

(defclass event ()
  ()
  (:documentation "An event for the main loop to process in some way."))

(defgeneric describe-event (event)
  (:documentation "Return a string describing EVENT."))

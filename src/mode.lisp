;;;; mode.lisp
;;;; Program modes such as insert and normal.

(in-package #:jj)

(defclass mode () ())

(defgeneric process-event (mode event)
  (:documentation "Process EVENT based on MODE."))

(defclass insert-mode (mode) ())

(defclass normal-mode (mode) ())

(defmethod process-mode ((mode insert-mode) (event key-event)))

(defmethod process-mode ((mode normal-mode) (event key-event)))

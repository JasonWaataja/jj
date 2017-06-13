;;;; conditions.lisp
;;;; Errors and other conditions.

(in-package #:jj)

(define-condition jj-error (error)
  ((text :initarg :text :reader jj-error-text)))

;;;; buffer.lisp
;;;; Text data

(in-package #:jj)

(defclass buffer ()
  ((lines :reader buffer-lines
          :initarg :reader
          :initform (make-array 0
                                :adjustable t
                                :fill-pointer 0
                                :element-type 'string
                                :initial-element ""))))

(define-condition buffer-line-index-error (jj-error)
  ((line :initarg :line :reader buffer-line-index-error-line)
   (buffer :initarg :buffer :reader buffer-line-index-error-buffer)))

(defun make-buffer (&optional (initial-lines 0) (default-line ""))
  (make-instance 'buffer :lines (make-array initial-lines
                                            :adjustable t
                                            :fill-pointer 0
                                            :element-type 'string
                                            :initial-element default-line)))

(defun buffer-lines-count (buffer)
  (array-dimension buffer 0))

(defun buffer-line (buffer line-number)
  "Unchecked access to buffer lines."
  (aref (buffer-lines buffer) line-number))

(defun (setf buffer-line) (line buffer line-number)
  (setf (aref (buffer-lines buffer) line-number) line))

;;;; frame.lisp
;;;; Representation of 2D text array.

(in-package #:jj)

(defparameter *empty-character* #\Space
  "The character to display when nothing's there.")

(defclass frame ()
  ((start-row :reader frame-start-row
              :initarg :start-row
              :initform 0
              :type 'integer
              :documentation "The starting row.")
   (start-column :reader frame-start-column
                 :initarg :start-column
                 :initform 0
                 :type 'integer
                 :documentation "The starting column.")
   (rows :reader rows
         :initarg :rows
         :initform 0
         :type 'integer
         :documentation "Row count.")
   (columns :reader frame-columns
            :initarg :columns
            :initform 0
            :type 'integer
            :documentation "Column count.")))

(define-condition invalid-frame-value (jj-error)
  ((start-row :initarg :start-row :reader invalid-frame-value-frame-start-row)
   (start-column :initarg :start-column :reader invalid-frame-value-start-column)
   (rows :initarg :rows :reader invalid-frame-value-rows)
   (columns :initarg :columns :reader invalid-frame-value-columns)))

(defun signal-invalid-frame-value (rows columns start-row start-column)
  (error 'invalid-frame-value
         :start-row start-row
         :start-column start-column
         :rows rows
         :columns columns
         :text (format nil "Invalid frame values: ~a ~a ~a ~a"
                       rows
                       columns
                       start-row
                       start-column)))

(defun make-frame (rows columns &key (start-row 0) (start-column 0))
  "Creates a frame, may throw an INVALID-FRAME-VALUE."
  (when (or (minusp rows)
            (minusp columns))
    (signal-invalid-frame-value rows columns start-row start-column))
  (make-instance 'frame
                 :rows rows
                 :columns columns
                 :start-row start-row
                 :start-column start-column))

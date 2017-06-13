;;;; frame.lisp
;;;; Representation of 2D text array.

(in-package #:jj)

(defparameter *empty-character* #\Space
  "The character to display when nothing's there.")

(defun make-default-display ()
  "Makes an empty 2D array of characters."
  (make-array '(0 0)
              :adjustable t
              :initial-element *empty-character*))

(defclass frame ()
  ((rows :accessor frame-rows
         :initarg :rows
         :initform 0
         :type 'integer
         :documentation "Current row count of display.")
   (columns :accessor frame-columns
            :initarg :columns
            :initform 0
            :type 'integer
            :documentation "Current column count of display.")
   (display :accessor frame-display
            :initarg :display
            :initform (make-default-display)
            :type 'array
            :documentation "The array of characters to display.")))

(define-condition invalid-frame-dimensions (jj-error)
  ((rows :initarg :rows :reader invalid-frame-dimensions-rows)
   (columns :initarg :columns :reader invalid-frame-dimensions-columns)))

(defun make-invalid-frame-dimensions (rows columns)
  (error 'invalid-frame-dimensions
         :rows rows
         :columns columns
         :text (format nil "Invalid frame dimensions: ~a ~a" rows columns)))

(defun make-frame (rows columns)
  "Creates a frame with the given dimensions, may signal a
INVALID-FRAME-DIMENSIONS condition."
  (when (or (minusp rows)
            (minusp columns))
    (make-invalid-frame-dimensions rows columns))
  (make-instance 'frame
                 :rows rows
                 :columns columns
                 :display (make-array (list rows columns)
                                      :adjustable t
                                      :initial-element *empty-character*
                                      :element-type 'character)))

(defun frame-at (frame row column)
  "Non-checked access to display."
  (aref (frame-display frame)
        row
        column))

(defun (setf frame-at) (value frame row column)
  "Non-checked access to display."
  (setf (aref (frame-display frame)
              row
              column)
        value))

(defmacro do-frame ((frame character-binding
                           &optional return-form row-binding column-binding) &body body)
  "Loop for each character in frame. Optionally bind ROW and COLUMN to the
current row and column. You can set CHARACTER-BINDING in the loop."
  (let ((row (if row-binding row-binding (gensym)))
        (column (if column-binding column-binding (gensym)))
        (frame-value (gensym)))
    `(let ((,frame-value ,frame))
       (dotimes (,row (frame-rows ,frame-value))
         (dotimes (,column (frame-columns ,frame-value))
           (let ((,character-binding (frame-at ,frame-value ,row ,column)))
             ,@body
             (setf (frame-at ,frame-value ,row ,column) ,character-binding))))
       ,return-form)))

(defun dump-frame (frame)
  "Make visualization of FRAME."
  (dotimes (row (frame-rows frame))
    (dotimes (column (frame-columns frame))
      (format t "~:[ ~;~]~c" (< column 1) (frame-at frame row column)))
    (format t "~%")))

;;;; buffer-frame.lisp
;;;; Representation of 2D text array.

(in-package #:jj)

(defclass frame ()
  ((rows :accessor frame-rows
         :initarg :rows
         :initform 0
         :type 'integer
         :documentation "Physical rows frame occupies.")
   (columns :accessor frame-columns
            :initarg :columns
            :initform 0
            :type 'integer
            :documentation "Physical columns frame occupies.")))

(defclass character-display ()
  ((rows :accessor character-display-rows
         :initarg :rows
         :initform 0
         :type 'integer
         :documentation "Amount of rows the display occupies.")
   (columns :accessor character-display-columns
            :initarg :columns
            :initform 0
            :type 'integer
            :documentation "Amount of columns the display occupies.")))

(defgeneric write-to-display (display character row column)
  (:documentation "Writes CHARACTER to DISPLAY at ROW and COLUMN."))

(defgeneric read-from-display (display row column)
  (:documentation "Reads the character at ROW and COLUMN in DISPLAY and returns it."))

(defclass dummy-display (character-display)
  ((char-array :accessor dummy-display-char-array
               :initarg :char-array
               :initform nil
               :type array
               :documentation "Array of characters.")))

(defmethod read-from-display ((display dummy-display) row column)
  (aref (dummy-display-char-array display) row column))

(defun dump-dummy-display (display)
  (dotimes (i (character-display-rows display))
    (dotimes (j (character-display-columns display))
      (format t "~:[ ~;~]~a" (< j 1) (read-from-display display i j)))
    (format t "~%")))

(defmethod write-to-display ((display dummy-display) character row column)
  (setf (aref (dummy-display-char-array display) row column) character)
  (dump-dummy-display display))

(defclass buffer-frame (frame)
  ((buffer :accessor buffer-frame-buffer
           :initarg :buffer
           :initform nil
           :documentation "The buffer the BUFFER-FRAME refers to.")
   (row :reader buffer-frame-row
        :initarg :row
        :initform 0
        :type 'integer
        :documentation "The starting row.")
   (column :reader buffer-frame-column
           :initarg :column
           :initform 0
           :type 'integer
           :documentation "The starting column.")))

(define-condition invalid-frame-value (jj-error)
  ((rows :initarg :rows :reader invalid-buffer-frame-value-rows)
   (columns :initarg :columns :reader invalid-buffer-frame-value-columns)))

(defun signal-invalid-buffer-frame-value (rows columns)
  (error 'invalid-buffer-frame-value
         :rows rows
         :columns columns
         :text (format nil "Invalid frame values: ~a ~a"
                       rows
                       columns)))

(defun make-buffer-frame (rows columns &key buffer (row 0) (column 0))
  "Creates a buffer-frame, may throw an INVALID-BUFFER-FRAME-VALUE."
  (when (or (minusp rows)
            (minusp columns))
    (signal-invalid-buffer-frame-value rows columns))
  (make-instance 'buffer-frame
                 :buffer buffer
                 :rows rows
                 :columns columns
                 :row row
                 :column column))

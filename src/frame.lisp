;;;; buffer-frame.lisp
;;;; Representation of 2D text array.

(in-package #:jj)

(defparameter *empty-character* #\Space
  "The character that makes no display.")

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

(defun set-display-character (display char)
  "Sets each element of DISPLAY to CHAR."
  (dotimes (i (character-display-rows display))
    (dotimes (j (character-display-columns display))
      (write-to-display display char i j))))

(defun dump-display (display)
  (dotimes (i (character-display-rows display))
    (dotimes (j (character-display-columns display))
      (format t "~a" (read-from-display display i j)))
    (format t "~%"))
  (format t "~%"))

(defclass dummy-display (character-display)
  ((char-array :accessor dummy-display-char-array
               :initarg :char-array
               :initform nil
               :type array
               :documentation "Array of characters.")
   (should-dump-on-write :accessor dummy-display-should-dump-on-write
                         :initarg :should-dump-on-write
                         :initform t
                         :documentation "Should it print every time something changes.")))

(defun make-dummy-display (rows columns)
  (make-instance 'dummy-display
                 :rows rows
                 :columns columns
                 :char-array (make-array (list rows columns)
                                         :initial-element *empty-character*)))

(defmethod read-from-display ((display dummy-display) row column)
  (aref (dummy-display-char-array display) row column))

(defmethod write-to-display ((display dummy-display) character row column)
  (setf (aref (dummy-display-char-array display) row column) character)
  (when (dummy-display-should-dump-on-write display)
    (dump-display display)))

(defun display-test ()
  (let ((disp (make-dummy-display 4 5)))
    (dotimes (i (character-display-rows disp))
      (dotimes (j (character-display-columns disp))
        (write-to-display disp #\a i j)))
    (dump-display disp)))

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
            :documentation "Physical columns frame occupies.")
   (display :accessor frame-display
            :initarg :display
            :initform nil
            :documentation "The display to write to.")))

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

(defun make-buffer-frame (rows columns &key buffer display (row 0) (column 0))
  "Creates a buffer-frame, may throw an INVALID-BUFFER-FRAME-VALUE."
  (when (or (minusp rows)
            (minusp columns))
    (signal-invalid-buffer-frame-value rows columns))
  (make-instance 'buffer-frame
                 :buffer buffer
                 :display display
                 :rows rows
                 :columns columns
                 :row row
                 :column column))

(defgeneric update-frame (frame)
  (:documentation "Updated the contents of FRAME."))

(defmethod update-frame ((frame buffer-frame))
  (loop for relative-line from 0
     for current-line = (+ relative-line (buffer-frame-row frame))
     with buffer = (buffer-frame-buffer frame)
     while (and (< relative-line (frame-rows frame))
                (< current-line (buffer-lines-count buffer)))
     do
       (loop with line = (buffer-line buffer current-line)
          for relative-column from 0
          for current-column = (+ relative-column (buffer-frame-column frame))
          while (and (< current-column (length line))
                     (< relative-column (frame-columns frame)))
          do
            (write-to-display (frame-display frame)
                              (char line current-column)
                              relative-line
                              relative-column))))

(defun update-frame-test ()
  (let* ((disp (make-dummy-display 4 12))
         (buffer (make-buffer))
         (frame (make-buffer-frame 4 12
                                   :buffer buffer
                                   :display disp
                                   :row 1
                                   :column 2)))
    (vector-push-extend "Hello World" (buffer-lines buffer))
    (vector-push-extend "This is the second line." (buffer-lines buffer))
    (update-frame frame)
    (dump-display disp)))

(defclass composite-frame (frame)
  ((subframes :accessor composite-frame-subframes
              :initarg :subframes
              :initform (make-array 0
                                    :adjustable t
                                    :fill-pointer 0)
              :documentation "The child frames to be displayed.")))

(defclass horizontal-frame (composite-frame)
  ())

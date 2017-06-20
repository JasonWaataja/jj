;;;; buffer-frame.lisp
;;;; Representation of 2D text array.

(in-package #:jj)

(defparameter *empty-character* #\Space
  "The character that makes no display.")

(defclass display ()
  ((rows :accessor display-rows
         :initarg :rows
         :initform 0
         :type 'integer
         :documentation "Amount of rows the display occupies.")
   (columns :accessor display-columns
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
  (dotimes (i (display-rows display))
    (dotimes (j (display-columns display))
      (write-to-display display char i j))))

(defun dump-display (display)
  (dotimes (i (display-rows display))
    (dotimes (j (display-columns display))
      (format t "~a" (read-from-display display i j)))
    (format t "~%"))
  (format t "~%"))

(defclass dummy-display (display)
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
    (dotimes (i (display-rows disp))
      (dotimes (j (display-columns disp))
        (write-to-display disp #\a i j)))
    (dump-display disp)))

(defclass frame ()
  ((display :accessor frame-display
            :initarg :display
            :initform nil
            :documentation "The display to write to.")))

(defun frame-rows (frame)
  (display-rows (frame-display frame)))

(defun frame-columns (frame)
  (display-columns (frame-display frame)))

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

(defun make-buffer-frame (&key buffer display (row 0) (column 0))
  "Creates a buffer-frame, may throw an INVALID-BUFFER-FRAME-VALUE."
  (make-instance 'buffer-frame
                 :buffer buffer
                 :display display
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
         (frame (make-buffer-frame
                 :buffer buffer
                 :display disp
                 :row 1
                 :column 2)))
    (vector-push-extend "Hello World" (buffer-lines buffer))
    (vector-push-extend "This is the second line." (buffer-lines buffer))
    (update-frame frame)
    (dump-display disp)))

(defclass composite-frame (frame)
  ((subdisplays :accessor composite-frame-subdisplays
                :initarg :subdisplays
                :initform (make-array 0
                                      :adjustable t
                                      :fill-pointer 0)
                :documentation "The displays for the child frames to be displayed.")
   (orientation :accessor composite-frame-orientation
                :initarg :orientation
                :initform :horizontal
                :documentation "The direction new frames are opened
                in. :HORIZONTAL means a vertial split and :VERTICAL means a
                horizontal split.")))

(defclass composite-frame-display (display)
  ((parent :accessor composite-frame-display-parent
           :initarg :parent
           :initform nil
           :documentation "The composite frame that it renders to.")))

(defun make-composite-frame-display (parent size)
  "Make a COMPOSITE-FRAME-DISPLAY with PARENT and SIZE tall if vertial, SIZE
wide of horizontal."
  (if (eql (composite-frame-orientation parent) :vertical)
      (make-instance 'composite-frame-display
                     :parent parent
                     :rows size
                     :columns (frame-columns parent))
      (make-instance 'composite-frame-display
                     :parent parent
                     :rows (frame-rows parent)
                     :columns size)))

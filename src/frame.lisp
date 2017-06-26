;;;; frame.lisp
;;;; Representation of 2D text array.

(in-package #:jj)

(defparameter *empty-character* #\Space
  "The character that makes no display.")

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
          with relative-column = 0
          for current-column from (+ relative-column (buffer-frame-column frame))
          while (and (< current-column (length line))
                     (< relative-column (frame-columns frame)))
          do
            (let ((src-char (char line current-column)))
              (cond ((char= src-char #\Tab)
                     (loop for i from 0
                        while (and (< i (get-setting 'tab-width))
                                   (< relative-column (frame-columns frame)))
                        do
                          (write-to-display (frame-display frame)
                                            #\Space
                                            relative-line
                                            relative-column)
                          (incf relative-column)))
                    (t (write-to-display (frame-display frame)
                                         (char line current-column)
                                         relative-line
                                         relative-column)
                       (incf relative-column)))))))

(defun update-frame-test ()
  (let* ((disp (make-dummy-display 4 12))
         (buffer (make-buffer))
         (frame (make-buffer-frame
                 :buffer buffer
                 :display disp
                 :row 1
                 :column 0)))
    (vector-push-extend "Hello World" (buffer-lines buffer))
    (vector-push-extend "a	This is the second line." (buffer-lines buffer))
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

(defun frame-size (frame)
  "Returns the rows of the COMPOSITE-FRAME if it's vertical, columns if it's
horizontal."
  (if (eql (composite-frame-orientation frame) :horizontal)
      (frame-columns frame)
      (frame-rows frame)))

;; (defun add-subframe (frame &key buffer autosize size proportion)
;;   "Resizes all subframes in FRAME and adds a new one. If buffer is provided,
;; then assign it to the COMPOSITE-FRAME-DISPLAY. If :AUTOSIZE is provided, the new
;; frame gets it proportional size. If :SIZE is provided, then it is given that
;; many rows/columns. If :PROPORTION is provided, then the new farme takes up that
;; much of the frame."
;;   (let ((current-size (loop for display in (composite-frame-subdisplays frame)
;;                          for i from 0
;;                          sum (frame-size frame) into size
;;                          if (> i 0) (incf size)
;;                          finally (return size))))
;;     (

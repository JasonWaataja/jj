;;;; frame.lisp
;;;; Representation of 2D text array.

(in-package #:jj)

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
   (row :accessor buffer-frame-row
        :initarg :row
        :initform 0
        :type integer
        :documentation "The starting row.")
   (column :accessor buffer-frame-column
           :initarg :column
           :initform 0
           :type integer
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
  (:documentation "Updated the contents of FRAME. Also expected to update
  FRAME's cursor position variables if necessary."))

(defun calculate-tab-width (current-column &optional (tab-width (get-setting 'tab-width)))
  "Return the width that a tab would occupy if inserted at CURRENT-COLUMN. Uses
TAB-WIDTH which defaults to the TAB-WIDTH setting."
  (let* ((new-column (1+ current-column))
         (tab-count (ceiling new-column tab-width)))
    (- (* tab-count tab-width) current-column)))

(defun leading-string-width (leading-string)
  "Calculate the amount of characters wide leading-string would be at the
beginning of a line. This could be more than just the length of LEADING-STRING
if there are tabs, for example."
  (loop for src-char across leading-string
     with current-width = 0
     do
       (cond ((char= src-char #\Tab)
              (incf current-width (calculate-tab-width current-width)))
             (t (incf current-width)))
     finally (return current-width)))

(defun render-leading-if-needed (line buffer-frame relative-line)
  "If there is a tab that goes over the fron of the display, renders it
correctly and returns first the index of the next character in the line to be
rendered and the relative column to start rendering at next."
  (loop for current-column from 0
     with current-width = 0
     while (< current-width (buffer-frame-column buffer-frame))
     for src-char = (char line current-column)
     for src-char-width = (cond ((char= src-char #\Tab) (calculate-tab-width current-width))
                                (t 1))
     do
       (incf current-width src-char-width)
     finally
       (when (> current-width (buffer-frame-column buffer-frame))
         ;; These two conditional branches do the same thing. They could
         ;; probably be made into a function. This conditional should be added
         ;; to later, such as for zero-width characters, etc.
         (cond ((char= src-char #\Tab)
                (loop for i from (buffer-frame-column buffer-frame) below current-width
                   do
                     (write-to-display (frame-display buffer-frame)
                                       #\Space
                                       relative-line
                                       (- i (buffer-frame-column buffer-frame)))))
               (t (loop for i from (buffer-frame-column buffer-frame) below current-width
                     do
                       (write-to-display (frame-display buffer-frame)
                                         #\Space
                                         relative-line
                                         (- i (buffer-frame-column buffer-frame)))))))
       (return (values current-column (- current-width (buffer-frame-column buffer-frame))))))

(defmethod update-frame ((frame buffer-frame))
  (loop for relative-line from 0
     for current-line = (+ relative-line (buffer-frame-row frame))
     with buffer = (buffer-frame-buffer frame)
     with display = (frame-display frame)
     while (and (< relative-line (frame-rows frame))
                (< current-line (buffer-lines-count buffer)))
     for line = (buffer-line buffer current-line)
     with cursor-mark = (buffer-cursor-mark buffer)
     with cursor-position = (text-mark-current-position cursor-mark)
     do
       (multiple-value-bind (current-column relative-column)
           (render-leading-if-needed (buffer-line buffer current-line) frame relative-line)
         (loop while (and (< current-column (length line))
                          (< relative-column (frame-columns frame)))
            for current-position = (make-text-position-with-line buffer
                                                                 current-line
                                                                 current-column)
            when (text-position= current-position
                                 cursor-position)
            do
              (setf (display-cursor-row display) relative-line
                    (display-cursor-column display) relative-column)
            when (position-in-selection-p current-position *selection*)
            do
              (start-highlight display)
            do
              (let ((src-char (char line current-column)))
                (cond ((char= src-char #\Tab)
                       (loop for i from 0
                          with tab-width = (calculate-tab-width (- relative-column
                                                                   (buffer-frame-column frame)))
                          while (and (< i tab-width)
                                     (< relative-column (frame-columns frame)))
                          do
                            (write-to-display display
                                              #\Space
                                              relative-line
                                              relative-column)
                            (incf relative-column)))
                      (t (write-to-display display
                                           (char line current-column)
                                           relative-line
                                           relative-column)
                         (incf relative-column))))
              (incf current-column)
            when (position-in-selection-p current-position *selection*)
            do
              (end-highlight display)
            finally
              (when (and (= current-line
                            (text-position-line-number cursor-position))
                         (= (length line)
                            (text-position-line-position cursor-position)))
                (setf (display-cursor-row display) relative-line
                      (display-cursor-column display) (length line)))))))

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

(defun scroll-buffer-frame (frame &optional (count 1))
  "Moves FRAME down by COUNT lines. COUNT can be negative."
  (decf (buffer-frame-row frame) count))

(defun center-buffer-frame (frame)
  "Moves FRAME to be centered around its buffer's cursor."
  (let ((line-number (text-position-line-number
                      (text-mark-current-position
                       (buffer-cursor-mark
                        (buffer-frame-buffer frame))))))
    (setf (buffer-frame-row frame)
          (- line-number
             (floor (frame-rows frame) 2)))))

;; TODO: Maybe only retreive SCROLLOFF once for performance in this function.
(defun autoscroll-buffer-frame (frame)
  (let* ((buffer (buffer-frame-buffer frame))
         (cursor-mark (buffer-cursor-mark buffer))
         (current-position (text-mark-current-position cursor-mark))
         (line-number (text-position-line-number current-position)))
    (when (< (- line-number
                (buffer-frame-row frame))
             (get-setting 'scrolloff))
      (let ((method (get-setting 'autoscroll-method)))
        (cond ((eql method :maintain-scrolloff)
               (setf (buffer-frame-row frame)
                     (- line-number
                        (get-setting 'scrolloff))))
              ((eql method :center)
               (center-buffer-frame frame))))
      (return-from autoscroll-buffer-frame))
    (let ((top-row (+ (buffer-frame-row frame)
                      (frame-rows frame)
                      -1)))
      (when (< (- top-row line-number)
               (get-setting 'scrolloff))
        (let ((method (get-setting 'autoscroll-method)))
          (cond ((eql method :maintain-scrolloff)
                 (setf (buffer-frame-row frame)
                       (- (+ line-number (get-setting 'scrolloff))
                          (frame-rows frame))))
                ((eql method :center)
                 (center-buffer-frame frame))))))))

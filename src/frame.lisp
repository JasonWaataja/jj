;;;; frame.lisp
;;;; Representation of 2D text array.

(in-package #:jj)

(defclass frame ()
  ((display :accessor frame-display
            :initarg :display
            :initform nil
            :documentation "The display to write to."))
  (:documentation "An array of text that is the middle between a buffer and a
  display. Some object, usually a buffer, figures out how to update a frame,
  which in turn knows how to update the display somehow."))

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
           :documentation "The starting column."))
  (:documentation "A frame that holds a buffer. This should be the most common
  type of frame."))

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

(defvar *main-frame* nil "The frame that the current buffer displays in.")

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

;; TODO: Rewrite this monster function.
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
            for on-highlight = (and (current-buffer-p buffer)
                                    (position-in-selection-p current-position *selection*))
            when (text-position= current-position
                                 cursor-position)
            do
              (setf (display-cursor-row display) relative-line
                    (display-cursor-column display) relative-column)
            when on-highlight
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
            when on-highlight
            do
              (end-highlight display)
            finally
              (when (and (= current-line
                            (text-position-line-number cursor-position))
                         (= (length line)
                            (text-position-line-position cursor-position)))
                (setf (display-cursor-row display) relative-line
                      (display-cursor-column display) (length line)))))))

(defun connect-buffer-frame (buffer frame)
  "Sets the frame of BUFFER to FRAME and the buffer of FRAME to BUFFER. The
frame still needs to be updated afterwards."
  (setf (buffer-frame buffer) frame
        (buffer-frame-buffer frame) buffer))

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

(defclass composite-frame (frame)
  ((child-displays :accessor composite-frame-child-displays
                :initarg :child-displays
                :initform (make-container 'vector-container)
                :type vector-container
                :documentation "The displays for the child frames to be displayed.")
           (orientation :accessor composite-frame-orientation
                :initarg :orientation
                :initform :horizontal
                :documentation "The direction new frames are opened
                in. :HORIZONTAL means a vertial split and :VERTICAL means a
                horizontal split.")
   (gap :accessor composite-frame-gap
        :initarg :gap
        :initform 0
        :type integer
        :documentation "The amount of space in between each child display.")
   (manager :accessor composite-frame-manager
            :initarg :manager
            :type function
            :documentation "A `function' that takes a `composite-frame' and
            returns a list of values corresponding lengths to make each child
            display.")
   (sizes-cache :accessor composite-frame-sizes-cache
                :initarg :sizes-cache
                :type list
                :documentation "A place to store a call to the manager, update
                this before using it if you want to be sure, which would usually
                be in UPDATE-FRAME."))
  (:documentation "A frame that holds more than one subframe and displays them
  all in some way."))

(defclass composite-frame-display (display)
  ((parent :accessor composite-frame-display-parent
           :initarg :parent
           :type composite-frame
           :documentation "The composite frame that it renders to.")
   (child :accessor composite-frame-display-child
          :initarg :child
          :type frame
          :documentation "The frame that renders to this frame."))
  (:documentation "A display that represents one part of a
  `composite-frame'. Writing to this display writes to its parent's display, and
  a frame would be able to write to this display as if it were a normal
  display."))

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

(defun connect-child-display (display frame)
  "Makes DISPLAY and FRAME point to each other and update together."
  (setf (composite-frame-display-child display) frame
        (frame-display frame) display))

(defun horizontal-frame-p (frame)
  "Returns if the `composite-frame' FRAME is horizontal."
  (eql (composite-frame-orientation frame) :horizontal))

(defun vertical-frame-p (frame)
  "Returns if the `composite-frame' FRAME is vertical."
  (eql (composite-frame-orientation frame) :vertical))

(defun composite-frame-size (frame)
  "Returns the rows of the COMPOSITE-FRAME if it's vertical, columns if it's
horizontal."
  (if (horizontal-frame-p frame)
      (frame-columns frame)
      (frame-rows frame)))

(defun composite-frame-display-size (child-display)
  "Returns the correct size of the display based on its orientation."
  (if (horizontal-frame-p (composite-frame-display-parent child-display))
      (display-columns child-display)
      (display-rows child-display)))

;; TODO: Perhaps redo this as a setf function.
(defun composite-frame-display-set-size (child-display size)
  "Sets the size of CHILD-DISPLAY in the correct dimension to SIZE."
  (if (horizontal-frame-p (composite-frame-display-parent child-display))
      (setf (display-columns child-display) size)
      (setf (display-rows child-display) size)))

(defun composite-frame-child-count (frame)
  "Returns the amount of children in a `composite-display'."
  (cl-containers:size (composite-frame-child-displays frame)))

(defun composite-frame-free-size (frame)
  "Returns the size of the `composite-frame' FRAME minus the amount that would
be taken up gaps."
  (- (composite-frame-size frame)
     (* (composite-frame-child-count frame)
        (composite-frame-gap frame))))

(defun composite-frame-sizes (frame)
  "Calls the frame's manager on the frame itself."
  (funcall (composite-frame-manager frame) frame))

(defun update-frame-sizes-cache (frame)
  "Updates the cache of the call to FRAME's manager."
  (setf (composite-frame-sizes-cache frame)
        (composite-frame-sizes frame)))

(defun update-child-display (child-display)
  "Updates the child of CHILD-DISPLAY"
  (update-frame (composite-frame-display-child child-display)))

(defun composite-frame-display-position (display)
  "Returns the position of DISPLAY in its parent's ordering, NIL if it cannot be
found."
  (let ((parent (composite-frame-display-parent display))
        (position 0))
    (do-container (child (composite-frame-child-displays parent))
      (when (eql display child)
        (return-from composite-frame-display-position
          position))
      (incf position))))

(defun composite-frame-display-start (display)
  "Returns the size of all the stuff in the display up until that point as
the second return value or NIL if it cannot be found."
  (let ((parent (composite-frame-display-parent display))
        (size 0))
    (do-container (child (composite-frame-child-displays parent))
      (when (eql display child)
        (return-from composite-frame-display-start
          size))
      (incf size (+ (composite-frame-display-size child)
                    (composite-frame-gap parent))))))

;; WARNING: Relies on the sizes cache being set correctly.
(defmethod write-to-display ((display composite-frame-display) character row column)
  (let ((parent (composite-frame-display-parent display))
        (size (composite-frame-display-start display)))
    (if (horizontal-frame-p parent)
        (write-to-display (frame-display parent)
                          character
                          row
                          (+ size column))
        (write-to-display (frame-display parent)
                          character
                          (+ size row)
                          column))))

;; WARNING: Relies on the sizes cache being set correctly.
(defmethod read-from-display ((display composite-frame-display) row column)
  (let ((parent (composite-frame-display-parent display))
        (size (composite-frame-display-start display)))
    (if (horizontal-frame-p parent)
        (read-from-display (frame-display parent)
                           row
                           (+ size column))
        (read-from-display (frame-display parent)
                           (+ size row)
                           column))))

(defun write-gap (frame start)
  "Writes the gap to the `composite-frame' FRAME starting at the row or column
START depending on the orientation."
  (dotimes (i (composite-frame-gap frame))
    (dotimes (j (composite-frame-size frame))
      (if (horizontal-frame-p frame)
          (write-to-display (frame-display frame)
                            #\|
                            j
                            (+ start i))
          (write-to-display (frame-display frame)
                            #\-
                            (+ start i)
                            j)))))

(defmethod update-frame ((frame composite-frame))
  (update-frame-sizes-cache frame)
  (let ((sizes (composite-frame-sizes-cache frame))
        (children (composite-frame-child-displays frame)))
    (loop with current-size = 0
       initially
         (when (not (empty-p children))
           ;; TODO: Figure out if I can use CL-CONTAINERS:FIRST-ITEM here.
           (let ((child (item-at children 0)))
             (composite-frame-display-set-size child (first sizes))
             (update-child-display child)
             (incf current-size (composite-frame-display-size child))))
       for i from 1 below (composite-frame-child-count frame)
       for child = (item-at children i)
       for size in (rest sizes)
       do
         (write-gap frame current-size)
         (incf current-size (composite-frame-gap frame))
         (composite-frame-display-set-size child size)
         (update-child-display child)
         (incf current-size (composite-frame-display-size child)))))

(defun equal-size-manager (frame)
  "Gives each display the same size if possible, less to the later ones if
necessary."
  (multiple-value-bind (size remainder)
      (ceiling (composite-frame-free-size frame)
               (composite-frame-child-count frame))
    (if (zerop remainder)
        (make-list (composite-frame-child-count frame) :initial-element size)
        (append (make-list (1- (composite-frame-child-count frame))
                           :initial-element size)
                (list (+ size remainder))))))

(defun composite-frame-test ()
  (let* ((display (make-dummy-display 50 50))
         (frame (make-instance 'composite-frame
                               :display display
                               :gap 2
                               :orientation :vertical
                               :manager #'equal-size-manager)))
    (loop repeat 3
       for child = (make-composite-frame-display frame 5)
       for buffer = (make-buffer 3 "Test line")
       for child-frame = (make-buffer-frame :buffer buffer
                                            :display child)
       do
         (connect-child-display child child-frame)
         (container-append (composite-frame-child-displays frame)
                           child))
    (update-frame frame)
    (dump-display display)))

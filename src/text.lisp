;;;; text.lisp
;;;; Most of the text processing.

(in-package #:jj)

(defclass text-position ()
  ((absolute-position :accessor text-position-absolute-position
                      :initarg :absolute-position
                      :initform 0
                      :type integer
                      :documentation "The numbered position of the character in
                      the whole buffer.")
   (line-number :accessor text-position-line-number
                :initarg :line-number
                :initform 0
                :type integer
                :documentation "The line the position is on.")
   (line-position :accessor text-position-line-position
                  :initarg :line-position
                  :initform 0
                  :type integer
                  :documentation "The position within the current line")
   (buffer :accessor text-position-buffer
           :initarg :buffer
           :type buffer
           :documentation "The position the position in on.")))

(define-condition invalid-text-position-error (jj-error)
  ((buffer :reader buffer :initarg :buffer)
   (absolute-position :reader absolute-position :initarg :absolute-position)))

(define-condtiion invalid-text-operation-error (error)
  ())

(defun signal-invalid-text-operation-error (text)
  (error 'signal-invalid-text-operation-error
         :text text))

(defun signal-invalid-text-position-error (buffer absolute-position)
  (error 'invalid-text-position-error
         :buffer buffer
         :absolute-position absolute-position
         :text (format nil "Invalid text position: ~a in ~a"
                       absolute-position
                       buffer)))

(defun make-text-position (buffer absolute-position)
  "Makes a position in BUFFER at ABSOLUTE-POSITION."
  (when (minusp absolute-position)
    (signal-invalid-text-position-error buffer absolute-position))
  (loop for current-line from 0
     with current-position = 0
     for on-last-line = (= current-line (1- (buffer-lines-count buffer)))
     ;; Checks if it's on the last line of the buffer. If it's not, then there's
     ;; a newline at the end of the line.
     while (and (< current-line (buffer-lines-count buffer))
                (< (1- (+ current-position
                          (+ (length (buffer-line buffer current-line))
                             (if on-last-line
                                 0
                                 1))))
                   absolute-position))
     do
       (format t "Current line: ~a~%Current position: ~a~%"
               current-line
               current-position)
       (incf current-position (+ (length (buffer-line buffer current-line))
                                 (if on-last-line
                                     0
                                     1)))
     finally
       (when (>= current-line (buffer-lines-count buffer))
         (signal-invalid-text-position-error buffer absolute-position))
       (return (make-instance 'text-position
                              :absolute-position absolute-position
                              :line-number current-line
                              :line-position (- absolute-position current-position)
                              :buffer buffer))))

(defun text-position= (position1 position2)
  (= (text-position-absolute-position position1)
     (text-position-absolute-position position2)))

(defun text-position< (position1 position2)
  (< (text-position-absolute-position position1)
     (text-position-absolute-position position2)))

(defun text-position> (position1 position2)
  (> (text-position-absolute-position position1)
     (text-position-absolute-position position2)))

(defun text-position-character (position)
  "Retreives the character represented by POSITION in its buffer."
  (let* ((buffer (text-position-buffer position))
         (line (buffer-line buffer (text-position-line-number position))))
    (if (< (text-position-line-position position)
           (length line))
        (char line (text-position-line-position position))
        #\Newline)))

(defun copy-text-position (position)
  (make-instance 'text-position
                 :absolute-position (text-position-absolute-position position)
                 :line-number (text-position-line-number position)
                 :line-position (text-position-line-position position)))

;; TODO: Optimize this.
(defun text-position-forward (position &optional (count 1))
  "Returns POSITION moved forward by COUNT."
  ;; TODO: Do something with negative values.
  (make-text-position (text-position-buffer position)
                      (+ (text-position-absolute-position position)
                         count)))

(defun text-position-backwards (position &optional (count 1))
  (make-text-position (text-position-buffer position)
                      (- (text-position-absolute-position position)
                         count)))

(defun text-position-difference (position1 position2)
  "Returns the distance POSITION2 must be moved forward to get to POSITION1. Can
be negative."
  (- (text-position-absolute-position position1)
     (text-position-absolute-position position2)))

;; TODO: Optimize this.
(defun text-position-get-string (start-position end-position)
  "Returns the `string' that would begin at START-POSITION and end on the
character before END-POSITION. If END-POSITION is at or before START-POSITION,
then return an empty string."
  ;; TODO: Maybe set the initial array dimension to END-POSITION - START-POSITION.
  (loop with result = (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)
     for current-position = start-position then (text-position-forward current-position)
     while (text-position< current-position end-position)
     do
       (vector-push-extend (text-position-character current-position) result)
     finally (return result)))

(defmacro with-text-positions ((buffer &rest positions) &body body)
  "For each elements of POSITIONS, if it is an `integer', convert it to a
`text-position' before running body. Each element of POSITIONS should be a
variable name, not some arbitrary expression. It wouldn't make sense that way."
  (let ((to-text-position (gensym))
        (position (gensym))
        (buffer-name (gensym)))
    (let ((to-text-position-forms (loop for position in positions
                                     collect `(,position (,to-text-position ,position)))))
      `(let ((,buffer-name ,buffer))
         (flet ((,to-text-position (,position)
                  (if (integerp ,position)
                      (make-text-position ,buffer-name ,position)
                      ,position)))
           (let (,@to-text-position-forms)
             ,@body)))))))

(defclass text-mark ()
  ((current-position :accessor text-mark-current-position
                     :initarg :current-position
                     :type text-position
                     :documentation "Where the text mark currently points
                     to. Needs to be updated when the buffer changes.")
   (buffer :accessor text-mark-buffer
           :initarg :buffer
           :type buffer
           :documentation "The buffer that the mark is a part of.")))

(defun create-mark (buffer position)
  "If POSITION may be an `integer' or `text-position'"
  (with-text-positions (buffer position)
    (let ((mark (make-instance 'text-mark
                               :current-position position
                               :buffer buffer)))
      (vector-push-extend mark (buffer-marks buffer)))))

(defun delete-mark (mark)
  (delete mark (buffer-marks (text-mark-buffer mark))))

(defun move-mark (mark new-position)
  "NEW-POSITION may be an `integer' or `text-position'"
  (with-text-positions ((text-mark-buffer mark) new-position)
    (setf (text-mark-current-position mark) new-position)))

(defmacro with-marks-as-positions ((&rest marks) &body body)
  "For each `mark' in MARKS, run BODY with that mark's current position bound to
that variable name. This also means that each elements of MARKS has to be a
variable name."
  (let ((binding-forms (loop for mark in marks
                          collect `(,mark (text-mark-current-position ,mark)))))
    `(let (,@binding-forms)
       ,@body)))

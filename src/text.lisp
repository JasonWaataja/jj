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
                  :documentation "The position within the current line")))

(define-condition invalid-text-position-error (jj-error)
  ((buffer :reader buffer :initarg :buffer)
   (absolute-position :reader absolute-position :initarg :absolute-position)))

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
                              :line-position (- absolute-position current-position)))))

(defun text-position= (position1 position2)
  (= (text-position-absolute-position position1)
     (text-position-absolute-position position2)))

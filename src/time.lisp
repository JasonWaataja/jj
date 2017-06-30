;;;; time.lisp
;;;; Keep track of time between frames.

(in-package #:jj)

(defun get-time ()
  "Returns the seconds since some arbitrary time in seconds, calculated by
dividing the internal real time by the interal time units per second."
  (float (/ (get-internal-real-time) internal-time-units-per-second)))

(defparameter *current-loop-time* (get-time)
  "The time of the beginning of the last run of the program's main loop, updated
  by UPDATE-TIME.")

(defun get-current-loop-time ()
  *current-loop-time*)

(defparameter *loop-time* 0
  "The time the last loop instance took to execute, updated by UPDATE-TIME.")

(defun update-time ()
  "Updates the time of the last loop and the time between them."
  (let ((current-time (get-time)))
    (setf *loop-time* (- current-time *current-loop-time*)
          *current-loop-time* current-time)))

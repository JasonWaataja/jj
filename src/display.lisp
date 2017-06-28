;;;; display.lisp
;;;; Interface for interacting with the user.

(in-package #:jj)

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

(defgeneric refresh-display (display)
  (:documentation "Makes sure all changes have been written, primarily for curses."))

(defun set-display-character (display char)
  "Sets each element of DISPLAY to CHAR."
  (dotimes (i (display-rows display))
    (dotimes (j (display-columns display))
      (write-to-display display char i j))))

(defun dump-display (display)
  (dotimes (i (+ (display-columns display) 2))
    (format t "*"))
  (format t "~%")
  (dotimes (i (display-rows display))
    (format t "*")
    (dotimes (j (display-columns display))
      (format t "~a" (read-from-display display i j)))
    (format t "*~%"))
  (dotimes (i (+ (display-columns display) 2))
    (format t "*"))
  (format t "~%~%"))

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

(defmethod refresh-display ((display dummy-display)))

(defun display-test ()
  (let ((disp (make-dummy-display 4 5)))
    (dotimes (i (display-rows disp))
      (dotimes (j (display-columns disp))
        (write-to-display disp #\a i j)))
    (dump-display disp)))

(defclass charms-display (display)
  ((charms-window :accessor charms-display-charms-window
                  :initarg :charms-window
                  :initform charms:*standard-window*
                  :documentation "The window to write to.")))

(defun make-charms-display (&optional (window charms:*standard-window*))
  "Make a `charms-display' that uses WINDOW."
  (make-instance 'charms-display :charms-window window))

(defmethod write-to-display ((display charms-display) character row column)
  (charms:write-char-at-point (charms-display-charms-window display)))

(defmethod read-from-display ((display charms-display) row column)
  (charms:char-at-point (charms-display-charms-window display) row column))

(defmethod refresh-display ((display charms-display))
  (charms:refresh-window (charms-display-charms-window display)))

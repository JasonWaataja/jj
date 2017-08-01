;;;; display.lisp
;;;; Interface for interacting with the user.

(in-package #:jj)

(defparameter *empty-character* #\Space
  "The character that makes no display.")

(defclass display ()
  ((rows :accessor display-rows
         :initarg :rows
         :initform 0
         :type integer
         :documentation "Amount of rows the display occupies.")
   (columns :accessor display-columns
            :initarg :columns
            :initform 0
            :type integer
            :documentation "Amount of columns the display occupies.")
   (cursor-row :accessor display-cursor-row
               :initarg :cursor-row
               :initform 0
               :documentation "The screen row of the cursor.")
   (cursor-column :accessor display-cursor-column
                  :initarg :cursor-column
                  :initform 0
                  :documentation "The screen column of the cursor."))
  (:documentation "An abstract array of characters to write to. Could represent
  a physical display or a subsection of another frame to write to. Manipulated
  one character at a time for now."))

(defgeneric write-to-display (display character row column)
  (:documentation "Writes CHARACTER to DISPLAY at ROW and COLUMN."))

(defgeneric read-from-display (display row column)
  (:documentation "Reads the character at ROW and COLUMN in DISPLAY and returns it."))

(defgeneric clear-display (display)
  (:documentation "Clears whatever buffer the display is writing to, mainly for
  ncurses. Make this method do nothing if it is not needed."))

(defmethod clear-display (display))

(defgeneric refresh-display (display)
  (:documentation "Makes sure all changes have been written, primarily for curses."))

(defmethod refresh-display (display))

(defgeneric start-highlight (display)
  (:documentation "Highlight the next text rendered to DISPLAY until
  END-HIGHLIGHT is called."))

(defmethod start-highlight (display))

(defgeneric end-highlight (display)
  (:documentation "Stop highlighting text rendered to DISPLAY."))

(defmethod end-highlight (display))

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
                         :documentation "Should it print every time something changes."))
  (:documentation "A display that simply stores an array of characters and can
  print them to a stream. Every time this display is modified, it does so to the
  standard output stream."))

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
  ((window :accessor charms-display-window
           :initarg :window
           :initform charms/ll:*stdscr*
           :documentation "The window to write to."))
  (:documentation "A display that writes to an ncurses window. This should be
  the predominant class used in a terminal implementation."))

(defun make-charms-display (&optional (window charms/ll:*stdscr*))
  "Make a `charms-display' that uses WINDOW."
  (multiple-value-bind (rows columns)
      (charms/ll:get-maxyx window)
    (make-instance 'charms-display
                   :window window
                   :rows rows
                   :columns columns)))

(defmethod write-to-display ((display charms-display) character row column)
  (charms/ll:mvwaddch (charms-display-window display) row column (char-code character)))

(defmethod clear-display ((display charms-display))
  (charms/ll:wclear (charms-display-window display)))

(defmethod read-from-display ((display charms-display) row column)
  (multiple-value-bind (y x)
      (charms/ll:get-yx (charms-display-window display))
    (let ((ch (charms/ll:mvwinch (charms-display-window display) row column)))
      (charms/ll:move y x)
      ch)))

(defmethod refresh-display ((display charms-display))
  (charms/ll:wmove (charms-display-window display)
                   (display-cursor-row display)
                   (display-cursor-column display))
  (charms/ll:wrefresh (charms-display-window display)))

(defmethod start-highlight ((display charms-display))
  (charms/ll:wattron (charms-display-window display)
                     charms/ll:a_standout))

(defmethod end-highlight ((display charms-display))
  (charms/ll:wattroff (charms-display-window display)
                      charms/ll:a_standout))

(defvar *main-display* (make-dummy-display 0 0)
  "The root display of the program.")

(defun call-with-charms-displays (displays function)
  (unwind-protect
       (funcall function)
    (dolist (display displays)
      (charms/ll:delwin (charms-display-window display)))))

(defmacro with-charms-displays ((&rest displays) &body body)
  "Runs BODY, guaranteeing that the charms windows in DISPLAY and MORE-DISPLAYS
are closed."
  `(call-with-charms-displays (list ,@displays)
                              (lambda () ,@body)))

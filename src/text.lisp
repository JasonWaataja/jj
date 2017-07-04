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

(define-condition invalid-text-operation-error (error)
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
  (when (zerop (buffer-lines-count buffer))
    (return-from make-text-position (make-instance 'text-position
                                                   :buffer buffer)))
  (loop for current-line from 0
     with current-position = 0
     while (and (< current-line (buffer-lines-count buffer))
                (< (+ current-position
                      (length (buffer-line buffer current-line)))
                   absolute-position))
     do
       (incf current-position (1+ (length (buffer-line buffer current-line))))
     finally
       (when (>= current-line (buffer-lines-count buffer))
         (signal-invalid-text-position-error buffer absolute-position))
       (return (make-instance 'text-position
                              :absolute-position absolute-position
                              :line-number current-line
                              :line-position (- absolute-position current-position)
                              :buffer buffer))))

(defun make-text-position-with-line (buffer &optional (line-number 0) (line-position 0))
  "Creates a `text-position' with the given LINE-NUMBER and LINE-POSITION."
  (loop
     initially
       (when (zerop (buffer-lines-count buffer))
         (if (and (zerop line-number) (zerop line-position))
             (make-instance 'text-position
                            :buffer buffer
                            :absolute-position 0
                            :line-number line-number
                            :line-position line-number)))
       ;; TODO: Make it so that this error is correct. The error only deals in
       ;; absolute positions right now, not line numbers.
       (when (>= line-number (buffer-lines-count buffer))
         (signal-invalid-text-position-error buffer 0))
     for current-line from 0
     with current-position = 0
     while (< current-line line-number)
     do
       (incf current-position (1+ (length (buffer-line buffer current-line))))
     finally
       (let ((line (buffer-line buffer line-number)))
       (when (> line-position (length line))
         ;; TODO: See above in this function.
         (signal-invalid-text-position-error buffer 0))
       (return (make-instance 'text-position
                              :buffer buffer
                              :absolute-position (+ current-position
                                                    line-position)
                              :line-number line-number
                              :line-position line-position)))))

(defun text-position= (position1 position2)
  (= (text-position-absolute-position position1)
     (text-position-absolute-position position2)))

(defun text-position< (position1 position2)
  (< (text-position-absolute-position position1)
     (text-position-absolute-position position2)))

(defun text-position> (position1 position2)
  (> (text-position-absolute-position position1)
     (text-position-absolute-position position2)))

(defun text-position<= (position1 position2)
  (<= (text-position-absolute-position position1)
      (text-position-absolute-position position2)))

(defun text-position>= (position1 position2)
  (>= (text-position-absolute-position position1)
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
             ,@body))))))

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

(defun create-text-mark (buffer &optional (position 0))
  "If POSITION may be an `integer' or `text-position'"
  (with-text-positions (buffer position)
    (let ((mark (make-instance 'text-mark
                               :current-position position
                               :buffer buffer)))
      (vector-push-extend mark (buffer-marks buffer))
      mark)))

(defmethod initialize-instance :after ((buffer buffer) &key)
  (format t "after getting run~%")
  (let ((mark (create-text-mark buffer 0)))
    (format t "~a~%" mark)
    (setf (buffer-cursor-mark buffer) mark)))

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

(defclass text-modification ()
  ((buffer :accessor text-modification-buffer
           :initarg :buffer
           :type buffer
           :documentation "The `buffer' the modifcation operates on.")))

(defgeneric apply-modifiction (modification)
  (:documentation "Perform the text modifiction pointed to by MODIFICTION. Must
  keep all text marks up to date."))

(defclass character-insertion (text-modification)
  ((character :accessor character-insertion-character
              :initarg :character
              :type character
              :documentation "The character to insert.")
   (position :accessor character-insertion-position
             :initarg :position
             :type text-position
             :documentation "Where to insert the text.")))

(defun make-character-insertion (buffer character position)
  (make-instance 'character-insertion
                 :buffer buffer
                 :character character
                 :position position))

(defmacro with-moved-marks-from-position ((buffer from-position distance) &body body)
  (alexandria:once-only (buffer from-position distance)
    (alexandria:with-gensyms (moved-marks
                              new-absolute-positions
                              mark
                              absolute-position)
      `(loop for ,mark in (buffer-marks ,buffer)
          when (text-position>= (text-mark-current-position ,mark)
                                ,from-position)
          collect ,mark into ,moved-marks
          and collect (+ (text-position-absolute-position (text-mark-current-position ,mark))
                         ,distance)
          into ,new-absolute-positions
          finally
            ,@body
            (loop for ,mark in ,moved-marks
               for ,absolute-position in ,new-absolute-positions
               do
                 (move-mark ,mark (make-text-position buffer ,absolute-position)))))))

(defmethod apply-modification ((modification character-insertion))
  (let* ((buffer (text-modification-buffer modification))
         (position (character-insertion-position modification))
         (line (buffer-line buffer (text-position-line-number position))))
    (cond ((char= (character-insertion-character modification) #\Newline)
           (let ((first-line (subseq line 0 (text-position-line-position position)))
                 (second-line (subseq line (text-position-line-position position))))
             (setf (buffer-line buffer (text-position-line-number position)) first-line)
             (array-insert-at (buffer-lines buffer)
                              second-line
                              (1+ (text-position-line-number position)))))
          (t
           (setf (buffer-line buffer (text-position-line-number position))
                 (concatenate 'string
                              (subseq line 0 (text-position-line-position position))
                              (list (character-insertion-character modification))
                              (subseq line (text-position-line-position position))))))))

;; (defclass insertion-modifiction (text-modification)
;;   ((position :accessor insertion-modifiction-position
;;              :initarg :position
;;              :type text-position
;;              :documentation "Where to insert the text.")
;;    (text :accessor insertion-modifiction-text
;;          :initarg :text
;;          :type string
;;          :documentation "The text to insert.")))

;; (defun make-insertion-modifiction (buffer position text)
;;   (with-text-positions (buffer position)
;;     (make-instance 'insertion-modifiction
;;                    :buffer buffer
;;                    :position position
;;                    :text text)))

;; (defgeneric apply-modifiction ((modifiction insertion-modification))
;;   (

(defparameter *current-buffer* (make-buffer)
  "The buffer that the user is currently working with.")

(defparameter *cursor-mark* (create-text-mark *current-buffer*)
  "The position of the cursor within the buffer.")

(defun text-position-move-line (position &optional (lines 1))
  "Returns a position moved LINES down, which can be negative, which would move
it up. Doesn't move before the first line or after the last line."
  (let ((new-line-number (+ (text-position-line-number position) lines))
        (new-line-position (text-position-line-position position))
        (buffer (text-position-buffer position)))
    (when (minusp new-line-number)
      (setf new-line-number 0))
    (when (>= new-line-number (buffer-lines-count buffer))
      (setf new-line-number (if (zerop (buffer-lines-count buffer))
                                       0
                                       (1- (buffer-lines-count buffer)))))
    (let ((line (buffer-line buffer new-line-number)))
      (when (> new-line-position (length line))
        (setf new-line-position (length line)))
      (make-text-position-with-line buffer
                                    new-line-number
                                    new-line-position))))

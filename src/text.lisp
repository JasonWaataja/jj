;;;; text.lisp
;;;; Most of the text processing.

(in-package #:jj)

;; TODO: Change these from accessors to readers.
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
           :documentation "The position the position in on."))
  (:documentation "Represents a character position in a buffer. These objects
  are supposed to be immutable and not modified. A `text-position' is only valid
  until the buffer is modified, using it after a buffer has been modified could
  lead to an error. All operations involving `text-position's should follow a
  functional style and return new objects instead of modifying them. You can
  safely pass these around by pointers because they should not be modified
  anyways."))

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
             (return (make-instance 'text-position
                                    :buffer buffer
                                    :absolute-position 0
                                    :line-number line-number
                                    :line-position line-number))))
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

(defun text-position-greatest (position &rest more-positions)
  "Returns the position that is greatest among POSITION and MORE-POSITIONS."
  (loop with greatest = position
     for other-position in more-positions
     when (text-position> other-position greatest)
     do
       (setf greatest other-position)
     finally (return greatest)))

(defun text-position-least (position &rest more-positions)
  "Returns the positions that is the closest to the front of the buffer."
  (loop with least = position
     for other-position in more-positions
     when (text-position> other-position least)
     do
       (setf least other-position)
     finally (return least)))

(defun buffer-first-position (buffer)
  "Returns the position of the first character in BUFFER."
  (make-text-position buffer 0))

(defun buffer-last-position (buffer)
  "Returns the position of the last character in BUFFER."
  (let ((line-count (buffer-lines-count buffer)))
    (cond ((zerop line-count) (make-text-position buffer 0))
          (t
           (let* ((last-line-index (1- line-count))
                  (last-line (buffer-line buffer last-line-index)))
             (make-text-position-with-line buffer
                                           last-line-index
                                           (length last-line)))))))

(defun buffer-line-first-position (buffer line-number)
  "Returns a `text-position' on line LINE-NUMBER in BUFFER that is the first on
that line."
  (make-text-position-with-line buffer line-number))

(defun buffer-line-last-position (buffer line-number)
  "Returns a `text-position' on line LINE-NUMBER in BUFFER that is the last on
that line."
  (make-text-position-with-line buffer
                                line-number
                                (length (buffer-line buffer line-number))))

(defun buffer-length (buffer)
  "Returns the number of characters in BUFFER including newlines."
  (- (text-position-absolute-position (buffer-last-position buffer))
     (text-position-absolute-position (buffer-first-position buffer))))

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

(defun to-text-position (buffer position)
  "Returns POSITION if it is a `text-position'. If it is an integer, then return
a `text-position'` with that absolute position in BUFFER."
  (if (integerp position)
      (make-text-position buffer position)
      position))

(defmacro with-text-positions ((buffer &rest positions) &body body)
  "For each elements of POSITIONS, if it is an `integer', convert it to a
`text-position' before running body. Each element of POSITIONS should be a
variable name, not some arbitrary expression. It wouldn't make sense that way."
  (alexandria:once-only (buffer)
    (let ((to-text-position-forms (loop for position in positions
                                     collect `(,position (to-text-position
                                                          ,buffer
                                                          ,position)))))
      `(let (,@to-text-position-forms)
         ,@body))))

(defun buffer-get-text (buffer &optional
                                 (start (buffer-first-position buffer))
                                 (end (buffer-last-position buffer)))
  "Returns the text in BUFFER bounded by START and END which may be either
`text-position's or `integer's. Passing no arguments gives the whole text of
BUFFER."
  (with-text-positions (buffer start end)
    (text-position-get-string start end)))

(defclass text-mark ()
  ((current-position :accessor text-mark-current-position
                     :initarg :current-position
                     :type text-position
                     :documentation "Where the text mark currently points
                     to. Needs to be updated when the buffer changes.")
   (buffer :accessor text-mark-buffer
           :initarg :buffer
           :type buffer
           :documentation "The buffer that the mark is a part of.")
   (gravity :accessor text-mark-gravity
            :initarg :gravity
            :initform :forward
            :documentation "The direction the mark moves when a character is
            inserted there. The possible values are :FORWARD and :BACKWARD."))
  (:documentation "A persitent place in a buffer, unlike a `text-position'. When
  text is modified correctly, these should also be moved correctly. The thing
  that affects this is GRAVITY which can be forwards or backward if a character
  is inserted at that position."))

(defun create-text-mark (buffer &optional (position 0))
  "If POSITION may be an `integer' or `text-position'"
  (with-text-positions (buffer position)
    (let ((mark (make-instance 'text-mark
                               :current-position position
                               :buffer buffer)))
      (vector-push-extend mark (buffer-marks buffer))
      mark)))

(defmethod initialize-instance :after ((buffer buffer) &key)
  "Add the mark. This is declared here to use `text-mark' stuff."
  (let ((mark (create-text-mark buffer 0)))
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

(defun buffer-cursor-position (buffer)
  "Returns the position of the cursor mark in BUFFER."
  (text-mark-current-position (buffer-cursor-mark buffer)))

(defclass text-modification ()
  ((buffer :accessor text-modification-buffer
           :initarg :buffer
           :type buffer
           :documentation "The `buffer' the modifcation operates on."))
  (:documentation "A modification to a buffer that does so cleanly. This means
  that it updates all text marks, etc. This class exists because simply
  modifying the lines of a buffer correctly would invalidate
  invariants. Therefore, all text modification should be through this class
  unless it is low level buffer code."))

(defgeneric apply-modification (modification)
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
             :documentation "Where to insert the text."))
  (:documentation "Insert one character into a buffer at some position."))

(defun make-character-insertion (buffer character position)
  (make-instance 'character-insertion
                 :buffer buffer
                 :character character
                 :position position))

(defmacro with-moved-marks-from-position ((buffer from-position distance
                                                  &key (obey-gravity t))
                                          &body body)
  "For each mark in BUFFER that would be affected by a change in text that
occured at FROM-POSITION from the beginning of BUFFER's text, move the mark
forward by DISTANCE. This is for the convenience of making modification
functions that can automatically move marks. When OBEY-GRAVITY is true the
gravity of a mark will affect whether or not it is moved. If it is :FORWARD,
then the mark is affected if its absolute position is equal to FROM-POSITION,
but will not be moved if the gravity is :BACKWARD."
  (alexandria:once-only (buffer from-position distance obey-gravity)
    (alexandria:with-gensyms (moved-marks
                              new-absolute-positions
                              mark
                              mark-position
                              absolute-position)
      `(with-text-positions (,buffer ,from-position)
         (loop for ,mark across (buffer-marks ,buffer)
            for ,mark-position = (text-mark-current-position ,mark)
            when (or (text-position> ,mark-position
                                     ,from-position)
                     (and (text-position= ,mark-position
                                          ,from-position)
                          (or (not ,obey-gravity)
                              (eql (text-mark-gravity ,mark)
                                   :forward))))
            collect ,mark into ,moved-marks
            and collect (+ (text-position-absolute-position ,mark-position)
                           ,distance)
            into ,new-absolute-positions
            finally
              ,@body
              (loop for ,mark in ,moved-marks
                 for ,absolute-position in ,new-absolute-positions
                 do
                   (move-mark ,mark (make-text-position buffer ,absolute-position))))))))

(defmethod apply-modification ((modification character-insertion))
  (let* ((buffer (text-modification-buffer modification))
         (position (character-insertion-position modification))
         (line (buffer-line buffer (text-position-line-number position))))
    (with-moved-marks-from-position (buffer
                                     (text-position-absolute-position position)
                                     1)
      (cond ((char= (character-insertion-character modification) #\Newline)
             (let ((first-line (subseq line 0 (text-position-line-position position)))
                   (second-line (subseq line (text-position-line-position position))))
               (setf (buffer-line buffer (text-position-line-number position)) first-line)
               (setf (buffer-lines buffer)
                     (array-insert-at (buffer-lines buffer)
                                      second-line
                                      (1+ (text-position-line-number position))))))
            (t
             (setf (buffer-line buffer (text-position-line-number position))
                   (concatenate 'string
                                (subseq line 0 (text-position-line-position position))
                                (list (character-insertion-character modification))
                                (subseq line (text-position-line-position position)))))))))

(defclass character-deletion (text-modification)
  ((position :accessor character-deletion-position
             :initarg :position
             :type text-position
             :documentation "The position of the character to delete."))
  (:documentation "Delete a character in a buffer at some position."))

(defun make-character-deletion (buffer position)
  (make-instance 'character-deletion
                 :buffer buffer
                 :position position))

(defmethod apply-modification ((modification character-deletion))
  (let* ((buffer (text-modification-buffer modification))
         (position (character-deletion-position modification))
         (line-number (text-position-line-number position))
         (line (buffer-line buffer line-number)))
    (unless (text-position= position (buffer-last-position buffer))
      (with-moved-marks-from-position (buffer
                                       (text-position-forward position)
                                       -1)
        (if (= (text-position-line-position position) (length line))
            (let ((new-line (concatenate 'string
                                         line
                                         (buffer-line buffer (1+ line-number)))))
              ;; TODO: Turn this into one SETF statement with CONCATENATE.
              (setf (buffer-lines buffer)
                    (array-delete-at (buffer-lines buffer) (1+ line-number)))
              (setf (aref (buffer-lines buffer) line-number)
                    new-line))
            (setf (buffer-line buffer line-number)
                  (concatenate 'string
                               (subseq line 0 (text-position-line-position position))
                               (subseq line (1+ (text-position-line-position position))))))))))

(defclass text-insertion (text-modification)
  ((position :accessor text-insertion-position
             :initarg :position
             :type text-position
             :documentation "The position to insert the text at.")
   (text :accessor text-insertion-text
         :initarg :text
         :initform ""
         :type string
         :documentation "The string to insert."))
  (:documentation "Insert a string of text into a buffer at some location."))

(defun make-text-insertion (buffer text position)
  (make-instance 'text-insertion
                 :buffer buffer
                 :position position
                 :text text))

(defmethod apply-modification ((modification text-insertion))
  (loop with text = (text-insertion-text modification)
     for character across text
     with start-position = (text-insertion-position modification)
     for absolute-position from (text-position-absolute-position start-position)
     for position = (make-text-position buffer absolute-position)
     with buffer = (text-modification-buffer modification)
     for insertion = (make-character-insertion buffer character position)
     do
       (apply-modification insertion)))

(defclass text-deletion (text-modification)
  ((start :accessor text-deletion-start
          :initarg :start
          :initform 0
          :type text-position
          :documentation "The position to start deletion from.")
   (end :accessor text-deletion-end
        :initarg :end
        :initform 0
        :type text-position
        :documentation "The position to stop deleting before."))
  (:documentation "Delete some amount of text in a buffer starting at start and
  going to end, not inclusive."))

(defun make-text-deletion (buffer start end)
  (make-instance 'text-deletion
                 :buffer buffer
                 :start start
                 :end end))

(defmethod apply-modification ((modification text-deletion))
  (loop with buffer = (text-modification-buffer modification)
     with start = (text-deletion-start modification)
     with end = (text-deletion-end modification)
     with absolute-position = (text-position-absolute-position start)
     with count = (- (text-position-absolute-position end)
                     absolute-position)
     repeat count
     for position = (make-text-position buffer absolute-position)
     for deletion = (make-character-deletion buffer
                                             position)
     do
       (apply-modification deletion)))

(defvar *current-buffer* (make-buffer)
  "The buffer that the user is currently working with. Initializied in the main
  function.")

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

(defun insert-text (buffer text position)
  "Insert TEXT at POSITION in BUFFER. POSITION may be either a `text-position'
or `integer'."
  (with-text-positions (buffer position)
    (let ((modification (make-text-insertion buffer text position)))
      (apply-modification modification))))

(defun delete-text (buffer start end)
  "Delete text starting at START and going up to but not including END. START
and END may be either `text-position's or `integer's."
  (with-text-positions (buffer start end)
    (apply-modification (make-text-deletion buffer start end))))

(defun replace-text (buffer text start end)
  "Replaces the text in BUFFER starting from START before END with TEXT."
  (let ((position (text-position-absolute-position start)))
    (delete-text buffer start end)
    (insert-text buffer text position)))

(defun set-text (buffer text)
  "Replaces the entirety of the contents of BUFFER with TEXT."
  (replace-text buffer
                text
                (buffer-first-position buffer)
                (buffer-last-position buffer)))

(defclass text-region ()
  ((buffer :accessor text-region-buffer
           :initarg :buffer
           :type text-buffer
           :documentation "The buffer that the region applies to.")
   (anchor :accessor text-region-anchor
           :initarg :anchor
           :type text-position
           :documentation "The anchor of the selection, may be in front or behind
          CURSOR.")
   (cursor :accessor text-region-cursor
           :initarg :cursor
           :type text-position
           :documentation "The cursor of the selection, may be in front or behind
        of ANCHOR. Unlike most use of iterators, this is inclusive, not
        exclusive."))
  (:documentation "Represents continuous a selection of text. In most programs,
  this would be sufficient to represent what the user is currently highlighting,
  but since you can have multiple selections, this is represents just one of
  them."))

(defun make-text-region (buffer &optional
                                  (anchor (buffer-first-position buffer))
                                  (cursor anchor))
  "Creates a text region in BUFFER from ANCHOR to CURSOR which default to the
first position of the buffer."
  (make-instance 'text-region
                 :buffer buffer
                 :anchor anchor
                 :cursor cursor))

(defun text-region-get-text (region)
  "Gets the text contained in REGION."
  (let ((start (text-region-anchor region))
        (end (text-region-cursor region))
        (buffer (text-region-buffer region)))
    (when (buffer-empty-p buffer)
      (return-from text-region-get-text ""))
    (when (text-position< end start)
      (rotatef start end))
    (buffer-get-text buffer start (text-position-forward end))))

(defun text-region-move (region &key
                                  (anchor (text-region-anchor region))
                                  (cursor (text-region-cursor region)))
  (setf (text-region-anchor region) anchor
        (text-region-cursor region) cursor))

(defun text-region-move-to-region (region to-region)
  "Moves REGION to select the same space as TO-REGION."
  (text-region-move region
                    :anchor (text-region-anchor to-region)
                    :cursor (text-region-cursor to-region)))

(defclass text-selection ()
  ((buffer :accessor text-selection-buffer
           :initarg :buffer
           :type buffer
           :documentation "The buffer that the region is part of.")
   (regions :accessor text-selection-regions
            :initarg :regions
            :initform (make-container 'vector-container)
            :type text-region
            :documentation "The sequence of regions that the selection
            contains. Should contain at least one region."))
  (:documentation "A set of text that the user can highlight, basically one or
  more regions."))

(defun make-text-selection (buffer &optional
                                     (anchor (buffer-first-position buffer))
                                     (cursor anchor))
  "Makes a new text selection with one region at ANCHOR and CURSOR. These
default to the beginning of the buffer."
  (let ((selection (make-instance 'text-selection :buffer buffer)))
    (container-append (text-selection-regions selection)
                      (make-text-region buffer anchor cursor))
    selection))

(defmacro do-regions ((region selection) &body body)
  (alexandria:once-only (selection)
    `(do-container (,region (text-selection-regions ,selection))
       ,@body)))

(defun position-in-region-p (position region)
  "Whether or not POSITION is contained in REGION."
  (let ((absolute-position (text-position-absolute-position position))
        (start (text-region-anchor region))
        (end (text-region-cursor region)))
    (when (text-position< end start)
      (rotatef start end))
    (and (>= absolute-position (text-position-absolute-position start))
         (<= absolute-position (text-position-absolute-position end)))))

(defun position-in-selection-p (position selection)
  (do-regions (region selection)
    (when (position-in-region-p position region)
      (return-from position-in-selection-p t)))
  nil)

(defvar *selection* nil
  "The text selection for the program, should be updated the main loop and may
not always be valid.")

(defun reset-selection ()
  "Sets *SELECTION* to point to point to the cursor on *CURRENT-BUFFER*."
  (setf *selection* (make-text-selection *current-buffer*
                                         (buffer-cursor-position *current-buffer*))))

(defmethod apply-modification :after (modification)
  "This is important because whenever a buffer is modified the selection is
invalid and needs to be reset."
  (reset-selection))

;;;; selecting.lisp
;;;; Code relating to how things are selected.

(in-package #:jj)

(defun selection-current-region ()
  "Return the region that is currently being manipulated by the user. If the
user has already selected one or more regions, this one will return the last
one, the one being manipulated."
  (last-item (text-selection-regions *selection*)))

(defmacro define-selector (name (buffer-var from-position-var) &body body)
  "Define a function that when called on a `buffer' and `text-position', returns
a `text-region' that would be selected. It may return NIL to not update the
selection."
  `(defun ,name (,buffer-var ,from-position-var)
     ,@body))

(define-selector character-selector (buffer position)
  (make-text-region buffer
                    position
                    position))

(defun word-character-p (character)
  "Returns if CHARACTER is an alphanumeric character or an underscore."
  (or (alphanumericp character) (char= character #\_)))

(define-selector word-selector (buffer position)
  (loop
     initially
       (when (or (text-position>= position (buffer-last-position buffer))
                 (text-position>= (text-position-forward position)
                                  (buffer-last-position buffer)))
         (return nil))
     with anchor = position
     for next-position = (text-position-forward position)
     then (text-position-forward next-position)
     for cursor = anchor
     then (text-position-forward cursor)
     for character = (text-position-character next-position)
     for found-non-word = (or found-non-word
                              (not (word-character-p character)))
     when (text-position>= next-position (buffer-last-position buffer))
     do (loop-finish)
     when (and (not found-non-word) (not (word-character-p character)))
     do (setf found-non-word t)
     when (and found-non-word (word-character-p character))
     do (loop-finish)
     finally
       (return (make-text-region buffer anchor cursor))))

(define-selector word-reverse-selector (buffer position)
  (loop
     initially
       (when (text-position<= position (buffer-first-position buffer))
         (return nil))
     with anchor = position
     for next-position = (text-position-backwards position)
     then (text-position-backwards next-position)
     for cursor = anchor
     then (text-position-backwards cursor)
     for character = (text-position-character next-position)
     for found-word = (or found-word
                          (word-character-p character))
     when (and (not found-word) (word-character-p character))
     do (setf found-word t)
     when (and found-word (not (word-character-p character)))
     do (loop-finish)
     when (text-position<= next-position (buffer-first-position buffer))
     do
       (setf cursor (text-position-backwards cursor))
       (loop-finish)
     finally
       (return (make-text-region buffer anchor cursor))))

(defun text-region-move-line-select (region new-region)
  "Move the bounds of region so that it encompases all the lines from the anchor
of REGION to the cursor of NEW-REGION."
  (let ((anchor (text-region-anchor region))
        (cursor (text-region-cursor new-region)))
    (let ((from-line (text-position-line-number anchor))
          (to-line (text-position-line-number cursor)))
      (if (text-position< anchor cursor)
          (text-region-move region
                            :anchor (buffer-line-first-position *current-buffer*
                                                                from-line)
                            :cursor (buffer-line-last-position *current-buffer*
                                                               to-line))
          (text-region-move region
                            :anchor (buffer-line-last-position *current-buffer*
                                                               from-line)
                            :cursor (buffer-line-first-position *current-buffer*
                                                                to-line))))))

(defun update-selection (selector)
  "Uses the `text-region' returned by SELECTOR to update the last region of
*SELECTION*, the one being edited. Affected by *SELECTION-MODE*, and can signal
an error if this variable has an invalid value."
  (let* ((region (selection-current-region))
         (new-region (funcall selector
                              *current-buffer*
                              (buffer-cursor-position *current-buffer*))))
    (when new-region
      (ecase *selection-mode*
        (:move (text-region-move-to-region region new-region))
        (:extend (text-region-move region
                                   :cursor (text-region-cursor new-region)))
        (:line (text-region-move-line-select region new-region))))))

(defun make-enter-selection-mode (mode)
  "Makes a function that enters the given selection mode and updates the
selection as if the user had just moved onto the cursor's current position."
  (lambda ()
    (setf *selection-mode* mode)
    (update-selection #'character-selector)))

(defun call-with-selection-mode (mode function)
  "Runs FUNCTION with *SELECTION-MODE* set to MODE. Sets it back at the end."
  (let ((old-mode *selection-mode*))
    (setf *selection-mode* mode)
    (funcall function)
    (setf *selection-mode* old-mode)))

(defmacro with-selection-mode ((mode) &body body)
  "Not really a useful macro, but wrote it anyways because
CALL-WITH-SELECTION-MODE already exists."
  `(call-with-selection-mode ,mode (lambda () ,@body)))

(defun make-with-selection-mode (mode function)
  "Makes a function that when called run function with the selection mode MODE."
  (lambda () (call-with-selection-mode mode function)))

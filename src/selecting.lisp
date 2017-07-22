;;;; selecting.lisp
;;;; Code relating to how things are selected.

(in-package #:jj)

(defun selection-current-region ()
  "Return the region that is currently being manipulated by the user. If the
user has already selected one or more regions, this one will return the last
one, the one being manipulated."
  (last-item (text-selection-regions *selection*)))

(defparameter *selection-mode* :move
  "The way moving the cursor affects selected text. The options
are: :MOVE (select only what the command would select), :EXTEND (extend the
selection to include what the movement would select), and :LINE (select all
lines between where the anchor started and where the cursor currently resides.")

(defmacro define-selector (name (buffer-var from-position-var) &body body)
  "Define a function that when called on a `buffer' and `text-position', returns
a `text-region' that would be selected."
  `(defun ,name (,buffer-var ,from-position-var)
     ,@body))

(define-selector character-selector (buffer position)
  (make-text-region buffer
                    position
                    position))

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
    (ecase *selection-mode*
      (:move (text-region-move-to-region region new-region))
      (:extend (text-region-move region
                                 :cursor (text-region-cursor new-region)))
      (:line (text-region-move-line-select region new-region)))))

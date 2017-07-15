;;;; editing.lisp
;;;; Common functions that the user might use to edit text.

(in-package #:jj)

(defun enter-insert-mode ()
  (setf *current-mode* *insert-mode*))

(defun enter-normal-mode ()
  (setf *current-mode* *normal-mode*))

(defun begin-command ()
  (set-buffer *command-buffer*)
  (setf *current-mode* *insert-mode*))

(defun move-cursor-down (&optional (buffer *current-buffer*))
  (let* ((mark (buffer-cursor-mark buffer)))
    (move-mark mark
               (text-position-move-line (text-mark-current-position mark))))
  (autoscroll-buffer-frame (buffer-frame buffer)))

(defun move-cursor-up (&optional (buffer *current-buffer*))
  (let ((mark (buffer-cursor-mark buffer)))
    (move-mark mark
               (text-position-move-line (text-mark-current-position mark)
                                        -1)))
  (autoscroll-buffer-frame (buffer-frame buffer)))

(defun move-cursor-forward (&optional (buffer *current-buffer*))
  (let* ((mark (buffer-cursor-mark buffer))
         (current-position (text-mark-current-position mark)))
    (let ((new-line-position (1+ (text-position-line-position current-position)))
          (line (buffer-line buffer (text-position-line-number current-position))))
      (cond ((minusp new-line-position)
             (setf new-line-position 0))
            ((> new-line-position
                (length line))
             (setf new-line-position (length line))))
      (move-mark mark
                 (make-text-position-with-line buffer
                                               (text-position-line-number current-position)
                                               new-line-position)))))

(defun move-cursor-backward (&optional (buffer *current-buffer*))
  (let* ((mark (buffer-cursor-mark buffer))
         (current-position (text-mark-current-position mark)))
    (let ((new-line-position (1- (text-position-line-position current-position)))
          (line (buffer-line buffer (text-position-line-number current-position))))
      (cond ((minusp new-line-position)
             (setf new-line-position 0))
            ((> new-line-position
                (length line))
             (setf new-line-position (length line))))
      (move-mark mark
                 (make-text-position-with-line buffer
                                               (text-position-line-number current-position)
                                               new-line-position)))))

(defun exit-command-mode ()
  "Assuming the user is in the command buffer, return to the previous buffer and
clear the command buffer. This function checks to see if it's in the command
buffer first."
  (when (eql *current-buffer* *command-buffer*)
    (set-to-last-buffer)
    (clear-command-buffer)))

(defun process-escape-key ()
  "Process the escape key when in insert mode. Special behavior when in the
  command buffer."
  (exit-command-mode))

(defun clear-buffer (buffer)
  "Deletes all text in BUFFER."
  (delete-text buffer 0 (buffer-length buffer)))

(defun clear-command-buffer ()
  "Calls CLEAR-BUFFER on *COMMAND-BUFFER*."
  (clear-buffer *command-buffer*))

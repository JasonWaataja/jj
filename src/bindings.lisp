;;;; bindings.lisp
;;;; The default bindings for the editor

(in-package #:jj)

(defun enter-insert-mode ()
  (setf *current-mode* *insert-mode*))

(defun enter-normal-mode ()
  (setf *current-mode* *normal-mode*))

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

(defmethod process-key ((mode normal-mode) chord))

(defmethod process-key ((mode insert-mode) chord)
  (let ((modification
         (make-character-insertion *current-buffer*
                                   (chord-character-code chord)
                                   (text-mark-current-position
                                    (buffer-cursor-mark *current-buffer*)))))
    (apply-modification modification)))

(defun enable-default-bindings ()
  (bind-keys "i" #'enter-insert-mode :mode *normal-mode*)
  (bind-keys "j" #'move-cursor-down :mode *normal-mode*)
  (bind-keys "k" #'move-cursor-up :mode *normal-mode*)
  (bind-keys "l" #'move-cursor-forward :mode *normal-mode*)
  (bind-keys "h" #'move-cursor-backward :mode *normal-mode*)
  (bind-keys "<esc>" #'enter-normal-mode :mode *insert-mode*))

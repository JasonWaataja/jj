;;;; bindings.lisp
;;;; The default bindings for the editor

(in-package #:jj)

(defun enter-insert-mode ()
  (setf *current-mode* *insert-mode*))

(defun enter-normal-mode ()
  (setf *current-mode* *normal-mode*))

(defun move-down-cursor (&optional (buffer *current-buffer*))
  (let* ((mark (buffer-cursor-mark buffer)))
    (move-mark mark
               (text-position-move-line (text-mark-current-position mark))))
  (autoscroll-buffer-frame (buffer-frame buffer)))

(defun move-up-cursor (&optional (buffer *current-buffer*))
  (let ((mark (buffer-cursor-mark buffer)))
    (move-mark mark
               (text-position-move-line (text-mark-current-position mark)
                                        -1)))
  (autoscroll-buffer-frame (buffer-frame buffer)))

(defun enable-default-bindings ()
  (bind-keys "i" #'enter-insert-mode :mode *normal-mode*)
  (bind-keys "j" #'move-down-cursor :mode *normal-mode*)
  (bind-keys "k" #'move-up-cursor :mode *normal-mode*)
  (bind-keys "<esc>" #'enter-normal-mode :mode *insert-mode*))

;;;; status.lisp
;;;; Make a status line.

(in-package #:jj)

(defun default-buffer-status-line ()
  "Gives some normal information about the buffer being edited."
  (let ((buffer (last-normal-buffer))
        (strings '()))
    (push (mode-output-name *current-mode*) strings)
    (when buffer
      (push (buffer-get-status-line buffer) strings))
    (concatenate-string-list (nreverse strings) " | ")))

(defparameter *status-line-function* #'default-buffer-status-line
  "The function that is called to produce the `string' to set the status line
to.")

(defun update-status-line ()
  (set-text *status-line-buffer* (funcall *status-line-function*)))

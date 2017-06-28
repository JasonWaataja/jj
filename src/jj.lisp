;;;; jj.lisp
;;;; Main file for jj

(in-package #:jj)

(defun main (argv)
  "Entry point for jj"
  (declare (ignore argv))
  (charms:with-curses ()
    (let* ((display (make-charms-display))
           (default-buffer (make-buffer))
           (default-frame (make-buffer-frame :buffer default-buffer :display display)))
      (charms:disable-echoing)
      (charms:enable-raw-input :interpret-control-characters t)
      (charms:enable-non-blocking-mode charms:*standard-window*)
      (charms:clear-window charms:*standard-window*)
      (loop
         for c = (charms:get-char charms:*standard-window* :ignore-error t)
         do
           (update-frame default-frame)
           (refresh-display display)
           (charms:refresh-window charms:*standard-window*)))))

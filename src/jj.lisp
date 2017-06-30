;;;; jj.lisp
;;;; Main file for jj

(in-package #:jj)

(defun main (argv)
  "Entry point for jj"
  (declare (ignore argv))
  (charms/ll:initscr)
  (charms/ll:cbreak)
  (charms/ll:noecho)
  (multiple-value-bind (rows columns)
      (charms/ll:get-maxyx charms/ll:*stdscr*)
    (let* ((charms-win (charms/ll:newwin rows columns 0 0))
           (default-buffer (make-buffer))
           (main-display (make-charms-display charms-win))
           (default-frame (make-buffer-frame
                           :buffer default-buffer
                           :display main-display)))
      (update-frame default-frame)
      (refresh-display main-display)
      (update-time)
      (loop for ch = (charms/ll:wgetch charms-win)
         while (or (eql ch charms/ll:ERR)
                   (not (eql ch (char-code #\a))))
         do
           (update-time)
           (update-frame default-frame)
           (refresh-display main-display))
      (charms/ll:delwin charms-win)
      (charms/ll:endwin))))

;;;; jj.lisp
;;;; Main file for jj

(in-package #:jj)

;; TODO: Handle control and alt keys.
(defun ncurses-input-to-chord (ch)
  "Converts a result from GETCHAR to the corresponding chord. Doesn't yet know
about the control or alt keys."
  ;; TODO: The cl-charms source code said this wasn't quite right. Maybe fix
  ;; this or something.
  (make-chord (code-char ch)))

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
      (create-mode-binding *current-mode*
                           "b"
                           :action (lambda () (format t "Got b~%")))
      (create-mode-binding *current-mode*
                           "bc"
                           :action (lambda () (format t "Got bc~%")))
      (create-mode-binding *current-mode*
                           "de"
                           :action (lambda () (format t "Got de")))
      (loop for ch = (charms/ll:wgetch charms-win)
         while (or (eql ch charms/ll:ERR)
                   (not (eql ch (char-code #\a))))
         for input-chord = (ncurses-input-to-chord ch)
         do
           (update-time)
           (process-input input-chord)
           (update-frame default-frame)
           (refresh-display main-display))
      (charms/ll:delwin charms-win)
      (charms/ll:endwin))))

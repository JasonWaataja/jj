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
           (main-display (make-charms-display charms-win))
           (default-frame (make-buffer-frame
                           :buffer *current-buffer*
                           :display main-display)))
      (setf (buffer-frame *current-buffer*) default-frame)
      (setf *current-mode* *normal-mode*)
      ;; Use this restart in case MAIN is run multiple times within one Lisp
      ;; instance.
      (handler-bind ((override-binding-error #'use-new-binding))
        (enable-default-bindings))
      (update-frame default-frame)
      (refresh-display main-display)
      (update-time)
      (loop for ch = (charms/ll:wgetch charms-win)
         while (or (eql ch charms/ll:ERR)
                   (not (eql ch (char-code #\a))))
         for input-chord = (ncurses-input-to-chord ch)
         do
           (when (get-setting 'dump-key-events)
             (format t "Received ncurses key ~a, equivalent to ~a~%"
                     ch
                     input-chord))
           (update-time)
           (clear-display main-display)
           (process-input input-chord)
           (update-frame default-frame)
           (refresh-display main-display))
      (charms/ll:delwin charms-win)
      (charms/ll:endwin))))

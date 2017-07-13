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
    (let* ((charms-win (charms/ll:newwin (- rows 2) columns 0 0))
           (command-win (charms/ll:newwin 1 columns (1- rows) 0))
           (main-display (make-charms-display charms-win))
           (command-display (make-charms-display command-win))
           (default-buffer (make-buffer))
           (command-buffer (make-buffer))
           (default-frame (make-buffer-frame
                           :buffer default-buffer
                           :display main-display))
           (command-frame (make-buffer-frame
                           :buffer command-buffer
                           :display command-display)))
      (set-buffer default-buffer)
      (setf (buffer-frame *current-buffer*) default-frame)
      (setf *command-buffer* command-buffer)
      (setf (buffer-frame *command-buffer*) command-frame)
      (setf *main-display* main-display)
      (setf *current-mode* *normal-mode*)
      ;; Use this restart in case MAIN is run multiple times within one Lisp
      ;; instance.
      (handler-bind ((override-binding-error #'use-new-binding))
        (enable-default-bindings))
      (update-frame default-frame)
      (update-frame command-frame)
      ;; (refresh-display main-displa)
      ;; (refresh-display command-display)
      (charms/ll:refresh)
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
           (clear-display *main-display*)
           (clear-display command-display)
           (process-input input-chord)
           (update-frame default-frame)
           (update-frame command-frame)
         ;; This is like this because I'm not sure how to ensure which window
         ;; the cursor displays on. I'm pretty sure it's just whichever is
         ;; refreshed last. So, this ensures that the cursor is in the correct
         ;; section, a hopefully temporary hack.

         ;; TODO: Figure out how to choose which window the cursor is on.
           (cond ((eql *current-buffer* *command-buffer*)
                  (refresh-display *main-display*)
                  (refresh-display command-display))
                 (t
                  (refresh-display command-display)
                  (refresh-display *main-display*))))
      (charms/ll:delwin charms-win)
      (charms/ll:endwin))))

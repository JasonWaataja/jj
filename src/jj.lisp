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
    ;; The line height for the charm windows are done this way because using (1-
    ;; rows) sometimes makes the command buffer non-visible I think. That should
    ;; be tested again.
    (let* ((charms-win (charms/ll:newwin rows columns 0 0))
           (main-display (make-charms-display charms-win))
           (root-frame (make-instance 'composite-frame
                                      :manager #'strong-request-manager
                                      :orientation :vertical
                                      :display main-display))
           (default-buffer (make-buffer))
           (command-buffer (make-buffer))
           (default-frame (make-buffer-frame
                           :buffer default-buffer))
           (command-frame (make-buffer-frame
                           :buffer command-buffer)))
      ;; This line also updates *SELECTION*, so no code is needed to initialize
      ;; it here.
      (setf *main-display* main-display)
      (set-buffer default-buffer)
      (setf *root-frame* root-frame)
      (setf *main-frame* default-frame)
      (connect-buffer-frame *current-buffer* *main-frame*)
      (setf (frame-size-manager *main-frame*)
            #'no-request-size-manager)
      (composite-frame-add-frame *root-frame* *main-frame*)
      (setf *command-buffer* command-buffer)
      (connect-buffer-frame *command-buffer* command-frame)
      (setf (frame-size-manager command-frame)
            #'buffer-frame-lines-size-manager)
      (composite-frame-add-frame *root-frame* command-frame)
      (setf *selection-mode* :move)
      (enter-mode 'normal-mode)
      ;; Use this restart in case MAIN is run multiple times within one Lisp
      ;; instance.
      (handler-bind ((override-binding-error #'use-new-binding))
        (enable-default-bindings))
      ;; Just in case this is being run multiple times in one Lisp instance.
      (clear-commands)
      (add-default-commands)
      (setf *exit-flag* nil)
      (update-frame *main-frame*)
      (update-frame command-frame)
      ;; (refresh-display main-displa)
      ;; (refresh-display command-display)
      (charms/ll:refresh)
      (update-time)
      (loop while (not *exit-flag*)
         for ch = (charms/ll:wgetch charms-win)
         for input-chord = (ncurses-input-to-chord ch)
         do
           (when (get-setting 'dump-key-events)
             (format t "Received ncurses key ~a, equivalent to ~a~%"
                     ch
                     input-chord))
           (update-time)
           (clear-display *main-display*)
           (process-input input-chord)
           (update-frame *root-frame*)
           (refresh-display *main-display*))
      (charms/ll:delwin charms-win)
      (charms/ll:endwin))))

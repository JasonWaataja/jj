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

(defun init-frames (rows columns)
  "Sets up all the default frames with their default buffers and displays given
that the maximum rows and columns of the display."
  (setf *main-display* (make-charms-display (charms/ll:newwin rows columns 0 0)))
  (setf *root-frame* (make-instance 'composite-frame
                                    :manager #'strong-request-manager
                                    :orientation :vertical
                                    :display *main-display*))
  (set-buffer (make-buffer))
  ;; This assumes the default size manager for a frame is to give no request,
  ;; this may change later.
  (setf *main-frame* (make-buffer-frame :display *main-display*))
  (connect-buffer-frame *current-buffer* *main-frame*)
  (composite-frame-add-frame *root-frame* *main-frame*)
  (setf *status-line-buffer* (make-buffer :name "Status Line")
        *message-buffer* (make-buffer :name "Messages")
        *command-buffer* (make-buffer :name "Command Buffer"))
  (composite-frame-add-buffer *root-frame*
                              *status-line-buffer*
                              :size-manager #'buffer-frame-lines-size-manager)
  (composite-frame-add-buffer *root-frame*
                              *message-buffer*
                              :size-manager #'buffer-frame-lines-invisible-size-manager)
  (composite-frame-add-buffer *root-frame*
                              *command-buffer*
                              :size-manager #'buffer-frame-lines-size-manager))

(defun main (argv)
  "Entry point for jj"
  (declare (ignore argv))
  (charms/ll:initscr)
  (charms/ll:cbreak)
  (charms/ll:noecho)
  (clear-buffers)
  (multiple-value-bind (rows columns)
      (charms/ll:get-maxyx charms/ll:*stdscr*)
    (init-frames rows columns))
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
  (update-status-line)
  (update-frame *root-frame*)
  (charms/ll:refresh)
  (update-time)
  (loop while (not *exit-flag*)
     for ch = (charms/ll:wgetch (charms-display-window *main-display*))
     for input-chord = (ncurses-input-to-chord ch)
     ;; TODO: Find out if EQL is the correct comparison to use here.
     when (eql ch charms/ll:KEY_RESIZE)
     do
       (multiple-value-bind (rows columns)
           (charms/ll:get-maxyx charms/ll:*stdscr*)
         (setf (display-rows *main-display*) rows
               (display-columns *main-display*) columns))
     do
       (when (get-setting 'dump-key-events)
         (format t "Received ncurses key ~a, equivalent to ~a~%"
                 ch
                 input-chord))
       (update-time)
       (clear-display *main-display*)
       (process-input input-chord)
       (update-status-line)
       (update-frame *root-frame*)
       (refresh-display *main-display*))
  (charms/ll:delwin (charms-display-window *main-display*))
  (charms/ll:endwin))

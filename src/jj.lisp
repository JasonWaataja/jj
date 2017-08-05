;;;; jj.lisp
;;;; Main file for jj

(in-package #:jj)

;; TODO: Handle control and alt keys.
(defun ncurses-input-to-chord (ch)
  "Converts a result from GETCHAR to the corresponding chord. Doesn't yet know
about the control or alt keys."
  ;; TODO: The cl-charms source code said this wasn't quite right in the
  ;; function where it transforms an ncurses input to a Lisp character. Maybe
  ;; fix this or something.

  ;; TODO: Figure out if EQL is the correct comparison
  ;; to use here.
  (if (eql ch charms/ll:ERR)
      nil
      ;; TODO: Figure how to do this whole thing more in Lisp if
      ;; possible. Running (princ-to-string (code-char ch)) does a very similar
      ;; thing, and I think detecting control characters could use the same code
      ;; with the above code instead of a call to keyname.
      (let ((as-string (cffi:foreign-string-to-lisp (charms/ll:keyname ch)))
            (as-char (code-char ch)))
        ;; TODO: Check if checking STANDARD-CHAR-P could replace these checks.
        ;; From https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node22.html, #\Esc was added.
        (cond ((or (char= as-char #\Backspace)
                   (char= as-char #\Tab)
                   (char= as-char #\Linefeed)
                   (char= as-char #\Page)
                   (char= as-char #\Return)
                   (char= as-char #\Rubout)
                   (char= as-char #\Esc))
               (make-chord (code-char ch)))
              ((and (> (length as-string) 1)
                    (char= (char as-string 0) #\^))
               ;; TODO: Check for the DEL key, https://linux.die.net/man/3/keyname
               (make-chord (char as-string 1) :control))
              ;; TODO: Get ncurses to actually return something of this form
              ;; like it says it should in documentation. This code doesn't
              ;; really do anything right now.
              ((and (> (length as-string) 2)
                    (string= as-string "M-" :end1 2))
               (make-chord (char as-string 2) :mod))
              (t
               (make-chord (code-char ch)))))))

(defun init-frames (rows columns)
  "Sets up all the default frames with their default buffers and displays given
that the maximum rows and columns of the display."
  (setf *main-display* (make-charms-display (charms/ll:newwin rows columns 0 0)))
  (setf *root-frame* (make-instance 'composite-frame
                                    :manager #'strong-request-manager
                                    :orientation :vertical
                                    :display *main-display*))
  ;; This assumes the default size manager for a frame is to give no request,
  ;; this may change later.
  (setf *main-frame* (make-buffer-frame :display *main-display*))
  (connect-buffer-frame *current-buffer* *main-frame*)
  (composite-frame-add-frame *root-frame* *main-frame*)
  (setf *status-line-buffer* (make-buffer :name "StatusLine"
                                          :should-check-write nil)
        *message-buffer* (make-buffer :name "Messages"
                                      :should-check-write nil)
        *command-buffer* (make-buffer :name "CommandBuffer"
                                      :should-check-write nil))
  (composite-frame-add-buffer *root-frame*
                              *status-line-buffer*
                              :size-manager #'buffer-frame-lines-size-manager)
  (composite-frame-add-buffer *root-frame*
                              *message-buffer*
                              :size-manager #'buffer-frame-lines-invisible-size-manager)
  (composite-frame-add-buffer *root-frame*
                              *command-buffer*
                              :size-manager #'buffer-frame-lines-size-manager))

(defun make-default-buffers (argv)
  "Creates some buffers and sets the current buffer to the correct one."
  (cond (argv
         (set-buffer (make-buffer-with-file (first argv)))
         (dolist (file (rest argv))
           (make-buffer-with-file file)))
        (t
         (set-buffer (make-buffer)))))

(defun call-with-charms (function)
  (charms/ll:initscr)
  (charms/ll:cbreak)
  (charms/ll:noecho)
  (unwind-protect
       (funcall function)
    (charms/ll:endwin)))

(defmacro with-charms (() &body body)
  "Runs charms/ll setup, then body, then guarantees its teardown."
  `(call-with-charms (lambda () ,@body)))

(defun run-main-loop ()
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
     when input-chord
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
       (refresh-display *main-display*)))

(defun main (argv)
  "Entry point for jj."
  (with-charms ()
    (clear-buffers)
    (make-default-buffers argv)
    (multiple-value-bind (rows columns)
        (charms/ll:get-maxyx charms/ll:*stdscr*)
      (init-frames rows columns))
    (with-charms-displays (*main-display*)
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
      (refresh-display *main-display*)
      (update-time)
      (run-main-loop))))

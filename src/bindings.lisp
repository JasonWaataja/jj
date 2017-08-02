;;;; bindings.lisp
;;;; The default bindings for the editor

(in-package #:jj)

(defmethod process-key ((mode normal-mode) chord))

(defmethod process-key ((mode insert-mode) chord)
  (let ((cursor-position (text-mark-current-position
                          (buffer-cursor-mark *current-buffer*))))
    (cond ((or (char= (chord-character-code chord) #\Backspace)
               (char= (chord-character-code chord) #\Rubout))
           (unless (text-position= cursor-position
                                   (buffer-first-position *current-buffer*))
             (let ((modification
                    (make-character-deletion *current-buffer*
                                             (text-position-backwards cursor-position))))
               (apply-modification modification))))
          ((and (command-mode-p)
                (char= (chord-character-code chord) #\Newline))
           (let ((command (buffer-get-text *command-buffer*)))
             (exit-command-mode)
             (process-command command)
             (enter-normal-mode)))
          (t (let ((modification
                    (make-character-insertion *current-buffer*
                                              (chord-character-code chord)
                                              cursor-position)))
               (apply-modification modification))))))

(defun enable-default-bindings ()
  (bind-keys "i" #'enter-insert-mode :mode-name 'normal-mode)
  (bind-keys '("<cr>" ":") #'begin-command :mode-name 'normal-mode)
  (bind-keys "j" #'move-cursor-down :mode-name 'normal-mode)
  (bind-keys "k" #'move-cursor-up :mode-name 'normal-mode)
  (bind-keys "l" #'move-cursor-forward :mode-name 'normal-mode)
  (bind-keys "h" #'move-cursor-backward :mode-name 'normal-mode)
  (bind-keys "v" (make-enter-selection-mode :extend) :mode-name 'normal-mode)
  (bind-keys "V" (make-enter-selection-mode :line) :mode-name 'normal-mode)
  (bind-keys "d" #'delete-selection :mode-name 'normal-mode)
  (bind-keys "<esc>" #'enter-normal-mode :mode-name 'insert-mode)
  (bind-keys "<esc>" #'process-escape-key :mode-name 'normal-mode))

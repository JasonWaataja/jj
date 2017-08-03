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

;; TODO: Figure out if it would make more sense to pass a single character
;; instead of a whole string, as that may make the intent more clear.
(defun bind-movement-key (key-string movement-function &key (mode-name (current-mode-name)))
  "Makes two new bindings. Assumes KEY is a single letter that is different
lowercase and uppercase. The lowercase version is bound to calling the movement
function with the :MOVE selection mode. The uppercase version is bound to the
same thing with the :EXTEND selection mode."
  (bind-keys (string-downcase key-string)
             (make-with-selection-mode :move movement-function)
             :mode-name mode-name)
  (bind-keys (string-upcase key-string)
             (make-with-selection-mode :extend movement-function)
             :mode-name mode-name))

(defun enable-default-bindings ()
  (bind-keys "i" #'enter-insert-mode :mode-name 'normal-mode)
  (bind-keys '("<cr>" ":") #'begin-command :mode-name 'normal-mode)
  (bind-movement-key "j" #'move-cursor-down :mode-name 'normal-mode)
  (bind-movement-key "k" #'move-cursor-up :mode-name 'normal-mode)
  (bind-movement-key "l" #'move-cursor-forward :mode-name 'normal-mode)
  (bind-movement-key "h" #'move-cursor-backward :mode-name 'normal-mode)
  (bind-movement-key "t" #'move-cursor-line-begin :mode-name 'normal-mode)
  (bind-movement-key "g" #'move-cursor-line-end :mode-name 'normal-mode)
  (bind-keys "d" #'delete-selection :mode-name 'normal-mode)
  (bind-keys "c" #'delete-selection :mode-name 'normal-mode)
  (bind-keys "<esc>" #'enter-normal-mode :mode-name 'insert-mode)
  (bind-keys "<esc>" #'process-escape-key :mode-name 'normal-mode))

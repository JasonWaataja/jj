;;;; commands.lisp
;;;; Read and execute commands from the user

(in-package #:jj)

(defclass command ()
  ((name :accessor command-name
         :initarg :name
         :initform ""
         :type string
         :documentation "The full name of the command.")
   (alt-names :accessor command-alt-names
                :initarg :alt-names
                :initform (make-container 'vector-container :test #'equal)
                :type vector-container
                :documentation "The alternative names the command may be called
                with.")
   (action :accessor command-action
           :initarg :action
           :initform (lambda (&rest args) args)
           :type function
           :documentation "The function to be called with the arguments as the
           remaining arguments")))

(defparameter *commands* (make-container 'vector-container)
  "The list of commands available to the program.")

(defun add-command (name action &optional alt-names)
  "Add a new command to *COMMANDS* with NAME and ACTION. ALT-NAMES should be a
`vector-container' such that it the variable could be set to it directly."
  (let ((command (if alt-names
                     (make-instance 'command
                                    :name name
                                    :action action
                                    :alt-names alt-names)
                     (make-instance 'command
                                    :name name
                                    :action action))))
    (container-append *commands* command)))

(defmacro define-command ((argv-var name &rest alt-names) &body body)
  "Defines a `command' with NAME and ALT-NAMES. The action of the command calls
body with ARGV-VAR bound to the list of arguments passed to the command."
  (alexandria:once-only (name)
    (alexandria:with-gensyms (names as-vector alt-name)
      `(let ((,names (loop with ,as-vector = (make-container 'vector-container
                                                             :test #'equal)
                        for ,alt-name in ',alt-names
                        do
                          (container-append ,as-vector ,alt-name)
                        finally (return ,as-vector))))
         (add-command ,name
                      (lambda (,argv-var)
                        (declare (ignorable ,argv-var))
                        ,@body)
                      ,names)))))

(defun command-matches-p (command name)
  "Tests if NAME is the NAME of COMMAND or a member of its alternative names."
  (or (string= name (command-name command))
      (as-bool (search-for-item (command-alt-names command) name))))

(define-condition no-command-found-error (jj-error)
  ((command :reader no-command-found-error-command :initarg :command)))

(defun signal-no-command-found-error (command-string)
  (error 'no-command-found-error
         :text (format nil "No command found in \"~a\"" command-string)
         :command command-string))

(defun split-command (command-string)
  "Splits COMMAND-STRING by whitespace, doesn't handle quotes, escaping,
etc. yet. The first return value is the command. The second return value is the
list of arguments. May signal a `no-command-found-error' if it could not be
split."
  ;; Split by instances of one or more whitespace characters.
  (let ((parts (cl-ppcre:split "\\s+" command-string)))
    (unless parts
      (signal-no-command-found-error command-string))
    (values (first parts) (rest parts))))

(define-condition no-such-command-error (jj-error)
  ((name :reader no-such-command-error-name
         :initarg :name)))

;; TODO Make a superclass of command syntax errors.
(defun signal-no-such-command-error (name)
  (error 'no-such-command-error
         :text (format nil "No such command: \"~a\"" name)
         :name name))

(defun find-command (name)
  "Finds a command that matches NAME with either its NAME or one of its
alternative names."
  (let ((command (search-for-match *commands*
                    (lambda (command)
                      (command-matches-p command name)))))
    (unless command
      (signal-no-such-command-error name))
    command))

(defun process-command (command-string)
  "Process the `string' command."
  (multiple-value-bind (name args)
      (handler-case (split-command command-string)
        (no-command-found-error () nil))
    (when name
      (let ((command (handler-case (find-command name)
                       (no-such-command-error () nil))))
        (if command
            (funcall (command-action command) args)
            ;; TODO: Make this a real error output.
            (format *error-output* "No such command for: \"~a\"~%" command-string))))))

(defun clear-commands ()
  "Gets rid of all commands."
  (empty! *commands*))

(defun add-default-commands ()
  "Adds the base commands to *COMMANDS*."
  (define-command (argv "quit" "q")
    (exit-clean)))

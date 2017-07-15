;;;; commands.lisp
;;;; Read and execute commands from the user

(in-package #:jj)

(defmethod process-command (command)
  "Process the `string' command."
  ;; TODO: Actually process the command.
  (format t "~a~%" command))

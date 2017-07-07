;;;; mode.lisp
;;;; Program modes such as insert and normal.

(in-package #:jj)

(defclass key-stroke-buffer ()
  ((chords :accessor key-stroke-buffer-chords
           :initarg :chords
           :initform (make-container 'vector-container)
           :type vector-container
           :documentation "The list of chords that were pressed, the most recent
           is last in the list.")
   (last-chord-time :accessor key-stroke-buffer-last-chord-time
                    :initarg :last-chord-time
                    :initform (get-time)
                    :type 'float
                    :documentation "The time the last chord was pressed.")
   (remaining-bindings :accessor key-stroke-buffer-remaining-bindings
                       :initarg :remaining-bindings
                       :initform (make-container 'vector-container)
                       :type 'vector-container
                       :documentation "The bindings to check on next key press.")))

(defun make-key-stroke-buffer ()
  (make-instance 'key-stroke-buffer))

(defun key-stroke-buffer-empty-p (buffer)
  (empty-p (key-stroke-buffer-chords buffer)))

(defun clear-key-stroke-buffer (buffer)
  "Removes all keys from BUFFER and resets time."
  (empty! (key-stroke-buffer-chords buffer))
  (setf (key-stroke-buffer-last-chord-time buffer) 0)
  (empty! (key-stroke-buffer-remaining-bindings buffer)))

(defun key-stroke-buffer-size (buffer)
  (cl-containers:size (key-stroke-buffer-chords buffer)))

(defun append-key-to-buffer (buffer chord)
  "Adds CHORD to the end and updates LAST-CHORD-TIME."
  (container-append (key-stroke-buffer-chords buffer) chord)
  (setf (key-stroke-buffer-last-chord-time buffer)
        (get-current-loop-time)))

(defparameter *key-stroke-buffer* (make-key-stroke-buffer)
  "The storage for the list of previously pressed keys.")

(defclass key-binding ()
  ((activation-sequence :accessor key-binding-activation-sequence
                        :initarg :activation-sequence
                        :initform nil
                        :type key-sequence
                        :documentation "The key sequence that activates the
                     binding.")
   (follow-sequences :accessor key-binding-follow-sequences
                     :initarg :follow-sequences
                     :documentation "Whether or not to follow new sequences that
                     arise from remapped keys.")
   (action :accessor key-binding-action
           :initarg :action
           :documentation "The thing to do when the binding is activated. It can
           be a `key-sequence', in which case the new keys are added to the
           queue. It can be a function, in which case the function is
           called.")))

(defun make-key-binding (activation-sequence &key (follow-sequences nil) action)
  "If activation-sequence is a `string', parse it into a key sequence. Otherwise
it is assumed that ACTIVATION-SEQUENCE is a `key-sequence'."
  (let ((key-sequence (if (stringp activation-sequence)
                          (parse-key-sequence activation-sequence)
                          activation-sequence))
        (new-action (if (stringp action)
                        (parse-key-sequence action)
                        action)))
    (make-instance 'key-binding
                   :activation-sequence key-sequence
                   :follow-sequences follow-sequences
                   :action new-action)))

(defclass mode ()
  ((key-bindings :accessor mode-key-bindings
                 :initarg :key-bindings
                 :initform (make-container 'vector-container)
                 :type vector-container
                 :documentation "The list of key bindings. Combinations at the
                 start have priority.")))

(defun add-mode-binding (mode binding)
  "Adds BINDING to the front of the bindings for MODE."
  (insert-item-at (mode-key-bindings mode) binding 0))

(defun create-mode-binding (mode activation-sequence &key (follow-sequences nil) action)
  "Combines ADD-MODE-BINDING and MAKE-KEY-BINDING."
  (add-mode-binding mode
                    (make-key-binding activation-sequence
                                      :follow-sequences follow-sequences
                                      :action action)))

(defgeneric process-event (mode event)
  (:documentation "Process EVENT based on MODE."))

(defgeneric process-key (mode chord)
  (:documentation "Take a key, which can have modifiers, hence using `chord',
  and process it based on MODE."))

(defun chord-matches-binding-p (binding chord index)
  "Checks if the character at INDEX matches the corresponding key in
KEY-SEQUENCE. Makes sure to account for being of out of bounds. Also returns if
it matched the last character of the binding. This is so that the caller can use
that binding and not keep checking others."
  (let* ((seq (key-sequence-keys (key-binding-activation-sequence binding)))
         (length (cl-containers:size seq))
         (matches (and (< index length)
                       (chord= chord
                               (item-at seq index)))))
    (values matches (= index (1- length)))))

(defun key-stroke-buffer-matches-binding-p (buffer binding)
  "Checks if BINDING is matched by the sequence in BUFFER. This function doesn't
check for following sequences, the caller should do that."
  (loop with buffer-size = (cl-containers:size (key-stroke-buffer-chords buffer))
     with sequence-size = (cl-containers:size (key-binding-activation-sequence binding))
     initially
       (unless (= buffer-size sequence-size)
         (return nil))
     for i below buffer-size
     if (not (chord= (aref (key-sequence-keys (key-binding-activation-sequence binding)) i)
                     (item-at (key-stroke-buffer-chords buffer) i)))
     return nil
     finally (return t)))

(defclass insert-mode (mode) ())

(defclass normal-mode (mode) ())

(defun fill-remaining-bindings (chord)
  "Called from PROCESS-INPUT to start filling the remaining bindings based on
the current mode."
  (do-container (binding (mode-key-bindings *current-mode*))
    (when (chord-matches-binding-p binding
                                   chord
                                   0)
      (insert-item (key-stroke-buffer-remaining-bindings
                    *key-stroke-buffer*)
                   binding))))

(defun filter-remaining-bindings (chord)
  (delete-item-if (key-stroke-buffer-remaining-bindings *key-stroke-buffer*)
                  (lambda (binding)
                    (not (chord-matches-binding-p binding
                                                  chord
                                                  (key-stroke-buffer-size
                                                   *key-stroke-buffer*))))))

(defun complete-binding-p (binding)
  "Returns if binding both matches *KEY-STROKE-BUFFER* and has the same number
of keys."
  (let* ((seq (key-sequence-keys (key-binding-activation-sequence binding)))
         (length (cl-containers:size seq)))
    (and (= length (key-stroke-buffer-size *key-stroke-buffer*))
         (loop for i below length
            unless (chord= (item-at seq i)
                           (item-at (key-stroke-buffer-chords *key-stroke-buffer*) i))
            return nil
            finally (return t)))))

(defun on-no-matches ()
  "Called when a new key would not match any key bindings. Dumps keys currently
in the buffer and processes the new one."
  (do-container (chord (key-stroke-buffer-chords *key-stroke-buffer*))
    (process-key *current-mode* chord))
  (clear-key-stroke-buffer *key-stroke-buffer*))

(defun execute-matching-bindings ()
  "Called when a new key still has potential candidates. Checks each to see if
would activate a binding. If it does, execute that binding and clear the
buffer. Otherwise, simply append it to the buffer. Returns NIL if no remappings
were activated and a list of new keys to process of there were."
  (let ((binding (some-element-p (key-stroke-buffer-remaining-bindings
                                  *key-stroke-buffer*)
                                 #'complete-binding-p))
        (remapped-keys nil))
    (when binding
      (if (functionp (key-binding-action binding))
          (funcall (key-binding-action binding))
          (do-container (chord (key-sequence-keys (key-binding-action binding)))
            (if (key-binding-follow-sequences binding)
                (push chord remapped-keys)
                (process-key *current-mode* chord))))
      (clear-key-stroke-buffer *key-stroke-buffer*))
    (nreverse remapped-keys)))

(defun process-input (chord)
  "Meant to be the main function called to process input, meant to be called
every instance of the main loop. Process chord based on bindings and the current
mode."
  ;; Flush *KEY-STROKE-BUFFER* when a key hasn't been pressed in a while.
  (when (and (not (key-stroke-buffer-empty-p *key-stroke-buffer*))
             (> (- (get-current-loop-time)
                   (key-stroke-buffer-last-chord-time *key-stroke-buffer*))
                (get-setting 'key-sequence-time)))
    (do-container (chord (key-stroke-buffer-chords *key-stroke-buffer*))
      (process-key *current-mode* chord))
    (clear-key-stroke-buffer *key-stroke-buffer*))
  ;; Returns a list of keys that would be processed due to remapping, NIL if no
  ;; remappings were activated.
  (labels ((process-chord (chord)
             (if (key-stroke-buffer-empty-p *key-stroke-buffer*)
                 (fill-remaining-bindings chord)
                 (filter-remaining-bindings chord))
             (append-key-to-buffer *key-stroke-buffer* chord)
             (cond ((empty-p (key-stroke-buffer-remaining-bindings *key-stroke-buffer*))
                    (on-no-matches)
                    nil)
                   (t (execute-matching-bindings)))))
    (loop for chords = (list chord) then (append (process-chord (first chords))
                                                 (rest chords))
       while chords)))

(define-condition override-binding-error (jj-error)
  ((old-binding :reader override-binding-error-old-binding
                :initarg :old-binding)
   (new-binding :reader override-binding-error-new-binding
                :initarg :new-binding)))

(defun signal-override-binding-error (old-binding new-binding)
  "Signals a `override-binding-error'"
  (error 'override-binding-error
         :old-binding old-binding
         :new-binding new-binding
         ;; TODO: Add nicer printing for the whole program in general.
         :text (format nil "Overriding ~a with ~a" old-binding new-binding)))

(define-restart use-new-binding)

(define-restart keep-old-binding)

;;; These functions are probably what the use is going to use inf configuration for modes.
(defun bind-keys (activation-sequence action &key
                                               (follow-sequences nil)
                                               (mode *current-mode*)
                                               (if-rebind :error))
  "Like CREATE-MODE-BINDING, but also checks if it matches any bindings. The
possible values for IF-REBIND are :ERROR and NIL, which control what happens
when the binding would overwrite an existing one. If the value is :ERROR, then
it may signal an `override-binding-error`. Returns NIL if NIL is passed."
  (let* ((binding (make-key-binding activation-sequence
                                    :follow-sequences follow-sequences
                                    :action action))
         (input-seq (key-binding-activation-sequence binding)))
    (do-container (existing-binding (mode-key-bindings mode))
      (when (key-sequence= (key-binding-activation-sequence existing-binding)
                           input-seq)
        (if (eql if-rebind :error)
            (restart-case (signal-override-binding-error existing-binding
                                                         binding)
              (use-new-binding ())
              (keep-old-binding () (return-from bind-keys)))
            (return-from bind-keys))))
    (add-mode-binding mode binding)))

(defparameter *normal-mode* (make-instance 'normal-mode))

(defparameter *insert-mode* (make-instance 'insert-mode))

(defparameter *current-mode* *normal-mode*
  "The current mode that the editor is in.")

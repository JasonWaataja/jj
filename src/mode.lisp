;;;; mode.lisp
;;;; Program modes such as insert and normal.

(in-package #:jj)

(defclass key-stroke-buffer ()
  ((chords :accessor key-stroke-buffer-chords
           :initarg :chords
           :initform (make-container 'dlist-container)
           :type dlist-container
           :documentation "The list of chords that were pressed, the most recent
           is last in the list.")
   (last-chord-time :accessor key-stroke-buffer-last-chord-time
                    :initarg :last-chord-time
                    :initform (get-time)
                    :type 'float
                    :documentation "The time the last chord was pressed.")
   (remaining-bindings :accessor key-stroke-buffer-remaining-bindings
                       :initarg :remaining-bindings
                       :initform (make-container 'dlist-container)
                       :type 'dlist-container
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

(defun update-key-stroke-buffer (buffer &optional chord)
  "Mean to be called on BUFFER on every instance of the main loop. Updates the
state of BUFFER."
  (when (> (- (get-current-loop-time) (key-stroke-buffer-last-chord-time buffer))
           (get-setting 'key-sequence-time))
    (clear-key-stroke-buffer buffer))
  (when chord
    (append-item (key-stroke-buffer-chords buffer) chord)))

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
           :initform ""
           :documentation "The thing to do when the binding is activated. It can
           be a `string', in which case the new keys are added to the queue. It
           can be a function, in which case the function is called.")))

(defclass mode ()
  ((key-bindings :accessor mode-key-bindings
                 :initarg :key-bindings
                 :initform (make-container 'dlist-container)
                 :type dlist-container
                 :documentation "The list of key bindings. Combinations at the
                 start have priority.")))

(defgeneric process-event (mode event)
  (:documentation "Process EVENT based on MODE."))

(defgeneric process-key (mode chord)
  (:documentation "Take a key, which can have modifiers, hence using `chord',
  and process it based on MODE."))

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

(defparameter *current-mode* (make-instance 'normal-mode)
  "The current mode that the editor is in.")

;; (defun process-input (chord)
;;   "Meant to be the main function called to process input, meant to be called
;; every instance of the main loop. Process chord based on bindings and the current
;; mode."
;;   (

;; (defmethod process-mode ((mode insert-mode) (event key-event)))

;; (defmethod process-mode ((mode normal-mode) (event key-event)))

;; (defun process-key-bindings (key-buffer)
;;   "Checks if the keys in KEY-BUFFER match each binding. Executes the correct
;; action and clears it if it does."
;;   (flet ((process-binding (binding)
;;            (when (key-stroke-buffer-matches-binding-p *key-stroke-buffer* binding)
;;              (if (key-binding-follow-sequences binding)
;;                  (

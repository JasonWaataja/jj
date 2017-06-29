;;;; event.lisp
;;;; Key events, etc.

(in-package #:jj)

(defclass event () ())

(defgeneric describe-event (event)
  (:documentation "Return a string describing EVENT."))

(defclass key-event (event)
  ((character-code :reader key-event-character-code
                   :initarg :character-code
                   :documentation "The code describing the character received.")))

(defun make-key-event (character-code)
  (make-instance 'key-event :charcter-code character-code))

(defclass chord ()
  ((character-code :reader chord-character-code
                   :initarg :character-code
                   :documentation "The primary character for the chord.")
   (modifiers :reader chord-modifiers
              :initarg :modifiers
              :initform (make-container 'set-container)
              :documentation "The list of modifier keys for the chord, can
              include :CONTROL or :MOD.")))

(defun make-chord (character-code &rest modifiers)
  (let ((modifier-container (make-container 'set-container)))
    (dolist (modifier modifiers)
      (insert-item modifier-container modifier))
    (make-instance 'chord
                   :character-code character-code
                   :modifiers modifier-container)))

(defclass key-sequence ()
  ((keys :reader key-sequence-keys
         :initarg :keys
         :initform (make-array 0
                               :element-type 'chord
                               :adjustable t
                               :fill-pointer 0)
         :type 'chord
         :documentation "The sequence of chords for the sequence.")))

(defun make-key-sequence (&rest key-chords)
  (let ((keys (make-array 0
                          :element-type 'chord
                          :adjustable t
                          :fill-pointer 0)))
    (dolist (chord key-chords)
      (vector-push-extend chord keys))
    (make-instance 'key-sequence
                   :keys keys)))

(defun string-has-prefix-p (string prefix)
  "Returns if STRING begins with PREFIX, case sensitive."
  (if (< (length string) (length prefix))
      nil
      (string= string prefix :end1 (length prefix))))

(defun string-has-prefix-insensitive-p (string prefix)
  "Returns if STRING begins with PREFIX, case insensitive."
  (if (< (length string) (length prefix))
      nil
      (string-equal string prefix :end1 (length prefix))))

(defun match-regular-character (input-string)
  "If the beginning of INPUT-STRING can be interpreted as an input character,
then return as the first value the remainder of INPUT-STRING, then the string of
the character that matched. Returns NIL if nothing matches."
  ;; TODO: Add more of these later, the <space> here is just an example.
  (cond ((string-has-prefix-insensitive-p input-string "<space>")
         (values (subseq input-string (length "<space>")) "<space>"))
        ;; TODO: Add a check to make sure it's a real character.
        (t (if (plusp (length input-string))
               (values (subseq input-string 1) (subseq input-string 0 1))))))

(defun match-regex (input-string regex)
  (multiple-value-bind (begin end reg-starts reg-ends)
      (cl-ppcre:scan regex input-string)
    (if (and begin (eql begin 0))
        (let ((remaining-string (subseq input-string end))
              (matches (loop for reg-start across reg-starts
                          for reg-end across reg-ends
                          collect (subseq input-string reg-start reg-end))))
          (push remaining-string matches)
          (values-list matches))
        nil)))

(defun combine-matches (input-string &rest matchers)
  "For each element of MATCHERS, if it is a `string', match it as a regex, if it
is a function, treat it as a matching function, and if it is a list, treat the
first element as a function and the rest as the arguments to pass to the
function other than the input string, i.e. not the first argument. If if passes
each match in sequence, being trimmed down each time, then return the rest of
the string, otherwise return NIL. Also returns the individual match groups in
the order that they appeared in MATCHERS."
  (loop with remaining-string = input-string
     with match-groups = nil
     for matcher in matchers
     for match-func = (cond ((stringp matcher)
                             (lambda (input-string)
                               (match-regex input-string matcher)))
                            ((listp matcher)
                             (lambda (input-string)
                               (apply (first matcher)
                                      (append (list input-string)
                                              (rest matcher)))))
                            (t matcher))
     do
       (let ((match-info (multiple-value-list (funcall match-func
                                                       remaining-string))))
         (cond ((first match-info)
                (setf remaining-string (first match-info))
                (dolist (match-group (rest match-info))
                  (push match-group match-groups)))
               (t (return nil))))
     finally
       (return (values-list (append (list remaining-string)
                                    (nreverse match-groups))))))

(defun parse-key-sequence (sequence-string)
  "Parses SEQUENCE-STRING into a `key-sequence'. These are of the form \"jk\",
  \"<c-r>t\", etc."
  (labels ((parse-element (sequence-string)
             (multiple-value-bind (begin end reg-starts reg-ends)
                 (cl-ppcre:scan "<[cC]-.>" sequence-string)
               (if begin
                   (

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

(defun make-special-characters ()
  (let ((special-characters (make-hash-table :test #'equalp)))
    (setf (gethash "<space>" special-characters) #\Space)
    special-characters))

(defparameter *special-characters* (make-special-characters)
  "Map of input character sequences as you might see in a config file such as
  \"a\" or \"<space>\" to the characters they represent.")

(defun character-expansion-to-character (character-expansion)
  "Converts character expansion to the character it represents, such as
  \"<space>\" to the space character. Returns NIL if there is no such
  character."
  (loop for test-expansion being the hash-keys in *special-characters*
     using (hash-value representation)
     if (string-equal test-expansion character-expansion)
     return representation
     finally
       (return (if (eql (length character-expansion) 1)
                   (aref character-expansion 0)
                   nil))))

(defun match-input-character (input-string)
  "If the beginning of INPUT-STRING can be interpreted as an input character,
then return as the first value the remainder of INPUT-STRING, then the string of
the character that matched. Returns NIL if nothing matches."
  (loop for character-expansion being the hash-keys in *special-characters*
     if (string-has-prefix-insensitive-p input-string character-expansion)
     return (values (subseq input-string (length character-expansion))
                    character-expansion)
     finally
       (return (if (plusp (length input-string))
                   (values (subseq input-string 1) (subseq input-string 0 1))
                   nil))))

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

(defun match-whitespace (input-string)
  (match-regex input-string "\\s+"))

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

(defun match-control-modified-character (input-string)
  "Matches strings of the form <C-A>, <c-b>, etc. Captures the second character."
  (combine-matches input-string
                   "<[cC]-"
                   #'match-input-character
                   ">"))

(defun match-mod-modified-character (input-string)
  "Matches strings of the form <M-a>, <m-b>, etc. Captures the second character."
  (combine-matches input-string
                   "<[mM]-"
                   #'match-input-character
                   ">"))

(defun parse-chord (chord-string)
  "Reads a chord from the beginning of CHORD-STRING and returns the parsed
`chord' and remaining string if one was found, NIL otherwise."
  (let ((control-chord-info
         (multiple-value-list (match-control-modified-character
                               chord-string))))
    (when (first control-chord-info)
      (return-from parse-chord
        (values (make-chord (character-expansion-to-character
                             (second control-chord-info)))
                (first control-chord-info)))))
  (let ((mod-chord-info
         (multiple-value-list (match-mod-modified-character
                               chord-string))))
    (when (first mod-chord-info)
      (return-from parse-chord
        (values (make-chord (character-expansion-to-character
                             (second mod-chord-info)))
                (first mod-chord-info)))))
  (let ((input-character-info
         (multiple-value-list (match-input-character
                               chord-string))))
    (when (first input-character-info)
      (return-from parse-chord
        (values (make-chord (second input-character-info))
                (first input-character-info)))))
  nil)

(defun parse-key-sequence (sequence-string)
  "Parses SEQUENCE-STRING into a `key-sequence'. These are of the form \"jk\",
  \"<c-r>t\", etc. Returns nil if it could not be matched"
  (loop with current-string = sequence-string
     for match-info = (multiple-value-list (parse-chord current-string))
     while (first match-info)
     collect (first match-info) into chords
     do
       (setf current-string (second match-info))
     finally
       (return (if chords
                   (apply #'make-key-sequence chords)
                   nil))))

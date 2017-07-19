;;;; jj.lisp
;;;; General tests for program.

(in-package #:jj-test)

(in-suite jj-test)

(test settings-test
  "Test the settings and their layers."
  (define-setting dummy-setting 0)
  (define-setting dummy-string-setting "Hello World")
  (is (not (null (find-setting 'dummy-setting))))
  (is (not (null (find-setting 'dummy-string-setting))))
  (is (null (cl-containers:find-item *settings* 'no-such-setting)))
  (let ((test-layer (make-settings-layer)))
    (set-setting 'test-setting 5 test-layer)
    (is (= (get-setting 'test-setting test-layer) 5))
    (is (= (get-setting 'test-setting (list test-layer *global-settings*)) 5))))

(test tab-width-test
  "Test if the setting TAB-WIDTH is set correctly and columns are calculated correctly."
  (is (= (get-setting 'jj:tab-width) 8))
  (is (= (calculate-tab-width 0) 8))
  (is (= (calculate-tab-width 4) 4))
  (is (= (calculate-tab-width 7) 1))
  (is (= (calculate-tab-width 8) 8))
  (is (= (calculate-tab-width 12) 4))
  (is (= (calculate-tab-width 16) 8)))

(test leading-string-width-test
  "Test the function LEADING-STRING-WIDTH."
  (is (= (leading-string-width "a") 1))
  (is (= (leading-string-width "	") 8))
  (is (= (leading-string-width "a	") 8))
  (is (= (leading-string-width "ab	cd") 10))
  (is (= (leading-string-width "	ab") 10)))

(test string-has-prefix-p-test
  (is (string-has-prefix-p "ab" "a"))
  (is (not (string-has-prefix-p "ba" "a")))
  (is (string-has-prefix-p "abc" "ab")))

(test string-has-prefix-insensitive-p-test
  (is (string-has-prefix-insensitive-p "ab" "a"))
  (is (not (string-has-prefix-insensitive-p "ab" "b")))
  (is (string-has-prefix-insensitive-p "AbC" "aB")))

(test match-regular-character-test
  (is (string= (match-input-character "abc") "bc"))
  (is (string= (match-input-character "") nil))
  (is (string= (match-input-character "<space>ab") "ab")))

(test match-regex-test
  (is (string= (match-regex "a" "a") ""))
  (is (string= (match-regex "abc" "d") nil))
  (is (string= (match-regex "abc" "(a)b") "c")))

(test combine-matches-test
  (is (string= (combine-matches "a" "a") ""))
  (is (string= (combine-matches "abc" "a") "bc"))
  (is (string= (combine-matches "abc<space>de" "abc" #'match-input-character) "de"))
  (is (null (combine-matches "abc" "b"))))

(test keys-test
  (let ((chord1 (make-chord #\a))
        (chord2 (make-chord #\a :control))
        (chord3 (make-chord #\b))
        (chord4 (make-chord #\c))
        (chord5 (make-chord #\a))
        (chord6 (make-chord #\b)))
    (is (chord= chord1 chord5))
    (is (not (chord= chord1 chord2)))
    (is (not (chord= chord1 chord3)))
    (let ((seq1 (make-key-sequence chord1 chord3))
          (seq2 (make-key-sequence chord5 chord6))
          (seq3 (make-key-sequence chord5 chord6 chord4)))
      (is (key-sequence= seq1 seq2))
      (is (not (key-sequence= seq1 seq3))))))

(test container-append-test
  (let ((test-vec (cl-containers:make-container 'cl-containers:vector-container)))
    (container-append test-vec 1)
    (container-append test-vec 3)
    (is (= (cl-containers:item-at test-vec 0) 1))
    (is (= (cl-containers:item-at test-vec 1) 3))))

(test do-container-test
  (let ((test-vec (cl-containers:make-container 'cl-containers:vector-container)))
    (container-append test-vec 1)
    (container-append test-vec 3)
    (let ((items nil))
      (is (eql (do-container (item test-vec)
                 (push item items))
               nil))
      (is (= (length items) 2))
      (is (= (first items) 3))
      (is (= (second items) 1)))
    (is (= (let ((sum 0))
             (do-container (item test-vec sum)
               (incf sum item)))
           4))))

(test parse-key-sequence-test
  ;; TODO: Check what happens on an empty string, I forget what it should do.
  (let ((seq1 (parse-key-sequence "a"))
        (seq2 (parse-key-sequence "ab"))
        (seq3 (parse-key-sequence "<space>a"))
        (seq4 (parse-key-sequence "a<space>")))
    (is (= (cl-containers:size (key-sequence-keys seq1)) 1))
    (is (chord= (cl-containers:item-at (key-sequence-keys seq1) 0)
                (make-chord #\a)))
    (is (= (cl-containers:size (key-sequence-keys seq2)) 2))
    (is (chord= (cl-containers:item-at (key-sequence-keys seq2) 0)
                (make-chord #\a)))
    (is (chord= (cl-containers:item-at (key-sequence-keys seq2) 1)
                (make-chord #\b)))
    (is (= (cl-containers:size (key-sequence-keys seq3)) 2))
    (is (chord= (cl-containers:item-at (key-sequence-keys seq3) 0)
                (make-chord #\Space)))
    (is (chord= (cl-containers:item-at (key-sequence-keys seq3) 1)
                (make-chord #\a)))
    (is (= (cl-containers:size (key-sequence-keys seq4)) 2))
    (is (chord= (cl-containers:item-at (key-sequence-keys seq4) 0)
                (make-chord #\a)))
    (is (chord= (cl-containers:item-at (key-sequence-keys seq4) 1)
                (make-chord #\Space)))))

(test process-input-test
  (let ((test-var 0))
    (flet ((make-test-var-setter (val)
             (lambda () (setf test-var val))))
      (create-mode-binding *current-mode* "a"
                           :action "b"
                           :follow-sequences t)
      (create-mode-binding *current-mode* "b"
                           :action (make-test-var-setter 1))
      ;; Dummy that shouldn't be activated.
      (create-mode-binding *current-mode* "ab"
                           :action (make-test-var-setter 2))
      (create-mode-binding *current-mode* "cd"
                           :action (make-test-var-setter 3))
      (clear-key-stroke-buffer *key-stroke-buffer*)
      (process-input (make-chord #\c))
      (is (= test-var 0))
      (clear-key-stroke-buffer *key-stroke-buffer*)
      (process-input (make-chord #\a))
      (is (= test-var 1))
      (clear-key-stroke-buffer *key-stroke-buffer*)
      (process-input (make-chord #\c))
      (process-input (make-chord #\d))
      (is (= test-var 3)))))

(defmacro test-condition ((condition &rest other-conditions) &body body)
  "Tests if a condition of type CONDITION or OTHER-CONDITIONS is
signalled. Returns the condition if it was, NIL otherwise."
  (let ((condition-name (gensym)))
    `(handler-case
         (progn
           ,@body
           nil)
       (,condition (,condition-name) ,condition-name)
       ,@(loop for condition in other-conditions
            collect `(,condition (,condition-name) ,condition-name)))))

(test make-text-position-test
  (let ((buffer (make-buffer)))
    (buffer-append buffer "abc")
    (buffer-append buffer "cd")
    (let ((position-1 (make-text-position buffer 0))
          (position-2 (make-text-position buffer 3))
          (position-3 (make-text-position buffer 6))
          (position-4 (make-text-position buffer 4)))
      (is (= (text-position-line-number position-1) 0))
      (is (= (text-position-line-position position-1) 0))
      (is (= (text-position-line-number position-2) 0))
      (is (= (text-position-line-position position-2)))
      (is (= (text-position-line-number position-3) 1))
      (is (= (text-position-line-position position-3) 2))
      (is (= (text-position-line-number position-4) 1))
      (is (= (text-position-line-position position-4) 0)))
    (is (not (null (test-condition (invalid-text-position-error)
                     (make-text-position buffer 7)))))
    (is (not (null (test-condition (invalid-text-position-error)
                     (make-text-position buffer -1)))))))

(define-condition dummy-condition ()
  ((data :reader dummy-condition-data
         :initarg :data)))

(defun signal-dummy-condition (&optional data)
  (if data
      (signal 'dummy-condition :data data)
      (signal 'dummy-condition)))

(test command-test
  "Test the finding and executing of commands."
  (clear-commands)
  (add-default-commands)
  (is (not (null (find-command "quit"))))
  (is (not (null (find-command "q"))))
  (is (not (null (test-condition (no-such-command-error)
                   (find-command "no-such-command")))))
  (define-command (argv "no-such-command" "nsc")
    (signal-dummy-condition))
  (is (not (null (test-condition (dummy-condition)
                   (process-command "no-such-command")))))
  (is (not (null (test-condition (dummy-condition)
                   (process-command "nsc"))))))

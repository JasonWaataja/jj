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
  (is (string= (match-regular-character "abc") "bc"))
  (is (string= (match-regular-character "") nil))
  (is (string= (match-regular-character "<space>ab") "ab")))

(test match-regex-test
  (is (string= (match-regex "a" "a") ""))
  (is (string= (match-regex "abc" "d") nil))
  (is (string= (match-regex "abc" "(a)b") "c")))

(test combine-matches-test
  (is (string= (combine-matches "a" "a") ""))
  (is (string= (combine-matches "abc" "a") "bc"))
  (is (string= (combine-matches "abc<space>de" "abc" #'match-regular-character) "de"))
  (is (null (combine-matches "abc" "b"))))

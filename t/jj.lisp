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
  "Test if the setting TAB-WIDTH is set correctly."
  (is (= (get-setting 'jj:tab-width) 8)))

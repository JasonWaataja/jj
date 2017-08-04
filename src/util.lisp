;;;; util.lisp
;;;; Various utilities

(in-package #:jj)

(defun regex-matches-p (regex target-string)
  "Returns whether or not TARGET-STRING as a whole matches the regex."
  (multiple-value-bind (begin end)
      (cl-ppcre:scan regex target-string)
    (cond ((not begin) nil)
          ((not (eql begin 0)) nil)
          ((not (eql end (length target-string))) nil)
          (t t))))

(defun set= (set1 set2)
  "Checks if the sets have the same number of elements and if each element in
SET1 is in SET2."
  (and (= (cl-containers:size set1) (cl-containers:size set2))
       (cl-containers:every-item-p set1
                                   (lambda (item)
                                     (find-item set2 item)))))

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

(defun container-append (container item)
  "Operates on a cl-containers container, which doesn't have append for some
  reason."
  (insert-item-at container
                  item
                  (cl-containers:size container)))

(defmacro do-container ((var container &optional result) &body body)
  `(progn (iterate-container ,container
                             (lambda (,var)
                               ,@body))
          ,result))

(defun container-to-list (container)
  "Converts CONTAINER to a new list with the same elements."
  (let ((as-list '()))
    (do-container (element container)
      (push element as-list))
    (nreverse as-list)))

(defun container-insert-list (container list &optional position)
  "Adds each element of LIST to CONTAINER in order starting at position which
defaults to appending to the container. Returns CONTAINER."
  (unless position
    (setf position (cl-containers:size container)))
  (dolist (element list container)
    (insert-item-at container element position)
    (incf position)))

(defun list-to-vector-container (list &key (test #'eql))
  "Returns a new `vector-container' containing the same elements as list."
  (let ((container (make-container 'vector-container :test test)))
    (container-insert-list container list)
    container))

(defun array-insert-at (array value index)
  "Inserts ELEMENT in ARRAY at INDEX destructively modifying array and returning
the result. Usually, SEFT ARRAY to the return value of this function."
  (vector-push-extend value array)
  (let ((new-array (replace array array
                            :start1 (1+ index)
                            :start2 index)))
    (setf (aref new-array index) value)
    new-array))

(defun array-delete-at (array index)
  "Deletes element at INDEX in ARRAY destructively and returns it. To do this,
one part is to move the fill pointer. I think this means that the last element
will still be in the array, and not garbage collected. This might be a bug or
something."
  ;; TODO: Figure out a better way to do this that doesn't just involve the
  ;; fill-pointer.
  (replace array array
           :start1 index
           :end1 (1- (length array))
           :start2 (1+ index))
  (setf (fill-pointer array) (1- (length array)))
  array)

(defmacro define-restart (restart-name)
  "Defines a function with name RESTART-NAME that invokes the restart given by RESTART-NAME."
  (alexandria:with-gensyms (condition)
    `(defun ,restart-name (,condition)
       (declare (ignore ,condition))
       (invoke-restart ',restart-name))))

(defun as-bool (val)
  "Returns NIL if VAL is NIL, T otherwise."
  (if val t))

(defun concatenate-separated (result-type separation-sequence &rest sequences)
  "Like CONCATENATE, but puts SEPARATION-SEQUENCE between each sequence in
SEQUENECES."
  (let ((new-sequences '()))
    (when sequences
      (push (first sequences) new-sequences)
      (dolist (sequence (rest sequences))
        (push separation-sequence new-sequences)
        (push sequence new-sequences)))
    (apply #'concatenate
           (append (list result-type)
                   (nreverse new-sequences)))))

(defun concatenate-string-list (strings &optional (separation-sequence " "))
  "Combines each `string' in STRINGS with SEPARATION-SEQUENCE."
  (apply #'concatenate-separated
         (append (list 'string separation-sequence)
                 strings)))

(defun traverse-tree (tree function)
  "Goes through TREE in-order running FUNCTION on each non-nil leaf."
  (when tree
    (flet ((process-leaf (leaf)
             (if (listp leaf)
                 (traverse-tree leaf function)
                 (funcall function leaf))))
      (process-leaf (car tree))
      (process-leaf (cdr tree)))))

(defun map-tree (tree function)
  "Returns a new tree with FUNCTION run on each non-nil leaf of the tree."
  (when tree
    (flet ((leaf-value (leaf)
             (if (listp leaf)
                 (map-tree leaf function)
                 (funcall function leaf))))
      (cons (leaf-value (car tree))
            (leaf-value (cdr tree))))))

;;;; buffer.lisp
;;;; Text data

(in-package #:jj)

(defclass buffer ()
  ((lines :accessor buffer-lines
          :initarg :lines
          :initform (make-array 0
                                :adjustable t
                                :fill-pointer 0
                                :element-type 'string
                                :initial-element ""))
   (marks :accessor buffer-marks
          :initarg :marks
          :initform (make-array 0
                                :adjustable t
                                :fill-pointer 0)
          :documentation "The list of marks for this buffer so that it knows
          how to update them.")
   (cursor-mark :accessor buffer-cursor-mark
                :initarg :cursor-mark
                :documentation "The position of the cursor in this buffer.")
   (frame :accessor buffer-frame
          :initarg frame
          :documentation "The frame that this buffer is currently assigned
          to.")
   (file :accessor buffer-file
         :initarg :file
         :type pathname
         :documentation "The associated file of the buffer. The file it would
         write to. Can be NIL because not all buffers are associated with a
         file. This implementation might change in the future, so instead of
         using the accessor function use BUFFER-FILE-PATHNAME and
         BUFFER-FILE-NAMESTRING."))
  (:documentation "Represents a set of text to manipulate in the form of a list
  of lines. This is what the user interacts with, and there is usually one per
  file. They can be used to simply display text as well, though."))

(define-condition buffer-line-index-error (jj-error)
  ((line :initarg :line :reader buffer-line-index-error-line)
   (buffer :initarg :buffer :reader buffer-line-index-error-buffer)))

(defun buffer-lines-count (buffer)
  (length (buffer-lines buffer)))

(defun buffer-line (buffer line-number)
  "Unchecked access to buffer lines. However, does return an empty string in the
case of no lines and attempting to access the first line."
  (if (and (zerop (length (buffer-lines buffer)))
           (zerop line-number))
      ""
      (aref (buffer-lines buffer) line-number)))

(defun (setf buffer-line) (line buffer line-number)
  "Unchecked access to buffer lines."
  (if (and (zerop (length (buffer-lines buffer)))
           (zerop line-number))
      (vector-push-extend line (buffer-lines buffer))
      (setf (aref (buffer-lines buffer) line-number) line)))

(defun buffer-append (buffer line)
  "Adds LINE to the end of the list of lines in BUFFER."
  (vector-push-extend line (buffer-lines buffer)))

(define-condition no-such-file-error (jj-error)
  ((pathspec :reader pathspec
             :initarg :pathspec
             :documentation "The file that couldn't be opened.")))

(defun signal-no-such-file-error (pathspec)
  (error 'no-such-file-error
         :pathspec pathspec
         :text (format nil "No such file or directory: ~a" pathspec)))

(define-condition unknown-file-error (jj-error)
  ((pathspec :reader pathspec
             :initarg :pathspec
             :documentation "The file that couldn't be opened.")))

(defun signal-unknown-file-error (pathspec)
  (error 'unknown-file-error
         :pathspec pathspec
         :text (format nil "Unknown file: ~a~%" pathspec)))

(defun buffer-file-pathname (buffer)
  "Returns the file of BUFFER as a `pathname'."
  (buffer-file buffer))

(defun buffer-file-namestring (buffer)
  "Returns the file of BUFFER as a `string'."
  (namestring (buffer-file buffer)))

(define-condition non-file-pathname-error (jj-error)
  ((pathspec :reader non-file-pathname-error-pathspec
             :initarg :pathspec
             :documentation "The pathspec in question that was in directory
             form."))
  (:documentation "An error for when a directory pathname or string was used
  when a file was expected."))

(defun ensure-file-pathname (pathspec &rest more-pathspecs)
  "Checks if pathspec is in file form. If not, signal a
`non-file-pathname-error'. Goes through PATHSPEC first, then each of
MORE-PATHSPECS in order and will signal the error on the first non-file pathname
it finds."
  (flet ((check-file (pathspec)
           (unless (uiop:file-pathname-p pathspec)
             (error 'non-file-pathname-error
                    :pathspec pathspec
                    :text (format nil "Non-file pathname ~a" pathspec)))))
    (check-file pathspec)
    (dolist (pathspec more-pathspecs)
      (check-file pathspec))))

(defun buffer-associate-file (buffer pathspec)
  "Makes the file of BUFFER point to the file at PATHSPEC. Ensures it's in file
form and will sign and may signal a `non-file-pathname-error'."
  (ensure-file-pathname pathspec)
  (setf (buffer-file buffer)
        (pathname pathspec)))

(defun buffer-load-file (buffer pathspec)
  "Reads the file pointed to by PATHSPEC into BUFFER. May signal a
`no-such-file-error' if there is no such file or `unknown-file-error' on other
errors. This function will remember to update the display in the future but it
doesn't now. Call UPDATE-FRAME after calling. Will associate BUFFER with
PATHSPEC and may signal a `non-file-pathname-error'."
  (ensure-file-pathname pathspec)
  (let ((found nil))
    (handler-case (with-open-file (reader pathspec :if-does-not-exist nil)
                    (when reader
                      (loop for line = (read-line reader nil)
                         while line
                         do (buffer-append buffer line))
                      (setf found t)
                      (buffer-associate-file buffer pathspec)))
      (file-error () (signal-unknown-file-error pathspec))
      (error () (signal-unknown-file-error pathspec)))
    (unless found
      (signal-no-such-file-error pathspec))))

;; TODO: Figure out the correct container for this.
(defparameter *buffers* (make-container 'vector-container)
  "The list of buffers for the program. Ordered by order of the last time they
were the current buffer.")

(defun make-buffer (&optional (initial-lines 0) (default-line ""))
  (let ((buffer (make-instance 'buffer :lines (make-array initial-lines
                                                          :adjustable t
                                                          :fill-pointer 0
                                                          :element-type 'string
                                                          :initial-element default-line))))
    (container-append *buffers* buffer)
    buffer))

(defun make-buffer-with-file (pathspec)
  "Creates a `buffer' and loads the file at PATHSPEC into it. May signal all of
the same errors as BUFFER-LOAD-FILE."
  (let ((buffer (make-buffer)))
    (buffer-load-file buffer pathspec)
    buffer))

(defun buffer-empty-p (buffer)
  "Returns if there is any text in the buffer."
  (not (or (zerop (length (buffer-lines buffer)))
           (and (= (length (buffer-lines buffer)) 1)
                (string= (aref (buffer-lines buffer) 0) "")))))

(defun set-buffer (buffer)
  "Use this function to switch buffers. Sets *CURRENT-BUFFER* to *BUFFER* and
moves it to the front of *BUFFERS*."
  (setf *current-buffer* buffer)
  (delete-item *buffers* buffer)
  (insert-item-at *buffers* buffer 0)
  (setf *selection* (make-text-selection buffer
                                         (buffer-cursor-position buffer))))

(defun set-to-last-buffer ()
  "Sets the current buffer to the next buffer after the most recent in *BUFFERS*
if it exists."
  (when (> (cl-containers:size *buffers*) 1)
    (set-buffer (item-at *buffers* 1))))

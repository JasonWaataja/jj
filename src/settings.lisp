;;;; settings.lisp
;;;; Program settings.

(in-package #:jj)

(defclass setting ()
  ((name :accessor setting-name
         :initarg :name
         :initform ""
         :type 'string
         :documentation "The name of the setting.")
   (default-value :accessor setting-default-value
          :initarg :value
          :initform nil
          :documentation "The value of the setting, can be of any type.")
   (documentation :accessor setting-documentation
                  :initarg :documentation
                  :initform ""
                  :type 'string
                  :documentation "The documentation string for the setting.")))

(defparameter *settings* (make-container 'set-container)
  "A set of `setting' objects which SETTINGS-LAYERs are built off of.")

(defun define-setting (name default-value &optional (documentation ""))
  "Adds a new `setting' to *SETTINGS* with the given values."
  (insert-item *settings*
               (make-instance 'setting
                              :name name
                              :default-value default-value
                              :documentation documentation)))

(defclass settings-layer ()
  ((settings :accessor settings-layer-settings
             :initarg :settings
             :initform (make-container 'simple-associative-container
                                       :test #'equal)
             :documentation "The map of setting names to their values.")))

(defun make-settings-layer ()
  (make-instance 'settings-layer))

(defparameter *global-settings* (make-settings-layer)
  "The top-level `settings-layer' for the program.")

(define-condition no-such-setting-error (jj-error)
  ((setting-name :reader no-such-setting-error-setting-name
                 :initarg :setting-name)))

(defun signal-no-such-setting-error (setting-name)
  (error 'no-such-setting-error
         :setting-name setting-name
         :text (format nil "No such setting: ~a" setting-name)))

(defun get-setting (layers setting-name)
  "If LAYERS is a list, then iterate through it, returning the first value of a
`setting' matching SETTING-NAME. Otherwise, treat LAYERS as a single layer and
get the setting from it. If no matching setting is found, then the default is
used. If no such setting exists, then a `no-such-setting-error' is signalled."
  (if (listp layers)
      (loop for layer in layers do
           (multiple-value-bind (setting-value found)
               (item-at (settings-layer-settings layer) setting-name)
             (when found
               (return setting-value)))
         finally
           (let ((setting (item-at *settings* setting-name)))
             (if setting
                 (setting-default-value setting)
                 (signal-no-such-setting-error setting-name))))
      (multiple-value-bind (setting-value found)
          (item-at (settings-layer-settings layers) setting-name)
        (if found
            setting-value
            (let ((setting (item-at *settings* setting-name)))
              (if setting
                  (setting-default-value setting)
                  (signal-no-such-setting-error setting-name)))))))

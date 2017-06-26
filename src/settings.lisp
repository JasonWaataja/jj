;;;; settings.lisp
;;;; Program settings.

(in-package #:jj)

(defclass setting ()
  ((name :accessor setting-name
         :initarg :name
         :initform nil
         :type 'symbol
         :documentation "The name of the setting.")
   (default-value :accessor setting-default-value
     :initarg :default-value
     :initform nil
     :documentation "The value of the setting, can be of any type.")
   (documentation :accessor setting-documentation
                  :initarg :documentation
                  :initform ""
                  :type 'string
                  :documentation "The documentation string for the setting.")))

(defparameter *settings* (make-container 'cl-containers:set-container)
  "A set of `setting' objects which SETTINGS-LAYERs are built off off. To
  manitulate this object, use INSERT-NEW-ITEM with SETTING-NAME as the key,
  etc.")

(defun find-setting (setting-name)
  "Acts like CL-CONTAINERS:FIND-ITEM on *SETTINGS* except with the correct key
finding functions."
  (search-for-item *settings* setting-name :key #'setting-name))

(defmacro define-setting (name default-value &optional (documentation ""))
  "Adds a new `setting' to *SETTINGS* with the given values."
  `(insert-new-item *settings*
                    (make-instance 'setting
                                   :name ',name
                                   :default-value ,default-value
                                   :documentation ,documentation)
                    :key #'setting-name))

(defclass settings-layer ()
  ((settings :accessor settings-layer-settings
             :initarg :settings
             :initform (make-container 'simple-associative-container)
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

(defun set-setting (setting-name setting-value &optional layer)
  "Sets the `setting' with SETTING-NAME to SETTING-VALUE for LAYER if passed and
*GLOBAL-SETTINGS* otherwise."
  (if layer
      (setf (item-at (settings-layer-settings layer) setting-name) setting-value)
      (setf (item-at (settings-layer-settings *global-settings*) setting-name) setting-value)))

(defun get-setting (setting-name &optional layers)
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
           (let ((setting (find-setting setting-name)))
             (if setting
                 (return (setting-default-value setting))
                 (return (signal-no-such-setting-error setting-name)))))
      (multiple-value-bind (setting-value found)
          (item-at (settings-layer-settings layers) setting-name)
        (if found
            setting-value
            (let ((setting (find-setting setting-name)))
              (if setting
                  (setting-default-value setting)
                  (signal-no-such-setting-error setting-name)))))))

(define-setting tab-width 8
  "The width of tab characters.")

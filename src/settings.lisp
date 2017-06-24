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
  "A set of SETTING objects which SETTINGS-LAYERs are built off of.")

(defun define-setting (name default-value &optional (documentation ""))
  "Adds a new setting to *SETTINGS* with the given values."
  (insert-item *settings*
               (make-instance 'setting
                              :name name
                              :default-value default-value
                              :documentation documentation)))

(defclass settings-layer ()
  ((settings :accessor settings-layer-settings
             :initarg :settings
             :initform (make-container 'simple-associative-container
                                       :test #'equal))))

(defun make-settings-layer ()
  (make-instance 'settings-layer))

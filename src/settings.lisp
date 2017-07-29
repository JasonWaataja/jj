;;;; settings.lisp
;;;; Program settings.

(in-package #:jj)

(defclass setting ()
  ((name :accessor setting-name
         :initarg :name
         :initform nil
         :type symbol
         :documentation "The name of the setting.")
   (default-value :accessor setting-default-value
     :initarg :default-value
     :initform nil
     :documentation "The value of the setting, can be of any type.")
   (documentation :accessor setting-documentation
                  :initarg :documentation
                  :initform ""
                  :type string
                  :documentation "The documentation string for the setting."))
  (:documentation "A program parameter that can be represented by a simple
  value. They are manipulated through their name and can be layered with a
  `settings-layer'."))

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
             :documentation "The map of setting names to their values."))
  (:documentation "A set of `setting' objects. These should be used together in
  an ordered list of some sort. Later layers have settings that override lower
  layers so that there can be buffer specific settings, etc."))

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

;; TODO: Figure out why I did it this way and not use *GLOBAL-SETTINGS* as the
;; default value for the LAYER argument. Change it back if there was no good
;; reason.
(defun set-setting (setting-name setting-value &optional layer)
  "Sets the `setting' with SETTING-NAME to SETTING-VALUE for LAYER if passed and
*GLOBAL-SETTINGS* otherwise."
  (if layer
      (setf (item-at (settings-layer-settings layer) setting-name) setting-value)
      (setf (item-at (settings-layer-settings *global-settings*) setting-name) setting-value)))

(defun get-setting (setting-name &optional (layers *global-settings*))
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

(define-setting key-sequence-time .5
  "The time, in seconds, that a key combination waits for the next press. Once
  this time is exceeded, the program assumes you are using a new key
  combination.")

(define-setting dump-key-events nil
  "Print debug information when receiving a key event.")

(define-setting scrolloff 0
  "The amount of space between the cursor and the top and bottom of the screen
when automatically scrolling.")

(define-setting autoscroll-method :maintain-scrolloff
  "The way the window is scrolled when it reaches SCROLLOFF. The options
are :MAINTAIN-SCROLLOFF and :CENTER.")

(define-setting should-wrap t
  "If, in a buffer, a line would go over the edge of the screen, wrap it around
and start rendering it on the next line. If NIL, just don't render the end.")

;;;; package.lisp

(defpackage #:jj
  (:use #:cl)
  (:import-from #:cl-containers
                #:simple-associative-container
                #:set-container
                #:item-at
                #:find-item
                #:insert-item
                #:insert-new-item
                #:search-for-item
                #:delete-item
                #:make-container)
  (:export #:setting
           #:*settings*
           #:define-setting
           #:settings-layer
           #:make-settings-layer
           #:*global-settings*
           #:no-such-setting-error
           #:signal-no-such-setting-error
           #:find-setting
           #:set-setting
           #:get-setting))

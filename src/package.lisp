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
           #:calculate-tab-width
           #:leading-string-width
           #:find-setting
           #:set-setting
           #:get-setting
           #:tab-width
           #:string-has-prefix-p
           #:string-has-prefix-insensitive-p
           #:match-regular-character
           #:match-regex
           #:combine-matches))

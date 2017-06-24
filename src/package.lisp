;;;; package.lisp

(defpackage #:jj
  (:use #:cl)
  (:import-from #:cl-containers
                #:simple-associative-container
                #:set-container
                #:item-at
                #:find-item
                #:insert-item
                #:delete-item
                #:make-container))


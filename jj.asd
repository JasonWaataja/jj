;;;; jj.asd

(asdf:defsystem #:jj
  :description "Common Lisp text editor"
  :author "Jason Waataja <jasonswaataja@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/JasonWaataja/jj"
  :version "0.0.0"
  :depends-on (#:cl-containers #:cl-charms #:cl-ppcre)
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "conditions")
                         (:file "util")
                         (:file "settings")
                         (:file "buffer")
                         (:file "display")
                         (:file "frame")
                         (:file "time")
                         (:file "event")
                         (:file "mode")
                         (:file "jj")))))

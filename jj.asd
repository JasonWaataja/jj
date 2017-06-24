;;;; jj.asd

(asdf:defsystem #:jj
  :description "Common Lisp text editor"
  :author "Jason Waataja <jasonswaataja@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/JasonWaataja/jj"
  :version "0.0.0"
  :depends-on (#:cl-containers)
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "settings")
                         (:file "conditions")
                         (:file "buffer")
                         (:file "display")
                         (:file "frame")
                         (:file "jj")))))

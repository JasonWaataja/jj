;;;; jj.asd

(asdf:defsystem #:jj
  :description "Common Lisp text editor"
  :author "Jason Waataja <jasonswaataja@gmail.com>"
  :license "MIT"
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "conditions")
                         (:file "buffer")
                         (:file "frame")
                         (:file "jj")))))

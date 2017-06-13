;;;; jj.asd

(asdf:defsystem #:jj
  :description "Common Lisp text editor"
  :author "Jason Waataja <jasonswaataja@gmail.com>"
  :license "MIT"
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "jj")))))

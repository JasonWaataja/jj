;;;; jj-test.asd

(asdf:defsystem jj-test
  :author "Jason Waataja <jasonswaataja@gmail.com>"
  :license "MIT"
  :depends-on (:jj
               :fiveam)
  :components ((:module "t"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "suite")
                         (:file "jj")))))

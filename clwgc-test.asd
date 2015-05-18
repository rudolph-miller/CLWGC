#|
  This file is a part of clwgc project.
  Copyright (c) 2015 Rudolph-Miller
|#

(in-package :cl-user)
(defpackage clwgc-test-asd
  (:use :cl :asdf))
(in-package :clwgc-test-asd)

(defsystem clwgc-test
  :author "Rudolph-Miller"
  :license "MIT"
  :depends-on (:clwgc
               :prove)
  :components ((:module "t"
                :components
                ((:file "init")
                 (:test-file "ast")
                 (:test-file "parser")
                 (:test-file "env")
                 (:test-file "llvm")
                 (:test-file "ir")
                 (:test-file "clwgc"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))

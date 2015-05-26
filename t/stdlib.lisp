(in-package :cl-user)
(defpackage clwgc-test.stdlib
  (:use :cl
        :prove
        :clwgc-test.init
        :clwgc.llvm
        :clwgc.stdlib)
  (:shadowing-import-from :clwgc.llvm
                          :run))
(in-package :clwgc-test.stdlib)

(plan nil)

(finalize)

(in-package :cl-user)
(defpackage clwgc-test.ir
  (:use :cl
        :prove
        :clwgc-test.init
        :clwgc.ast
        :clwgc.llvm
        :clwgc.ir)
  (:shadowing-import-from :clwgc.llvm
                          :run
                          :make-cons))
(in-package :clwgc-test.ir)

(plan nil)


(finalize)

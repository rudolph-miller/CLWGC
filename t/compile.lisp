(in-package :cl-user)
(defpackage clwgc-test.compile
  (:use :cl
        :prove
        :clwgc-test.init
        :clwgc.compile))
(in-package :clwgc-test.compile)

(plan nil)

(finalize)

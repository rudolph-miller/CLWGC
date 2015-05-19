(in-package :cl-user)
(defpackage clwgc-test.parser
  (:use :cl
        :prove
        :esrap
        :clwgc-test.init)
  (:import-from :clwgc.parser
                :whitespace
                :atom
                :string
                :list
                :sexp))
(in-package :clwgc-test.parser)

(plan nil)


(finalize)

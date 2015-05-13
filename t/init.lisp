(in-package :cl-user)
(defpackage clwgc-test.init
  (:use :cl
        :prove
        :clwgc.ast)
  (:import-from :prove.reporter
                :format-report)
  (:export :is-ast))
(in-package :clwgc-test.init)

(defmethod format-report :around (stream reporter report &rest args &key count)
  (declare (ignore args count))
  (let ((*debug-mode* t))
    (call-next-method)))

(defmacro is-ast (target expect &optional comment)
  `(is ,target
       ,expect
       ,@(when comment (list comment))
       :test #'ast-equal))

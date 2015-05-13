(in-package :cl-user)
(defpackage clwgc-test.init
  (:use :cl)
  (:import-from :prove.reporter
                :format-report)
  (:import-from :clwgc.ast
                :*debug-mode*))
(in-package :clwgc-test.init)

(defmethod format-report :around (stream reporter report &rest args &key count)
  (declare (ignore args count))
  (let ((*debug-mode* t))
    (call-next-method)))

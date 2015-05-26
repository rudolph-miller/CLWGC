(in-package :cl-user)
(defpackage clwgc
  (:use :cl
        :clwgc.env
        :clwgc.llvm
        :clwgc.ir
        :clwgc.parser
        :clwgc.stdlib
        :clwgc.compile))
(in-package :clwgc)

(defparameter *debug* nil)

(defun rep (string)
  (with-stdlib
    (loop with len = (length string)
          with cur = 0
          while (< cur len)
          do (multiple-value-bind (cons pos)
                 (read-from-string string nil nil :start cur)
               (setq cur pos)
               (print (gencode (genir cons))))))
  (when *debug*
    (print (llvm:print-module-to-string *module*))))

(in-package :cl-user)
(defpackage clwgc-test.compile
  (:use :cl
        :prove
        :clwgc.llvm
        :clwgc-test.init
        :clwgc.env
        :clwgc.parser
        :clwgc.compile)
  (:shadowing-import-from :clwgc.llvm
                          :run))
(in-package :clwgc-test.compile)

(defmacro compile-subtest (dest &body body)
  `(subtest ,dest
     (with-module
       (let ((*current-env* (make-env)))
         ,@body))))

(defmacro is-compiled-into (target expect &optional comment)
  `(is (gencode (parse ,target))
       ,expect
       ,(if comment
            comment
            "can be compiled.")))

(plan nil)

(compile-subtest "run-if-toplevel"
  (is-compiled-into "1"
                    1
                    "can run."))

(compile-subtest "<nil>"
  (is-compiled-into "nil"
                    nil))

(compile-subtest "<t>"
  (is-compiled-into "t"
                    t))

(compile-subtest "<constant>"
  (is-compiled-into "1"
                    1))

(compile-subtest "<variable>"
  (is-compiled-into "(progn (setq a 1) a)"
                    1))

(compile-subtest "<update-variable>"
  (is-compiled-into "(progn (setq a 1) (setq a 2) a)"
                    2))

(compile-subtest "<symbol-value>"
  (is-compiled-into "(progn (setq a 1) a)"
                    1))

(compile-subtest "<let>"
  (is-compiled-into "(let ((a 1)) a)"
                    1))

(compile-subtest "<progn>"
  (is-compiled-into "(progn 1 2)"
                    2))

(compile-subtest "<if>"
  (is-compiled-into "(if t 1 0)"
                    1))

(compile-subtest "<lambda>"
  (is-compiled-into "(progn (defun a () 1) (a))"
                    1)

  (is-compiled-into "(let ((a 0)) (defun fn () (if a (setq a 2) (setq a 1))) (fn) (fn))"
                    2
                    "closure.")

  (let ((curret-pos *current-position*))
    (gencode (parse "(defun a () 1)"))
    (is *current-position*
        curret-pos
        "can return to the last position after compiled.")))

(compile-subtest "<funcall>"
  (is-compiled-into "(progn (defun a () 1) (a))"
                    1))

(finalize)

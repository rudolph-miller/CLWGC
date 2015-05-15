(in-package :cl-user)
(defpackage clwgc-test.semantic
  (:use :cl
        :prove
        :clwgc.ast
        :clwgc.semantic)
  (:import-from :alexandria
                :symbolicate))
(in-package :clwgc-test.semantic)

(plan nil)


(subtest "<constant>"
  (ok (make-constant 1 :integer)
      "can make-constant."))

(subtest "<variable>"
  (ok (make-variable "var" (make-constant 1 :integer) :integer)
      "can make-variable."))

(subtest "<special-form>"
  (ok (make-special-form "special" (list 1) (cons (list :integer) (list :integer)))
      "can make-special-form."))

(subtest "<macro-form>"
  (ok (make-macro-form "macro" (list 1))
      "can make-macro-form."))

(subtest "<function-form>"
  (ok (make-functior-form "fun" (list 1) (cons (list :integer) (list :integer)))
      "can make-functior-form."))

(subtest "<env>"
  (let* ((env1 (make-instance '<env>))
         (env2 (make-instance '<env> :parent env1))
         (*current-env* env2))
    (subtest "make-instance"
      (ok env1
          "without :parent.")

      (ok env2
          "with :parent."))

    (macrolet ((add-and-get-test (var-or-fn)
                 (let ((add-method (symbolicate 'add- var-or-fn))
                       (get-method (symbolicate 'get- var-or-fn)))
                 `(progn
                    (subtest "add"
                      (,add-method "dummy1" :dummy1 env1)
                      (is (length (vars env1))
                          1
                          "with specificd env.")

                      (,add-method "dummy2" :dummy2)
                      (is (length (vars env2))
                          1
                          "with *current-env*."))
                    (subtest "get"
                      (is (,get-method "dummy1" env1)
                          :dummy1
                          "with specificd env.")

                      (is (,get-method "dummy2")
                          :dummy2
                          "with *current-env*.")

                      (is (,get-method "dummy1")
                          :dummy1
                          "with parent."))))))
      (subtest "var"
        (add-and-get-test var))

      (subtest "fn"
        (add-and-get-test fn)))))

(finalize)

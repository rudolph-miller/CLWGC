(in-package :cl-user)
(defpackage clwgc-test.semantic
  (:use :cl
        :prove
        :clwgc.ast
        :clwgc.parser
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
  (ok (make-function-form "fun" (list 1) (cons (list :integer) (list :integer)))
      "can make-functior-form."))

(subtest "exp-equal"
  (subtest "T"
    (macrolet ((exp-equal-ok (target expect &optional comment)
                 `(ok (exp-equal ,target ,expect)
                      ,@(when comment (list comment)))))
      (exp-equal-ok (make-constant 1 :integer)
                    (make-constant 1 :integer)
                    "with <constant>.")

      (exp-equal-ok (make-variable "var" (make-constant 1 :integer) :integer)
                    (make-variable "var" (make-constant 1 :integer) :integer)
                    "with <variable>.")

      (exp-equal-ok (make-special-form "special" (list 1) (cons (list :integer) (list :integer)))
                    (make-special-form "special" (list 1) (cons (list :integer) (list :integer)))
                    "with <special-form>.")

      (exp-equal-ok (make-macro-form "macro" (list 1))
                    (make-macro-form "macro" (list 1))
                    "with <macro-form>.")

      (exp-equal-ok (make-function-form "fun" (list 1) (cons (list :integer) (list :integer)))
                    (make-function-form "fun" (list 1) (cons (list :integer) (list :integer)))
                    "with <function-form>.")))

  (subtest "NIL"
    (macrolet ((exp-equal-not-ok (target expect &optional comment)
                 `(ok (not (exp-equal ,target ,expect))
                      ,@(when comment (list comment)))))
      (exp-equal-not-ok (make-constant 1 :integer)
                        (make-variable "var" (make-constant 1 :integer) :integer)
                        "with different types.")

      (subtest "<constant>"
      (exp-equal-not-ok (make-constant 1 :integer)
                        (make-constant 2 :integer)
                        "with different values.")

      (exp-equal-not-ok (make-constant 1 :integer)
                        (make-constant 1 :float)
                        "with different types."))

      (subtest "<variable>"
        (exp-equal-not-ok (make-variable "var1" (make-constant 1 :integer) :integer)
                          (make-variable "var2" (make-constant 1 :integer) :integer)
                          "with different names.")

        (exp-equal-not-ok (make-variable "var" (make-constant 1 :integer) :integer)
                          (make-variable "var" (make-constant 2 :integer) :integer)
                          "with different values.")

        (exp-equal-not-ok (make-variable "var" (make-constant 1 :integer) :integer)
                          (make-variable "var" (make-constant 1 :integer) :float) ; This should raise error. (Implicitly type casting)
                          "with different types."))

      (subtest "<special-form>"
        (exp-equal-not-ok (make-special-form "special1" (list 1) (cons (list :integer) (list :integer)))
                          (make-special-form "special2" (list 1) (cons (list :integer) (list :integer)))
                          "with different names.")

        (exp-equal-not-ok (make-special-form "special" (list 1) (cons (list :integer) (list :integer)))
                          (make-special-form "special" (list 2) (cons (list :integer) (list :integer)))
                          "with different args.")

        (exp-equal-not-ok (make-special-form "special" (list 1) (cons (list :integer) (list :integer)))
                          (make-special-form "special" (list 1) (cons (list :integer) (list :float)))
                          "with different types."))

      (subtest "<macro-form>"
        (exp-equal-not-ok (make-macro-form "macro1" (list 1))
                          (make-macro-form "macro2" (list 1))
                          "with different names.")

        (exp-equal-not-ok (make-macro-form "macro" (list 1))
                          (make-macro-form "macro" (list 2))
                          "with different types."))

      (subtest "<function-form>"
        (exp-equal-not-ok (make-function-form "function1" (list 1) (cons (list :integer) (list :integer)))
                          (make-function-form "function2" (list 1) (cons (list :integer) (list :integer)))
                          "with different names.")

        (exp-equal-not-ok (make-function-form "function" (list 1) (cons (list :integer) (list :integer)))
                          (make-function-form "function" (list 2) (cons (list :integer) (list :integer)))
                          "with different args.")

        (exp-equal-not-ok (make-function-form "function" (list 1) (cons (list :integer) (list :integer)))
                          (make-function-form "function" (list 1) (cons (list :integer) (list :float)))
                          "with different types.")))))

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

(subtest "semanticize"
  (macrolet ((semanticize-test (target expect &optional comment)
               `(is (semanticize ,target)
                    ,expect
                    ,@(when comment (list comment))
                    :test #'exp-equal)))
    (semanticize-test (parse "1")
                      (make-constant 1 :integer)
                      "<integer>.")

    (semanticize-test (parse "1.1")
                      (make-constant 1.1 :float)
                      "<float>.")

    (semanticize-test (parse "\"string\"")
                      (make-constant "string" :string)
                      "<string>.")

    (skip 1 "with <cons>.") ;; special, macro, function.

    (semanticize-test (parse "nil")
                      (make-constant *nil* :nil) "*nil*.")
    ))

(finalize)

(in-package :cl-user)
(defpackage clwgc-test.ir
  (:use :cl
        :prove
        :clwgc-test.init
        :clwgc.env
        :clwgc.llvm
        :clwgc.ir)
  (:shadowing-import-from :clwgc.llvm
                          :run
                          :make-cons))
(in-package :clwgc-test.ir)

(plan nil)

(subtest "<nil>"
  (is-type (make-nil)
           '<nil>
           "can make-nil."))

(subtest "<t>"
  (is-type (make-t)
           '<t>
           "can make-t."))

(subtest "<constant>"
  (is-type (make-constant 1)
           '<constant>
           "can make-constant without type.")

  (is-type (make-constant 1 :integer)
           '<constant>
           "can make-constant with type.")

  (let ((constant (make-constant 1 :integer)))
    (is (value constant)
        1
        "can set value.")

    (is (exp-type constant)
        :integer
        "can set type.")))

(subtest "<variable>"
  (let ((*current-env* (make-env)))
    (is-type (make-variable "var1")
             '<variable>
             "can make-variable without value, type nor global.")

    (is-type (make-variable "var2" (make-t))
             '<variable>
             "can make-variable with value without type nor global.")

    (is-type (make-variable "var3" (make-t) :bool)
             '<variable>
             "can make-variable with value and type without global.")

    (is-type (make-variable "var3" (make-t) :bool t)
             '<variable>
             "can make-variable with value, type and global."))

  (let* ((*current-env* (make-env))
         (var (make-variable "var" (make-t) :bool)))
    (is (name var)
        "var"
        "can set name.")

    (is-type (value var)
             '<t>
             "can set value.")

    (is (exp-type var)
        :bool
        "can set type.")

    (is (global var)
        nil
        "can set global.")

    (ok (get-var "var")
        "can add-var.")))

(subtest "<update-variable>"
  (let* ((*current-env* (make-env))
         (var (make-variable "var" (make-t) :bool))
         (value (make-nil)))
    (is-type (make-update-variabel var value)
             '<update-variable>
             "can make-update-variabel.")

    (let ((update-variable (make-update-variabel var value)))
      (is-type (var update-variable)
               '<variable>
               "can set var.")

      (is-type (value update-variable)
               '<nil>
               "can set value."))))

(subtest "<symbol-value>"
  (let ((*current-env* (make-env)))
    (make-variable "var" (make-t) :bool)
    (is-type (make-symbol-value "var")
             '<symbol-value>
             "can make-symbol-value.")

    (is-error (make-symbol-value "error")
              'simple-error
              "can raise an error with undeclared variable.")

    (let ((symbol-value (make-symbol-value "var")))
      (is-type (var symbol-value)
               '<variable>
               "can set var."))))

(subtest "<let>"
  (let ((*current-env* (make-env)))
    (is-type (make-let (list (cons "var" (make-t))) (list (make-symbol-value "var")))
             '<let>
             "can make-let.")

    (let ((let (make-let (list (cons "var" (make-t))) (list (make-symbol-value "var")))))
      (is-type (car (vars let))
               '<variable>
               "can set vars.")

      (ok (> (length (body let)) 0)
          "can set body."))

    (is (get-var "var")
        nil
        "can add-var locally.")))

(subtest "<progn>"
  (is-type (make-progn (list (make-constant 1 :integer)))
           '<progn>
           "can make-progn.")

  (let ((progn (make-progn (list (make-constant 1 :integer)))))
    (ok (> (length (body progn)) 0)
        "can set body.")))

(subtest "<if>"
  (is-type (make-if (make-t) (make-constant 1 :integer) (make-constant 0 :integer))
           '<if>
           "can make-if.")

  (let ((if (make-if (make-t) (make-constant 1 :integer) (make-constant 0 :integer))))
    (is-type (pred if)
             '<t>
             "can set pred.")

    (is-type (then if)
             '<constant>
             "can set then.")

    (is-type (else if)
             '<constant>
             "can set else.")))

(subtest "<lambda>"
  (is-type (make-lambda (list (make-variable "var")) (list (make-symbol-value "var")))
           '<lambda>
           "can make-lambda.")

  (let ((lambda (make-lambda (list (make-variable "var")) (list (make-symbol-value "var")))))
    (is-type (car (args lambda))
             '<variable>
             "can set vars.")

    (ok (> (length (body lambda)) 0)
        "can set body.")))

(subtest "<funcall>"
  (let ((lambda (make-lambda (list (make-variable "var")) (list (make-symbol-value "var")))))
    (is-type (make-funcall lambda (list (make-constant 1 :integer)))
             '<funcall>
             "can make-funcall.")

    (let ((funcall (make-funcall lambda (list (make-constant 1 :integer)))))
      (is-type (fn funcall)
               '<lambda>
               "can set fn.")

      (is-type (car (args funcall))
               '<constant>
               "can set args."))))

(finalize)

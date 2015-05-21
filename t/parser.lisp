(in-package :cl-user)
(defpackage clwgc-test.parser
  (:use :cl
        :prove
        :clwgc-test.init
        :clwgc.ir
        :clwgc.env
        :clwgc.parser))
(in-package :clwgc-test.parser)

(defmacro with-env (&body body)
  `(let ((*current-env* (make-env)))
     ,@body))

(plan nil)

(subtest "setq"
  (with-env
    (subtest "<variable>"
      (is-type (parse "(setq var 1)")
               '<variable>
               "can generate.")
      
      (is-type (get-var "var")
               '<variable>
               "can add-var.")

      (is (value (value (get-var "var")))
          1
          "can set value."))

    (subtest "<update-variable>"
      (is-type (parse "(setq var 2)")
               '<update-variable>
               "can generate <update-variable>.")

      (let ((update-package (parse "(setq var 2)")))
        (is-type (var update-package)
                 '<variable>
                 "can set var.")

        (is-type (value update-package)
                 '<constant>
                 "can set value.")))))

(subtest "progn"
  (with-env
    (is-type (parse "(progn 1 2)")
             '<progn>
             "can generate.")

    (let ((progn (parse "(progn 1 2)")))
      (is-type (car (body progn))
               '<constant>
               "can set body."))))

(subtest "lambda"
  (with-env
    (is-type (parse "(lambda (a) a)")
             '<lambda>
             "can generate.")

    (let ((lambda (parse "(lambda (a) a)")))
      (is (name lambda)
          nil
          "can set name.")

      (is-type (car (args lambda))
               '<variable>
               "can set args.")

      (is-type (car (body lambda))
               '<symbol-value>
               "can set body."))))

(subtest "defun"
  (with-env
    (is-type (parse "(defun fn (a) a)")
             '<lambda>
             "can generate.")

    (is-type (get-fn "fn")
             '<lambda>
             "can add-fn.")

    (let ((fn (parse "(defun fn (a) a)")))
      (is (name fn)
          "FN"
          "can set name.")

      (is-type (car (args fn))
               '<variable>
               "can set variable.")

      (is-type (car (body fn))
               '<symbol-value>
               "can set body."))))

(subtest "let"
  (with-env
    (is-type (parse "(let ((a 1)) a)")
             '<let>
             "can generate.")

    (let ((let (parse "(let ((a 1)) a)")))
      (is-type (car (vars let))
               '<variable>
               "can set vars.")

      (is-type (car (body let))
               '<symbol-value>
               "can set body."))))

(subtest "if"
  (with-env
    (macrolet ((if-test (target pred-t then-t else-t)
                 (let ((if (gensym "if")))
                   `(progn
                      (is-type (parse ,target)
                               '<if>
                               "can generate.")

                      (let ((,if (parse ,target)))
                        (is-type (pred ,if)
                                 ',pred-t
                                 "can set pred.")

                        (is-type (then ,if)
                                 ',then-t
                                 "can set then.")

                        (is-type (else ,if)
                                 ',else-t
                                 "can set else."))))))

      (subtest "with else"
        (if-test "(if t 1 0)" <t> <constant> <constant>))

      (subtest "without else"
        (if-test "(if t 1)" <t> <constant> <nil>)))))

(subtest "funcall"
  (with-env
    (is-error (parse "(fn 1)")
              'simple-error
              "can raise the error with an undefined function.")

    (parse "(defun fn (a) a)")

    (is-type (parse "(fn 1)")
             '<funcall>
             "can generate.")

    (let ((funcall (parse "(fn 1)")))
      (is-type (fn funcall)
               '<lambda>
               "can set fn.")

      (is-type (car (args funcall))
               '<constant>
               "can set args."))))

(subtest "atom"
  (with-env
    (subtest "t"
      (is-type (parse "t")
               '<t>
               "can generate."))

    (subtest "nil"
      (is-type (parse "nil")
               '<nil>
               "can generate."))

    (subtest "symbol-value"
      (is-error (parse "var")
                'simple-error
                "can raise the error with an undeclared variable.")

      (parse "(setq var 1)")

      (is-type (parse "var")
               '<symbol-value>
               "can generate.")

      (let ((var (parse "var")))
        (is-type (var var)
                 '<variable>
                 "can set var.")))

    (subtest "integer"
      (is-type (parse "1")
               '<constant>
               "can generate.")

      (let ((int (parse "1")))
        (is (value int)
            1
            "can set value.")))))

(finalize)

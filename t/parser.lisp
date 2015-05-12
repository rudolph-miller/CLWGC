(in-package :cl-user)
(defpackage clwgc-test.parser
  (:use :cl
        :prove
        :esrap)
  (:import-from :clwgc.parser
                :whitespace
                :atom
                :string
                :list
                :sexp))
(in-package :clwgc-test.parser)

(plan nil)

(subtest "whitespace"
  (macrolet ((whitespace-test ((&rest characters) comment)
               `(is (parse 'whitespace (format nil "~{~a~}" ',characters))
                    nil
                    ,comment)))
    (whitespace-test (#\Space)
                     "wiht #\Space.")

    (whitespace-test (#\Newline)
                     "wiht #\Newline.")

    (whitespace-test (#\Tab)
                     "wiht #\Tab.")

    (whitespace-test (#\Space #\Space)
                     "with two whitespaces.")))

(subtest "atom"
  (macrolet ((atom-test (target expect comment)
               `(is (parse 'atom ,target)
                    ,expect
                    ,comment)))
    (atom-test "1"
               1
               "with integer.")

    (atom-test "1.1e0"
               1.1
               "with float.")

    (atom-test "test"
               "test"
               "with symbol.")))

(subtest "string"
  (is (parse 'string "\"test\"")
      "test"
      "ok."))

(subtest "list"
  (is (parse 'list "(1 2 3)")
      (list 1 2 3)
      "ok."))

(subtest "sexp"
  (macrolet ((sexp-test (target expect comment)
               `(is (parse 'sexp ,target)
                    ,expect
                    ,comment)))
    (sexp-test "1"
               1
               "with atom.")

    (sexp-test "\"test\""
               "test"
               "with string.")

    (sexp-test "(1 2 3)"
               (list 1 2 3)
               "with list.")))

(subtest "parse"
  (is (clwgc.parser:parse "(1 2 3)")
      (list 1 2 3)
      "ok."))

(finalize)

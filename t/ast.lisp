(in-package :cl-user)
(defpackage clwgc-test.ast
  (:use :cl
        :prove
        :clwgc-test.init
        :clwgc.ast))
(in-package :clwgc-test.ast)

(plan nil)

(subtest "<integer>"
  (let ((obj (make-integer 1)))
    (subtest "instanct"
      (is-type obj
               '<integer>
               "can make-instance.")

      (is (content obj)
          1
          "can read slot."))

    (subtest "print-object"
      (is-print (princ obj)
                "1"
                "without *debug-mode*.")

      (let ((*debug-mode* t))
        (is-print (princ obj)
                  "#<<INTEGER> :content 1>"
                  "with *debug-mode*.")))))

(subtest "<float>"
  (let ((obj (make-float 1.1e0)))
    (subtest "instanct"
      (is-type obj
               '<float>
               "can make-instance.")

      (is (content obj)
          1.1e0
          "can read slot."))

    (subtest "print-object"
      (is-print (princ obj)
                "1.1"
                "without *debug-mode*.")

      (let ((*debug-mode* t))
        (is-print (princ obj)
                  "#<<FLOAT> :content 1.1>"
                  "with *debug-mode*.")))))

(subtest "<symbol>"
  (let ((obj (make-sym "symbol")))
    (subtest "instanct"
      (is-type obj '<symbol>
               "can make-instance.")
      
      (is (content obj)
          "symbol"
          "can access slot.")

      (subtest "print-object"
        (is-print (princ obj)
                  "symbol"
                  "without *debug-mode*.")

        (let ((*debug-mode* t))
          (is-print (princ obj)
                    "#<<SYMBOL> :content symbol>"
                    "with *debug-mode*."))))))

(subtest "<string>"
  (let ((obj (make-instance '<string> :content "string")))
    (subtest "instanct"
      (is-type obj '<string>
               "can make-instance.")
      
      (is (content obj)
          "string"
          "can access slot."))

    (subtest "print-object"
      (is-print (princ obj)
                "\"string\""
                "without *debug-mode*.")

      (let ((*debug-mode* t))
        (is-print (princ obj)
                  "#<<STRING> :content string>"
                  "with *debug-mode*.")))))

(subtest "<cons>"
  (let ((obj1 (make-cons (make-integer 1) (make-integer 2)))
        (obj2 (make-cons (make-integer 1) *nil*))
        (obj3 (make-cons (make-integer 1)
                         (make-cons (make-integer 2) *nil*))))
    (subtest "instanct"
      (is-type obj1
               '<cons>
               "can make-instance."))

    (subtest "print-object"
      (macrolet ((print-cons-test (obj without-debug-mode with-debug-mode)
                   `(progn
                      (is-print (princ ,obj)
                                ,without-debug-mode
                                "without *debug-mode*.")
                      (let ((*debug-mode* t))
                        (is-print (princ ,obj)
                                  ,with-debug-mode
                                  "with *debug-mode*.")))))
      (subtest "pair"
        (print-cons-test obj1 "(1 . 2)" "(#<<INTEGER> :content 1> . #<<INTEGER> :content 2>)"))
      (subtest "length = 1"
        (print-cons-test obj2 "(1)" "(#<<INTEGER> :content 1>)"))
      (subtest "length > 1"
        (print-cons-test obj3 "(1 2)" "(#<<INTEGER> :content 1> #<<INTEGER> :content 2>)"))))))

(subtest "null-p"
  (ok (null-p *nil*)
      "can return T.")

  (ok (not (null-p (make-sym "non-nil")))
      "can return NIL."))

(subtest "atom-p"
  (subtest "T"
    (ok (atom-p (make-sym "sym"))
        "with <atom>.")

    (ok (atom-p *nil*)
        "with NIL."))

  (subtest "NIL"
    (ok (not (atom-p (make-cons (make-integer 1) *nil*)))
        "ok.")))

(subtest "cons-p"
  (ok (cons-p (make-cons (make-integer 1) *nil*))
      "can return T.")

  (ok (not (cons-p (make-sym "non-cons")))
      "can return NIL."))

(subtest "paip-p"
  (ok (pair-p (make-cons (make-integer 1) (make-integer 2)))
      "can return T.")

  (ok (not (pair-p (make-cons (make-integer 1) *nil*)))
      "can return NIL."))

(subtest "ast-equal"
  (subtest "T"
    (ok (ast-equal *nil* *nil*)
        "with *nil*.")

    (ok (ast-equal (make-integer 1) (make-integer 1))
        "with <atom>.")

    (ok (ast-equal (make-cons (make-integer 1)
                              (make-cons (make-integer 2)
                                         (make-integer 3)))
                   (make-cons (make-integer 1)
                              (make-cons (make-integer 2)
                                         (make-integer 3))))
        "with <cons>."))

  (subtest "NIL"
    (ok (not (ast-equal (make-integer 1)
                        (make-sym "sym")))
        "with not same objects.")

    (ok (not (ast-equal (make-integer 1)
                        (make-integer 2)))
        "with <atom>.")

    (ok (not (ast-equal (make-cons (make-integer 1)
                                   (make-cons (make-integer 2)
                                              (make-integer 3)))
                        (make-cons (make-integer 1)
                                   (make-cons (make-integer 2)
                                              (make-integer 4)))))
        "with <cons>.")))

(subtest "make-lst"
  (is-ast (make-lst (make-integer 1) (make-integer 2))
          (make-cons (make-integer 1)
                     (make-cons (make-integer 2)
                                *nil*))
          "ok."))

(subtest "make-lst*"
  (is-ast (make-lst* (make-integer 1) (make-integer 2))
          (make-cons (make-integer 1)
                     (make-integer 2))
          "ok."))

(finalize)

(in-package :cl-user)
(defpackage clwgc-test.llvm
  (:use :cl
        :prove
        :clwgc-test.init
        :clwgc.llvm)
  (:shadowing-import-from :clwgc.llvm
                          :run))
(in-package :clwgc-test.llvm)

(plan nil)

(defmacro is-ptr (target expect &optional commet)
  `(is ,target
       ,expect
       ,@(when commet (list commet))
       :test #'cffi:pointer-eq))

(subtest "with-module"
  (with-module
    (ok *builder*
        "*builder* is bound.")

    (ok *module*
        "*module* is bound.")

    (ok *ee*
        "*ee* is bound.")

    (ok *fpm*
        "*fpm* is bound.")

    (ok *cons*
        "*cons* is bound.")))


(subtest "get-type"
  (with-module
    (is-ptr (get-type :integer)
            (llvm:int64-type)
            ":integer.")

    (is-ptr (get-type :bool)
            (llvm:int1-type)
            ":bool.")

    (is-ptr (get-type :cons)
            *cons*
            ":cons.")

    (is-ptr (get-type :pointer (get-type :cons))
            (llvm:pointer-type *cons*)
            ":pointer.")))

(subtest "append-block"
  (with-module
    (let ((main (add-function "main" nil nil)))
      (append-block main "entry")
      (is (llvm:count-basic-blocks main)
          1
          "con add block."))))

(subtest "move-to"
  (with-module
    (skip 1 "can not get current block.")))

(subtest "add-function"
  (with-module
    (let ((main (add-function "main" nil nil)))
      (is-ptr (llvm:named-function *module* "main")
              main
              "can add a named function."))))

(subtest "add-function-and-move-into"
  (with-module
    (skip 1 "can not get current block.")))

(subtest "args"
  (with-module
    (let ((main (add-function "main" (list :integer) :integer)))
      (is (length (args main))
          1
          "with function pointer.")

      (is (length (args "main"))
          1
          "with string."))))

(subtest "alloca"
  (with-module
    (add-function-and-move-into "main" nil nil)
    (ok (not (cffi:null-pointer-p (alloca :integer)))
        "can alloca.")))

(subtest "ret"
  (with-module
    (let ((main (add-function-and-move-into "main" nil :integer)))
      (ret (constant :integer 1))
      (is (run main)
          1
          "can return value."))))

(subtest "constant"
  (with-module
    (let ((main (add-function-and-move-into "main" nil :integer)))
      (ret (constant :integer 1))
      (is (run main)
          1
          ":integer."))

    (let ((main (add-function-and-move-into "main" nil :integer)))
      (ret (constant :bool 1))
      (is (run main)
          1
          ":bool."))))

(subtest "store-var, load-var"
  (with-module
    (let ((main (add-function-and-move-into "main" nil :integer))
          (i (alloca :integer)))
      (store-var i (constant :integer 1))
      (ret (load-var i))
      (is (run main)
          1
          "can strore and load."))))

(subtest "init-var"
  (with-module
    (let ((main (add-function-and-move-into "main" nil :integer))
          (i (init-var :integer (constant :integer 1))))
      (ret (load-var i))
      (is (run main)
          1
          "can alloca and store."))))

(subtest "br"
  (with-module
    (let* ((main (add-function-and-move-into "main" nil :integer))
           (end (append-block main "end")))
      (br end)
      (move-to end)
      (ret (constant :integer 1))
      (is (run main)
          1
          "can br."))))

(subtest "cond-br"
  (with-module
    (let* ((main (add-function-and-move-into "main" (list :bool) :integer))
           (then (append-block main "then"))
           (else (append-block main "else")))
      (cond-br (car (args main)) then else)
      (move-to then)
      (ret (constant :integer 1))
      (move-to else)
      (ret (constant :integer 2))
      (let ((sub (add-function-and-move-into "sub" nil :integer)))
         (ret (call main (list (constant :bool 1))))
        (is (run sub)
            1
            "then with 1 in int1."))

      (let ((sub (add-function-and-move-into "sub" nil :integer)))
         (ret (call main (list (constant :bool 0))))
        (is (run sub)
            2
            "then with 0 in int1.")))

    (let* ((main (add-function-and-move-into "main" (list :integer) :integer))
           (then (append-block main "then"))
           (else (append-block main "else")))
      (cond-br (car (args main)) then else)
      (move-to then)
      (ret (constant :integer 1))
      (move-to else)
      (ret (constant :integer 2))
      (let ((sub (add-function-and-move-into "sub" nil :integer)))
         (ret (call main (list (constant :integer 1))))
        (is (run sub)
            1
            "then with 1 in int64."))

      (let ((sub (add-function-and-move-into "sub" nil :integer)))
         (ret (call main (list (constant :integer 0))))
        (is (run sub)
            2
            "then with 0 in int64.")))))

(subtest "call"
  (with-module
    (let ((main (add-function-and-move-into "main" (list :integer) :integer)))
      (ret (car (args main)))
      (let ((sub (add-function-and-move-into "sub" nil :integer)))
        (ret (call main (list (constant :integer 1))))
        (is (run sub)
            1
            "can call function.")))))

(subtest "run-pass"
  (with-module
    (let ((main (add-function-and-move-into "main" nil :integer)))
      (ret (llvm:build-add *builder* (constant :integer 1) (constant :integer 2) "tmp"))
      (run-pass main)
      (ok t "do not raise errors."))))

(subtest "run"
  (with-module
    (let ((main (add-function-and-move-into "main" nil :integer)))
      (ret (constant :integer 1))
      (is (run main)
          1
          "can run function."))))

(subtest "make-cons"
  (with-module
    (add-function-and-move-into "main" nil :integer)

      (is-ptr (llvm:type-of (load-var (make-cons (constant :integer 1) (constant :integer 2))))
              (get-type :cons)
              "can make-cons.")))

(subtest "get-car"
  (with-module
    (let ((main (add-function-and-move-into "main" nil :integer))
          (i (make-cons (constant :integer 1) (constant :integer 2))))
      (ret (get-car i))
      (is (run main)
          1
          "can get."))

    (let ((main (add-function-and-move-into "main" nil :integer))
          (i (make-cons (constant :integer 1) (constant :integer 2))))
      (setf (get-car i) (constant :integer 2))
      (ret (get-car i))
      (is (run main)
          2
          "can setf."))))

(subtest "get-cdr"
  (with-module
    (let ((main (add-function-and-move-into "main" nil :integer))
          (i (make-cons (constant :integer 1) (constant :integer 2))))
      (ret (get-cdr i))
      (is (run main)
          2
          "can get."))

    (let ((main (add-function-and-move-into "main" nil :integer))
          (i (make-cons (constant :integer 1) (constant :integer 2))))
      (setf (get-cdr i) (constant :integer 1))
      (ret (get-cdr i))
      (is (run main)
          1
          "can setf."))))

(finalize)

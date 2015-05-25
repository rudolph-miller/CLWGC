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
        "*cons* is bound.")

    (skip 1 "with-obj-declared.")

    (ok clwgc.llvm::*va-list*
        "*va-list* is bound.")

    (ok (not (cffi:null-pointer-p (llvm:named-function *module* "llvm.va_start")))
        "llvm.va_start is declared.")

    (ok (not (cffi:null-pointer-p (llvm:named-function *module* "llvm.va_end")))
        "llvm.va_end is declared.")))


(subtest "get-type"
  (with-module
    (is-ptr (get-type :integer)
            (llvm:int32-type)
            ":integer.")

    (is-ptr (get-type :int8)
            (llvm:int8-type)
            ":int8.")

    (is-ptr (get-type :bool)
            (llvm:int1-type)
            ":bool.")

    (is-ptr (get-type :cons)
            *cons*
            ":cons.")

    (is-ptr (get-type :pointer (get-type :cons))
            (llvm:pointer-type *cons*)
            ":pointer.")

    (is-ptr (get-type '(:pointer :cons))
            (llvm:pointer-type *cons*)
            "with list.")

    (is-ptr (get-type :obj)
            clwgc.llvm::*obj*
            "with :obj.")

    (is-ptr (get-type '(:obj :integer))
            clwgc.llvm::*obj.integer*
            "with (:obj :integer).")

    (is-ptr (get-type '(:obj :cons))
            clwgc.llvm::*obj.cons*
            "with (:obj :cons).")))

(subtest "get-type-of"
  (with-module
    (is (get-type-of (constant :integer 1))
        :integer
        ":integer.")

    (is (get-type-of (constant :int8 1))
        :int8
        ":int8.")

    (is (get-type-of (constant :bool 1))
        :bool
        ":bool.")

    (is (get-type-of (init-var :integer (constant :integer 1)))
        '(:pointer :integer)
        "with (:pointer :integer).")

    (is (get-type-of (init-var :bool (constant :bool 1)))
        '(:pointer :bool)
        "with (:pointer :bool).")

    (skip 1 ":obj.")
    (skip 1 "(:obj :integer).")
    (skip 1 "(:obj :bool).")))

(subtest "append-block"
  (with-module
    (let ((main (add-function "main" nil nil)))
      (append-block "entry")
      (is (llvm:count-basic-blocks main)
          1
          "con add block."))))

(subtest "move-to"
  (with-module
    (add-function-and-move-into "main" nil nil)
    (let ((current-pos *current-position*)
          (block (append-block "next")))
      (move-to block)
      (skip 1 "can not get current block.")

      (ok (not (cffi:pointer-eq current-pos
                                *current-position*))
        "can set *current-position*."))))

(subtest "add-function"
  (with-module
    (let ((main (add-function "main" nil nil)))
      (is-ptr (llvm:named-function *module* "main")
              main
              "can add a named function."))))

(subtest "add-function-and-move-into"
  (with-module
    (skip 1 "can not get current block.")))

(subtest "params"
  (with-module
    (let ((main (add-function "main" (list :integer) :integer)))
      (is (length (params main))
          1
          "with function pointer.")

      (is (length (params "main"))
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
      (ret (constant :int8 1))
      (is (run main)
          1
          ":integer."))

    (let ((main (add-function-and-move-into "main" nil :integer)))
      (ret (constant :bool 1))
      (is (run main)
          t
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

(subtest "init-global-var"
  (with-module
    (let ((main (add-function-and-move-into "main" nil :integer))
          (global (init-global-var :integer (constant :integer 1))))
      (ret (load-var global))
      (is (llvm:constantp global)
          t
          "can create global variable.")

      (is (run main)
          1
          "can set initial value."))))

(subtest "br"
  (with-module
    (let* ((main (add-function-and-move-into "main" nil :integer))
           (end (append-block "end")))
      (br end)
      (move-to end)
      (ret (constant :integer 1))
      (is (run main)
          1
          "can br."))))

(subtest "cond-br"
  (with-module
    (let* ((main (add-function-and-move-into "main" (list :bool) :integer))
           (then (append-block "then"))
           (else (append-block "else")))
      (cond-br (car (params main)) then else)
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
           (then (append-block "then"))
           (else (append-block "else")))
      (cond-br (car (params main)) then else)
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
      (ret (car (params main)))
      (let ((sub (add-function-and-move-into "sub" nil :integer)))
        (ret (call main (list (constant :integer 1))))
        (is (run sub)
            1
            "can call function.")))))

(subtest "incoming"
  (with-module
    (let* ((main (add-function-and-move-into "main" nil :integer))
           (then (append-block "then"))
           (else (append-block "else"))
           (end (append-block "end")))
      (cond-br (constant :bool 0) then else)
      (move-to then)
      (let ((then-val (init-var :integer (constant :integer 1))))
        (br end)
        (move-to else)
        (let ((else-val (init-var :integer (constant :integer 2))))
          (br end)
          (move-to end)
          (ret (load-var
                (incoming '(:pointer :integer)
                          (list (cons then-val then) (cons else-val else)))))))
      (is (run main)
          2
          "can add-incoming."))))

(subtest "bit-cast"
  (with-module
    (add-function-and-move-into "main" nil :integer)
    (let ((i (init-var :bool (constant :bool 1))))
      (is (get-type-of (bit-cast i '(:pointer :integer)))
          '(:pointer :integer)
          "can bet-cast."))))


(subtest "va-arg"
  (skip 1 "not tested yet."))

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

(in-package :cl-user)
(defpackage clwgc.ir
  (:use :cl
        :clwgc.ast
        :clwgc.env
        :clwgc.llvm
        :named-readtables
        :fare-quasiquote)
  (:shadowing-import-from :clwgc.llvm
                          :make-cons)
  (:import-from :alexandria
                :ensure-list
                :with-gensyms)
  (:export :to-ir
           :ir))
(in-package :clwgc.ir)

(in-readtable :fare-quasiquote)

(defparameter *toplevel-p* t)

(defmacro with-not-toplevel (&body body)
  `(let ((*toplevel-p* nil))
     ,@body))

(defmacro run-if-toplevel (&body body)
  (with-gensyms (run entry result)
    `(let* ((,run (add-function "run" nil :integer))
            (,entry (append-block "entry" ,run)))
       (when *toplevel-p* (move-to ,entry))
       (let ((,result (with-not-toplevel ,@body)))
         (if *toplevel-p*
             (progn (ret ,result)
                    (run ,run))
             ,result)))))

(defun add-someops ()
  (let ((add (add-function-and-move-into "add" (list :integer :integer) :integer)))
    (ret (llvm:build-add *builder* (car (params)) (cadr (params)) "add"))
    (add-fn "+" add))

  (let ((sub (add-function-and-move-into "sub" (list :integer :integer) :integer)))
    (ret (llvm:build-sub *builder* (car (params)) (cadr (params)) "add"))
    (add-fn "-" sub))

  (let ((mul (add-function-and-move-into "mul" (list :integer :integer) :integer)))
    (ret (llvm:build-mul *builder* (car (params)) (cadr (params)) "add"))
    (add-fn "*" mul))


  (let ((eql (add-function-and-move-into "eql" (list :integer :integer) :bool)))
    (ret (llvm:build-i-cmp *builder* := (car (params)) (cadr (params)) "cmp"))
    (add-fn "eql" eql)
    (add-fn "=" eql))

  (let ((cons (add-function-and-move-into "cons" (list :integer :integer) :cons)))
    (ret (make-cons (car (params)) (cadr (params))))
    (add-fn "cons" cons))

  (let ((car (add-function-and-move-into "car" (list '(:pointer :cons)) :integer)))
    (ret (get-car (car (params))))
    (add-fn "car" car))

  (let ((cdr (add-function-and-move-into "cdr" (list '(:pointer :cons)) :integer)))
    (ret (get-cdr (car (params))))
    (add-fn "cdr" cdr)))

(defun to-ir (cons)
  (optima:match cons
    (`(setq ,var ,val ,@rest)
      (run-if-toplevel
        (let* ((name (symbol-name var))
               (val (to-ir val))
               (bind (init-var :integer val name)))

          (add-var name bind)
          (if rest
              (to-ir (cons 'setq rest))
              val))))
    (`(progn ,@body)
      (run-if-toplevel
        (loop for stm in (ensure-list body)
              for result = (to-ir stm)
              finally (return result))))
    (`(lambda (,@args) ,@body)
      (with-not-toplevel
        (let* ((arg-t (loop repeat (length args)
                            collecting :integer))
               (fn (add-function-and-move-into "lambda" arg-t :integer)))
          (let ((*current-env* (make-env *current-env*)))
            (loop for sym in args
                  for name = (symbol-name sym)
                  for i from 0
                  for bind = (init-var :integer (elt (params) i) name)
                  do (add-var name bind))
            (ret (to-ir `(progn ,@(ensure-list body)))))
          (run-pass fn)
          fn)))
    (`(defun ,name-s (,@args) ,@body)
      (let* ((name (symbol-name name-s))
             (arg-t (loop repeat (length args)
                          collecting :integer))
             (fn (add-function-and-move-into name arg-t :integer)))
        (add-fn name fn)
        (let ((*current-env* (make-env *current-env*)))
          (with-not-toplevel
            (loop for sym in args
                  for name = (symbol-name sym)
                  for i from 0
                  for bind = (init-var :integer (elt (params) i) name)
                  do (add-var name bind))
            (ret (to-ir `(progn ,@(ensure-list body))))))
        (run-pass fn)
        name))
    (`(let (,@bind-forms) ,@body)
      (run-if-toplevel
        (let ((*current-env* (make-env *current-env*)))
          (loop for item in bind-forms
                for name = (symbol-name (car item))
                do (add-var name (init-var :integer (to-ir (cadr item)) name)))
          (loop for stm in (ensure-list body)
                for result = (to-ir stm)
                finally (return result)))))
    (`(if ,pred ,then ,else)
      (run-if-toplevel
        (let ((if-val (to-ir pred))
              (then-b (append-block "if.then"))
              (else-b (append-block "if.else"))
              (end-b (append-block "if.end")))
          (cond-br if-val then-b else-b)
          (move-to then-b)
          (let ((then-val (to-ir then)))
            (br end-b)
            (move-to else-b)
            (let ((else-val (to-ir else)))
              (br end-b)
              (move-to end-b)
              (incoming :integer (list (cons then-val then-b)
                                       (cons else-val else-b))))))))
    (`(if ,pred ,then)
      (to-ir `(if ,pred ,then nil)))
    (`(,fn-s ,@args)
      (run-if-toplevel
        (let* ((name (symbol-name fn-s))
               (fn (get-fn name)))
          (if fn
              (call fn (with-not-toplevel (mapcar #'to-ir args)))
              (error "The function ~a is undefined." name)))))
    (atom
     (run-if-toplevel
       (cond
         ((symbolp atom)
          (let* ((name (symbol-name atom))
                 (var (get-var name)))
            (cond
              ((string-equal name "t") (constant :bool 1))
              ((string-equal name "nil") (constant :bool 0))
              (var (load-var var))
              (t (error "The variable ~a is undefined." name)))))
         ((integerp atom) (constant :integer atom))
         (t (error "Not supported: ~a." atom)))))))

(defun ir (string)
  (with-module
    (let ((*current-env* (make-env)))
      (add-someops)
      (loop with len = (length string)
            with cur = 0
            while (< cur len)
            do (multiple-value-bind (cons pos)
                   (read-from-string string nil nil :start cur)
                 (setq cur pos)
                 (print (to-ir cons))))
      (llvm:dump-module *module*))))

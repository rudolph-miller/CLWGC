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
       (let ((,result (progn ,@body)))
         (if *toplevel-p*
             (progn (ret ,result)
                    (run ,run))
             ,result)))))

(defun add-someops ()
  (let ((add (add-function-and-move-into "add" (list :integer :integer) :integer)))
    (ret (llvm:build-add *builder* (car (params)) (cadr (params)) "add"))
    (add-fn "+" add))

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
      (let* ((name (symbol-name var))
             (bind (init-var :integer (to-ir val) name)))
        (add-var (symbol-name var) bind)
        (when rest
          (to-ir (cons 'setq rest)))))
    (`(defun ,name-s (,@args) ,@body)
      (let* ((name (symbol-name name-s))
             (arg-t (loop repeat (length args)
                          collecting :integer))
             (fn (add-function-and-move-into name arg-t :integer)))
        (let ((*current-env* (make-env *current-env*)))
          (with-not-toplevel
            (loop for sym in args
                  for name = (symbol-name sym)
                  for i from 0
                  for bind = (init-var :integer (elt (params) i) name)
                  do (add-var name bind))
            (loop for stm in (ensure-list body)
                  for i from 1
                  for result = (to-ir stm)
                  finally (ret result))))
        (add-fn name fn)
        name))
    (`(if ,pred ,then ,else)
      (run-if-toplevel
        (let ((if-val (with-not-toplevel (to-ir pred)))
              (then-b (append-block "if.then"))
              (else-b (append-block "if.else"))
              (end-b (append-block "if.end")))
          (cond-br if-val then-b else-b)
          (move-to then-b)
          (let ((then-val (with-not-toplevel (to-ir then))))
            (br end-b)
            (move-to else-b)
            (let ((else-val (with-not-toplevel (to-ir else))))
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

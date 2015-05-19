(in-package :cl-user)
(defpackage clwgc
  (:use :cl
        :clwgc.env
        :clwgc.llvm
        :clwgc.ir
        :clwgc.compile))
(in-package :clwgc)

(defun add-someops ()
  (macrolet ((add-op (name (arg-t ret-t) &body body)
               `(let ((fn (add-function-and-move-into ,name (list ,@arg-t) ,ret-t)))
                  ,@body
                  (make-instance '<lambda> :name ,name :ptr fn))))
    (add-op "+" ((:integer :integer) :integer)
            (ret (llvm:build-add *builder* (car (params)) (cadr (params)) "add")))

    (add-op "-" ((:integer :integer) :integer)
            (ret (llvm:build-sub *builder* (car (params)) (cadr (params)) "sub")))

    (add-op "*" ((:integer :integer) :integer)
            (ret (llvm:build-mul *builder* (car (params)) (cadr (params)) "mul")))


    (add-op "=" ((:integer :integer) :bool)
            (ret (llvm:build-i-cmp *builder* := (car (params)) (cadr (params)) "cmp")))

    (add-op "cons" ((:integer :integer) :cons)
             (ret (make-cons (car (params)) (cadr (params)))))

    (add-op "car" (('(:pointer :cons)) :integer)
            (ret (get-car (car (params)))))

    (add-op "cdr" (('(:pointer :cons)) :integer)
            (ret (get-cdr (car (params)))))))

(defun rep (string)
  (with-module
    (let ((*current-env* (make-env)))
      (add-someops)
      (loop with len = (length string)
            with cur = 0
            while (< cur len)
            do (multiple-value-bind (cons pos)
                   (read-from-string string nil nil :start cur)
                 (setq cur pos)
                 (print (gencode (genir cons)))))
      (llvm:dump-module *module*))))

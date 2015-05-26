(in-package :cl-user)
(defpackage clwgc.stdlib
  (:use :cl
        :clwgc.llvm))
(in-package :clwgc.stdlib)


(defparameter *stdlib* nil)

(defmacro with-stdlib (&body body)
  `(with-module
       (loop for pair in *stdlib*
             do (funcall (cdr pair)))
     ,@body))

(defmacro deflib (name &body body)
  `(push (cons ,name (lambad () ,@body)) *stdlib*))

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

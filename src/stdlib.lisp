(in-package :cl-user)
(defpackage clwgc.stdlib
  (:use :cl
        :clwgc.env
        :clwgc.llvm
        :clwgc.ir)
  (:export :*stdlib*
           :with-stdlib))
(in-package :clwgc.stdlib)

(defparameter *stdlib* nil)

(defmacro with-stdlib (&body body)
  `(with-module
     (with-global-env
       (loop for pair in *stdlib*
             do (funcall (cdr pair)))
       ,@body)))

(defmacro deflib (name &body body)
  `(push (cons ,name (lambda () ,@body)) *stdlib*))

(defmacro deflibfun (name (arg-t ret-t) &body body)
  `(deflib ,name
     (let ((fn (add-function-and-move-into ,name (list ,@arg-t) ,ret-t)))
       ,@body
       (make-instance '<lambda> :name ,name :ptr fn :global t))))

(deflibfun "+" ((:integer :integer) :integer)
  (ret (llvm:build-add *builder* (car (params)) (cadr (params)) "add")))

(deflibfun "-" ((:integer :integer) :integer)
  (ret (llvm:build-sub *builder* (car (params)) (cadr (params)) "sub")))

(deflibfun "*" ((:integer :integer) :integer)
  (ret (llvm:build-mul *builder* (car (params)) (cadr (params)) "mul")))


(deflibfun "=" ((:integer :integer) :bool)
  (ret (llvm:build-i-cmp *builder* := (car (params)) (cadr (params)) "cmp")))


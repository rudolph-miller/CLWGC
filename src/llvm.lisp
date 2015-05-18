(in-package :cl-user)
(defpackage clwgc.llvm
  (:use :cl
        :clwgc.semantic)
  (:export :*builder*
           :*module*
           :*ee*
           :*fpm*
           :*cons*
           :with-module
           :get-type
           :append-block
           :move-to
           :add-function
           :add-function-and-move-into
           :args
           :alloca
           :ret
           :constant
           :store-var
           :load-var
           :init-var
           :br
           :cond-br
           :call
           :run-pass
           :run
           :make-cons
           :get-car
           :get-cdr))
(in-package :clwgc.llvm)

(defparameter *builder* nil)

(defparameter *module* nil)

(defparameter *ee* nil)

(defparameter *fpm* nil)

(defparameter *cons* nil)

(defun get-type (key &optional arg)
  (if (null key)
      (llvm:void-type)
      (ecase key
        (:integer (llvm:int64-type))
        (:bool (llvm:int1-type))
        (:cons *cons*)
        (:pointer (llvm:pointer-type arg)))))

(defun declare-cons ()
  (let ((ty (llvm:struct-create-named (llvm:global-context) "cons")))
    (llvm:struct-set-body ty (list (get-type :integer) (get-type :integer)))
    ty))

(defmacro with-cons-declared (&body body)
  `(let ((*cons* (declare-cons)))
     ,@body))

(defmacro with-module (&body body)
  `(llvm:with-objects ((*builder* llvm:builder)
                       (*module* llvm:module "CLWGC")
                       (*ee* llvm:execution-engine *module*)
                       (*fpm* llvm:function-pass-manager *module*))
     (with-cons-declared
         (llvm:add-target-data (llvm:target-data *ee*) *fpm*)
       (llvm:add-promote-memory-to-register-pass *fpm*)
       (llvm:add-reassociate-pass *fpm*)
       (llvm:add-gvn-pass *fpm*)
       (llvm:initialize-function-pass-manager *fpm*)
       ,@body)))

(defun append-block (fn name)
  (llvm:append-basic-block fn name))

(defun move-to (block)
  (llvm:position-builder *builder* block))

(defun add-function (name arg-t ret-t)
  (let ((arr (make-array (length arg-t)))
        (ret (if (consp ret-t)
                 (get-type (car ret-t) (get-type (cadr ret-t)))
                 (get-type ret-t))))
    (when arg-t
      (loop for i from 0
            for item in arg-t
            do (setf (aref arr i) (get-type item))))
    (llvm:add-function *module* name
                       (llvm:function-type ret arr))))

(defun add-function-and-move-into (name arg-t ret-t)
  (let ((fn (add-function name arg-t ret-t)))
    (move-to (append-block fn "entry"))
    fn))

(defun args (fn)
  (when (stringp fn)
    (setq fn (llvm:named-function *module* fn)))
  (llvm:params fn))

(defun alloca (type &optional (name "tmp"))
  (llvm:build-alloca *builder* (get-type type) name))

(defun ret (val)
  (llvm:build-ret *builder* val))

(defun constant (type value)
  (ecase type
    (:integer (llvm:const-int (get-type type) value))
    (:bool (llvm:const-int (get-type type) value))))

(defun store-var (var val)
  (llvm:build-store *builder* val var))

(defun load-var (var &optional (name "tmp"))
  (llvm:build-load *builder* var name))

(defun init-var (type &optional val (name "tmp"))
  (let ((var (alloca type name)))
    (when val
      (store-var var val))
    var))

(defun br (dest)
  (llvm:build-br *builder* dest))

(defun cond-br (if then else)
  (let* ((comp (if (cffi:pointer-eq (llvm:type-of if) (get-type :bool))
                   if
                   (llvm:build-i-cmp *builder* :> if (constant :integer 0) "comp"))))
    (llvm:build-cond-br *builder* comp then else)))

(defun call (fn &optional args (name "tmp"))
  (when (stringp fn)
    (setq fn (llvm:named-function *module* fn)))
  (llvm:build-call *builder* fn args name))

(defun run-pass (fn)
  (when (stringp fn)
    (setq fn (llvm:named-function *module* fn)))
  (llvm:run-function-pass-manager *fpm* fn))

(defun run (fn &optional args)
  (when (stringp fn)
    (setq fn (llvm:named-function *module* fn)))
  (llvm:generic-value-to-int (llvm:run-function *ee* fn args) nil))

(defun make-cons (car cdr)
  (let ((cons (alloca :cons)))
    (setf (get-car cons) car)
    (setf (get-cdr cons) cdr)
    cons))

(defun get-car (cons &optional (name "tmp"))
  (load-var (llvm:build-struct-gep *builder* cons 0 name)))

(defun get-cdr (cons &optional (name "tmp"))
  (load-var (llvm:build-struct-gep *builder* cons 1 name)))

(defun (setf get-car) (val cons)
  (let ((car (llvm:build-struct-gep *builder* cons 0 "tmp")))
    (store-var car val)))

(defun (setf get-cdr) (val cons)
  (let ((cdr (llvm:build-struct-gep *builder* cons 1 "tmp")))
    (store-var cdr val)))

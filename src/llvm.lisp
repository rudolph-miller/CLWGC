(in-package :cl-user)
(defpackage clwgc.llvm
  (:use :cl)
  (:export :*builder*
           :*module*
           :*ee*
           :*fpm*
           :*current-fn*
           :*cons*
           :with-module
           :get-type
           :append-block
           :move-to
           :add-function
           :add-function-and-move-into
           :params
           :alloca
           :ret
           :constant
           :store-var
           :load-var
           :init-var
           :br
           :cond-br
           :call
           :incoming
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

(defparameter *current-fn* nil)

(defparameter *cons* nil)

(defun get-type (key &optional arg)
  (cond
    ((null key) (llvm:void-type))
    ((consp key) (get-type (car key) (get-type (cadr key))))
    (t (ecase key
         (:integer (llvm:int64-type))
         (:bool (llvm:int1-type))
         (:cons *cons*)
         (:pointer (llvm:pointer-type arg))))))

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

(defun append-block (name &optional (fn *current-fn*))
  (llvm:append-basic-block fn name))

(defun move-to (block)
  (llvm:position-builder *builder* block))

(defun add-function (name arg-t ret-t)
  (let ((arr (make-array (length arg-t))))
    (when arg-t
      (loop for i from 0
            for item in arg-t
            do (setf (aref arr i) (get-type item))))
    (setq *current-fn*
          (llvm:add-function *module* name
                             (llvm:function-type (get-type ret-t) arr)))))

(defun add-function-and-move-into (name arg-t ret-t)
  (let ((fn (add-function name arg-t ret-t)))
    (setq *current-fn* fn)
    (move-to (append-block "entry"))
    fn))

(defun params (&optional (fn *current-fn*))
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

(defun call (&optional (fn *current-fn*) args (name "tmp"))
  (when (stringp fn)
    (setq fn (llvm:named-function *module* fn)))
  (llvm:build-call *builder* fn args name))

(defun incoming (type pairs)
  (let ((result (llvm:build-phi *builder* (get-type type) "result")))
    (llvm:add-incoming result (mapcar #'car pairs) (mapcar #'cdr pairs))
    result))

(defun run-pass (&optional (fn *current-fn*))
  (when (stringp fn)
    (setq fn (llvm:named-function *module* fn)))
  (llvm:run-function-pass-manager *fpm* fn))

(defun run (&optional (fn *current-fn*) args)
  (when (stringp fn)
    (setq fn (llvm:named-function *module* fn)))
  (let* ((result (llvm:run-function *ee* fn args))
         (width (llvm:generic-value-int-width result))
         (int (llvm:generic-value-to-int result nil)))
    (if (= width 1)
        (when (= int 1) t)
        int)))

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

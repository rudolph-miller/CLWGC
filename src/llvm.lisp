(in-package :cl-user)
(defpackage clwgc.llvm
  (:use :cl
        :clwgc.ast))
(in-package :clwgc.llvm)

(defparameter *builder* nil)

(defparameter *module* nil)

(defparameter *ee* nil)

(defparameter *fpm* nil)

(defparameter *integer* nil)

(defparameter *void* nil)

(defmacro with-module (&body body)
  `(llvm:with-objects ((*builder* llvm:builder)
                       (*module* llvm:module "CLWGC")
                       (*ee* llvm:execution-engine *module*)
                       (*fpm* llvm:function-pass-manager *module*))
     (llvm:add-target-data (llvm:target-data *ee*) *fpm*)
     (llvm:add-promote-memory-to-register-pass *fpm*)
     (llvm:add-instruction-combining-pass *fpm*)
     (llvm:add-reassociate-pass *fpm*)
     (llvm:add-cfg-simplification-pass *fpm*)
     ,@body))

(defun get-type (key)
  (if (null key)
      (llvm:void-type)
      (ecase key
        (:integer (llvm:int64-type)))))

(defun append-block (fn name)
  (llvm:append-basic-block fn name))

(defun move-to (block)
  (llvm:position-builder *builder* block))

(defun add-function (name arg-t ret-t)
  (let ((arr (make-array (length arg-t))))
    (when arg-t
      (loop for i from 0
            for item in arg-t
            do (setf (aref arr i) (get-type item))))
    (llvm:add-function *module* name
                       (llvm:function-type (get-type ret-t) arr))))

(defun alloca (type &optional (name "tmp"))
  (llvm:build-alloca *builder* (get-type type) name))

(defun ret (val)
  (llvm:build-ret *builder* val))

(defun constant (type value)
  (ecase type
    (:integer (llvm:const-int (get-type type) value))))

(defun store-var (var val)
  (llvm:build-store *builder* val var))

(defun load-var (var &optional (name "tmp"))
  (llvm:build-load *builder* var name))

(defun br (dest)
  (llvm:build-br *builder* dest))

(defun run-pass (fn)
  (when (stringp fn)
    (setq fn (llvm:named-function *module* fn)))
  (llvm:run-function-pass-manager *fpm* fn))

(defun call (fn args &optional (name "tmp"))
  (when (stringp fn)
    (setq fn (llvm:named-function *module* fn)))
  (llvm:build-call *builder* fn args name))

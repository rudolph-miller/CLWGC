(in-package :cl-user)
(defpackage clwgc.llvm
  (:use :cl)
  (:export :*builder*
           :*module*
           :*ee*
           :*fpm*
           :*current-fn*
           :*current-position*
           :*cons*
           :with-module
           :get-type
           :get-type-of
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
           :init-global-var
           :br
           :cond-br
           :call
           :incoming
           :bit-cast
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

(defparameter *current-position* nil)

(defparameter *cons* nil)

(defparameter *obj* nil)

(defparameter *obj.integer* nil)

(defparameter *obj.cons* nil)

(defparameter *va-list* nil)

(defun get-type (key &optional arg)
  (cond
    ((null key) (llvm:void-type))
    ((consp key)
     (ecase (car key)
       (:pointer (get-type (car key) (get-type (cadr key))))
       (:obj (get-type (car key) (cadr key)))))
    ((cffi:pointerp key) key)
    (t (ecase key
         (:integer (llvm:int64-type))
         (:bool (llvm:int1-type))
         (:cons *cons*)
         (:va-list *va-list*)
         (:pointer (llvm:pointer-type arg))
         (:obj (if arg
                   (ecase arg
                     (:integer *obj.integer*)
                     (:cons *obj.cons*))
                   *obj*))))))

(defun get-type-of (val)
  (find (llvm:type-of val)
        (list :integer
              :bool
              :cons
              :va-list
              :obj
              '(:pointer :integer)
              '(:pointer :bool)
              '(:pointer :cons)
              '(:pointer :va-list)
              '(:obj :integer)
              '(:obj :cons))
        :test #'cffi:pointer-eq
        :key #'(lambda (item)
                 (get-type item))))

(defmacro with-obj-declared (&body body)
  `(let ((*cons* (llvm:struct-create-named (llvm:global-context) "cons"))
         (*obj* (llvm:struct-create-named (llvm:global-context) "obj"))
         (*obj.integer* (llvm:struct-create-named (llvm:global-context) "obj.integer"))
         (*obj.cons* (llvm:struct-create-named (llvm:global-context) "obj.cons")))
     ;; cons:: obj * obj
     (llvm:struct-set-body *cons* (list (get-type :integer) (get-type :integer)))
     ;; obj:: tag:int * inner:int
     (llvm:struct-set-body *obj* (list (get-type :integer) (get-type :integer)))
     (llvm:struct-set-body *obj.integer* (list (get-type :integer)))
     (llvm:struct-set-body *obj.cons* (list *cons*))
     ,@body))

(defmacro with-va-list-declared (&body body)
  `(let ((*va-list* (llvm:struct-create-named (llvm:global-context) "va_list")))
     (llvm:struct-set-body *va-list* (list (llvm:int32-type)
                                           (llvm:int32-type)
                                           (get-type (list :pointer (llvm:int8-type)))
                                           (get-type (list :pointer (llvm:int8-type)))))
     ,@body))

(defmacro with-module (&body body)
  `(llvm:with-objects ((*builder* llvm:builder)
                       (*module* llvm:module "CLWGC")
                       (*ee* llvm:execution-engine *module*)
                       (*fpm* llvm:function-pass-manager *module*))
     (with-obj-declared
       (with-va-list-declared
         (llvm:add-target-data (llvm:target-data *ee*) *fpm*)
         (llvm:add-promote-memory-to-register-pass *fpm*)
         (llvm:add-reassociate-pass *fpm*)
         (llvm:add-gvn-pass *fpm*)
         (llvm:initialize-function-pass-manager *fpm*)
         ,@body))))

(defun append-block (name &optional (fn *current-fn*))
  (llvm:append-basic-block fn name))

(defun move-to (block)
  (setq *current-position* block)
  (llvm:position-builder *builder* block))

(defun add-function (name arg-t ret-t &key var-arg-p)
  (let ((arr (make-array (length arg-t))))
    (when arg-t
      (loop for i from 0
            for item in arg-t
            do (setf (aref arr i) (get-type item))))
    (setq *current-fn*
          (llvm:add-function *module* name
                             (llvm:function-type (get-type ret-t) arr :var-arg-p var-arg-p)))))

(defun add-function-and-move-into (name arg-t ret-t &key var-arg-p)
  (let ((fn (add-function name arg-t ret-t :var-arg-p var-arg-p)))
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

(defun init-global-var (type &optional val (name "tmp"))
  (let ((global (llvm:add-global *module* (get-type type) name)))
    (when (llvm:set-initializer global val))
    global))

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

(defun bit-cast (val dest-ty &optional (name "tmp"))
  (llvm:build-bit-cast *builder* val (get-type dest-ty) name))

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

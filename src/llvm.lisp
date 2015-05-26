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
           :build-if
           :call
           :incoming
           :bit-cast
           :va-arg
           :run-pass
           :run
           :make-cons
           :get-car
           :get-cdr
           :obj-tag
           :obj-value))
(in-package :clwgc.llvm)

(defparameter *tmp-name* "")

(defparameter *builder* nil)

(defparameter *module* nil)

(defparameter *ee* nil)

(defparameter *fpm* nil)

(defparameter *current-fn* nil)

(defparameter *current-position* nil)

(defparameter *cons* nil)

(defparameter *obj* nil)

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
         (:integer (llvm:int32-type))
         (:int8 (llvm:int8-type))
         (:bool (llvm:int1-type))
         (:cons *cons*)
         (:va-list *va-list*)
         (:pointer (llvm:pointer-type arg))
         (:obj *obj*)))))

(defun get-type-of (val)
  (find (llvm:type-of val)
        (list :integer
              :int8
              :bool
              :cons
              :va-list
              :obj
              '(:pointer :integer)
              '(:pointer :bool)
              '(:pointer :cons)
              '(:pointer :va-list))
        :test #'cffi:pointer-eq
        :key #'(lambda (item)
                 (get-type item))))

(defmacro with-obj-declared (&body body)
  `(let ((*cons* (llvm:struct-create-named (llvm:global-context) "cons"))
         (*obj* (llvm:struct-create-named (llvm:global-context) "obj")))
     ;; cons:: obj * obj
     (llvm:struct-set-body *cons* (list (get-type :integer) (get-type :integer)))
     ;; obj:: tag:int * inner:int
     (llvm:struct-set-body *obj* (list (get-type :integer) (get-type :integer)))
     ,@body))

(let ((list '((:bool . 0)
              (:integer . 1)
              (:cons . 2))))
  (defun tag->num (tag)
    (cdr (find tag list :key #'car)))

  (defun num->tag (num)
    (car (find num list :key #'cdr))))

(defmacro with-va-list-declared (&body body)
  `(let ((*va-list* (llvm:struct-create-named (llvm:global-context) "va_list")))
     (llvm:struct-set-body *va-list* (list (llvm:int32-type)
                                           (llvm:int32-type)
                                           (get-type (list :pointer (llvm:int8-type)))
                                           (get-type (list :pointer (llvm:int8-type)))))
     (add-function "llvm.va_start" (list (list :pointer (llvm:int8-type))) nil)
     (add-function "llvm.va_end" (list (list :pointer (llvm:int8-type))) nil)
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

(defun alloca (type &optional (name *tmp-name*))
  (llvm:build-alloca *builder* (get-type type) name))

(defun ret (val)
  (llvm:build-ret *builder* val))

(defun constant (type value)
  (ecase type
    (:integer (llvm:const-int (get-type type) value))
    (:int8 (llvm:const-int (get-type type) value))
    (:bool (llvm:const-int (get-type type) (if (typep value 'boolean)
                                               (if value 1 0)
                                               value)))))

(defun store-var (var val)
  (llvm:build-store *builder* val var))

(defun load-var (var &optional (name *tmp-name*))
  (llvm:build-load *builder* var name))

(defun init-var (type &optional val (name *tmp-name*))
  (let ((var (alloca type name)))
    (when val
      (if (cffi:pointerp val)
          (store-var var val)
          (store-var var (constant type val))))
    var))

(defun init-global-var (type &optional val (name *tmp-name*))
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

(defmacro build-if (pred then else)
  `(let ((if-val ,pred)
         (then-b (append-block "if.then"))
         (else-b (append-block "if.else"))
         (end-b (append-block "if.end")))
     (cond-br if-val then-b else-b)
     (move-to then-b)
     (let ((then-val ,then))
       (br end-b)
       (move-to else-b)
       (let ((else-val ,else))
         (br end-b)
         (move-to end-b)
         (incoming :integer (list (cons then-val then-b)
                                  (cons else-val else-b)))))))

(defun call (&optional (fn *current-fn*) args (name *tmp-name*))
  (when (stringp fn)
    (setq fn (llvm:named-function *module* fn)))
  (llvm:build-call *builder* fn args name))

(defun incoming (type pairs)
  (let ((result (llvm:build-phi *builder* (get-type type) *tmp-name*)))
    (llvm:add-incoming result (mapcar #'car pairs) (mapcar #'cdr pairs))
    result))

(defun bit-cast (val dest-ty &optional (name *tmp-name*))
  (llvm:build-bit-cast *builder* val (get-type dest-ty) name))

(defun va-arg (list ty &optional (name *tmp-name*))
  (llvm:build-va-arg *builder* list ty name))

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

(defun get-car (cons &optional (name *tmp-name*))
  (load-var (llvm:build-struct-gep *builder* cons 0 name)))

(defun get-cdr (cons &optional (name *tmp-name*))
  (load-var (llvm:build-struct-gep *builder* cons 1 name)))

(defun (setf get-car) (val cons)
  (let ((car (llvm:build-struct-gep *builder* cons 0 *tmp-name*)))
    (store-var car val)))

(defun (setf get-cdr) (val cons)
  (let ((cdr (llvm:build-struct-gep *builder* cons 1 *tmp-name*)))
    (store-var cdr val)))

(defun obj-tag (obj &optional (name *tmp-name*))
  (load-var (llvm:build-struct-gep *builder* obj 0 name)))

(defun (setf obj-tag) (int obj)
  (store-var (llvm:build-struct-gep *builder* obj 0 *tmp-name*)
             (constant :integer int)))

(defun obj-value (obj &optional (name *tmp-name*))
  (load-var (llvm:build-struct-gep *builder* obj 1 name)))

(defun (setf obj-value) (value obj)
  (store-var (llvm:build-struct-gep *builder* obj 1 *tmp-name*)
             value))

(defun make-int-obj (value)
  (let ((obj (init-var :obj)))
    (setf (obj-tag obj) (tag->num :integer))
    (setf (obj-value obj)
          (if (cffi:pointerp value)
              value
              (constant :integer value)))
    obj))

(defun obj-value-as-int (obj)
  (bit-cast (obj-value obj) :integer))

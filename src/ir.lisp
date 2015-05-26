(in-package :cl-user)
(defpackage clwgc.ir
  (:use :cl
        :clwgc.env)
  (:export :<expression>
           :<nil>
           :<t>
           :<constant>
           :<variable>
           :<update-variable>
           :<symbol-value>
           :<let>
           :<progn>
           :<if>
           :<lambda>
           :<funcall>
           :*inner-lambda*
           :*current-fn-env-layer*
           :exp-type
           :ptr
           :value
           :name
           :global
           :var
           :vars
           :body
           :pred
           :then
           :else
           :args
           :fn
           :make-nil
           :make-t
           :make-constant
           :make-variable
           :make-update-variabel
           :make-symbol-value
           :make-let
           :make-progn
           :make-if
           :make-lambda
           :make-funcall))
(in-package :clwgc.ir)

(defclass <expression> ()
  ((type :initarg :type
         :reader exp-type)
   (ptr :initarg :ptr
        :reader ptr)))

(defclass <nil> (<expression>) ())

(defclass <t> (<expression>) ())

(defclass <constant> (<expression>)
  ((value :initarg :value
          :reader value)))

(defclass <variable> (<expression>)
  ((name :initarg :name
         :reader name)
   (value :initarg :value
          :reader value)
   (global :initarg :global
           :reader global)))

(defclass <update-variable> (<expression>)
  ((var :initarg :var
        :reader var)
   (value :initarg :value
          :reader value)))

(defclass <symbol-value> (<expression>)
  ((var :initarg :var
        :reader var)))

(defclass <let> (<expression>)
  ((vars :initarg :vars
         :reader vars)
   (body :initarg :body
         :reader body)))

(defclass <progn> (<expression>)
  ((body :initarg :body
         :reader body)))

(defclass <if> (<expression>)
  ((pred :initarg :pred
         :reader pred)
   (then :initarg :then
         :reader then)
   (else :initarg :else
         :reader else)))

(defclass <lambda> (<expression>)
  ((name :initarg :name
         :reader name
         :initform nil)
   (args :initarg :args
         :reader args)
   (body :initarg :body
         :reader body)
   (global :initarg :global
           :reader global
           :initform nil)))

(defclass <funcall> (<expression>)
  ((fn :initarg :fn
       :reader fn)
   (args :initarg :args
         :reader args)))

(defparameter *inner-lambda* nil)

(defparameter *current-fn-env-layer* nil)

(defun make-nil ()
  (make-instance '<nil>))

(defun make-t ()
  (make-instance '<t>))

(defun make-constant (value &optional type)
  (make-instance '<constant> :value value :type type))

(defun make-variable (name &optional value type global)
  (make-instance '<variable>
                 :name name
                 :value value
                 :type type
                 :global global))

(defun make-update-variabel (var value)
  (make-instance '<update-variable>
                 :var var
                 :value value))

(defun make-symbol-value (name)
  (multiple-value-bind (var parent-p layer) (get-var name)
    (if var
        (progn (when (and *inner-lambda*
                          parent-p
                          (< layer *current-fn-env-layer*))
                 (setf (slot-value var 'global) t))
               (make-instance '<symbol-value> :var var))
        (error "The variable ~a is undefined." name))))

(defmacro make-let (pairs body)
  `(let* ((*current-env* (make-env *current-env*)))
     (make-instance '<let>
                    :vars (mapcar #'(lambda (pair)
                                      (make-variable (car pair) (cdr pair) :integer))
                                  ,pairs)
                    :body ,body)))

(defun make-progn (body)
  (make-instance '<progn> :body body))

(defun make-if (pred then &optional else)
  (make-instance '<if>
                 :pred pred
                 :then then
                 :else (or else (make-nil))))

(defmacro make-lambda (args body &optional name global)
  `(let ((fn (make-instance '<lambda>
                            ,@(when name (list :name name))
                            ,@(when global (list :global global)))))
     (let* ((*current-env* (make-env *current-env*))
            (*current-fn-env-layer* (layer *current-env*))
            (*inner-lambda* t))
       (setf (slot-value fn 'args) ,args)
       (setf (slot-value fn 'body) ,body))
     fn))

(defun make-funcall (fn args)
  (make-instance '<funcall> :fn fn :args args))

(defmethod initialize-instance :after ((obj <variable>) &rest initargs)
  (declare (ignore initargs))
  (add-var (name obj) obj))

(defmethod initialize-instance :after ((obj <lambda>) &rest initargs)
  (declare (ignore initargs))
  (when (name obj)
    (if (and (slot-boundp obj 'global)
             (global obj))
        (add-fn (name obj) obj *global-env*)
        (add-fn (name obj) obj))))


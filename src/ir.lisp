(in-package :cl-user)
(defpackage clwgc.ir
  (:use :cl
        :clwgc.env
        :named-readtables
        :fare-quasiquote)
  (:import-from :alexandria
                :ensure-list)
  (:export :<expression>
           :<nil>
           :<t>
           :<constant>
           :<variable>
           :<symbol-value>
           :<let>
           :<progn>
           :<if>
           :<lambda>
           :<funcall>
           :exp-type
           :ptr
           :value
           :name
           :var
           :vars
           :body
           :pred
           :then
           :else
           :args
           :fn
           :genir))
(in-package :clwgc.ir)

(in-readtable :fare-quasiquote)

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
         :reader body)))

(defclass <funcall> (<expression>)
  ((fn :initarg :fn
       :reader fn)
   (args :initarg :args
         :reader args)))

(defun make-nil ()
  (make-instance '<nil>))

(defun make-t ()
  (make-instance '<t>))

(defun make-constant (value &optional type)
  (make-instance '<constant> :value value :type type))

(defun make-variable (name &optional value type)
  (make-instance '<variable>
                 :name name
                 :value value
                 :type type))

(defun make-symbol-value (name)
  (let ((var (get-var name)))
    (if var
        (make-instance '<symbol-value> :var var)
        (error "The variable ~a is undefined." name))))

(defun make-let (vars body)
  (make-instance '<let> :vars vars :body body))

(defun make-if (pred then &optional else)
  (make-instance '<if>
                 :pred pred
                 :then then
                 :else (or else (make-nil))))

(defmacro make-lambda (args body &optional name)
  `(let ((fn (make-instance '<lambda> ,@(when name (list :name name)))))
     (let ((*current-env* (make-env *current-env*)))
       (setf (slot-value fn 'args) ,args)
       (setf (slot-value fn 'body) ,body))
     fn))

(defmethod initialize-instance :after ((obj <variable>) &rest initargs)
  (declare (ignore initargs))
  (add-var (name obj) obj))

(defmethod initialize-instance :after ((obj <lambda>) &rest initargs)
  (declare (ignore initargs))
  (when (name obj)
    (add-fn (name obj) obj)))

(defun genir (cons)
  (optima:match cons
    (`(setq ,var ,val ,@rest)
      (let* ((name (symbol-name var))
             (obj (make-variable name (genir val) :integer)))
        (if rest
            (genir (cons 'setq rest))
            obj)))
    (`(progn ,@body)
      (make-instance '<progn> :body (mapcar #'genir (ensure-list body))))
    (`(lambda (,@args) ,@body)
      (make-lambda (mapcar #'(lambda (sym)
                               (make-variable (symbol-name sym)))
                           args)
                   (mapcar #'genir (ensure-list body))))
    (`(defun ,name-s (,@args) ,@body)
      (let ((name (symbol-name name-s)))
        (make-lambda (mapcar #'(lambda (sym)
                                 (make-variable (symbol-name sym)))
                             args)
                     (mapcar #'genir (ensure-list body))
                     name)))
    (`(let (,@bind-forms) ,@body)
      (let* ((*current-env* (make-env *current-env*))
             (vars (loop for list in bind-forms
                         for name = (symbol-name (car list))
                         collecting (make-variable name
                                                   (genir (cadr list))
                                                   :integer))))
        (make-let vars (mapcar #'genir (ensure-list body)))))
    (`(if ,pred ,then ,else)
      (make-if (genir pred) (genir then) (genir else)))
    (`(if ,pred ,then)
      (make-if (genir pred) (genir then)))
    (`(,fn-s ,@args)
      (let* ((name (symbol-name fn-s))
             (fn (get-fn name)))
        (if fn
            (make-instance '<funcall> :fn fn :args (mapcar #'genir args))
            (error "The function ~a is undefined." name))))
    (atom
     (cond
       ((symbolp atom)
        (let ((name (symbol-name atom)))
          (cond
            ((string-equal name "t") (make-t))
            ((string-equal name "nil") (make-nil))
            (t (make-symbol-value name)))))
       ((integerp atom) (make-constant atom :integer))
       (t (error "Not supported: ~a." atom))))))

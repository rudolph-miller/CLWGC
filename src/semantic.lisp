(in-package :cl-user)
(defpackage clwgc.semantic
  (:use :cl
        :clwgc.ast)
  (:import-from :alexandria
                :symbolicate)
  (:export :<expression>
           :<costant>
           :<variable>
           :<form>
           :<special-form>
           :<macro-form>
           :<function-form>
           :value
           :exp-type
           :name
           :label
           :<env>
           :vars
           :fns
           :parent
           :*current-env*
           :add-var
           :add-fn
           :get-var
           :get-fn))
(in-package :clwgc.semantic)

(defclass <expression> () ())

(defclass <costant> (<expression>)
  ((value :initarg :value
          :reader value)
   (type :initarg :type
         :reader exp-type)))

(defclass <variable> (<expression>)
  ((name :initarg :name
         :reader name)
   (value :initarg :value
          :reader value)
   (type :initarg :type
         :reader exp-type)))

(defclass <form> (<expression>)
  ((label :initarg :label
          :reader label)
   (args :initarg :args
         :reader exp-type)))

(defclass <special-form> (<form>)
  ((type :initarg :type
         :reader exp-type)))

(defclass <macro-form> (<form>) ())

(defclass <function-form> (<form>)
  ((type :initarg :type
         :reader exp-typ)))

(defclass <env> ()
  ((vars :initarg :vars
         :accessor vars
         :initform nil)
   (fns :initarg :fns
        :accessor fns
        :initform nil)
   (parent :initarg :parent
           :reader parent
           :initform nil)))

(defparameter *current-env* nil)

(defun add-var (name value &optional (env *current-env*))
  (push (cons name value) (vars env)))

(defun add-fn (name fn &optional (env *current-env*))
  (push (cons name fn) (fns env)))

(defmacro get-smt (var-or-fn)
  `(let ((result (find name (,(symbolicate var-or-fn 's) env)
                       :test #'string-equal
                       :key #'car)))
     (if result
         (cdr result)
         (when (parent env)
           (,(symbolicate 'get- var-or-fn) name (parent env))))))

(defun get-var (name &optional (env *current-env*))
  (get-smt var))

(defun get-fn (name &optional (env *current-env*))
  (get-smt fn))

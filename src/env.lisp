(in-package :cl-user)
(defpackage clwgc.env
  (:use :cl)
  (:import-from :alexandria
                :symbolicate)
  (:export :<env>
           :vars
           :fns
           :parent
           :make-env
           :*current-env*
           :add-var
           :add-fn
           :get-var
           :get-fn))
(in-package :clwgc.env)

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

(defun make-env (&optional parent)
  (make-instance '<env> :parent parent))

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

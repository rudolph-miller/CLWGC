(in-package :cl-user)
(defpackage clwgc.env
  (:use :cl)
  (:import-from :alexandria
                :symbolicate)
  (:export :<env>
           :vars
           :fns
           :parent
           :layer
           :make-env
           :*current-env*
           :*global-env*
           :with-global-env
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
           :initform nil)
   (layer :initarg :layer
          :reader layer)))

(defun make-env (&optional parent)
  (if parent
      (make-instance '<env> :parent parent :layer (1+ (layer parent)))
      (make-instance '<env> :layer 0)))

(defparameter *current-env* nil)

(defparameter *global-env* nil)

(defmacro with-global-env (&body body)
  `(let* ((*global-env* (make-env))
          (*current-env* *global-env*))
     ,@body))

(defun add-var (name value &optional (env *current-env*))
  (push (cons name value) (vars env)))

(defun add-fn (name fn &optional (env *current-env*))
  (push (cons name fn) (fns env)))

(defmacro get-smt (var-or-fn)
  `(let ((result (find name (,(symbolicate var-or-fn 's) env)
                       :test #'string-equal
                       :key #'car)))
     (if result
         (values (cdr result) nil (layer env))
         (when (parent env)
           (multiple-value-bind (result parent-p layer)
               (,(symbolicate 'get- var-or-fn) name (parent env))
             (declare (ignore parent-p))
             (values result t layer))))))

(defun get-var (name &optional (env *current-env*))
  (get-smt var))

(defun get-fn (name &optional (env *current-env*))
  (get-smt fn))

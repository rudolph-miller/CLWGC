(in-package :cl-user)
(defpackage clwgc.semantic
  (:use :cl
        :clwgc.ast)
  (:import-from :alexandria
                :symbolicate
                :make-keyword)
  (:export :<expression>
           :<constant>
           :<variable>
           :<form>
           :<special-form>
           :<macro-form>
           :<function-form>
           :value
           :exp-type
           :name
           :make-constant
           :make-variable
           :make-special-form
           :make-macro-form
           :make-function-form
           :exp-equal
           :<env>
           :make-env
           :vars
           :fns
           :parent
           :*current-env*
           :add-var
           :add-fn
           :get-var
           :get-fn
           :semanticize
           :semanticize-special-op))
(in-package :clwgc.semantic)

(defclass <expression> () ())

(defclass <constant> (<expression>)
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
  ((name :initarg :name
          :reader name)
   (args :initarg :args
         :reader args)))

(defclass <special-form> (<form>)
  ((type :initarg :type
         :reader exp-type)))

(defclass <macro-form> (<form>) ())

(defclass <function-form> (<form>)
  ((type :initarg :type
         :reader exp-type)))

(defun make-constant (value type)
  (make-instance '<constant> :value value :type type))

(defun make-variable (name value type)
  (make-instance '<variable> :name name :value value :type type))

(defun make-special-form (name args type)
  (make-instance '<special-form> :name name :args args :type type))

(defun make-macro-form (name args)
  (make-instance '<macro-form> :name name :args args))

(defun make-function-form (name args type)
  (make-instance '<function-form> :name name :args args :type type))

(defun exp-equal (a b)
  (and (typep a (type-of b))
       (etypecase a
         (<constant> (and (equal (value a) (value b))
                          (equal (exp-type a) (exp-type b))))
         (<variable> (and (equal (name a) (name b))
                          (exp-equal (value a) (value b))
                          (equal (exp-type a) (exp-type b))))
         (<special-form> (and (equal (name a) (name b))
                              (every #'ast-equal (args a) (args b))
                              (equalp (exp-type a) (exp-type b))))
         (<macro-form> (and (equal (name a) (name b))
                            (every #'ast-equal (args a) (args b))))
         (<function-form> (and (equal (name a) (name b))
                               (every #'ast-equal (args a) (args b))
                               (equalp (exp-type a) (exp-type b)))))))

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

(defparameter *special-ops*
  (mapcar #'(lambda (name)
              (cons name (make-keyword (string-upcase name))))
          (list "block"
                "catch"
                "eval-when"
                "flet"
                "function"
                "go"
                "if"
                "labels"
                "let"
                "let*"
                "load-time-value"
                "lacally"
                "macrolet"
                "multiple-value-call"
                "multiple-value-prog1"
                "progn"
                "prigv"
                "quote"
                "returv-from"
                "setq"
                "symbol-macrolet"
                "tagbody"
                "the"
                "throw"
                "unwind-pretect")))

(defun special-op-p (name)
  (when (member name *special-ops*
                :test #'string-equal
                :key #'car)
    t))

(defun special-op-keyword (name)
  (cdr (assoc name *special-ops*
              :test #'string-equal)))

(defun special-op-name (keyword)
  (car (find keyword *special-ops* :key #'cdr)))

(defun macro-p (obj)
  (typep obj '<macro-form>))

(defgeneric semanticize (obj))

(defmethod semanticize ((obj <integer>))
  (make-constant (content obj) :integer))

(defmethod semanticize ((obj <float>))
  (make-constant (content obj) :float))

(defmethod semanticize ((obj <symbol>))
  (let ((val (get-var (content obj))))
    (if val
        val
        (error "The variable ~a is undefined." (content obj)))))

(defmethod semanticize ((obj <string>))
  (make-constant (content obj) :string))

(defmethod semanticize ((obj <nil>))
  (declare (ignore obj))
  (make-constant *nil* :nil))

(defmethod semanticize ((obj <cons>))
  (let* ((name (content (cons-car obj)))
         (args (cons-rest obj))
         (fn (get-fn name)))
    (cond
      ((special-op-p name) (semanticize-special-op name args))
      ((and fn (macro-p fn)) nil)
      (fn nil)
      (t (error "The function ~a is undefined." name)))))

(defgeneric semanticize-special-op (name args)
  (:method ((name t) args)
    (semanticize-special-op (special-op-keyword name) args)))

(defmethod semanticize-special-op ((keyword (eql :setq)) args)
  (let ((var-name (content (car args)))
        (var-val (cadr args)))
    (add-var var-name (semanticize var-val))
    (make-special-form (special-op-name keyword) args :integer)))

(in-package :cl-user)
(defpackage clwgc.parser
  (:use :cl
        :clwgc.ir
        :clwgc.env
        :named-readtables
        :fare-quasiquote)
  (:import-from :alexandria
                :ensure-list)
  (:export :genir
           :parse))
(in-package clwgc.parser)

(in-readtable :fare-quasiquote)

(defun genir (cons)
  (optima:match cons
    (`(setq ,var ,val ,@rest)
      (let* ((name (symbol-name var))
             (var (get-var name))
             (value (genir val))
             (obj (if var
                      (make-update-variabel var value)
                      (make-variable name value :integer))))
        (if rest
            (genir (cons 'setq rest))
            obj)))
    (`(progn ,@body)
      (make-progn (mapcar #'genir (ensure-list body))))
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
                     name
                     t)))
    (`(let (,@bind-forms) ,@body)
      (make-let (mapcar #'(lambda (form)
                            (cons (symbol-name (car form)) (genir (cadr form))))
                        bind-forms)
                (mapcar #'genir (ensure-list body))))
    (`(if ,pred ,then ,else)
      (make-if (genir pred) (genir then) (genir else)))
    (`(if ,pred ,then)
      (make-if (genir pred) (genir then)))
    (`(,fn-s ,@args)
      (let* ((name (symbol-name fn-s))
             (fn (get-fn name)))
        (if fn
            (make-funcall fn (mapcar #'genir args))
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

(defun parse (string)
  (genir (read-from-string string)))

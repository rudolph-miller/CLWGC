(in-package :cl-user)
(defpackage clwgc.ast
  (:use :cl)
  (:export :*debug-mode*
           :<ast>
           :<atom>
           :<integer>
           :<float>
           :<symbol>
           :<string>
           :<cons>
           :<nil>
           :make-integer
           :make-float
           :make-sym
           :make-str
           :make-cons
           :content
           :cons-car
           :cons-cdr
           :*nil*
           :null-p
           :atom-p
           :cons-p
           :pair-p
           :list-p
           :ast-equal
           :make-lst
           :make-lst*))
(in-package :clwgc.ast)

(defparameter *debug-mode* nil)

(defclass <ast> () ())

(defclass <atom> (<ast>)
  ((content :initarg :content
            :reader content)))

(defclass <integer> (<atom>) ())

(defclass <float> (<atom>) ())

(defclass <symbol> (<atom>) ())

(defclass <string> (<atom>) ())

(defclass <cons> (<ast>)
  ((car :initarg :car
        :reader cons-car)
   (cdr :initarg :cdr
        :reader cons-cdr)))

(defclass <nil> (<ast>) ())

(defparameter *nil* (make-instance '<nil>))

(defmethod cons-car ((obj <nil>))
  *nil*)

(defmethod cons-cdr ((obj <nil>))
  *nil*)

(defun make-integer (integer)
  (make-instance '<integer> :content integer))

(defun make-float (float)
  (make-instance '<float> :content float))

(defun make-sym (symbol)
  (make-instance '<symbol> :content symbol))

(defun make-str (string)
  (make-instance '<string> :content string))

(defun make-cons (car cdr)
  (make-instance '<cons> :car car :cdr cdr))

(defun null-p (obj)
  (eql obj *nil*))

(defun atom-p (obj)
  (or (null-p obj)
      (typep obj '<atom>)))

(defun cons-p (obj)
  (or (null-p obj)
      (typep obj '<cons>)))

(defun pair-p (obj)
  (and (cons-p obj)
       (atom-p (cons-cdr obj))
       (not (null-p (cons-cdr obj)))))

(defun list-p (obj)
  (or (null-p obj)
      (when (cons-p (cons-cdr obj))
          (list-p (cons-cdr obj)))))

(defmethod print-object ((obj <atom>) stream)
  (apply #'format stream
         (if *debug-mode*
             (list "#<~a :content ~a>" (class-name (class-of obj)) (content obj))
             (list "~a" (content obj)))))

(defmethod print-object ((obj <string>) stream)
  (apply #'format stream
         (if *debug-mode*
             (list "#<~a :content ~a>" (class-name (class-of obj)) (content obj))
             (list "\"~a\"" (content obj)))))

(defmethod print-object ((obj <cons>) stream)
  (labels ((sub (cons acc)
             (macrolet ((%push (item)
                          `(push ,item acc)))
               (%push (cons-car cons))
               (if (pair-p cons)
                   (progn (%push ".")
                          (%push (cons-cdr cons)))
                   (if (null-p (cons-cdr cons))
                       acc
                       (sub (cons-cdr cons) acc))))))
    (format stream "(~{~a~^ ~})"
            (nreverse (sub obj nil)))))

(defmethod print-object ((obj <nil>) stream)
  (declare (ignore obj))
  (format stream "NIL"))

(defun ast-equal (obj1 obj2)
  (when (typep obj1 (type-of obj2))
    (if (and (null-p obj1)
             (null-p obj2))
        t
        (etypecase obj1
          (<cons> (and (ast-equal (cons-car obj1)
                                  (cons-car obj2))
                       (ast-equal (cons-cdr obj1)
                                  (cons-cdr obj2))))
          (<atom> (equal (content obj1)
                         (content obj2)))))))

(defun make-lst (&rest objs)
  (reduce #'(lambda (cdr car)
              (make-cons car cdr))
          (cons *nil* (nreverse objs))))

(defun make-lst* (&rest objs)
  (reduce #'(lambda (cdr car)
              (make-cons car cdr))
          (nreverse objs)))

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
           :pair-p))
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

(defparameter *nil* (make-cons nil nil))

(defun null-p (obj)
  (eql obj *nil*))

(defun atom-p (obj)
  (typep obj '<atom>))

(defun cons-p (obj)
  (typep obj '<cons>))

(defun pair-p (obj)
  (and (cons-p obj)
       (atom-p (cons-cdr obj))
       (not (null-p (cons-cdr obj)))))

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

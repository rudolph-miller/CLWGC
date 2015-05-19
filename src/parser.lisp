(in-package :cl-user)
(defpackage clwgc.parser
  (:use :cl
        :cl-ppcre
        :esrap)
  (:shadow :parse)
  (:export :parse))
(in-package clwgc.parser)

(let ((scanner (create-scanner "^([+-]?(\\d)+)\\.?$")))
  (defun integer-p (string)
    (when (scan scanner string) t)))

(let ((scanner (create-scanner "^([+-]?[0-9]*\\.?[0-9]+([eE][+-]?[0-9]+)?)$")))
  (defun float-p (string)
    (when (scan scanner string) t)))

(defrule whitespace (+ (or #\Space #\Newline #\Tab))
  (:constant nil))

(defrule atom (+ (not (or whitespace #\( #\))))
  (:lambda (list)
    (let ((text (text list)))
      (cond
        ((integer-p text)
         (make-integer (read-from-string text)))
        ((float-p text)
         (make-float (read-from-string text)))
        (t
         (if (string-equal text "NIL")
             *nil*
             (make-sym text)))))))

(defrule string (and #\" (* (not #\")) #\")
  (:destructure (op inner cp)
    (declare (ignore op cp))
    (make-str (text inner))))

(defrule list (and #\( (* (or #\. sexp)) (? whitespace) #\))
  (:destructure (op inner w cp)
    (declare (ignore op w cp))
    (let ((len (length inner)))
      (if (or (= len 1)
              (and (= len 2)
                   (null-p (cadr inner))))
          (make-cons (car inner) *nil*)
          (let ((dot (position "." inner :test #'equal)))
            (if dot
                (if (= (1+ dot) (1- len))
                    (apply #'make-lst* (remove "." inner :test #'equal))
                    (error "More than one object follows . in list."))
                (apply #'make-lst inner)))))))

(defrule sexp (and (? whitespace)
                   (or list string atom)
                   (? whitespace))
  (:destructure (w1 inner w2)
    (declare (ignore w1 w2))
    inner))

(defun parse (string)
  (esrap:parse 'sexp string))

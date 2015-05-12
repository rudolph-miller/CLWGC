(in-package :cl-user)
(defpackage clwgc.parser
  (:use :cl
        :cl-ppcre
        :esrap)
  (:shadow :parse))
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
         (read-from-string text))
        ((float-p text)
         (read-from-string text))
        (t text)))))

(defrule string (and #\" (* (not #\")) #\")
  (:destructure (op inner cp)
    (declare (ignore op cp))
    (text inner)))

(defrule list (and #\( (* sexp) (? whitespace) #\))
  (:destructure (op inner w cp)
    (declare (ignore op w cp))
    inner))

(defrule sexp (and (? whitespace)
                   (or list string atom)
                   (? whitespace))
  (:destructure (w1 inner w2)
    (declare (ignore w1 w2))
    inner))

(defun parse (string)
  (esrap:parse 'sexp string))

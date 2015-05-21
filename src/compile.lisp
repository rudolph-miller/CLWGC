(in-package :cl-user)
(defpackage clwgc.compile
  (:use :cl
        :clwgc.ir
        :clwgc.llvm)
  (:import-from :alexandria
                :with-gensyms)
  (:export :gencode))
(in-package :clwgc.compile)

(defparameter *toplevel-p* t)

(defmacro run-if-toplevel (&body body)
  (with-gensyms (run entry result)
    `(let* ((,run (add-function "run" nil :integer))
            (,entry (append-block "entry" ,run)))
       (when *toplevel-p* (move-to ,entry))
       (let ((,result (let ((*toplevel-p* nil))
                        ,@body)))
         (if (and *toplevel-p* (cffi:pointerp ,result))
             (progn (ret ,result)
                    (run ,run))
             ,result)))))

(defgeneric gencode (obj))

(defmethod gencode :around (obj)
  (if *toplevel-p*
      (run-if-toplevel (call-next-method))
      (call-next-method)))

(defmethod gencode ((obj <nil>))
  (declare (ignore obj))
  (constant :bool 0))

(defmethod gencode ((obj <t>))
  (declare (ignore obj))
  (constant :bool 1))

(defmethod gencode ((obj <constant>))
  (constant (exp-type obj) (value obj)))

(defmethod gencode ((obj <variable>))
  (let ((var (init-var (exp-type obj) (gencode (value obj)) (name obj))))
    (setf (slot-value obj 'ptr) var)
    var))

(defmethod gencode ((obj <symbol-value>))
  (load-var (ptr (var obj))))

(defmethod gencode ((obj <let>))
  (loop for var in (vars obj)
        do (gencode var))
  (loop for stm in (body obj)
        for result = (gencode stm)
        finally (return result)))

(defmethod gencode ((obj <progn>))
  (loop for stm in (body obj)
        for result = (gencode stm)
        finally (return result)))

(defmethod gencode ((obj <if>))
  (let ((if-val (gencode (pred obj)))
        (then-b (append-block "if.then"))
        (else-b (append-block "if.else"))
        (end-b (append-block "if.end")))
    (cond-br if-val then-b else-b)
    (move-to then-b)
    (let ((then-val (gencode (then obj))))
      (br end-b)
      (move-to else-b)
      (let ((else-val (gencode (else obj))))
        (br end-b)
        (move-to end-b)
        (incoming :integer (list (cons then-val then-b)
                                 (cons else-val else-b)))))))

(defmethod gencode :around ((obj <lambda>))
  (let* ((return-position *current-position*))
    (prog1 (call-next-method)
      (move-to return-position))))

(defmethod gencode ((obj <lambda>))
  (let* ((name (name obj))
         (arg-t (loop repeat (length (args obj))
                      collecting :integer))
         (fn (add-function-and-move-into (or name "lambda") arg-t :integer)))
    (setf (slot-value obj 'ptr) fn)
    (loop for var in (args obj)
          for name = (name var)
          for i from 0
          for bind = (init-var :integer (elt (params) i) name)
          do (setf (slot-value var 'ptr) bind))
    (ret (loop for stm in (body obj)
               for ret = (gencode stm)
               finally (return ret)))
    (run-pass fn)
    (if name
        name
        fn)))

(defmethod gencode ((obj <funcall>))
  (call (ptr (fn obj)) (mapcar #'gencode (args obj))))

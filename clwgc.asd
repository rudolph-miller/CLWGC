#|
  This file is a part of clwgc project.
  Copyright (c) 2015 Rudolph-Miller
|#

#|
  Author: Rudolph-Miller
|#

(in-package :cl-user)
(defpackage clwgc-asd
  (:use :cl :asdf))
(in-package :clwgc-asd)

(defsystem clwgc
  :version "0.1"
  :author "Rudolph-Miller"
  :license "MIT"
  :depends-on (:alexandria
               :esrap
               :cl-ppcre
               :llvm)
  :components ((:module "src"
                :serial t
                :components
                ((:file "ast")
                 (:file "parser")
                 (:file "semantic")
                 (:file "clwgc"))))
  :description "WIP"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op clwgc-test))))

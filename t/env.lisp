(in-package :cl-user)
(defpackage clwgc-test.env
  (:use :cl
        :prove
        :clwgc.env)
  (:import-from :alexandria
                :symbolicate))
(in-package :clwgc-test.env)

(plan nil)

(let* ((env1 (make-env))
       (env2 (make-env env1))
       (*current-env* env2))
  (subtest "make-env"
    (ok env1
        "without :parent.")

    (is (layer env1)
        0
        "Layer of root env is 0.")

    (ok env2
        "with :parent.")

    (is (layer env2)
        1
        "Layer is incremented."))

  (macrolet ((add-and-get-test (var-or-fn)
               (let ((add-method (symbolicate 'add- var-or-fn))
                     (get-method (symbolicate 'get- var-or-fn)))
                 `(progn
                    (subtest "add"
                      (,add-method "dummy1" :dummy1 env1)
                      (is (length (vars env1))
                          1
                          "with specificd env.")

                      (,add-method "dummy2" :dummy2)
                      (is (length (vars env2))
                          1
                          "with *current-env*."))

                    (subtest "get"
                      (is-values (,get-method "dummy1" env1)
                                 (list :dummy1 nil 0)
                                 "with specificd env.")

                      (is-values (,get-method "dummy2")
                                 (list :dummy2 nil 1)
                                 "with *current-env*.")

                      (is-values (,get-method "dummy1")
                                 (list :dummy1 t 0)
                                 "with parent."))))))
    (subtest "var"
      (add-and-get-test var))

    (subtest "fn"
      (add-and-get-test fn))))

(finalize)

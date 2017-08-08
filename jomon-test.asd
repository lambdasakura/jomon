(in-package :cl-user)
(defpackage jomon-test-asd
  (:use :cl :asdf))
(in-package :jomon-test-asd)

(defsystem jomon-test
  :author "<lambda.sakura@gmail.com>"
  :license "MIT"
  :depends-on (:jomon
               :lack-test
               :osicat
               :sxql
               :datafly
               :jonathan
               :cl-json
               :prove)
  :components ((:module "t"
                :components
                ((:file "jomon")
                 (:file "jomon-model"))))
  :description "Test system for jomon-server"
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))

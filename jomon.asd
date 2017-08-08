(in-package :cl-user)
(defpackage jomon-asd
  (:use :cl :asdf))
(in-package :jomon-asd)

(defsystem jomon
  :version "0.1"
  :author "<lambda.sakura@gmail.com>"
  :license "MIT"
  :depends-on (:clack
               :lack
               :caveman2
               :envy
               :cl-ppcre
               :log4cl
               :datafly
               :sxql
               :uuid
               :jose)
  :components ((:module "src"
                :components
                ((:file "config")
                 (:file "db" :depends-on ("config"))
                 (:file "view" :depends-on ("config"))
                 (:file "model" :depends-on ("db"))
                 (:file "web" :depends-on ("view"))
                 (:file "main" :depends-on ("config" "view" "model")))
                :serial t
                ))
  :description "Jomon Server."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op jomon-test))))

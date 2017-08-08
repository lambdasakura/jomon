(in-package :cl-user)
(defpackage jomon-test
  (:use :cl
        :jomon
        :jomon.model
        :jomon.config
        :jomon.test.utils
        :sxql
        :datafly
        :prove
        :cl-interpol)
  (:import-from :lack.builder
                :builder)
  (:import-from :flexi-streams
                :octets-to-string)
  (:import-from  :jomon.config
                 :productionp
                 :testingp
                 :appenv)
  (:import-from :json
                :decode-json-from-string)
  (:import-from :lack.test
                :generate-env))
(in-package :jomon-test)

(plan nil)
(cl-interpol:enable-interpol-syntax)
(setf (osicat:environment-variable "APP_ENV") "development")

(defparameter test-app (builder
                   (if (productionp)
                       nil
                       :accesslog)
                   (if (getf (config) :error-log)
                       `(:backtrace
                         :output ,(getf (config) :error-log))
                       nil)
                   (when (not (testingp))
                     (if (productionp)
                         nil
                         (lambda (app)
                           (lambda (env)
                             (let ((datafly:*trace-sql* t))
                               (funcall app env))))))
                   jomon.web:*web*))

(destructuring-bind (status headers body)
    (funcall test-app (generate-env "/"))
  (is status 200)
  (is (getf headers :content-type) "application/json")
  (is (jojo:parse (first body)) "hello, world"))

(let ((target-uuid (uuid:make-v4-uuid)))
  (save-account (make-account
              :uuid target-uuid
              :name (format nil "test_account")
              :password (generate-hash (format nil "password"))
              :image (format nil "image")
              :date (princ-to-string (local-time:now))))

  (destructuring-bind (status headers body)
      (funcall test-app (generate-env #?"/auth/"
                                      :method :post
                                      :content
                                      `(("name" . "test_account")
                                        ("password" . "password"))))
    (is status 200)
    (is (getf headers :content-type) "application/json")
    (isnt (getf (jojo:parse (first body)) :|token|) nil))

  (destructuring-bind (status headers body)
      (funcall test-app (generate-env #?"/accounts/${target-uuid}"))
    (is status 200)
    (is (getf headers :content-type) "application/json")
    (is (getf (jojo:parse (first body)) :|uuid|) #?"${target-uuid}")))
(clear-database)

(finalize)

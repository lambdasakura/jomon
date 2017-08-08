(ql:quickload :jomon)

(defpackage jomon.app
  (:use :cl)
  (:import-from :lack.builder
                :builder)
  (:import-from :jomon.web
                :*web*)
  (:import-from :jomon.config
                :config
                :productionp
                :testingp
                :*static-directory*))
(in-package :jomon.app)

(builder
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
 *web*)

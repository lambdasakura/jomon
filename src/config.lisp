(in-package :cl-user)
(defpackage jomon.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*
           :appenv
           :developmentp
           :productionp))
(in-package :jomon.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :jomon))

(defconfig :common
  `())

(defconfig |development|
  `(:debug T
    :database-type :postgres
    :database-connection-spec (:username "postgres"
                               :password "example"
                               :host "db"
                               :database-name "jomon")))

(defconfig |production|
  '())

(defconfig |test|
  '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (asdf::getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))

(defun testingp ()
  (string= (appenv) "testing"))

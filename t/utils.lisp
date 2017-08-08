(in-package :cl-user)
(defpackage jomon.test.utils
  (:use :cl
        :osicat
        :jomon
        :jomon.config
        :jomon.model
        :jomon.db
        :local-time
        :uuid
        :datafly
        :sxql
        :prove)
  (:import-from  :jomon.db
                 :db)
  (:import-from  :jomon.config
                 :appenv)
  (:export :clear-database))
(in-package :jomon.test.utils)

(defun clear-database ()
  (with-connection (db)
    (execute (delete-from :account))
    (execute (delete-from :notebook))
    (execute (delete-from :page))))

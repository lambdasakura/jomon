(in-package :cl-user)
(defpackage jomon.db
  (:use :cl
        :datafly
        :sxql)
  (:import-from :jomon.config
                :config)
  (:import-from :datafly
                :*connection*
                :connect-cached)
  (:export :connection-settings
           :db
           :last-insert-id
           :with-connection
           :insert-test-data
           ))
(in-package :jomon.db)

(defun connection-settings ()
  `(,(config :database-type)
     ,@(config :database-connection-spec)))

(defun db ()
  (apply #'connect-cached (connection-settings)))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     (block :db-execute-block
       (handler-bind ((error #'(lambda (condition)
                                 (format t "~A~%" condition)
                                 (return-from :db-execute-block nil))))
         ,@body))))

(defun last-insert-id (table_name column_name &optional (db :maindb))
  (let ((db-kind (car (cdr (assoc db (config :databases))))))
    (case db-kind
      ((:postgres)
       ;; postgresql use sequence for serial
       ;; this sequence name is 'table_name + "_"  + column_name + "_" + "seq"'
       (let ((seq-name (intern (format nil "~A_~A_seq" table_name column_name))))
         (retrieve-one
          (select :last_value (from seq-name))))))))

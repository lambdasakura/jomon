(in-package :cl-user)
(defpackage jomon-model-test
  (:use :cl
        :osicat
        :jomon
        :jomon.config
        :jomon.model
        :jomon.db
        :jomon.test.utils
        :local-time
        :uuid
        :datafly
        :sxql
        :prove)
  (:import-from  :jomon.db
                 :db)
  (:import-from  :jomon.config
                 :appenv))
(in-package :jomon-model-test)

(setf (osicat:environment-variable "APP_ENV") "development")
(plan nil)

;;
;; Test Starting
;;
(diag "Account Test")
(let ((target-uuid (uuid:make-v4-uuid)))
  (save-account (make-account
              :uuid target-uuid
              :name (format nil "test_account")
              :password (generate-hash (format nil "password"))
              :image (format nil "image")
              :date (princ-to-string (local-time:now))))

  (is (find-account :uuid 0) nil)
  (isnt (find-account :uuid target-uuid) nil)

  (let ((target (find-account :uuid target-uuid)))

    (is (authenticate-account target "wrong_password") nil)
    (is (authenticate-account target "password") t)
    (is (account-name target) "test_account")
    (is (account-image target) "image"))

  (let ((target nil))
    (setf target(find-account :uuid target-uuid))
    (isnt (generate-token target) nil)
    (setf (account-name target) "updated_name")
    (save-account target)
    (setf target (find-account :uuid (account-uuid target)))
    (is (account-name target) "updated_name"))

  (let ((target nil))
    (setf target(find-account :uuid target-uuid))
    (delete-account target)
    (is (find-account :uuid target-uuid) nil))

  (loop for i from 0 to 10
     do (save-account (make-account
                    :uuid (uuid:make-v4-uuid)
                    :name (format nil "test_account~A" i)
                    :password (format nil "password~A" i)
                    :image (format nil "image~A" i)
                    :date (princ-to-string (local-time:now)))))

  (is (length (get-all-account)) 11 "ユーザ数は11")
  (clear-database))

(diag "Page Test")
(with-connection (db)
  (let ((target-account (make-account :uuid (uuid:make-v4-uuid)
                                      :name (format nil "test_account")
                                      :password (format nil "password")
                                      :image (format nil "image")
                                      :date (princ-to-string (local-time:now))))
        (target-uuid (uuid:make-v4-uuid)))
    (save-account target-account)
    (setf target-account (find-account :uuid (account-uuid target-account)))
    (save-page (make-page :uuid target-uuid
                          :title "sample"
                          :accountid (account-id target-account)
                          :updatedat (princ-to-string (local-time:now))
                          :content "content"))
    (isnt (find-page :uuid target-uuid) nil)
    (let ((target (find-page :uuid target-uuid)))
      (isnt (page-account target) nil)
      (is (account-name (page-account target)) "test_account"))

    (let ((target (find-page :uuid target-uuid)))
      (setf (page-title target) "new title")
      (save-page target)
      (setf target (find-page :uuid target-uuid))
      (is (page-title target) "new title"))

    (loop for i from 0 to 10
       do
         (save-page (make-page :uuid (uuid:make-v4-uuid)
                               :title "sample"
                               :accountid (account-id target-account)
                               :updatedat (princ-to-string (local-time:now))
                               :content "content")))
    (is (length (get-all-page)) 12)))
(clear-database)

(with-connection (db)
  (let ((target-account (make-account :uuid (uuid:make-v4-uuid)
                                      :name (format nil "test_account")
                                      :password (format nil "password")
                                      :image (format nil "image")
                                      :date (princ-to-string (local-time:now))))
        (target-notebook nil)
        (target-page-uuid (uuid:make-v4-uuid))
        (target-uuid (uuid:make-v4-uuid)))

    ;; prepare testing
    (save-account target-account)
    (setf target-account (find-account :uuid (account-uuid target-account)))

    ;; create notebook
    (save-notebook (make-notebook :uuid target-uuid
                                  :name "sample"
                                  :accountid (account-id target-account)
                                  :createdat (princ-to-string (local-time:now))
                                  :updatedat (princ-to-string (local-time:now))))
    (setf target-notebook (find-notebook :uuid target-uuid))
    (isnt (find-notebook :uuid target-uuid) nil "notebook was created and found it")

    (setf target-page (make-page :uuid target-page-uuid
                                 :title "sample"
                                 :notebookid (notebook-id target-notebook)
                                 :accountid (account-id target-account)
                                 :updatedat (princ-to-string (local-time:now))
                                 :content "content"))
    (save-page target-page)

    (setf target-notebook (find-notebook :uuid target-uuid))
    (isnt (notebook-account target-notebook) nil "notebook account was created")
    (isnt (notebook-pages target-notebook) nil "notebook pages was created")

    (setf (notebook-name target-notebook) "updated name")
    (save-notebook target-notebook)
    (isnt (find-notebook :uuid target-uuid) nil)
    (is (notebook-name (find-notebook :uuid target-uuid)) "updated name")

    ;; delete notebook
    (delete-notebook target-uuid)
    (is (find-notebook :uuid target-uuid) nil)))
(clear-database)

;; (subtest "ファイル関連のテスト"
;;   (defparameter *test-image-data* (read-test-image))
;;   (diag "ファイルの追加ができる")
;;   (is (create-filedata  :file_name "test.jpg" :data *test-image-data*) nil)
;;   (is (create-filedata  :file_name "test.jpg" :data *test-image-data*) nil)
;;   (is (length (get-all-filedata-from-db)) 2)

;;   (diag "ファイルの取得/検索")
;;   (is (find-filedata-from-db :id 0) nil "存在しないデータはnil")
;;   (isnt (find-filedata-from-db :id 1) nil "データが見つかった")

;;   (diag "ファイルの削除")
;;   (isnt (find-filedata-from-db :id 1) nil "削除前はfiledataが存在している")
;;   (delete-filedata 1)
;;   (is (find-filedata-from-db :id 1) nil  "削除するとfiledataが消えている"))

(finalize)

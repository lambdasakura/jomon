(in-package :cl-user)
(defpackage jomon-test
  (:use :cl
        :jomon
        :jomon.config
        :sxql
        :datafly
        :cl-test-more)
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

(subtest "lack-middleware-auth-basic"
  (let ((app (builder
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
              jomon.web:*web*)))
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"))
      (is status 200)
      (is (getf headers :content-type) "application/json")
      (is (jojo:parse (first body)) "hello, world")
      )))



;; (defun read-fixtures ()
;;   (loop for i in (cl-fad:list-directory
;;                   (merge-pathnames
;;                    "./t/fixtures/markdown/"
;;                    (asdf:system-source-directory :my-evernote-v2-backend)))
;;      collect (read-file-into-string i)))

;; (defun read-test-image ()
;;   (read-file-into-byte-vector
;;    (merge-pathnames
;;     "./t/fixtures/image/test.jpg"
;;     (asdf:system-source-directory :my-evernote-v2-backend))))

;; (defparameter *test-port* 5001)
;; (defparameter *server-url* (format nil "http://localhost:~A/" *test-port*))
;; (defparameter *cookie-jar* (make-instance 'drakma:cookie-jar))
;; (defparameter *test-memo-data* (read-fixtures))
;; (defparameter *test-image-data* (read-test-image))

;; (defparameter test-timestamp0 "2013-10-15T06:40:51.816220Z")
;; (defparameter test-timestamp1 "2014-10-15T06:40:51.816220Z")

;; ;;
;; ;; Set for test environment
;; (defun my-evernote-v2-backend.config:appenv () "testing")

;; ;;
;; ;; Helper Methods

;; (defun insert-test-data ()
;;   (with-connection (db)
;;     (loop for i in '("user1_note" "user2_note" "test_note")
;;        do
;;          (execute (insert-into :notebooks (set= :notebook_name (princ-to-string i)))))
;;     (loop for i from 0 to 100
;;        do
;;          (execute (insert-into :users (set= :user_name (format nil "test_user~A" i)
;;                                             :user_password (format nil "password~A" i)
;;                                             :user_image *test-image-data*
;;                                             ))))
;;     (loop for i in *test-memo-data*
;;        do
;;          (execute (insert-into :notes (set= :note_author 1
;;                                             :note_last_saved_by 2
;;                                             :note_create_date test-timestamp0
;;                                             :note_update_date test-timestamp1
;;                                             :notebook_id 1
;;                                             :note_title 1
;;                                             :note_content i))))))

;; (defun send-request (uri &key (method :get) parameters)
;;   (http-request (concatenate 'string *server-url* uri)
;;                 :method method
;;                 :parameters parameters
;;                 :cookie-jar *cookie-jar*))

;; (defun send-json-request (uri &key (method :get) parameters)
;;   (octets-to-string (send-request uri :method method :parameters parameters)))

;; (defun length-from-json-string (json-string)
;;   (length (json:decode-json-from-string
;;            json-string)))


;; (defun chdir (pathspec)
;;   "change current directory"
;;   (setf *default-pathname-defaults* (truename pathspec))
;;   #+sbcl(sb-posix:chdir pathspec)
;;   #+ccl(ccl::%chdir (namestring pathspec))
;;   #-(or sbcl ccl)(error "no chdir"))


;; ;;
;; ;; change to Project Directory and Initialize DB

;; (chdir (asdf:system-source-directory :my-evernote-v2-backend))
;; (uiop:run-program `("sh" "db_initialize.sh"))

;; (plan nil)

;; ;;
;; ;; Server Start

;; (start :port *test-port* :server :hunchentoot)

;; ;;
;; ;; Login Error

;; (diag "Get Error 403, yet logined")

;; (multiple-value-bind (body
;;                       status-code
;;                       headers
;;                       uri
;;                       stream
;;                       must-close
;;                       reason-phrase)
;;     (send-request "user.json?id=1")
;;   (is reason-phrase "Forbidden")
;;   (is status-code 403))

;; ;; Login
;; (diag "Login")
;; (multiple-value-bind (body
;;                       status-code
;;                       headers
;;                       uri
;;                       stream
;;                       must-close
;;                       reason-phrase)
;;     (send-request "login?user_name=admin&user_password=asdf")
;;   (is reason-phrase "OK")
;;   (is status-code 200))


;; (diag "DB nothing")
;; (multiple-value-bind (body
;;                       status-code
;;                       headers
;;                       uri
;;                       stream
;;                       must-close
;;                       reason-phrase)
;;     (send-request "note.json?id=1")
;;   (is reason-phrase "OK")
;;   (is status-code 200)
;;   (is (octets-to-string body) "null"  "DBが空の時はusers.jsonはnullが返る"))


;; (diag "insert default data")
;; (is (insert-test-data) nil "デフォルトデータをDBに格納")

;; (diag "Get User Data")
;; (isnt (send-json-request "user.json?id=2")
;;       "null"
;;       "DBにデータが入っている場合はusers.jsonはnullは返らない")

;; (is (send-json-request "user.json?id=2")
;;     "{\"userId\":2,\"userName\":\"test_user0\",\"userPassword\":\"password0\"}"
;;     "'users.json?id=2'は、データはuser_id:2のデータが返ってくる")


;; (diag "Get All Users Data")

;; (is (length-from-json-string (send-json-request "users.json"))
;;     102
;;     "users.json APIは初期データのユーザ数は102人でそれと同じ数のjsonデータが返ってきている")

;; (is (car (json:decode-json-from-string
;;           (send-json-request "users.json")))
;;     '((:USER-ID . 1)
;;       (:USER-NAME . "admin")
;;       (:USER-PASSWORD . "password"))
;;     "全ユーザの先頭が初期データの先頭の値と同じになっている")

;; (is (car (last (json:decode-json-from-string
;;                 (send-json-request "users.json"))))
;;     '((:USER-ID . 102)
;;       (:USER-NAME . "test_user100")
;;       (:USER-PASSWORD . "password100"))
;;     "全ユーザの末尾が初期データの末尾の値と同じになっている")


;; (diag "Add User")

;; (isnt (send-json-request "user.json?id=2"
;;                          :method :post
;;                          :parameters '(("user_name" . "test_userXXX")
;;                                        ("user_password" . "password")))
;;       "null"
;;       "POSTで新しくユーザを追加する")

;; (is (length-from-json-string (send-json-request "users.json"))
;;     103
;;     "新しくユーザが追加されたため、users.jsonが返すユーザ数が増えている")


;; (diag "Update User")

;; (is (send-json-request "user.json?id=2"
;;                        :method :put
;;                        :parameters '(("user_name" . "new_sakura")
;;                                      ("user_password" . "password0")))
;;     "null"
;;     "PUTメソッドでユーザの情報(ユーザ名,パスワード)が更新できる")

;; (is (json:decode-json-from-string
;;      (send-json-request "user.json?id=2"))
;;     '((:USER-ID . 2)
;;       (:USER-NAME . "new_sakura")
;;       (:USER-PASSWORD . "password0"))
;;     "取得できるデータのユーザ名もPUTで置き換えた内容になっている")


;; (is (send-json-request "user.json?id=2"
;;                        :method :put
;;                        :parameters '(("user_password" . "password_change")))
;;     "null"
;;     "パスワードだけの変更もできる")

;; (is '((:USER-ID . 2)
;;       (:USER-NAME . "new_sakura")
;;       (:USER-PASSWORD . "password_change"))
;;     (json:decode-json-from-string
;;      (send-json-request "user.json?id=2"))
;;     "取得したパスワードの値も変更した値になっている")

;; (is "null"
;;     (send-json-request "user.json?id=2"
;;                        :method :put
;;                        :parameters '(("user_name" . "lambda_sakura")))
;;     "ユーザ名だけ変更してみる")

;; (is  (json:decode-json-from-string (send-json-request "user.json?id=2"))
;;      '((:USER-ID . 2)
;;        (:USER-NAME . "lambda_sakura")
;;        (:USER-PASSWORD . "password_change"))
;;      "ユーザ名のみ変更できている")


;; (diag "Delete User")

;; (is (length-from-json-string (send-json-request "users.json")) 103
;;     "ユーザの数は削除する前は103")

;; (is "null" (send-json-request "user.json"
;;                               :method :DELETE
;;                               :parameters '(("id" . "2")))
;;     "DELETEメソッドでユーザの削除が出来る")

;; (is (length-from-json-string (send-json-request "users.json")) 102
;;     "ユーザの数が削除したので102になっている")
;; ;;
;; ;; Get Memo

;; (diag "Memo Test")
;; (is (length-from-json-string (send-json-request "notes.json")) 5
;;     "notes.jsonでメモデータ全てが取得でき、初期データで入っているメモの数は5")

;; (is (first (json:decode-json-from-string
;;             (send-json-request "notes.json")))
;;     '((:NOTE-ID . 1) (:NOTE-AUTHOR . 1) (:NOTE-LAST-SAVED-BY . 2)
;;       (:NOTE-CREATE-DATE . "2013-10-15 15:40:51.81622+09")
;;       (:NOTE-UPDATE-DATE . "2014-10-15 15:40:51.81622+09")
;;       (:NOTEBOOK-ID . 1) (:NOTE-TITLE . "1")
;;       (:NOTE-CONTENT .
;;        "#test-title1
;; hogehogehoge
;; "))
;;     "ノートの先頭の内容は初期データと一致している"
;;     )

;; (is (car (last (json:decode-json-from-string
;;                 (send-json-request "notes.json"))))
;;     '((:NOTE-ID . 5) (:NOTE-AUTHOR . 1) (:NOTE-LAST-SAVED-BY . 2)
;;       (:NOTE-CREATE-DATE . "2013-10-15 15:40:51.81622+09")
;;       (:NOTE-UPDATE-DATE . "2014-10-15 15:40:51.81622+09")
;;       (:NOTEBOOK-ID . 1) (:NOTE-TITLE . "1")
;;       (:NOTE-CONTENT .
;;        "#test-title5
;; hogehogehoge
;; "))
;;     "ノートの末尾の内容は初期データと一致している")


;; (isnt nil (http-request (concatenate 'string *server-url* "upload")
;;               :method :post
;;               :form-data nil
;;               :parameters `(("file_name" . "test_image_file")
;;                             ("file_data" . (,(merge-pathnames
;;                                               "t/fixtures/image/test.jpg"
;;                                               (asdf:system-source-directory :my-evernote-v2-backend)))))
;;               :cookie-jar *cookie-jar*)
;;       "ファイルアップロードができる")
;; (stop)
;; (finalize)

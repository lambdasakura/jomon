(in-package :cl-user)
(defpackage jomon.web
  (:use :cl
        :caveman2
        :jomon.config
        :jomon.view
        :jomon.model
        :datafly
        :local-time
        :sxql)
  (:import-from :caveman2
                :*request*
                :request-headers)
  (:import-from :alexandria
                 :read-file-into-byte-vector )
  (:export :*web*))
(in-package :jomon.web)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Default Timezone
;;
(setf local-time:*default-timezone* local-time:+utc-zone+)

(defun current-time-string ()
  (local-time:format-timestring nil (local-time:now)))

(defun check-token (request)
  "validate token and return account"
  (let* ((token (gethash :access-token (request-headers request)))
         (account-info (validate-token token)))
    (if account-info
        (find-account :uuid
                      (cdr (assoc "UUID" account-info :test #'string=)))
        nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routing rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defroute "/" ()
  (render-json "hello, world"))

;;
;; Authenticate
;;
(defroute ("/auth/" :method :POST) (&key |name| |password|)
  "generate token"
  (let ((account (find-account :name |name|)))
    (if (and account
             (authenticate-account account |password|))
        (render-json `(:token ,(generate-token account)))
        (render-json `((:success . nil)
                       (:message . "Invalid parameters"))))))

(defroute ("/auth/") (&key |name| |password|)
  "regenerate token"
  (let ((self (check-token *request*)))
    (if self
        (render-json `(:token ,(generate-token self))))))

;;;
;;; Accounts API
;;;
(defroute ("/accounts/" :method :POST) (&key |name|
                                             |password|
                                             |image|)
  "new account create"
  (let ((account (find-account :name name)))
    (when (null account)
      (let ((new-uuid (uuid:make-v4-uuid)))
        (save-account (make-account
                       :uuid new-uuid
                       :name |name|
                       :password (generate-hash |password|)
                       :date (princ-to-string (local-time:now))))
        (render-json (find-account :uuid new-uuid))))))

(defroute "/accounts/:account_id" (&key account_id)
  "get account information"
  (let ((self (check-token *request*))
        (account (find-account :uuid account_id)))
    (if (and self account)
        (render-json account)
        nil)))

(defroute ("/accounts/:account_id" :method :PUT) (&key account_id
                                                        |name|
                                                        |password|
                                                        |image|)
  "update account information"
  (let ((self (check-token *request*))
        (account (find-account :uuid account_id)))
    (if (and self
             account
             (equalp (account-uuid account) (account-uuid self)))
        (progn
          (if |name| (setf (account-name account) |name|))
          (if |password| (setf (account-password account) (generate-hash |image|)))
          (if |image| (setf (account-image account) |image|))
          (save-account account)
          (render-json  (find-account :uuid account_id)))
        nil)))

;;;
;;; Notebook API
;;;
(defroute ("/notebooks/" :method :POST) (&key |name|)
  "new notebook create"
  (let ((self (check-token *request*)))
    (when self
      (let ((new-uuid (uuid:make-v4-uuid)))
        (save-notebook (make-notebook
                       :uuid new-uuid
                       :name |name|
                       :accountId (account-id self)
                       :createdat (princ-to-string (local-time:now))
                       :updatedat (princ-to-string (local-time:now))))
        (render-json (find-notebook :uuid new-uuid))))))

(defroute "/notebooks/" ()
  "list notebook information"
  (let ((self (check-token *request*)))
    (if self
        (jomon.db:with-connection (jomon.db:db)
          (render-json (account-notebooks self)))
        nil)))

(defroute ("/notebooks/:notebook_id") (&key notebook_id)
  "get notebook information"
  (let ((self (check-token *request*))
        (notebook (find-notebook :uuid notebook_id)))
    (if (and self
             notebook
             (equalp (account-id self) (notebook-accountid notebook)))
        (progn
          (render-json  (find-notebook :uuid notebook_id))))))

(defroute ("/notebooks/:notebook_id" :method :PUT) (&key notebook_id |name|)
  "update notebook information"
  (let ((self (check-token *request*))
        (notebook (find-notebook :uuid notebook_id)))
    (if (and self
             notebook
             (equalp (account-id self) (notebook-accountid notebook)))
        (progn
          (if |name| (setf (notebook-name notebook) |name|))
          (save-notebook notebook)
          (render-json  (find-notebook :uuid notebook_id))))))

;;;
;;; Pages API
;;;
(defroute ("/notebooks/:notebook_id/pages") (&key notebook_id)
  "get notebook information"
  (let ((self (check-token *request*))
        (notebook (find-notebook :uuid notebook_id)))
    (if (and self
             notebook
             (equalp (account-id self) (notebook-accountid notebook)))
        (progn
          (jomon.db:with-connection (jomon.db:db)
            (render-json  (notebook-pages (find-notebook :uuid notebook_id)))))
        nil)))

(defroute "/pages/" ()
  "list page information"
  (let ((self (check-token *request*)))
    (if self
        (jomon.db:with-connection (jomon.db:db)
          (render-json (account-pages self)))
        nil)))

(defroute ("/pages/" :method :post) (&key |notebook_id| |title| |content|)
  "create new page"
  (labels ((check-ownership (account notebook)
                           (or (null notebook)
                             (and notebook
                                  (equal (notebook-accountid notebook)
                                         (account-id self))))))
    (let ((self (check-token *request*))
          (notebook (find-notebook :uuid |notebook_id|)))
    (when (and self (check-ownership account notebook))
      (let ((new-uuid (uuid:make-v4-uuid)))
        (save-page (make-page :uuid new-uuid
                              :title |title|
                              :notebookid (if notebook (notebook-uuid notebook))
                              :accountid (account-id self)
                              :updatedat (princ-to-string (local-time:now))
                              :content |content|))
        (render-json (find-page :uuid new-uuid)))))))

(defroute "/pages/:page_id" (&key page_id)
  "get page information"
  (let ((self (check-token *request*))
        (page (find-page :uuid page_id)))
    (if (and self
             page
             (equalp (account-id self) (page-accountid page)))
          (render-json  page))))

(defroute ("/pages/:page_id" :method :put) (&key page_id |notebook_id| |title| |content|)
  "update page information"
  (let ((self (check-token *request*))
        (page (find-page :uuid page_id)))
    (if (and self
             page
             (equalp (account-id self) (page-accountid page)))
        (progn
          (if |title| (setf (page-title page) |title|))
          (if |content| (setf (page-content page) |content|))
          (save-page page)
          (render-json  (find-page :uuid page_id))))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  ())
;;  (merge-pathnames #P"_errors/404.html" *template-directory*))

(defmethod on-exception ((app <web>) (code (eql 403)))
  (declare (ignore app))
  ())
;;  (merge-pathnames #P"_errors/403.html" *template-directory*))

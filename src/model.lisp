(in-package :cl-user)
(defpackage jomon.model
  (:use :cl
        :caveman2
        :jomon.config
        :jomon.view
        :jomon.db
        :datafly
        :log4cl
        :sxql
        :ironclad
        :jose
        :uuid)
  (:shadowing-import-from #:ironclad #:null)
  (:import-from :local-time
                :now
                :+utc-zone+
                :format-timestring)
  (:export :find-account
           :get-all-account
           :make-account
           :account-id
           :account-uuid
           :account-name
           :account-password
           :account-image
           :account-notebooks
           :account-pages
           :account-createdat
           :account-updatedat
           :save-account
           :create-account
           :delete-account
           :update-account
           :generate-hash
           :generate-token
           :validate-token
           :authenticate-account

           :find-page
           :make-page
           :page-account
           :page-uuid
           :page-title
           :page-content
           :page-createdat
           :page-updatedat
           :save-page
           :create-page
           :delete-page
           :update-page
           :get-all-page

           ;; ノートブック関連
           :make-notebook
           :notebook-id
           :notebook-uuid
           :notebook-name
           :notebook-pages
           :notebook-account
           :notebook-createdat
           :notebook-updatedat
           :get-all-notebook
           :find-notebook
           :save-notebook
           :create-notebook
           :update-notebook
           :delete-notebook

           ;; File関連
           :get-all-filedata-from-db
           :find-filedata-from-db
           :create-filedata
           :read-filedata
           :delete-filedata
           ))

(in-package :jomon.model)

(setf ironclad:*prng* (ironclad:make-prng :FORTUNA :seed :urandom))
(defvar *key* (ironclad:ascii-string-to-byte-array "my$ecret"))
(defvar *token*
  (jose:encode :hs256 *key* '(("hello" . "world"))))

(defun generate-hash (password)
  (setf *prng* (make-prng :FORTUNA :seed :urandom))
  (ironclad:pbkdf2-hash-password-to-combined-string
   (ironclad:ascii-string-to-byte-array password)))

(defun generate-token (account)
    (jose:encode :hs256 *key*
                 `((uuid . ,(account-uuid account))
                   (name . ,(account-name account)))))

(defun validate-token (token)
  (block main
    (handler-bind ((error
                    (lambda (c) (return-from main nil))))
    (jose:decode :hs256 *key* token))))

(defun authenticate-account (account password)
  (setf ironclad:*prng* (make-prng :FORTUNA :seed :urandom))
  (let ((hash (account-password account)))
      (pbkdf2-check-password (ascii-string-to-byte-array password) hash)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                              (subseq source 0 n)
                              acc))
                   (nreverse
                    (cons source acc))))))
    (if source (rec source nil) nil)))

(defun search-query (params)
  (let* ((param-list (group params 2)))
    `(:or ,@(remove-if
             #'null
             (mapcar #'(lambda (x)
                         (if (second x)
                             `(:= ,(first x) ,(second x)))) param-list)))))
;;;
;;; Account model
;;;
(defmodel (account (:has-many (notebooks notebook)
                              (select :*
                                (from :notebook)
                                (where (:= :accountId id))))
                   (:has-many (pages page)
                              (select :*
                                (from :page)
                                (where (:= :accountId id)))))
  id
  uuid
  name
  password
  image
  createdAt
  updatedAt)

(defun create-account (&key name uuid password image date)
  (with-connection (db)
    (dbi:with-transaction (db)
      (execute
       (insert-into :account (set= :name name
                                  :uuid uuid
                                  :password password
                                  :image image
                                  :updatedAt (princ-to-string date)
                                  :createdAt (princ-to-string date)))))))

(defun find-account (&key uuid name)
  (cond (name
         (with-connection (db)
           (retrieve-one
            (select :* (from :account) (where (:= :name name))) :as 'account)))
        (uuid
         (with-connection (db)
           (retrieve-one
            (select :* (from :account) (where (:= :uuid uuid))) :as 'account)))))

(defun get-all-account ()
  (with-connection (db)
    (retrieve-all (select :* (from :account)) :as 'account)))

(defun save-account (account &optional (date (local-time:now)))
  (if (find-account :uuid (account-uuid account))
      (update-account (account-uuid account)
                   :name (account-name account)
                   :password (account-password account)
                   :image (account-image account)
                   :date date)
      (create-account :name (account-name account)
                   :uuid (account-uuid account)
                   :password (account-password account)
                   :image (account-image account)
                   :date date)))

(defun update-account (uuid &key name password image date)
  (with-connection (db)
    (execute
     (update :account (set= :name name
                           :password password
                           :image image
                           :updatedAt (princ-to-string date))
             (where (:= :uuid uuid))))))

(defun delete-account (account)
  (with-connection (db)
    (execute (delete-from :account (where (:= :uuid (account-uuid account)))))))

;;;
;;; Page model
;;;
(defmodel (page (:has-a account (where (:= :id accountId)))
                (:has-a notebook (where (:= :id notebookId))))
  id
  uuid
  title
  notebookId
  accountId
  content
  createdAt
  updatedAt)

(defun save-page (page &optional (date (local-time:now)))
  (if (find-page :uuid (page-uuid page))
      (update-page (page-uuid page)
                   :title (page-title page)
                   :notebook_id (page-notebookid page)
                   :account_id (page-accountid page)
                   :date date
                   :content (page-content page))
      (create-page :uuid (page-uuid page)
                   :title (page-title page)
                   :notebook_id (page-notebookid page)
                   :account_id (page-accountid page)
                   :date date
                   :content (page-content page))))

(defun create-page (&key title uuid account_id content notebook_id date)
  (with-connection (db)
    (dbi:with-transaction (db)
      (execute
       (insert-into :page
         (set= :title title
               :uuid uuid
               :notebookId notebook_id
               :accountId account_id
               :createdAt date
               :updatedAt date
               :content content))))))

(defun update-page (uuid &key title account_id content notebook_id date)
  (with-connection (db)
    (dbi:with-transaction (db)
      (execute
       (update :page
         (set= :title title
               :notebookId notebook_id
               :accountId account_id
               :updatedAt date
               :content content)
         (where (:= :uuid uuid)))))))

(defun find-page (&key uuid)
  (with-connection (db)
      (retrieve-one (select :* (from :page) (where (:= :uuid uuid))) :as 'page)))

(defun delete-page (uuid)
  (with-connection (db)
    (execute
     (delete-from :page (where (:= :uuid uuid))))))

(defun get-all-page ()
  (with-connection (db)
    (retrieve-all (select :* (from :page)))))

;;
;; Notebooks Model
;;
(defmodel (notebook (:has-a account (where (:= :id accountId)))
                    (:has-many (pages page)
                               (select :*
                                 (from :page)
                                 (where (:= :notebookId id)))))
  id
  uuid
  accountId
  name
  createdAt
  updatedAt)

(defun find-notebook (&key uuid)
  (with-connection (db)
      (retrieve-one (select :* (from :notebook) (where (:= :uuid uuid))) :as 'notebook)))


(defun get-all-notebook ()
  (with-connection (db)
    (retrieve-all (select :* (from :notebook) :as 'notebook))))

(defun save-notebook (notebook &optional (date (local-time:now)))
  (if (find-notebook :uuid (notebook-uuid notebook))
      (update-notebook (notebook-uuid notebook)
                       :name (notebook-name notebook)
                       :account_id (notebook-accountid notebook)
                       :date date)
      (create-notebook :uuid (notebook-uuid notebook)
                       :name (notebook-name notebook)
                       :account_id (notebook-accountid notebook)
                       :date date)))

(defun create-notebook (&key name uuid account_id date)
  (with-connection (db)
    (dbi:with-transaction (db)
      (execute
       (insert-into :notebook
         (set= :name name
               :uuid uuid
               :accountId account_id
               :createdAt date
               :updatedAt date))))))

(defun update-notebook (uuid &key name account_id date)
  (with-connection (db)
    (dbi:with-transaction (db)
      (execute
       (update :notebook
         (set= :name name
               :uuid uuid
               :accountId account_id
               :updatedAt date)
         (where (:= :uuid uuid)))))))

(defun delete-notebook (uuid)
  (with-connection (db)
    (execute (delete-from :notebook (where (:= :uuid uuid))))))

;; ;;
;; ;; File Model

;; (defun find-filedata-from-db (&key id name)
;;   (with-connection (db)
;;     (let ((statements (gen-search-query (:file_id id :file_name name ))))
;;       (retrieve-one
;;        (select :*
;;          (from :file)
;;          (where statements))))))

;; (defun get-all-filedata-from-db ()
;;   (with-connection (db)
;;     (retrieve-all
;;      (select (:file_id :file_name)
;;        (from :file)))))

;; (defun create-filedata (&key file_name data)
;;   (with-connection (db)
;;     (dbi:with-transaction (db)
;;       (execute
;;        (insert-into :file
;;          (set= :file_name file_name :file_content data))))))

;; (defun read-filedata (&key id)
;;   (with-connection (db)
;;     (dbi:with-transaction (db)
;;       (retrieve-one
;;        (select :file_content (from :file)
;;                (where (:= :file_id file_id)))))))

;; (defun delete-filedata (id)
;;   (with-connection (db)
;;     (execute
;;      (delete-from :file
;;        (where (:= :file_id id))))))

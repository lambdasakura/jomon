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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routing rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defroute "/" ()
  (render-json "hello, world"))

(defroute ("/auth/" :method :POST) (&key |name| |password|)
  (let ((account (find-account :name |name|)))
    (if (authenticate-account account |password|)
        (render-json `(:token ,(generate-token account))))))

(defroute "/accounts/:account_id" (&key account_id)
  (let ((account (find-account :uuid account_id)))
    (if account
        (render-json account)
        nil)))

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

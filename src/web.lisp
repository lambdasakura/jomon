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

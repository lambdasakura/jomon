(in-package :cl-user)
(defpackage jomon.view
  (:use :cl)
  (:import-from :jomon.config
                :*template-directory*)
  (:import-from :caveman2
                :*response*
                :response-headers)
  (:import-from :datafly
                :encode-json)
  (:export :render-json))
(in-package :jomon.view)

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))

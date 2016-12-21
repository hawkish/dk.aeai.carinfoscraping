;;;; carinfoscraping.lisp

(in-package #:carinfoscraping)

;;; "carinfoscraping" goes here. Hacks and glory await!

(defun i ()
  (parse-request-as (http-get "https://www.tinglysning.dk/tinglysning/unsecrest/soegbil?stelnr=WVWZZZ1HZSB012475")))

(defun http-get (url)
  (let ((drakma:*text-content-types* (cons '("application" . "json")
                                           drakma:*text-content-types*))
	(drakma:*drakma-default-external-format* :utf-8))
    (multiple-value-bind (request-result status-code headers)
        (drakma:http-request url
                         :accept "application/json"
                         :content-type "application/json"
                         :method :get
                         :external-format-out :utf-8
                         :external-format-in :utf-8
                         :redirect 100
                         :parameters '(("a" . "b")))
      (cond ((not (equalp status-code 200)) (error request-result))
            ((not (string= (cdr (assoc ':content-type headers)) "application/json; charset=UTF-8"))) 
            (t request-result)))))
 
(defun parse-request-as (string)
    (json:encode-json string)))





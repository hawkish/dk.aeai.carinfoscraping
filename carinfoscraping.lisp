;;;; carinfoscraping.lisp

(in-package #:carinfoscraping)

;;; "carinfoscraping" goes here. Hacks and glory await!

(defun init ()
    (parse-request (tinglysning-request "https://www.tinglysning.dk/tinglysning/unsecrest/soegbil?stelnr=WVWZZZ1HZSB012475")))

(defun tinglysning-request (url)
  (drakma:http-request url
                       :accept "application/json"
                       :method :get
                       :external-format-out :utf-8
                       :external-format-in :utf-8
                       :redirect 100
                       :want-stream t))
 
(defun parse-request (stream)
  (let ((yason:*parse-object-as* :alist))
    (assoc 'uuid (yason:parse stream))))


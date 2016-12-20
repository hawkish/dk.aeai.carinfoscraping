;;;; carinfoscraping.lisp

(in-package #:carinfoscraping)

;;; "carinfoscraping" goes here. Hacks and glory await!

(defun test ()
    (tinglysning-request "https://www.tinglysning.dk/tinglysning/unsecrest/soegbil?stelnr=WVWZZZ1HZSB012475"))

(defun tinglysning-request (url)
  (let ((stream (drakma:http-request url
                       :accept "application/json"
                       :method :get
                       :external-format-out :utf-8
                       :external-format-in :utf-8
                       :redirect 100
                       :want-stream t)))
    (yason:parse stream)))
 

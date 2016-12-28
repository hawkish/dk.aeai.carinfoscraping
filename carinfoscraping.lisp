;;;; carinfoscraping.lisp

(in-package #:carinfoscraping)

;;; "carinfoscraping" goes here. More hacks and glory await!

(defun i ()
  (handler-case
      (parse-request (http-get-json "https://www.tinglysning.dk/tinglysning/unsecrest/soegbil?stelnr=WVWZZZ1HZSB012475"))
    (on-response-not-ok (ex)
      (format t "An error happened: ~a~%" (text ex)))))

    

(defun http-get-json (url)
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
                         :redirect 100)
      (cond ((not (equalp status-code 200))
             (error 'on-response-not-ok :text (concatenate 'string "HTTP error: " (write-to-string status-code))))
            ((not (string= (cdr (assoc ':content-type headers)) "application/json; charset=UTF-8"))
             (error 'on-response-not-ok :text "Not JSON content.")) 
            (t request-result)))))
 
(defun parse-request (json-string)
  (let ((data (json:decode-json-from-string json-string)))
    (get-uuid data)))

(defun get-uuid (alist)
  (cdr (assoc :UUID (first (get-items alist)))))

(defun get-items (alist)
  (rest (assoc :ITEMS alist)))

(define-condition on-response-not-ok (error)
  ((text :initarg :text :reader text))) 

(define-condition on-response-not-json (error)
  ((text :initarg :text :reader text))) 



;;;; carinfoscraping.lisp

(in-package #:carinfoscraping)

;;; "carinfoscraping" goes here. More hacks and glory await!

(defparameter *cookie-jar* (make-instance 'drakma:cookie-jar))

(defun requests ()
  (handler-case
     
      (let* ((first-response (first-request "https://www.tinglysning.dk/tinglysning/unsecrest/soegbil?stelnr=WVWZZZ1HZSB012475"))
             (uuid (get-uuid (parse-request (first first-response))))
             (second-response (second-request (concatenate 'string "https://www.tinglysning.dk/tinglysning/unsecrest/bil/uuid/" uuid "?xhtml=false"))))
        (print second-response))
    (on-response-not-ok (ex)
      (format t "An error happened: ~a~%" (text ex)))))

(defun second-request (url)
  (let ((drakma:*text-content-types* (cons '("application" . "xml")
                                           drakma:*text-content-types*))
	(drakma:*drakma-default-external-format* :utf-8))
    (multiple-value-bind (request-result status-code)
        (drakma:http-request url
                         :accept "application/json"
                         :method :get
                         :cookie-jar *cookie-jar*)
      (cond ((not (equalp status-code 200))
             (error 'on-response-not-ok :text (concatenate 'string "HTTP error: " (write-to-string status-code))))
            (t (list request-result status-code))))))
    
(defun first-request (url)
  (let ((drakma:*text-content-types* (cons '("application" . "json")
                                           drakma:*text-content-types*))
	(drakma:*drakma-default-external-format* :utf-8))
    (multiple-value-bind (request-result status-code headers)
        (drakma:http-request url
                         :accept "application/json"
                         :content-type "application/json"
                         :method :get
                         :cookie-jar *cookie-jar*)
      (cond ((not (equalp status-code 200))
             (error 'on-response-not-ok :text (concatenate 'string "HTTP error: " (write-to-string status-code))))
            ((not (string= (cdr (assoc ':content-type headers)) "application/json; charset=UTF-8"))
             (error 'on-response-not-ok :text "Not JSON content.")) 
            (t (list request-result status-code))))))

(defun parse-request (json-string)
  (json:decode-json-from-source json-string))

(defun get-uuid (alist)
  (cdr (assoc :UUID (first (get-items alist)))))

(defun get-items (alist)
  (rest (assoc :ITEMS alist)))

(define-condition on-response-not-ok (error)
  ((text :initarg :text :reader text))) 

(define-condition on-response-not-json (error)
  ((text :initarg :text :reader text))) 



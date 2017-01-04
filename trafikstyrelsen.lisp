;;;; trafikstyrelsen.lisp

(in-package #:carinfoscraping)

;;; "trafikstyrelsen" goes here. More hacks and glory await!

(defun request-trafikstyrelsen ()
  (handler-case
      (let* ((response (first-trafikstyrelsen-request "http://selvbetjening.trafikstyrelsen.dk/Sider/resultater.aspx?Reg=AF12345")))
             
        
        (print response))
    (on-response-not-ok (ex)
      (format t "An error happened: ~a~%" (text ex)))))


(defun first-trafikstyrelsen-request (url)
  (let ((drakma:*text-content-types* (cons '("text" . "html")
                                           drakma:*text-content-types*))
	(drakma:*drakma-default-external-format* :utf-8))
    (multiple-value-bind (request-result status-code headers)
        (drakma:http-request url
                         :accept "text/html"
                         :method :get)
                         ;;:cookie-jar *cookie-jar*)
      (cond ((not (equalp status-code 200))
             (error 'on-response-not-ok :text (concatenate 'string "HTTP error: " (write-to-string status-code))))
            ((not (string= (cdr (assoc ':content-type headers)) "text/html; charset=utf-8"))
             (error 'on-response-not-ok :text "Not HTML content.")) 
            (t request-result)))))

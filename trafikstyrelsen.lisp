;;;; trafikstyrelsen.lisp

(in-package #:dk.aeai.carinfoscraping)

;;; "trafikstyrelsen" goes here. More hacks and glory await!

(defun request-trafikstyrelsen ()
  (handler-case
      (let* ((response (trafikstyrelsen-request "http://selvbetjening.trafikstyrelsen.dk/Sider/resultater.aspx?Reg=AF12345"))
             (link (first (get-surveyor-links (parse-html response))))
             (response (trafikstyrelsen-request (concatenate 'string "http://selvbetjening.trafikstyrelsen.dk" link))))
             
        (print response)
        (print link))
    (on-response-not-ok (ex)
      (format t "An error happened: ~a~%" (text ex)))))

(defun trafikstyrelsen-request (url)
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

(defun get-surveyor-links (dom)
  (mapcar #'replace-extra-tags
          (remove-if (complement #'(lambda (x) (is-match "location.href" x)))
           (split-at-quote
            (serialize-to-string
             (get-onclick dom))))))

(defun parse-html (source)
  "Parse a string into a Plump DOM."
  (plump:parse source))

(defun get-onclick (dom)
  "Returns Plump DOMs with TRs containing both a class and an onclick attribute."
  (clss:select "tr[class][onclick]" dom))

(defun serialize-to-string (dom)
  "Returns a string serialized from a Plump DOM"
  (plump:serialize dom nil))

(defun split-at-quote (string)
  "Returns a list of strings from a string split at all quotes."
  (cl-utilities:split-sequence #\" string))

(defun is-match (substring string)
  "Returns true if the substring is any part of the string."
  (not (not (search substring string))))

(defun replace-extra-tags (string)
  "Replaces three substrings in a string."
  (let* ((result (cl-ppcre:regex-replace "location.href=&quot;" string ""))
        (result (cl-ppcre:regex-replace "&quot;" result "")))
    (cl-ppcre:regex-replace "&amp;" result "&")))

(defun get-class (name dom)
  (clss:select (concatenate 'string "div[class=" name "]") dom))

(defun get-pairvalues (name dom)
  (let* ((elements (clss:select (concatenate 'string "div[class=" name "]") dom)))
    (iterate (iterate:for plump-dom:element iterate:in elements)
             (plump:text element))))
                  

(defparameter *html*
  "<div class=\"floatLeft grid6\">
    <h2>Køretøj</h2>
	<div class=\"pairName\">Mærke</div>
	<div class=\"pairValue\">YAMAHA</div>
	<div class=\"pairName\">Model</div>
	<div class=\"pairValue\">XJ</div>
	<div class=\"pairName\">Køretøjsart</div>
	<div class=\"pairValue\">Motorcykel</div>
    <div class=\"pairName\">Reg.nr.</div>
	<div class=\"pairValue\">EZ12647</div>
	<div class=\"pairName\">Stelnr.</div>
	<div class=\"pairValue\">11M007467</div>
	<div class=\"pairName\">Køretøjs-ID</div>
	<div class=\"pairValue\">4016507198411177</div>
	<div class=\"clear\"></div>
	<br /><br />
</div>
<div class=\"floatLeft grid6\">
	<h2>Synsdetaljer</h2><div class=\"pairName\">Synsart</div>
	<div class=\"pairValue\">Registreringssyn</div>
	<div class=\"pairName\">Synstype</div>
	<div class=\"pairValue\">Første syn</div>
	<div class=\"pairName\">Synsdato</div>
	<div class=\"pairValue\">12-05-2010</div>
    <div class=\"pairName\">Sluttid</div>
	<div class=\"pairValue\">09:37</div>
	<div class=\"pairName\">Km-stand</div>
	<div class=\"pairValue\">65.000</div>
	<div class=\"pairName\">Synsresultat</div>
	<div class=\"pairValue\">Godkendt</div>
	<div class=\"pairName\">Sidste frist for omsyn/genfremstilling</div>
	<div class=\"pairValue\"></div>
	<div class=\"clear\"></div>
	<br />")

;;  (clss:select "div[class=pairValue]" (PARSE-HTML *html*)))

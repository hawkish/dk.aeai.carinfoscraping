;;;; carinfoscraping.lisp

(in-package #:carinfoscraping)

;;; "carinfoscraping" goes here. More hacks and glory await!

(defparameter *cookie-jar* (make-instance 'drakma:cookie-jar))

(defun requests ()
  (handler-case
      (let* ((first-response (first-request "https://www.tinglysning.dk/tinglysning/unsecrest/soegbil?stelnr=WVWZZZ1HZSB012475"))
             (uuid (get-uuid (parse-request first-response)))
             (second-response (second-request (concatenate 'string "https://www.tinglysning.dk/tinglysning/unsecrest/bil/uuid/" uuid "?xhtml=false"))))
        ;;(print second-response))
        ;;(cxml:parse second-response (cxml-dom:make-dom-builder)))
        (cxml:make-source second-response))
    (on-response-not-ok (ex)
      (format t "An error happened: ~a~%" (text ex)))))

(defun second-request (url)
  (let ((drakma:*text-content-types* (cons '("application" . "xml")
                                           drakma:*text-content-types*))
	(drakma:*drakma-default-external-format* :utf-8))
    (multiple-value-bind (request-result status-code)
        (drakma:http-request url
                         :accept "application/json"
                         :method :get)
                         ;;:cookie-jar *cookie-jar*)
      (cond ((not (equalp status-code 200))
             (error 'on-response-not-ok :text (concatenate 'string "HTTP error: " (write-to-string status-code))))
            (t request-result)))))
    
(defun first-request (url)
  (let ((drakma:*text-content-types* (cons '("application" . "json")
                                           drakma:*text-content-types*))
	(drakma:*drakma-default-external-format* :utf-8))
    (multiple-value-bind (request-result status-code headers)
        (drakma:http-request url
                         :accept "application/json"
                         :content-type "application/json"
                         :method :get)
                         ;;:cookie-jar *cookie-jar*)
      (cond ((not (equalp status-code 200))
             (error 'on-response-not-ok :text (concatenate 'string "HTTP error: " (write-to-string status-code))))
            ((not (string= (cdr (assoc ':content-type headers)) "application/json; charset=UTF-8"))
             (error 'on-response-not-ok :text "Not JSON content.")) 
            (t request-result)))))

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



(defparameter *source*
  "<BilSummariskHentResultat xmlns=\"http://rep.oio.dk/tinglysning.dk/service/message/elektroniskakt/1/\" xmlns:ns=\"http://rep.oio.dk/tinglysning.dk/schema/elektroniskakt/1/\" xmlns:ns1=\"http://rep.oio.dk/tinglysning.dk/schema/model/1/\" xmlns:xd=\"http://www.w3.org/2000/09/xmldsig#\">
  <ns:BilSummarisk>
    <ns:BilStamoplysninger>
      <ns1:BilIdentifikator>
        <ns1:Stelnummer>WVWZZZ1HZSB012475</ns1:Stelnummer>
      </ns1:BilIdentifikator>
      <ns1:BilStruktur>
        <ns1:BilMaerke>
          <ns1:BilFabrikatTekst>VOLKSWAGEN</ns1:BilFabrikatTekst>
          <ns1:BilModelTekst>GOLF</ns1:BilModelTekst>
          <ns1:BilVariantTekst>1,8</ns1:BilVariantTekst>
        </ns1:BilMaerke>
        <ns1:RegistreringsnummerTekst>EF41859</ns1:RegistreringsnummerTekst>
        <ns1:FoersteRegistreringsAar>1994</ns1:FoersteRegistreringsAar>
      </ns1:BilStruktur>
    </ns:BilStamoplysninger>
    <ns1:ModelId>1174088768</ns1:ModelId>
  </ns:BilSummarisk>
  <ns:UdskriftDatoTid>2016-12-30T11:40:18.951+01:00</ns:UdskriftDatoTid>
  <ns:AnmeldelseModtagetIndikator>false</ns:AnmeldelseModtagetIndikator>
  <xd:Signature>
    <xd:SignedInfo>
      <xd:CanonicalizationMethod Algorithm=\"\"/>
      <xd:SignatureMethod Algorithm=\"\"/>
      <xd:Reference>
        <xd:DigestMethod Algorithm=\"\"/>
        <xd:DigestValue/>
      </xd:Reference>
    </xd:SignedInfo>
    <xd:SignatureValue/>
  </xd:Signature>
</BilSummariskHentResultat>")

(defun parse-xml (source)
  (let* ((document (cxml:parse source (cxml-dom:make-dom-builder)))
         (stelnummer (get-text-value document "ns1:Stelnummer"))
         (bilfabrikant (get-text-value document "ns1:BilFabrikatTekst"))
         (bilmodel (get-text-value document "ns1:BilModelTekst"))
         (bilvariant (get-text-value document "ns1:BilVariantTekst"))
         (registreringsnummer (get-text-value document "ns1:RegistreringsnummerTekst"))
         (foersteregistreringsaar (get-text-value document "ns1:FoersteRegistreringsAar")))
    (list stelnummer bilfabrikant bilmodel bilvariant registreringsnummer foersteregistreringsaar)))

(defun get-text-value (document tag-name)
  (let* ((node (dom:item (dom:get-elements-by-tag-name document tag-name) 0))
         (node-child (dom:first-child node)))
    (if (dom:text-node-p node-child)
        (dom:node-value node-child)
        ())))

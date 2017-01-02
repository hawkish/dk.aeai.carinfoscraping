;;;; carinfoscraping.lisp

(in-package #:carinfoscraping)

;;; "carinfoscraping" goes here. More hacks and glory await!

(defparameter *cookie-jar* (make-instance 'drakma:cookie-jar))

(define-condition on-response-not-ok (error)
  ((text :initarg :text :reader text))) 

(define-condition on-response-not-json (error)
  ((text :initarg :text :reader text)))


(defun requests ()
  (handler-case
      (let* ((first-response (first-request "https://www.tinglysning.dk/tinglysning/unsecrest/soegbil?stelnr=VF12RFL1H49621453"))
             (uuid (get-uuid (parse-request first-response)))
             (second-response (second-request (concatenate 'string "https://www.tinglysning.dk/tinglysning/unsecrest/bil/uuid/" uuid "?xhtml=false"))))
        (print second-response)
        (read-xml second-response))
        ;;(print second-response))
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

(defun maybe-convert-to-keyword (js-name)
  (or (find-symbol (string-upcase js-name) :keyword)
      js-name))

(defun parse-request (json-string)
  (let* ((yason:*parse-object-as* :alist)
         (yason:*parse-object-key-fn* #'maybe-convert-to-keyword))
    (yason:parse json-string)))

(defun get-uuid (alist)
  (cdr (assoc :UUID (first (get-items alist)))))

(defun get-items (alist)
  (rest (assoc :items alist)))

(defun parse-xml(source)
  (cxml:parse source (cxml-dom:make-dom-builder)))

(defun read-xml (source)
  (let* ((document (parse-xml source)))
    (list
     (cons "tinglysningsdato" (get-text-element-by-tag document "ns1:TinglysningsDato"))
     (cons "prioritetnummer" (get-text-element-by-tag document"ns1:PrioritetNummer"))
     (cons "haeftelsetype" (get-text-element-by-tag document "ns:HaeftelseType"))
     (cons "legalunitname" (get-text-element-by-tag document "ns2:LegalUnitName"))
     (cons "cvrnumberidentifier" (get-text-element-by-tag document "ns2:CVRnumberIdentifier"))
     (cons "personname" (get-text-element-by-tag document "ns7:PersonName"))
     (cons "birthdate" (get-text-element-by-tag document "ns8:BirthDate"))
     (cons "beloebvaerdi" (get-text-element-by-tag document "ns1:BeloebVaerdi"))
     (cons "valutakode" (get-text-element-by-tag document "ns1:ValutaKode"))
     (cons "haeftelserentepaalydendesats" (get-text-element-by-tag document "ns1:HaeftelseRentePaalydendeSats"))
     (cons "tillaegstekstsamling" (concatenate-text-sections document))
     (cons "stelnummer" (get-text-element-by-tag document "ns1:Stelnummer"))
     (cons "bilfabrikant" (get-text-element-by-tag document "ns1:BilFabrikatTekst"))
     (cons "bilmodel" (get-text-element-by-tag document "ns1:BilModelTekst"))
     (cons "bilvariant" (get-text-element-by-tag document "ns1:BilVariantTekst"))
     (cons "registreringsnummer" (get-text-element-by-tag document "ns1:RegistreringsnummerTekst"))
     (cons "foersteregistreringsaar" (get-text-element-by-tag document "ns1:FoersteRegistreringsAar")))))

(defun get-text-element (node)
  (let* ((node-child
          (if (dom:element-p node)
              (dom:first-child node)
              (values))))
    (if (dom:text-node-p node-child)
        (dom:node-value node-child)
        (values))))

(defun concatenate-text-sections (document)
  (let* ((nodelist (dom:get-elements-by-tag-name document "ns1:Afsnit"))
         (result '()))
    (dom:do-node-list (node nodelist)
      (push (get-text-element node) result))
    (format nil "~{~a~^. ~}" result)))

(defun get-text-element-by-tag (document tag-name)
    (get-text-element (dom:item (dom:get-elements-by-tag-name document tag-name) 0)))


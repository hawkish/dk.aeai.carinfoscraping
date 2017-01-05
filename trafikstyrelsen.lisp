;;;; trafikstyrelsen.lisp

(in-package #:carinfoscraping)

;;; "trafikstyrelsen" goes here. More hacks and glory await!

(defun request-trafikstyrelsen ()
  (handler-case
      (let* ((response (trafikstyrelsen-request "http://selvbetjening.trafikstyrelsen.dk/Sider/resultater.aspx?Reg=AF12345"))
             (link (first (get-surveyor-links (parse-response response))))
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

(defparameter *html*
  "   <table id=\"tblInspections\" class=\"reports responsive\">
                        <thead>
                            <tr>
                                <th>Dato</th>
                                <th>Resultat</th>
                                <th>Km-stand</th>
                                <th>Reg.nr.</th>
                                <th class=\"hideOnSmall\">Gem/send</th>
                            </tr>
                        </thead>
                        <tbody>
                
                    <tr class=\"odd\" onclick=\"location.href=&quot;/Sider/synsrapport.aspx?Inspection=8974365&amp;Vin=11M007467&quot;\">
						<td>12-05-2010</td>
						<td><span class=\"noLink infoIcon\" title='Godkendt'>GOD</span></td>
						<td>65.000</td>
						<td>EZ12647</td>
						<td class=\"hideOnSmall\">
                            <a id=\"ctl00_m_g_6f9923c7_5864_4e71_bcd8_017cfab1946d_ctl00_rptInspections_ctl01_lnkSave\" class=\"saveIcon\" href=\"/_layouts/Web.Inspections/SaveReport.aspx?Inspection=8974365&amp;Vin=11M007467\"></a>
                            <a id=\"ctl00_m_g_6f9923c7_5864_4e71_bcd8_017cfab1946d_ctl00_rptInspections_ctl01_lnkSend\" class=\"sendIcon send popup\" href=\"../_CONTROLTEMPLATES/Web.Inspections.WebParts/SearchResultWebPart/#8974365\"></a>
                        </td>
					</tr>
					
                
                    <tr class=\"even\" onclick=\"location.href=&quot;/Sider/synsrapport.aspx?Inspection=509709&amp;Vin=11M007467&quot;\">
						<td>10-10-2005</td>
						<td><span class=\"noLink infoIcon\" title='Godkendt'>GOD</span></td>
						<td></td>
						<td></td>
						<td class=\"hideOnSmall\">
                            <a id=\"ctl00_m_g_6f9923c7_5864_4e71_bcd8_017cfab1946d_ctl00_rptInspections_ctl02_lnkSave\" class=\"saveIcon\" href=\"/_layouts/Web.Inspections/SaveReport.aspx?Inspection=509709&amp;Vin=11M007467\"></a>
                            <a id=\"ctl00_m_g_6f9923c7_5864_4e71_bcd8_017cfab1946d_ctl00_rptInspections_ctl02_lnkSend\" class=\"sendIcon send popup\" href=\"../_CONTROLTEMPLATES/Web.Inspections.WebParts/SearchResultWebPart/#509709\"></a>
                        </td>
					</tr>
					
                
                        </tbody>
                    </table>")

(defun get-surveyor-links (dom)
  (mapcar #'replace-extra-tags
          (remove-if (complement #'(lambda (x) (is-match "location.href" x)))
           (split-at-quote
            (serialize-to-string
             (get-onclick dom))))))

(defun parse-response (source)
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

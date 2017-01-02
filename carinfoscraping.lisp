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

(defun parse-request (json-string)
  (json:decode-json-from-source json-string))

(defun get-uuid (alist)
  (cdr (assoc :UUID (first (get-items alist)))))

(defun get-items (alist)
  (rest (assoc :ITEMS alist)))

(defun parse-xml(source)
  (cxml:parse source (cxml-dom:make-dom-builder)))

(defun read-xml (source)
  (let* ((document (parse-xml source)))
    (list
     (cons "tinglysningsdato" (get-text-value document "ns1:TinglysningsDato"))
     (cons "prioritetnummer" (get-text-value document"ns1:PrioritetNummer"))
     (cons "haeftelsetype" (get-text-value document "ns:HaeftelseType"))
     (cons "legalunitname" (get-text-value document "ns2:LegalUnitName"))
     (cons "cvrnumberidentifier" (get-text-value document "ns2:CVRnumberIdentifier"))
     (cons "personname" (get-text-value document "ns7:PersonName"))
     (cons "birthdate" (get-text-value document "ns8:BirthDate"))
     (cons "beloebvaerdi" (get-text-value document "ns1:BeloebVaerdi"))
     (cons "valutakode" (get-text-value document "ns1:ValutaKode"))
     (cons "haeftelserentepaalydendesats" (get-text-value document "ns1:HaeftelseRentePaalydendeSats"))
     (cons "stelnummer" (get-text-value document "ns1:Stelnummer"))
     (cons "bilfabrikant" (get-text-value document "ns1:BilFabrikatTekst"))
     (cons "bilmodel" (get-text-value document "ns1:BilModelTekst"))
     (cons "bilvariant" (get-text-value document "ns1:BilVariantTekst"))
     (cons "registreringsnummer" (get-text-value document "ns1:RegistreringsnummerTekst"))
     (cons "foersteregistreringsaar" (get-text-value document "ns1:FoersteRegistreringsAar")))))

(defparameter *xml*
  "<BilSummariskHentResultat xmlns=\"http://rep.oio.dk/tinglysning.dk/service/message/elektroniskakt/1/\" xmlns:ns=\"http://rep.oio.dk/tinglysning.dk/schema/elektroniskakt/1/\" xmlns:ns1=\"http://rep.oio.dk/tinglysning.dk/schema/model/1/\" xmlns:ns2=\"http://rep.oio.dk/cvr.dk/xml/schemas/2005/03/22/\" xmlns:ns3=\"http://rep.oio.dk/xkom.dk/xml/schemas/2006/01/06/\" xmlns:ns4=\"http://rep.oio.dk/xkom.dk/xml/schemas/2005/03/15/\" xmlns:ns5=\"http://rep.oio.dk/cpr.dk/xml/schemas/core/2005/03/18/\" xmlns:ns6=\"http://rep.oio.dk/ebxml/xml/schemas/dkcc/2003/02/13/\" xmlns:ns7=\"http://rep.oio.dk/itst.dk/xml/schemas/2006/01/17/\" xmlns:ns8=\"http://rep.oio.dk/ebxml/xml/schemas/dkcc/2005/03/15/\" xmlns:xd=\"http://www.w3.org/2000/09/xmldsig#\">
  <ns:BilSummarisk>
    <ns:BilStamoplysninger>
      <ns1:BilIdentifikator>
        <ns1:Stelnummer>VF12RFL1H49621453</ns1:Stelnummer>
      </ns1:BilIdentifikator>
      <ns1:BilStruktur>
        <ns1:BilMaerke>
          <ns1:BilFabrikatTekst>RENAULT</ns1:BilFabrikatTekst>
          <ns1:BilModelTekst>Captur</ns1:BilModelTekst>
          <ns1:BilVariantTekst>dCi 90</ns1:BilVariantTekst>
        </ns1:BilMaerke>
        <ns1:RegistreringsnummerTekst>AB12345</ns1:RegistreringsnummerTekst>
        <ns1:FoersteRegistreringsAar>2013</ns1:FoersteRegistreringsAar>
      </ns1:BilStruktur>
    </ns:BilStamoplysninger>
    <ns:HaeftelseSummariskSamling>
      <ns:HaeftelseSummarisk>
        <ns1:DokumentRevisionIdentifikator>
          <ns1:DokumentIdentifikator>dea6daaa-ffb5-4450-b224-09a9421df589</ns1:DokumentIdentifikator>
          <ns1:RevisionNummer>1</ns1:RevisionNummer>
        </ns1:DokumentRevisionIdentifikator>
        <ns1:TinglysningsDato>2016-01-26T00:00:00.000+01:00</ns1:TinglysningsDato>
        <ns1:Pantrettighed>
          <ns1:RettighedIdentifikator>a851a003-1110-4fe7-8520-bc11bec38e2f</ns1:RettighedIdentifikator>
          <ns1:PrioritetNummer>1</ns1:PrioritetNummer>
        </ns1:Pantrettighed>
        <ns:DokumentAlias>
          <ns:DokumentAliasIdentifikator>20160126-1007035433</ns:DokumentAliasIdentifikator>
        </ns:DokumentAlias>
        <ns:HaeftelseType>udlaeg</ns:HaeftelseType>
        <ns:DokumentOverfoertIndikator>false</ns:DokumentOverfoertIndikator>
        <ns:KreditorInformationSamling>
          <ns1:RolleInformation>
            <ns1:VirksomhedSimpelIdentifikator>
              <ns2:LegalUnitName>DONG ENERGY SALES &amp; DISTRIBUTION A/S</ns2:LegalUnitName>
              <ns2:CVRnumberIdentifier>20214414</ns2:CVRnumberIdentifier>
            </ns1:VirksomhedSimpelIdentifikator>
            <ns3:AddressSpecific>
              <ns4:AddressAccess>
                <ns5:MunicipalityCode>0607</ns5:MunicipalityCode>
                <ns5:StreetCode>5041</ns5:StreetCode>
                <ns6:StreetBuildingIdentifier>53</ns6:StreetBuildingIdentifier>
              </ns4:AddressAccess>
            </ns3:AddressSpecific>
          </ns1:RolleInformation>
        </ns:KreditorInformationSamling>
        <ns:DebitorInformationSamling>
          <ns1:RolleInformation>
            <ns1:PersonSimpelIdentifikator>
              <ns7:PersonName>Sabri Elhaj Moussa</ns7:PersonName>
              <ns8:BirthDate>1977-06-05</ns8:BirthDate>
            </ns1:PersonSimpelIdentifikator>
            <ns3:AddressSpecific>
              <ns4:AddressAccess>
                <ns5:MunicipalityCode>0155</ns5:MunicipalityCode>
                <ns5:StreetCode>1889</ns5:StreetCode>
                <ns6:StreetBuildingIdentifier>122</ns6:StreetBuildingIdentifier>
              </ns4:AddressAccess>
              <ns6:FloorIdentifier>st</ns6:FloorIdentifier>
              <ns6:SuiteIdentifier>tv</ns6:SuiteIdentifier>
            </ns3:AddressSpecific>
          </ns1:RolleInformation>
        </ns:DebitorInformationSamling>
        <ns1:HaeftelseBeloeb>
          <ns1:BeloebValuta>
            <ns1:BeloebVaerdi>72183</ns1:BeloebVaerdi>
            <ns1:ValutaKode>DKK</ns1:ValutaKode>
          </ns1:BeloebValuta>
        </ns1:HaeftelseBeloeb>
        <ns1:HaeftelseRente>
          <ns1:HaeftelseRenteFast>
            <ns1:HaeftelseRentePaalydendeSats>0</ns1:HaeftelseRentePaalydendeSats>
            <ns1:HaeftelseRenteSatsForeloebigIndikator>false</ns1:HaeftelseRenteSatsForeloebigIndikator>
          </ns1:HaeftelseRenteFast>
        </ns1:HaeftelseRente>
        <ns:TillaegstekstSamling>
          <ns1:TekstAngivelse>
            <ns1:TekstGruppe>
              <ns1:Afsnit>Advarsel: Køretøjet i anmeldelsen kan være pantsat efter de før 1/6 1993 gældende regler.</ns1:Afsnit>
            </ns1:TekstGruppe>
          </ns1:TekstAngivelse>
          <ns1:TekstAngivelse>
            <ns1:TekstGruppe>
              <ns1:Afsnit>Advarsel: Ejeren: 101076XXXX i Motorregisteret af bilen med stelnr: VF12RFL1H49621453 findes ikke blandt debitorerne i anmeldelsen. Kontroller stelnr</ns1:Afsnit>
            </ns1:TekstGruppe>
          </ns1:TekstAngivelse>
          <ns1:TekstAngivelse>
            <ns1:TekstGruppe>
              <ns1:Afsnit>Advarsel: Ejeren: 100976XXXX i Motorregisteret af bilen med stelnr: VF12RFL1H49621453 findes ikke blandt debitorerne i anmeldelsen. Kontroller stelnr</ns1:Afsnit>
            </ns1:TekstGruppe>
          </ns1:TekstAngivelse>
        </ns:TillaegstekstSamling>
        <ns:TinglysningAfgiftBetalt>0</ns:TinglysningAfgiftBetalt>
        <ns:TinglysningAfgiftOverfoerselIndikator>false</ns:TinglysningAfgiftOverfoerselIndikator>
        <ns1:BilStruktur>
          <ns1:BilMaerke>
            <ns1:BilFabrikatTekst>RENAULT</ns1:BilFabrikatTekst>
            <ns1:BilModelTekst>Captur</ns1:BilModelTekst>
            <ns1:BilVariantTekst>dCi 90</ns1:BilVariantTekst>
          </ns1:BilMaerke>
          <ns1:RegistreringsnummerTekst>AB12345</ns1:RegistreringsnummerTekst>
          <ns1:FoersteRegistreringsAar>2013</ns1:FoersteRegistreringsAar>
        </ns1:BilStruktur>
      </ns:HaeftelseSummarisk>
    </ns:HaeftelseSummariskSamling>
    <ns1:ModelId>64176074819</ns1:ModelId>
  </ns:BilSummarisk>
  <ns:UdskriftDatoTid>2017-01-02T10:39:33.230+01:00</ns:UdskriftDatoTid>
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

(defun get-text-value (document tag-name)
  (let* ((node (dom:item (dom:get-elements-by-tag-name document tag-name) 0))
         (node-child
          (if (dom:element-p node)
              (dom:first-child node)
              (values))))
    (if (dom:text-node-p node-child)
        (dom:node-value node-child)
        (values))))


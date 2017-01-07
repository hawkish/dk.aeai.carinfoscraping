;;;; carinfoscraping.asd

(asdf:defsystem #:dk.aeai.carinfoscraping
  :description "Describe carinfoscraping here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:drakma
               #:cxml
               #:yason
               #:plump
               #:clss
               #:cl-utilities
               #:cl-ppcre
               #:iterate)
  :components ((:file "package")
               (:file "tinglysningen")
               (:file "trafikstyrelsen")
               (:file "carinfoscraping")))


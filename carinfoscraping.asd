;;;; carinfoscraping.asd

(asdf:defsystem #:carinfoscraping
  :description "Describe carinfoscraping here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma
               #:cxml
               #:yason
               #:plump
               #:clss
               #:cl-utilities
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "tinglysningen")
               (:file "trafikstyrelsen")
               (:file "carinfoscraping")))


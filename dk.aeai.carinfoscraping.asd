;;;; carinfoscraping.asd

(asdf:defsystem #:dk.aeai.carinfoscraping
  :description "Describe carinfoscraping here"
  :author "Morten Høgh <mortenhogh@gmail.com>"
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
               (:file "tinglysningen"
                      :depends-on ("package"))
               (:file "trafikstyrelsen"
                      :depends-on ("package"))
               (:file "dk.aeai.carinfoscraping"
                      :depends-on ("package"))))


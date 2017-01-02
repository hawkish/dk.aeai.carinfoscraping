;;;; carinfoscraping.asd

(asdf:defsystem #:carinfoscraping
  :description "Describe carinfoscraping here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma
               #:cxml
               #:yason
               #:plump
               #:clss)
  :serial t
  :components ((:file "package")
               (:file "carinfoscraping")))


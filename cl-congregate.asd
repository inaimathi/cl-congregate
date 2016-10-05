;;;; cl-congregate.asd
(asdf:defsystem #:cl-congregate
  :description "Describe cl-congregate here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:house
               #:alexandria
	       #:drakma
	       #:yason
	       #:cl-who
	       #:fact-base
               #:split-sequence
	       #:cl-ppcre)
  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "secrets")
	       (:file "model")
               (:file "cl-congregate")))

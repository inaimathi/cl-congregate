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
               #:split-sequence)
  :serial t
  :components ((:file "package")
	       (:file "secrets")
	       (:file "model")
               (:file "cl-congregate")))

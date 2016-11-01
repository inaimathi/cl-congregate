;;;; cl-congregate.asd
(asdf:defsystem #:cl-congregate
  :description "Describe cl-congregate here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria

	       #:house #:drakma #:fact-base
	       #:yason #:cl-who #:cl-css #:parenscript #:local-time

               #:optima #:cl-ppcre #:split-sequence)
  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "secrets")
	       (:file "recurrence")
	       (:file "model")
	       (:file "cosmetics")
	       (:file "authentication")
               (:file "cl-congregate")))

(in-package :cl-congregate)

(defun user-link (qualified-user)
  (destructuring-bind (site name) (cl-ppcre:split ":" qualified-user :limit 2)
    (let ((template (case (house::->keyword site)
		      (:github "https:github.com/~a")
		      (t nil))))
      (with-html-output (*standard-output*)
	(if template
	    (htm (:a :href (format nil template name) :target "_BLANK" (str name)))
	    (str name))))))

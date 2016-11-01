(in-package #:cl-congregate)

(defun auth-params (&rest k/v-pairs)
  (let ((params (loop for (k v) on k/v-pairs by #'cddr
		   collect (cons k v))))
    (if *dev-mode*
	(cons
	 (cons "redirect_uri" (format nil "http://localhost:~a/auth/github/callback" *http-port*))
	 params)
	params)))

(define-handler (auth/github/callback :content-type "text/plain") ((code :string))
  (let* ((raw (drakma:http-request
	       "https://github.com/login/oauth/access_token"
	       :method :post
	       :parameters (auth-params
			    "client_id" +github-api-id+
			    "client_secret" +github-api-secret+
			    "code" code)))
	 (params (house::parse-params (map 'string #'code-char raw))))
    (aif (cdr (assoc :access_token params))
	 (let* ((raw (drakma:http-request
		      "https://api.github.com/user"
		      :parameters (list (cons "access_token" it))))
		(u (yason:parse (map 'string #'code-char raw) :object-key-fn #'house::->keyword)))
	   (setf (lookup :user session)
		 (make-instance
		  'user
		  :source :github :access-token it
		  :name (gethash :login u) :url (gethash :html_url u)))
	   (let ((dest (lookup :destination session)))
	     (setf (lookup :destination session) nil)
	     (redirect! (or dest "/"))))
	 "AUTHENTICATION ERROR")))

(defun auth-redirect-tree (test destination body)
  `(cond (,test ,@body)
	 (t
	  (setf (lookup :destination session) ,destination)
	  (redirect! "https://github.com/login/oauth/authorize?client_id=50798a26a6cdfa15a5b8"))))

(defmacro logged-in-only (destination &body body)
  (auth-redirect-tree '(lookup :user session) destination body))

(defmacro organizers-only (thing destination &body body)
  (auth-redirect-tree `(organizer-of? (lookup :user session) ,thing) destination body))

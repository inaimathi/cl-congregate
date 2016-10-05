;;;; cl-congregate.lisp
(in-package #:cl-congregate)

(defun user-link (qualified-user)
  (destructuring-bind (site name) (cl-ppcre:split ":" qualified-user :limit 2)
    (let ((template (case (house::->keyword site)
		      (:github "https:github.com/~a")
		      (t nil))))
      (with-html-output (*standard-output*)
	(if template
	    (htm (:a :href (format nil template name) :target "_BLANK" (str name)))
	    (str name))))))

(define-handler (auth/github/callback :content-type "text/plain") ((code :string))
  (let* ((raw (drakma:http-request
	       "https://github.com/login/oauth/access_token"
	       :method :post
	       :parameters (list
			    (cons "client_id" +github-api-id+)
			    (cons "client_secret" +github-api-secret+)
			    (cons "code" code))))
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

(define-handler (group) ((group :group))
  (with-html
    (:html
     (:head (:title "A Group - congregate"))
     (:body
      (:h1 (str (getf group :name)))
      (:iframe
       :width 400 :height 350 :frameborder 0 :style "border: 0;"
       :src (group-map-url group))
      (:h2 (fmt "Recurring ~a on ~a at ~a"
		(getf group :recurring)
		(getf group :on)
		(getf group :at)))
      (:ul
       (loop for (k v) on (getf group :links) by #'cddr
	  do (htm (:li (:a :href v (str k))))))
      (:h2 "Events")
      (:ul
       (loop for e in (group-events (getf group :id))
	  do (htm (:li (:a :href (format nil "/event?event=~a" (getf e :id))
			   (str (getf e :name)))))))
      (:h2 "Organizers")
      (:ul
       (loop for o in (getf group :organizers)
	  do (htm (:li (user-link o)))))))))

(define-handler (event) ((event :event))
  (with-html
    (:html
     (:head (:title "An Event - congregate"))
     (:body
      (:h1 (str (getf event :name)))
      (:iframe
       :width 400 :height 350 :frameborder 0 :style "border: 0;"
       :src (event-map-url event))
      (:h2 (fmt "On ~a at ~a" (getf event :date) (getf event :time)))
      (:a :href (format nil "/event/interested?event=~a" (getf event :id))
	  "I'll Be There!")
      (:h2 "Attending")
      (:ul (loop for u in (getf event :interested)
	      do (htm (:li (user-link u)))))))))

(define-handler (event/interested) ((event :event))
  (cond ((lookup :user session)
	 (register-interest (lookup :user session) event)
	 (redirect! (format nil "/event?event=~a" (getf event :id))))
	(t
	 (setf (lookup :destination session)
	       (format nil "/event/interested?event=~a" (getf event :id)))
	 (redirect! "https://github.com/login/oauth/authorize?client_id=50798a26a6cdfa15a5b8"))))

(define-handler (root) ()
  (with-html
    (:html
     (:head (:title "Congregate"))
     (:body
      (:h1 "Welcome to friggin' Congregate")
      (:ul
       (loop for g in (list-groups)
	  do (htm
	      (:li (:a :href (format nil "/group?group=~a" (getf g :id))
		       (str (getf g :name)))))))
      (:p (fmt "Your session looks like: ~s" session))))))

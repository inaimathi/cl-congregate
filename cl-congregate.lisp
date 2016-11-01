;;;; cl-congregate.lisp
(in-package #:cl-congregate)

(define-handler (root) ()
  (aif (group-by-host (cdr (assoc :host (headers request))))
       (redirect! (format nil "/group?group=~a" (getf it :id)) :permanent? t)
       (redirect! "/index")))

(define-handler (index) ()
  (page (:title "congregate.ca")
	(:ul
	 (loop for g in (list-groups)
	    do (htm
		(:li (:a :href (format nil "/group?group=~a" (getf g :id))
			 (str (getf g :name)))))))))

;; TODO - create new group

;; TODO - solve this in a more elegant way (might be in the form of a change in :house)
(define-redirect-handler (code-retreat :permanent? t)
    (format nil "/group?group=~a" (getf (group-by-host "code-retreat") :id)))
(define-redirect-handler (toronto-haskell :permanent? t)
    (format nil "/group?group=~a" (getf (group-by-host "toronto-haskell") :id)))
(define-redirect-handler (toronto-lisp :permanent? t)
    (format nil "/group?group=~a" (getf (group-by-host "toronto-lisp") :id)))
(define-redirect-handler (cscabal :permanent? t)
    (format nil "/group?group=~a" (getf (group-by-host "cscabal") :id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Groups
(define-handler (group) ((group :group))
  (page (:title (getf group :name))
    (:h2 (fmt "Recurring ~a on ~a at ~a to ~a"
	      (getf group :recurring) (getf group :on)
	      (getf group :at) (getf group :to)))
    (:iframe :class "map" :src (group-map-url group))
    (when (organizer-of? (lookup :user session) group)
      (htm (:div
	    :class "admin"
	    (:h3 "Admin Options")
	    (:ul :class "menu"
	     (:li (:a :href (format nil "/group/create-event-form?group=~a" (getf group :id)) "[Create Event]"))))))
    (:div
     :class "column"
     (:h3 "Resources")
     (:ul
      (loop for (k v) on (getf group :links) by #'cddr
	 do (htm (:li (:a :href v (str k)))))))
    (:div
     :class "column"
     (:h3 "Upcoming Events")
     (:ul
      (loop for e in (group-events (getf group :id))
	 do (htm (:li (:a :href (format nil "/event?event=~a" (getf e :id))
			  (str (getf e :name))))))))
    (:div
     :class "column"
     (:h3 "Organizers")
     (:ul
      (loop for o in (getf group :organizers)
	 do (htm (:li (user-link o))))))
    (:hr :class "clear")
    (:p (str (getf group :description)))))
;; TODO - group should allow editing in-line for administrators

(define-handler (group/create-event-form) ((group :group))
  (page (:title (format nil "New Event for ~a" (getf group :name)))
    (str group)
    (:link :rel "stylesheet" :href "https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/themes/smoothness/jquery-ui.css")
    (:script :src "https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js")
    (:script :src "https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js")
    (:script
     "$(function  () { $(\"#datepicker\").datepicker({showOtherMonths: true, selectOtherMonths: true, dateFormat: \"yy-mm-dd\"})})"
     )
    (:form
     :action "/group/create-event" :method "post"
     (:input :type "hidden" :name "group" :value (getf group :id))
     (:ul
      (:li (:label "Name:") (:input :type "text" :name "name"))
      (:li (:label "Location:") (:input :type "text" :name "location"))
      (:li (:label "Date:")
	   (:input
	    :id "datepicker" :type "date" :name "date"
	    :value (first (split-sequence:split-sequence #\T (next-event-date group)))))
      (:li (:label "At:") (:input :type "text" :name "at" :value (getf group :at)))
      (:li (:label "To:") (:input :type "text" :name "to" :value (getf group :to))))
     (:input :type "submit" :value "Create"))))

(define-handler (group/create-event) ((group :group) (name :string) (location :string) (date :date) (at :integer) (to :integer))
  (organizers-only
      group (format nil "/group/create-event?group=~a&name=~a&location=~a&date=~a&at=~a&to=~a"
		    (getf group :id) name location date at to)
    (let ((event-id (create-event!
		     group :name name :location location
		     :date (local-time:format-timestring nil date))))
      (redirect! (format nil "/event?event=~a" event-id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Events
(define-handler (event) ((event :event))
  (page (:title (getf event :name))
    (:iframe :class "map" :src (event-map-url event))
    (:h2 (fmt "On ~a at ~a to ~a"
	      (local-time:format-timestring
	       nil (event-date event)
	       :format '(:long-weekday " the " :ordinal-day " of " :short-month ", " :year))
	      (getf event :at) (getf event :to)))
    (:div
     :class "controls"
     (:ul :class "menu"
      (:li (:a :href (format nil "/event/interested?event=~a" (getf event :id)) "[I'll Be There!]"))
      (:li (:a :href (format nil "/group?group=~a" (getf event :group)) "[Back to Group]"))
      (when (organizer-of? (lookup :user session) event)
	(htm (:li (:a :href (format nil "/event/delete?event=~a" (getf event :id)) "[Cancel Event]"))))))
    (aif (getf event :interested)
	 (htm
	  (:h2 "Attending")
	  (:ul (loop for u in (getf event :interested)
		  do (htm (:li (user-link u)))))))))
;; TODO - events should allow editing in-line for group administrators

(define-handler (event/delete) ((event :event))
  (organizers-only event (format nil "/event/delete?event=~a" (getf event :id))
    (delete-event! (getf event :id) (lookup :user session))
    (redirect! (format nil "/group?group=~a" (getf event :group)))))

(define-handler (event/take-attendance) ((event :event) (attendees :list-of-string))
  (organizers-only event (format nil "/event/interested?event=~a" (getf event :id))
    (take-attendance attendees event)
    (redirect! (format nil "/event?event=~a" (getf event :id)))))

(define-handler (event/interested) ((event :event))
  (logged-in-only (format nil "/event/interested?event=~a" (getf event :id))
    (register-interest (lookup :user session) event)
    (redirect! (format nil "/event?event=~a" (getf event :id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; User
(define-handler (me) ()
  (logged-in-only "/me"
    (labels ((link-ul (title link-type list)
	       (with-html-output (s *standard-output*)
		 (when list
		   (htm
		    (:h2 (str title))
		    (:ul
		     (loop for id/name-plist in list
			do (htm
			    (:li
			     (:a :href (format nil "/~(~a~)?~(~a~)=~a"
					       link-type link-type (getf id/name-plist :id))
				 (str (getf id/name-plist :name))))))))))))
      (let ((deets (user-details (lookup :user session))))
	(page (:title "My Profile")
	  (link-ul "You organize..." :group (getf deets :organizer-of))
	  (link-ul "You subscribe to..." :group (getf deets :subscribed-to))
	  (link-ul "You've been to..." :event (getf deets :attended)))))))

(defun start! (port &key (host usocket:*wildcard-host*))
  (let ((s *standard-output*))
    (setf
     *http-port* port
     *threads*
     (list
      (bt:make-thread
       (lambda ()
	 (loop
	    do (sleep (* 60 60))
	    do (format s "Doing a state update...~%")
	    do (update-state!)))
       :name "congregate-cron-thread")
      (bt:make-thread
       (lambda ()
	 (format s "Listening on ~a...~%" port)
	 (house:start port host))
       :name "congregate-http-thread"))))
  nil)

(defun stop! ()
  (loop for th in *threads*
     do (format t "Destroying ~a...~%" (bt:thread-name th))
     do (bt:destroy-thread th))
  (mapc #'bt:destroy-thread *threads*)
  (setf *threads* nil))

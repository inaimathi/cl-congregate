;;;; model.lisp
(in-package #:cl-congregate)

(defparameter *public-data*
  (fact-base:base!
   (merge-pathnames "public-data.base" (user-homedir-pathname))
   :in-memory? t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Custom subdomains
(defun set-custom-subdomain! (group subdomain)
  (unless (subdomain-taken? subdomain)
    (fact-base:insert!
     *public-data*
     `(,(getf group :id) :subdomain ,subdomain))
    (fact-base:write! *public-data*)
    t))

(defun subdomain-taken? (subdomain)
  (first
   (fact-base:for-all
    `(?id :subdomain ,subdomain)
    :in *public-data* :collect ?id)))

(defun group-by-host (host)
  (group-by-subdomain (first (split-sequence:split-sequence #\. host))))

(defun group-by-subdomain (subdomain)
  (aif (first
	(fact-base:for-all
	 `(and (?id :group nil) (?id :subdomain ,subdomain))
	 :in *public-data*
	 :collect ?id))
       (group-by-id it)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Users
(defclass user ()
  ((source :reader source :initform :github :initarg :source)
   (name :accessor name :initarg :name)
   (access-token :accessor access-token :initarg :access-token)
   (url :reader url :initarg :url)))

(defmethod user-id ((u user))
  (format nil "~(~a~):~a" (source u) (name u)))

(defmethod organizer-of? ((u null) group) nil)
(defmethod organizer-of? ((u user) thing)
  (fact-base:for-all
   `(,(or (getf thing :group) (getf thing :id)) :organizer ,(user-id u))
   :in *public-data* :do (return t)))

(defmethod register-interest ((u user) event)
  (fact-base:insert-if-unique!
   *public-data*
   (list (getf event :id) :interested (user-id u)))
  (fact-base:write! *public-data*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Groups
(defun list-groups ()
  (fact-base:for-all
   (and (?id :group nil)
	(?id :name ?name)
	(?id :country ?country)
	(?id :region ?region)
	(?id :city ?city)
	(?id :location ?location))
   :in *public-data*
   :collect (list
	     :id ?id
	     :name ?name
	     :country ?country
	     :region ?region
	     :city ?city
	     :location ?location)))

(defun create-group! (name description &key (country "CAN") (region "Ontario") (city "Toronto") organizers location recurring on at to links)
  (assert (and organizers location recurring on at to) nil "Missing some mandatory keyword inputs")
  (let ((items `((:group nil) (:name ,name)
		 (:description ,description)
		 (:country ,country) (:region ,region) (:city ,city)
		 (:location ,location)
		 (:recurring ,recurring) (:on ,on) (:at ,at) (:to ,to)
		 ,@(loop for name in organizers
		      collect (list :organizer name))
		 ,@(loop for name/url-pair in links
		      collect (list :link name/url-pair)))))
    (fact-base:multi-insert! *public-data* items)
    (fact-base:write! *public-data*)
    nil))

(defun group-by-id (group-id)
  (let ((orgs nil)
	(links nil)
	(group nil))
    (fact-base:for-all
     `(,group-id ?k ?v)
     :in *public-data*
     :do (case ?k
	   (:group nil)
	   (:organizer (push ?v orgs))
	   (:link (setf links (nconc links ?v)))
	   (t (push ?v group) (push ?k group))))
    (cons :id (cons group-id (cons :organizers (cons orgs (cons :links (cons links group))))))))

(defun group-events (group-id &key in show-deleted?)
  (assert (or (null in) (eq in :past) (eq in :future)) nil ":in must be one of nil, :future or :past")
  (fact-base:for-all
   `(and (,group-id :group nil)
	 (?id :group ,group-id)
	 (?id :event nil)
	 ,@(unless show-deleted? `((not (?id :deleted nil))))
	 ,@(case in
		 (:past '((?id :passed nil)))
		 (:future '((not (?id :passed nil)))))
	 (?id :date ?date)
	 (?id :name ?name)
	 (?id :country ?country)
	 (?id :region ?region)
	 (?id :city ?city)
	 (?id :location ?location))
   :in *public-data*
   :collect (list
	     :id ?id
	     :name ?name
	     :date ?date
	     :country ?country
	     :region ?region
	     :city ?city
	     :location ?location)))

(defun group-has-event-at? (group-id date)
  (fact-base:for-all
   `(and (,group-id :group nil)
	 (?id :group ,group-id)
	 (?id :event nil)
	 (?id :date ,date))
   :in *public-data*
   :do (return t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Events
(defun create-event! (group &key name location date at to)
  (let* ((d (or date (next-event-date group)))
	 (new-id
	  (fact-base:multi-insert!
	   *public-data*
	   `((:event nil)
	     (:group ,(getf group :id))
	     (:name ,(or name (default-event-name group d)))
	     (:country ,(getf group :country))
	     (:region ,(getf group :region))
	     (:city ,(getf group :city))
	     (:location ,(or location (getf group :location)))
	     (:date ,d)
	     (:at ,(or at (getf group :at))) (:to ,(or to (getf group :to)))))))
    (fact-base:write! *public-data*)
    new-id))

(defmethod delete-event! (event-id (user user))
  (loop for f in `((,event-id :deleted nil) (,event-id :deleted-by ,user))
     do (fact-base:insert! *public-data* f)))

(defun get-event (event-id)
  (let ((ints nil)
	(atts nil)
	(links nil)
	(event nil))
    (fact-base:for-all
     `(,event-id ?k ?v)
     :in *public-data*
     :do (case ?k
	   (:event nil)
	   (:interested (push ?v ints))
	   (:attended (push ?v atts))
	   (:link (setf links (nconc links ?v)))
	   (t (push ?v event) (push ?k event))))
    (concatenate
     'list
     (list :id event-id
	   :interested ints
	   :attended atts
	   :links links)
     event)))

(defun event-date (event)
  (local-time:parse-timestring (getf event :date)))

(defmethod take-attendance (users event)
  (let ((id (getf event :id)))
    (dolist (u users)
      (fact-base:insert-if-unique! *public-data* (list id :attended u)))
    (fact-base:write! *public-data*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Utility
(defun map-url (country region city location)
  (format
   nil "https://www.google.com/maps/embed/v1/place?key=~a&q=~a,~a,~a,~a"
   +google-api-key+ location city region country))

(defun group-map-url (group)
  (map-url (getf group :country) (getf group :region) (getf group :city) (getf group :location)))

(defun event-map-url (event)
  (map-url (getf event :country) (getf event :region) (getf event :city) (getf event :location)))

(defun default-event-name (group date)
  (format
   nil "~a - ~a"
   (local-time:format-timestring
    nil (local-time:parse-timestring date)
    :format (case (house::->keyword (getf group :recurring))
	      (:yearly '(:year))
	      (:monthly '(:long-month))
	      (:weekly '(:long-month " " :ordinal-day))))
   (getf group :name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Periodic updates
(defun create-new-events! ()
  "Creates new events for all groups on file according to their recurrence settings.
Intentionally doesn't check for :deleted events. Because:
1. If there are no events there at all, we create a new one
2. If there's a deleted event at the appointed time, that means we've created one
   in the past and some organizer deleted it. Thus we should not re-create it."
  (fact-base:for-all
   (?id :group nil)
   :in *public-data*
   :do (let ((g (group-by-id ?id)))
	 (unless (group-has-event-at? ?id (next-event-date g))
	   (create-event! g)))))

(defun mark-passed-events! ()
  "Goes through all events not marked :passed, and marks them as such if their date is in the past."
  (fact-base:for-all
   (and (?id :event nil)
	(not (?id :passed nil))
	(?id :date ?date))
   :in *public-data*
   :do (when (local-time:timestamp>
	      (local-time:today)
	      (local-time:parse-timestring ?date))
	 (fact-base:insert! *public-data* (list ?id :passed nil)))))

(defun update-state! ()
  "Makes time-based updates to the *public-data* fact base"
  (create-new-events!)
  (mark-passed-events!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dummy data

;; (progn
;;   (create-group!
;;    "Toronto Code Retreat"
;;    "A place for fellow travellers on a path to higher understanding to practice the craft of writing software."
;;    :organizers (list "github:inaimathi" "github:dxnn")
;;    :location "Lighthouse Labs"
;;    :recurring "monthly" :on "the last wednesday" :at "6:30pm" :to "8:30pm"
;;    :links '(("github" "https://github.com/CodeRetreatTO")))
;;   (set-custom-subdomain! (group-by-id 0) "code-retreat")

;;   (create-group!
;;    "Toronto Lisp Users Group"
;;    "A place to discuss things that Common Lispers in Toronto may find interesting"
;;    :organizers (list "github:guitarvydas" "github:inaimathi")
;;    :location "Bento Miso"
;;    :recurring "monthly" :on "the first tuesday" :at "6:30pm" :to "8:30pm"
;;    :links '(("wiki" "http://lispwiki.inaimathi.ca/")
;; 	    ("group" "https://groups.google.com/forum/?hl=en&fromgroups#!forum/toronto-lisp-users-group")
;; 	    ("github" "https://github.com/LispTO")))
;;   (set-custom-subdomain! (group-by-id 1) "toronto-lisp")

;;   (create-group!
;;    "Comp Sci Cabal"
;;    "We read computer science books for fun. Then we talk about it every friday night."
;;    :organizers (list "github:dxnn" "github:inaimathi")
;;    :location "Bento Miso"
;;    :recurring "weekly" :on "Friday" :at "6:30pm" :to "9:30pm"
;;    :links '(("site" "http://cscabal.com")
;; 	    ("wiki" "https://github.com/CompSciCabal/SMRTYPRTY/wiki")
;; 	    ("github" "https://github.com/CompSciCabal")))
;;   (set-custom-subdomain! (group-by-id 2) "cscabal")

;;   (create-event! (group-by-id 0))
;;   (create-event! (group-by-id 1))
;;   (create-event! (group-by-id 2)))

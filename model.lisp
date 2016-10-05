;;;; model.lisp
(in-package #:cl-congregate)

(defparameter *public-data*
  (fact-base:base!
   (merge-pathnames "public-data.base" (user-homedir-pathname))
   :in-memory? t))

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

(defun create-group! (name description &key organizers location recurring on at links)
  (assert (and organizers location recurring on at) nil "Missing some mandatory keyword inputs")
  (let ((items `((:group nil) (:name ,name)
		 (:description ,description)
		 (:country "CAN") (:region "Ontario") (:city "Toronto")
		 (:location ,location)
		 (:recurring ,recurring) (:on ,on) (:at ,at)
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

(defun group-events (group-id)
  (fact-base:for-all
   `(and (,group-id :group nil)
	 (?id :event nil)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Events
(defun create-event! (group &key name location time date)
  (fact-base:multi-insert!
   *public-data*
   `((:event nil)
     (:name ,(or name (format nil "September - ~a" (getf group :name))))
     (:country ,(getf group :country))
     (:region ,(getf group :region))
     (:city ,(getf group :city))
     (:location ,(or location (getf group :location)))
     (:time ,(or time (getf group :at)))
     (:date ,(or date (next-event-date group)))))
  (fact-base:write! *public-data*)
  nil)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Users
(defclass user ()
  ((source :reader source :initform :github :initarg :source)
   (name :accessor name :initarg :name)
   (access-token :accessor access-token :initarg :access-token)
   (url :reader url :initarg :url)))

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

(defun next-event-date (group)
  "Monday, September 5th")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dummy data

;; (create-group!
;;  "Toronto Code Retreat"
;;  "A place for fellow travellers on a path to higher understanding to practice the craft of writing software."
;;  :organizers (list "github:inaimathi" "github:dxnn")
;;  :location "Lighthouse Labs"
;;  :recurring "monthly" :on "the last wednesday" :at "6:30pm"
;;  :links '(("github" "https://github.com/CodeRetreatTO")))

;; (create-group!
;;  "Toronto Lisp Users Group"
;;  "A place to discuss things that Common Lispers in Toronto may find interesting"
;;  :organizers (list "github:guitarvydas" "github:inaimathi")
;;  :location "Bento Miso"
;;  :recurring "monthly" :on "the first tuesday" :at "6:30pm"
;;  :links '(("wiki" "http://lispwiki.inaimathi.ca/")
;; 	  ("group" "https://groups.google.com/forum/?hl=en&fromgroups#!forum/toronto-lisp-users-group")
;; 	  ("github" "https://github.com/LispTO")))

;; (create-event! (group-by-id 0))

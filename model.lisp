;;;; model.lisp
(in-package #:cl-congregate)

(defparameter *public-data*
  (fact-base:base!
   (merge-pathnames "groups.base" (user-homedir-pathname))
   :in-memory? t))

(defparameter *users*
  (fact-base:base!
   (merge-pathnames "users.base" (user-homedir-pathname))
   :in-memory? t))

(defparameter +groups+
  (list
   '(:id 0
     :name "Tornto Code Retreat"
     :organizers ("github:inaimathi" "github:dxnn")
     :location "Bento Miso,Toronto,ON"
     :recurring "monthly" :on "every first monday" :at "6:30pm"
     :links ("github" "https://github.com/CodeRetreatTO"
             "host" "https://bentomiso.com/"))))

(defparameter +events+
  (list
   '(:id 0 :group 0
     :name "September - Monthly Code Retreat" :date "Monday, September 5th" :time "6:30pm"
     :location "Bento Miso,Toronto,ON"
     :interested ("github:inaimathi" "github:dxnn")
     :attended ("github:inaimathi" "github:dxnn")
     :links ("comments" "link-to-comments"))))

(defun list-groups () +groups+)

(defun group-by-id (group-id)
  (find-if
   (lambda (g) (= group-id (getf g :id)))
   +groups+))

(defun group-events (group-id)
  (remove-if-not
   (lambda (e) (= group-id (getf e :group)))
   +events+))

(defun get-event (event-id)
  (find-if
   (lambda (e) (= event-id (getf e :id)))
   +events+))

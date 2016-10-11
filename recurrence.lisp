(in-package :cl-congregate)

(defun group-on (group)
  (let ((raw
	 (mapcar
	  #'house::->keyword
	  (cl-ppcre:split " +" (getf group :on)))))
    (if (eq :THE (first raw))
	(rest raw)
	raw)))

(defun group-recurrence (group)
  (cons (house::->keyword (getf group :recurring)) (group-on group)))

(defun weekday->index (weekday)
  (getf '(:sunday 0 :monday 1 :tuesday 2 :wednesday 3 :thursday 4 :friday 5 :saturday 6) weekday))

(defun days-in-months (weekday &key (count-months 3))
  "Takes a weekday, and a count of months (defaulting to 3).
Returns a list of timestamps in those months that specify
the given weekday."
  (let ((now (local-time:adjust-timestamp
		 (local-time:now)
	       (offset :day (- (- (local-time:timestamp-day (local-time:now)) 1)))))
	(wday-ix (weekday->index weekday)))
    (loop repeat count-months for i from 0
       collect (let ((m (local-time:timestamp+ now i :month)))
		 (loop repeat (local-time:days-in-month
			       (local-time:timestamp-month m)
			       (local-time:timestamp-year m))
		    for ix from 0
		    for d = (local-time:timestamp+ m ix :day)
		    when (= wday-ix (local-time:timestamp-day-of-week d)) collect d)))))

(defun in-future? (timestamp)
  (local-time:timestamp> timestamp (local-time:now)))

(defun next-event-date (group)
  (let ((now (local-time:now)))
    (local-time:format-timestring
     nil
     (match (group-recurrence group)
       ((list* :yearly _)
	;; TODO - support for yearly recurrence
	now)
       ((list :monthly index weekday)
	(let ((f (case index
		   (:first #'first)
		   (:second #'second)
		   (:third #'third)
		   (:fourth #'fourth)
		   (:fifth #'fifth)
		   (:last (lambda (m) (first (last m))))
		   (t #'first))))
	  (loop for m in (days-in-months weekday)
	     for d = (funcall f m)
	     when (and d (in-future? d)) return d)))

       ((list :weekly day)
	(let ((next-day (weekday->index day))
	      (today (local-time:timestamp-day-of-week now)))
	  (if (null next-day)
	      now
	      (local-time:timestamp+
	       now
	       (if (> next-day today)
		   (- next-day today)
		   (- 7 (- today next-day)))
	       :day))))))))

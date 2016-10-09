;;;; cosmetics.lisp
(in-package #:cl-congregate)

(defmacro page ((&key title) &body contents)
  (with-gensyms (tl)
    `(let ((,tl ,title))
       (with-html-output-to-string (*standard-output* nil :indent t :prologue t)
	 (:html
	  (:head
	   (:title (fmt "~@[~a - ~]congregate" ,tl))
	   (:link :href "/style/default.css" :rel "stylesheet" :type "text/css"))
	  (:body
	   (:div :class "header" (:h1 (str ,tl)))
	   (:div :class "content" ,@contents)
	   (:div :class "footer")))))))

(defvar css-main-color "#369")
(defvar white "#fff")
(defvar tight '(:margin 0 :padding 0))

(define-handler (style/default.css :content-type "text/css") ()
  (css `(("body" ,@tight :background-color ,css-main-color)
	 (.header :padding "5% 5% 1% 5%")

	 (.content :background-color ,white :padding "1% 5%")
	 (.map :width 100% :height 300px :border 0px)
	 (h1 :color ,white ,@tight)
	 ("h2, h3, h4" :color ,css-main-color))))

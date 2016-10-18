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
	   (:div
	    :class "footer"
	    (:ul
	     :class "footer-menu"
	     (:li (:a :href "/index" "Index"))
	     (:li (:a :href "/me" "My Profile"))))))))))

(defvar css-main-color "#369")
(defvar white "#fff")
(defvar tight '(:margin 0 :padding 0))

(define-handler (style/default.css :content-type "text/css") ()
  (css `(("body" ,@tight :background-color ,css-main-color)
	 (.header :padding "5% 5% 1% 5%")
	 (.footer :padding "1% 5% 5% 5%")
	 (".footer a" :font-weight bolder :color ,white)
	 (".footer a:hover" :font-weight bolder :color \#ddd)

	 (.column :width 32% :float left)
	 (.clear :clear both)
	 (hr :border-color ,css-main-color)

	 (.content :background-color ,white :padding "1% 5%")
	 (.map :width 100% :height 300px :border 0px)
	 (h1 :color ,white ,@tight)
	 (h2 :margin-top 0px :padding-top 0px)
	 ("h2, h3, h4" :color ,css-main-color))))

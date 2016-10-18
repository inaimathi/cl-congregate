;;;; package.lisp

(defpackage #:cl-congregate
  (:use #:cl #:house #:anaphora #:cl-who #:cl-css #:optima)
  (:shadow #:start)
  (:import-from #:alexandria :with-gensyms)

  (:export :start! :stop!))

(in-package :cl-congregate)

(define-http-type (:group)
    :type-expression `(group-by-id (parse-integer ,parameter :junk-allowed t))
    :type-assertion `(not (null ,parameter)))

(define-http-type (:event)
    :type-expression `(get-event (parse-integer ,parameter :junk-allowed t))
    :type-assertion `(not (null ,parameter)))

(define-http-type (:list-of-string)
    :type-expression `(json:decode-json-from-string ,parameter)
    :type-assertion `(every #'stringp ,parameter))

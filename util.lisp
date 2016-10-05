(in-package :cl-congregate)

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :indent t :prologue t)
     ,@body))

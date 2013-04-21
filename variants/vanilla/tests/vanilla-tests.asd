;;; -*- Mode: Lisp -*-

(in-package :cl-user)

(defpackage :vanilla-tests-system 
  (:use :cl :asdf))

(in-package :vanilla-tests-system)

;;; System definition

(asdf:defsystem :vanilla-tests
    :version "0.0.20"
    :components ((:file "package")
		 (:file "object" :depends-on ("package")))
    :depends-on (langband-tests))

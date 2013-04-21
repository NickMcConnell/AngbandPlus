;;; -*- Mode: Lisp -*-

(in-package :cl-user)

(defpackage :langband-tests-system 
  (:use :cl :asdf))

(in-package :langband-tests-system)

;;; System definition

(asdf:defsystem :langband-tests
    :version "0.0.20"
    :components ((:module basic
			  :pathname ""
			  :components ((:file "package")
				       (:file "base"
					      :depends-on ("package"))
				       (:file "equal")))
		 (:module usual
			  :pathname ""
			  :components ((:file "core")
				       (:file "stat")
				       (:file "checking")
				       (:file "player")
				       (:file "building")
				       (:file "save")
				       #+never
				       (:file "ffi")
				       (:file "format")
				       (:file "monster")
				       ;;(:file "object")
				       )
			  :depends-on (basic))
		 )
    :depends-on (xp-test))

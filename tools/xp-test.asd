;;; -*- Mode: Lisp -*-
;;;; XPTest --- XP Test Framework, inspired by Kent Beck
;;;;
;;;; Put in public domain by onShore, Inc.
;;;; 
;;;; XPTest.system --- system definition for XPTest


(in-package :cl-user)

(defpackage :xptest-system 
  (:use :cl :asdf))

(in-package :xptest-system)

;;; System definition

(asdf:defsystem xp-test
    :version "0.1"
    :components ((:file "package")
		 (:file "xptestsuite"
			:depends-on ("package"))
;;		 (:file "xptest-example"
;;			:depends-on ("xptestsuite"))
		 ))





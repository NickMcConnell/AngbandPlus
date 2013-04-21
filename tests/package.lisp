;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

#|

DESC: tests/package.lisp - package def for langband-test

|#

(in-package :cl-user)

(defpackage :org.langband.testing
  (:nicknames :lb-test :langband-test)
  (:use :common-lisp :langband :xp-test)
  (:export #:def-lb-fixture
	   #:pre-variant
	   #:post-variant
	   #:in-game)

  #+lisp2csf
  (:documentation "This is the testing of the Langband code."))

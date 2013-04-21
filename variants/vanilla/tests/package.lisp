;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

#|

DESC: variants/vanilla/tests/package.lisp - package def for vanilla-tests

|#

(in-package :cl-user)

(defpackage :org.langband.vanilla-testing
  (:nicknames :vanilla-test)
  (:use :common-lisp :langband :xp-test :org.langband.testing)
  #+lisp2csf
  (:documentation "This is the testing of the Langband code."))

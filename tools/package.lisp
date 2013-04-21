;;; -*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: cl-user -*-


(in-package :cl-user)

(defpackage :xp-test-framework
  (:use :common-lisp)
  (:nicknames #:xp-test #:xptest)
  ;; framework classes
  (:export #:setup
	   #:teardown
	   #:perform-test
	   #:test-failure
	   #:failure
	   #:test-assert
	   #:run-test
	   #:def-test-fixture
	   #:make-test-case
	   #:make-test-suite
	   #:setup-testsuite-named
	   #:teardown-testsuite-named
	   #:add-test
	   #:test-named
	   #:remove-test
	   #:tests
	   #:test-result
	   #:report-result
	   )
  #+lisp2csf
  (:documentation "this is the xp testsuite framework."))

  


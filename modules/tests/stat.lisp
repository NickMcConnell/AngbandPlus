;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.testing -*-

#|

DESC: tests/stat.lisp - testing code for stats

|#

(in-package :org.langband.testing)

(def-lb-fixture stat-fixture (pre-variant)
  ()
  (:documentation "Simple (pre) fixture for testing stats."))



(defmethod perform-test ((fix stat-fixture))

  #||
  (dotimes (i 18)
    (test-assert (equal (format nil "~6d" i)
			(lb::cnv-stat i nil)))
    )
  

  (loop for i from 0 to 103 do
	(test-assert (equal (format nil "~5d" i)
			    (lb::%get-5str i)))
	(test-assert (equal (format nil "~4d" i)
			    (lb::%get-4str i)))
	(test-assert (equal (format nil "~9d" i)
			    (lb::%get-9str i)))
	(test-assert (equal (format nil "~8d" i)
			    (lb::%get-8str i)))
	(test-assert (equal (format nil "~13@a" i)
			    (lb::%get-13astr i)))
	)
  ||#
  
  t)



;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND-TEST -*-

#|

DESC: tests/building.lisp - testing code for buildings

|#

(in-package :lb-test)

(def-lb-fixture house-fixture (pre-variant)
  ()
  (:documentation "Simple (pre) fixture for testing a house."))


(def-lb-fixture post-house-fixture (post-variant)
  ((house :accessor fixhouse.house))
  (:documentation "Simple (post) fixture for testing a house."))


  
(defmethod setup ((fix post-house-fixture))
  (setf (fixhouse.house fix) nil))

;;(defmethod teardown ((fix house-fixture))
;;  t)

(defmethod perform-test ((fix house-fixture))
  
  (let ((var-obj lb::*variant*))

    ;; do dummy tests with no meaning
    (test-assert (eq nil (lb::establish-house& var-obj nil)))
    (test-assert (eq nil (lb::establish-owner& var-obj nil)))
    (test-assert (eq nil (lb::get-house nil var-obj)))
    (test-assert (eq nil (lb::get-owner nil var-obj)))
    (test-assert (eq nil (lb::find-owner-for-house nil nil)))

    ))


(defmethod perform-test ((fix post-house-fixture))
  
  (let ((var-obj lb::*variant*))
	

    (test-assert (not (eq var-obj nil)))
    ;; house should start as nil
    (test-assert (eq nil (fixhouse.house fix)))
    
    ))



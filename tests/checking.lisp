;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND-TEST -*-

#|

DESC: tests/checking.lisp - testing code for equality functions

|#

(in-package :lb-test)

(def-lb-fixture pre-check-fixture (pre-variant)
  ()
  (:documentation "Simple fixture for testing equality functions."))

(def-lb-fixture post-check-fixture (post-variant)
  ()
  (:documentation "Simple fixture for testing equality functions."))

(def-lb-fixture in-check-fixture (in-game)
  ()
  (:documentation "Simple fixture for testing equality functions."))

(defun %test-basic-objs ()
  (test-assert (lb::lang-equal lb::*dungeon* lb::*dungeon*))
  (test-assert (lb::lang-equal lb::*player*  lb::*player*))
  (test-assert (lb::lang-equal lb::*level*   lb::*level*))
  (test-assert (lb::lang-equal lb::*variant* lb::*variant*)))

(defmethod perform-test ((fix pre-check-fixture))
  (%test-basic-objs))

(defmethod perform-test ((fix post-check-fixture))
  (%test-basic-objs))

(defmethod perform-test ((fix in-check-fixture))
  (%test-basic-objs))

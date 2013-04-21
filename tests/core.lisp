;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND-TEST -*-

#|

DESC: tests/core.lisp - testing code for variant obj

|#

(in-package :lb-test)

(def-lb-fixture var-fixture (pre-variant)
  ()
  (:documentation "Simple (pre) fixture for testing a var obj."))


(def-lb-fixture post-var-fixture (post-variant)
  ()
  (:documentation "Simple (post) fixture for testing a var obj."))


(defmethod perform-test ((fix var-fixture))
  
  (let ((var-obj lb::*variant*))

    ;; do dummy tests with no meaning
    (test-assert (eq nil var-obj)) ;; it really shouldn't have a value
    ))


(defmethod perform-test ((fix post-var-fixture))
  
  (let ((var-obj lb::*variant*))

    ;; simple checks to see that our variant has some valid values
    (test-assert (not (eq nil var-obj)))
    (test-assert (typep var-obj 'lb::variant))
    (test-assert (lb::ok-object? var-obj))
    (test-assert (plusp (hash-table-count (lb::variant.races var-obj))))
    (test-assert (plusp (hash-table-count (lb::variant.classes var-obj))))
    (test-assert (plusp (hash-table-count (lb::variant.floor-features var-obj))))
    (test-assert (plusp (hash-table-count (lb::variant.room-builders var-obj))))
    (test-assert (plusp (hash-table-count (lb::variant.objects var-obj))))
    (test-assert (plusp (hash-table-count (lb::variant.monsters var-obj))))
    (test-assert (plusp (lb::variant.day-length var-obj)))
    (test-assert (not (eq nil (lb::variant.xp-table var-obj))))
    ))


;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.testing -*-

#|

DESC: tests/format.lisp - testing code for stats

|#

(in-package :lb-test)

(def-lb-fixture format-fixture (pre-variant)
  ()
  (:documentation "Simple (pre) fixture for testing lb-format."))

(defmacro meq (a b)
  `(test-assert (equalp ,a ,b)))

(defmethod perform-test ((fix format-fixture))

  (flet ((reset (s)
	   (setf (fill-pointer s) 0)))
  
  (lb::with-foreign-str (s)

    (lb::lb-format s "foo ~d" 56)
    (meq s "foo 56")
    (reset s)

    (lb::lb-format s "foo ~a" 56)
    (meq s "foo 56")
    (reset s)

    (lb::lb-format s "foo ~a" :foo)
    (meq s "foo :foo")
    (reset s)

    (lb::lb-format s "foo ~a" 'foo)
    (test-assert (equalp s "foo FOO"))
    (reset s)

    (lb::lb-format s "foo ~a" "foo")
    (meq "foo foo" s)
    (reset s)

    (lb::lb-format s "foo ~~")
    (meq "foo ~" s)
    (reset s)
    
    t)))

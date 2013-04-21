;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.testing -*-

#|

DESC: tests/ffi.lisp - testing code for stats

|#

(in-package :org.langband.testing)

(def-lb-fixture ffi-fixture (pre-variant)
  ()
  (:documentation "Simple (pre) fixture for testing ffi code."))


(defmethod perform-test ((fix ffi-fixture))

  (flet ((c-test-calling! (arg)
	   (org.langband.ffi:c_test_calling! (org.langband.ffi:to-arr arg))))
  
    (let* ((a lb::+c-null-value+)
	   (b "frob"))
    
      (test-assert (eql 0 (c-test-calling! "")))
      (test-assert (eql 4 (c-test-calling! b)))
      (test-assert (eql -1 (c-test-calling! a)))
    
      (lb::with-foreign-str (s)
	(format s "fribo")
	(let ((val (c-test-calling! s)))
;;	  (format t "Val is ~a~%" val)
	  (test-assert (eql 5 val))))
    
      )))

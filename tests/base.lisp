;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND-TEST -*-

#|

DESC: tests/base.lisp - base code for customising xptest to Langband

|#

(in-package :lb-test)

(defvar *pre-variant-load* (make-test-suite "pre-variant-load tests"
					   "Tests prior to loading variant"
					   ))

(defvar *post-variant-load* (make-test-suite "post-variant-load tests"
					    "Tests after loading the variant"
					    ))

(defvar *in-game-tests* (make-test-suite "in-game tests"
					"Tests in the town"
					))

(def-test-fixture pre-variant ()
  ())

(def-test-fixture post-variant ()
  ())

(def-test-fixture in-game ()
  ())

(defun register-fixture& (type fixture)
  (flet ((add-to-var (var)
	   (add-test (make-test-case (string-downcase (symbol-name fixture))
				     fixture)
		     var)))
  
  
    (ecase type
      (:pre (add-to-var *pre-variant-load*))
      (:post (add-to-var *post-variant-load*))
      (:in (add-to-var *in-game-tests*)))))

(defmacro def-lb-fixture (name supers slotdefs &rest class-options)
  "Define a new test-fixture class.  Works just like defclass, but
ensure that test-fixture is a super."
  `(progn
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (defclass ,name ,supers
	,slotdefs ,@class-options))
    (register-fixture& ,(ecase (car supers)
			       (pre-variant :pre)
			       (post-variant :post)
			       (in-game :in))
     ',name)))


(defun run-lb-test (type &key (verbose nil))
  
  (flet ((my-run (var)
	   (report-result (run-test var) :verbose verbose)))
    
    (ecase type
      (:pre  (my-run *pre-variant-load*))
      (:post (my-run *post-variant-load*))
      (:in   (my-run *in-game-tests*))
      )))

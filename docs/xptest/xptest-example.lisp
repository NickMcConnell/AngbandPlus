;;; -*- Mode: Lisp -*-
;;;; xptest-eaxmple.lisp --- Example of test suite based on Extreme
;;;;                         Programming Framework by Kent Beck
;;;;
;;;; Author: Craig Brozefsky <craig@onshore.com>
;;;; Put in public domain by onShore, Inc
(in-package "XP-TEST-EXAMPLE")

;;; First we define some basic fixtures that we are going to need to
;;; perform our tests.  A fixture is a place to hold data we need
;;; during testing.  Often there are many test cases that use the same
;;; data.  Each of these test cases is an instance of a test-fixture.

(def-test-fixture math-fixture ()
  ((numbera
    :accessor numbera)
   (numberb
    :accessor numberb))
  (:documentation "Test fixture for math testing"))

;;; Then we define a setup method for the fixture.  This method is run
;;; prior to perfoming any test with an instance of this fixture.  It
;;; should perform all initialization needed, and assume that it is starting
;;; with a pristine environment, well to a point, use your head here.

(defmethod setup ((fix math-fixture))
  (setf (numbera fix) 2)
  (setf (numberb fix) 3))

;;; Then we define a teardown method, which should return the instance
;;; to it's original form and reset the environment.  In this case
;;; there is little for us to do since the fixture is quite static.
;;; In other cases we may need to clear some database tables, or
;;; otherwise get rid of state built up while perofmring the test.
;;; Here we just return T.

(defmethod teardown ((fix math-fixture))
  t)

;;; Once we hav a fixture we can start defining method on it which
;;; will perform tests.  These methods should take one argument, an
;;; instance of the fixture.  The method performs some operation and
;;; then performs some tests to determine if the proper behavior
;;; occured.  If there is a failure to behave as excpeted the method
;;; raises a test-failure object by calling the method FAILURE.  This
;;; is much like calling ERROR in that it stops processing that
;;; method.  Each method should only check for one aspect of behavior.
;;; This way triggering one failure would not result in another
;;; behavior check from being skipped.  It does not matter what these
;;; methods return

(defmethod addition-test ((test math-fixture))
  (let ((result (+ (numbera test) (numberb test))))
    (unless (= result 5)
      (failure "Result was not 5 when adding ~A and ~A"
	       (numbera test) (numberb test)))))

(defmethod subtraction-test ((test math-fixture))
  (let ((result (- (numberb test) (numbera test))))
    (unless (= result 1)
      (failure "Result was not 1 when subtracting ~A ~A"
	      (numberb test) (numbera test)))))

;;; This method is meant to signal a failure
(defmethod subtraction-test2 ((test math-fixture))
  (let ((result (- (numbera test) (numberb test))))
    (unless (= result 1)
      (failure "Result was not 1 when subtracting ~A ~A"
	      (numbera test) (numberb test)))))


;;; Now we can create a test-suite.  A test-suite contains a group of
;;; test-cases (instances of test-fixture) and/or other test-suites.
;;; We can specify which tests are in a test-suite when we define the
;;; test-suite, or we can add them later.  See the documentation and
;;; argument list for make-test-case for details on how to specify a
;;; test-case.

(setf math-test-suite (make-test-suite
		       "Math Test Suite"
		       "Simple test suite for arithmetic operators."
		       ("Addition Test" 'math-fixture
			:test-thunk 'addition-test
			:description "A simple test of the + operator")
		       ("Subtraction Test" 'math-fixture
			:test-thunk 'subtraction-test
			:description "A simple test of the - operator")))

(add-test (make-test-case "Substraction Test 2" 'math-fixture
			  :test-thunk 'subtraction-test2
			  :description "A broken substraction test, should fail.")
	  math-test-suite)

;;;; Finally we can run our test suite and see how it performs.
;;;; (report-result (run-test math-test-suite) :verbose t)

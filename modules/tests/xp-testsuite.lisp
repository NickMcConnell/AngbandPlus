;;; -*- Mode: Lisp -*-
;;;; xptestsuite.lisp --- Test suite based on Extreme Programming
;;;;                      Framework by Kent Beck
;;;;
;;;; Inspired by http://www.xprogramming.com/testfram.htm
;;;;
;;;; Author: Craig Brozefsky <craig@onshore.com>
;;;; Put in public domain by onShore, Inc
;;;;


(in-package :xp-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defclass test-fixture ()
    ((test-thunk
      :initarg :test-thunk
      :reader test-thunk
      :initform 'perform-test
      :documentation
      "A thunk or symbol which will be applied to this instance, a
test-case, to perform that test-case. Defaults to 'perform-test")
     (test-name
      :initarg :test-name
      :reader test-name
      :documentation
      "The name of this test-case, used in reports.")
     (test-description
      :initarg :description
      :reader description
      :documentation
      "Short description of this test-case, uses in reports"))
    (:documentation
     "Base class for test-fixtures.  Test-cases are instances of test-fixtures."))


  (define-condition test-failure (simple-condition) ()
		    (:documentation "Base class for all test failures."))

   
  (defclass test-suite ()
    ((name
      :initarg :name
      :reader test-suite-name)
     (tests
      :initarg :tests
      :accessor tests-hash
      :initform (make-hash-table :test 'equal))
     (description
      :initarg :description
      :reader description
      :initform "No description.")))


  (defclass test-result ()
    ((start-time
      :initarg :start-time
      :reader start-time)
     (stop-time
      :initarg :stop-time
      :reader stop-time)
     (test
      :initarg :test
      :reader result-test)
     (failures
      :initarg :failures
      :reader test-failures
      :initform nil)
     (errors
      :initarg :errors
      :reader test-errors
      :initform nil))
    (:documentation
     "The result of applying a test"))
  )

(defmethod setup ((test test-fixture))
  "Method called before performing a test, should set up the
environment the test-case needs to operate in."
  t)

(defmethod teardown ((test test-fixture))
  "Method called after performing a test.  Should reverse everything that the
setup method did for this instance."
  t)

(defun failure (format-str &rest args)
  "Signal a test failure and exit the test."
  (signal 'test-failure
	  #+cmu :format-control
	  #+allegro :format-control
	  #-(or cmu allegro) :format-string
	  format-str
	  :format-arguments args))

(defmacro test-assert (test)
  `(unless ,test
    (failure "Test assertion failed: ~s" ',test)))

(defmethod perform-test ((test test-fixture))
  "Default method for performing tests upon a test-fixture."
  t)

(defmacro handler-case-if (test form &body cases)
  `(if ,test
       (handler-case
        ,form
	,@cases)
     ,form))

(defmacro unwind-protect-if (test protected cleanup)
  `(if ,test
       (unwind-protect
	   ,protected
	 ,cleanup)
     (progn ,protected ,cleanup)))

(defmethod run-test ((test test-fixture) &key (handle-errors t))
  "Perform the test represented by the given test-case or test-suite.
Returns one or more test-result objects, one for each test-case
performed."
  #+cmu
  (declare (optimize (ext:inhibit-warnings 3)))

  (let ((start-time (get-universal-time))
	(failures ())
	(errs ()))
    (unwind-protect-if handle-errors
	(handler-case-if handle-errors
	 (let ((res (progn (setup test)
			   (apply (test-thunk test) (list test)))))
	   (if (typep res 'test-failure)
	       (setf failures (cons res failures))))
	 (test-failure (failure)
		       (setf failures (cons failure failures)))
	 (t (err)
	    (setf errs (cons err errs))))
      (handler-case-if handle-errors
       (teardown test)
       (t (err)
	  (setf errs (cons err errs)))))
    (make-instance 'test-result
		   :test test
		   :start-time start-time
		   :stop-time (get-universal-time)
		   :failures failures
		   :errors errs)))

(defmacro def-test-fixture (name supers slotdefs &rest class-options)
  "Define a new test-fixture class.  Works just like defclass, but
ensure that test-fixture is a super."
  `(defclass ,name ,(append supers (list 'test-fixture))
     ,slotdefs ,@class-options))

(defmacro make-test-case (name fixture &key
			       (test-thunk 'perform-test)
			       (test-suite nil)
			       (description "No description."))
  "Create a test-case which is an instance of FIXTURE.  TEST-THUNK is
the method that will be invoked when perfoming this test, and can be a
symbol or a lambda taking a single argument, the test-fixture
instance.  DESCRIPTION is obviously what it says it is."
  (let ((newtest (gensym "new-test")))
    `(let ((,newtest (make-instance ,fixture
				    :test-name ,name
				    :test-thunk ,(if (eq test-thunk 'perform-test)
						     ''perform-test
						   test-thunk)
				    :description ,description)))
       (if ,test-suite (add-test ,newtest ,test-suite))
       ,newtest)))
	
(defmethod tests ((suite test-suite))
  (let ((tlist nil))
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (setf tlist (cons v tlist)))
	     (tests-hash suite))
    (reverse tlist)))

(defmacro make-test-suite (name description &rest testspecs)
  "Returns a new test-suite.  TESTSPECS are just like lists of
arguments to MAKE-TEST-CASE."
  (let* ((newsuite (gensym "test-suite"))
	 (testforms (mapcar #'(lambda (spec)
				(list
				 'add-test
				 (cons 'make-test-case spec)
				 newsuite))
			    testspecs)))
    `(let ((,newsuite (make-instance 'test-suite :name ,name
				     :description ,description)))
       ,@testforms
       ,newsuite)))

(defmethod add-test ((test test-fixture) (suite test-suite))
  (setf (gethash (test-name test) (tests-hash suite)) test))

(defmethod add-test ((test test-suite) (suite test-suite))
  (setf (gethash (test-suite-name test) (tests-hash suite)) test))

(defmethod remove-test ((test test-fixture) (suite test-suite))
  (remhash (test-name test) (tests-hash suite)))

(defmethod remove-test ((test test-suite) (suite test-suite))
  (remhash (test-suite-name test) (tests-hash suite)))

(defmethod test-named ((name string) (suite test-suite))
  (gethash name (tests-hash suite)))

(defmethod setup-testsuite-named (name)
  (declare (ignore name))
  t)

(defmethod teardown-testsuite-named (name)
  (declare (ignore name))
  t)

(defmethod run-test ((suite test-suite) &key (handle-errors t))
  (setup-testsuite-named (slot-value suite 'name))
  (let ((res (mapcar (lambda (test) (run-test test
                                              :handle-errors handle-errors))
                     (tests suite))))
    (teardown-testsuite-named (slot-value suite 'name))
    res))


(defmethod report-result ((result test-result) &key (stream t) (verbose nil))
  "Print out a test-result object for a report to STREAM, default to
standard-output.  If VERBOSE is non-nil then will produce a lengthy
and informative report, otherwise just prints wether the test passed
or failed or errored out."
  (if verbose (format stream
		      "------------------------------------------------------~%"))
  (format stream "Test ~A ~A ~%"
	  (test-name (result-test result))
	  (cond
	   ((test-failures result) "Failed")
	   ((test-errors result) "Errored")
	   (t "Passed")))
  (if verbose
      (progn
	(format stream "Description: ~A~%" (description (result-test result)))
	(if (test-failures result)
	    (progn
	      (format stream "Failures:~%")
	      (mapcar #'(lambda (fail) (format stream "    ~A" fail))
		      (test-failures result))))
	(if (test-errors result)
	    (progn
	      (format stream "Errors:~%")
	      (mapcar #'(lambda (fail) (format stream "    ~A" fail))
		      (test-errors result))))))
  (format stream "~%~%"))

(defmethod report-result ((results list) &key (stream t) (verbose nil))
  (dolist (foo results)
    (report-result foo :stream stream :verbose verbose)))








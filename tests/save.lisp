;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND-TEST -*-

#|

DESC: tests/save.lisp - testing code for save/load of game

|#

(in-package :lb-test)

(def-lb-fixture save-fixture (in-game)
  ()
  (:documentation "Simple fixture for testing save/load."))


(defmethod perform-test ((fix save-fixture))

;;  (%loc-save-test lb::*dungeon* :dungeon)
  (let ((var-obj lb::*variant*))
    (%loc-save-test var-obj :variant :load-variant nil :save-variant var-obj)
    (%loc-save-test lb::*level* :level :load-variant var-obj :save-variant var-obj)
    (%loc-save-test lb::*player* :player :load-variant var-obj :save-variant var-obj)
    ))

(defun %loc-save-test (obj type &key load-variant save-variant)
  "Type should be a string."

  (test-assert (lb::ok-object? obj))
  
  (flet ((scat (a b)
	   (concatenate 'string (string a) (string b))))
    (let ((r-save (scat "dumps/rsave." type))
	  (b-save (scat "dumps/bsave." type))
	  (*variant* nil)
	  )
      
      (lb::do-save save-variant r-save obj :readable)
      (lb::do-save save-variant b-save obj :binary)
    
      (let* ((readable (lb::do-load load-variant r-save type :readable))
	     (binary   (lb::do-load load-variant b-save type :binary))
	     (original obj))

	(when (listp readable)
	  (setf readable (car readable)))
	(when (listp binary)
	  (setf binary (car binary)))
		
	(test-assert (lb::ok-object? readable))
	(test-assert (lb::ok-object? binary))
	
	(lb::%save-obj readable (scat "dumps/rafter." type))
	(lb::%save-obj binary   (scat "dumps/bafter." type))
	(lb::%save-obj original (scat "dumps/oafter." type))

	;; the following two are not interesting anymore as we have lang-equal
;;	(test-assert (not (equal readable binary)))
;;	(test-assert (not (equalp readable binary)))
	(test-assert (lb::lang-equal readable binary))
	(test-assert (lb::lang-equal original readable))
	(test-assert (lb::lang-equal original binary)))
      
    )))



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
  (%loc-save-test lb::*variant* :variant)
  (%loc-save-test lb::*level* :level)
  (%loc-save-test lb::*player* :player)
  )

(defun %loc-save-test (obj type)
  "Type should be a string."

  (test-assert (lb::ok-object? obj))
  
  (flet ((scat (a b)
	   (concatenate 'string (string a) (string b))))
    (let ((r-save (scat "dumps/rsave." type))
	  (b-save (scat "dumps/bsave." type)))
      
      (lb::do-save :readable r-save obj)
      (lb::do-save :binary   b-save obj)
    
      (let ((readable (lb::do-load :readable r-save type))
	    (binary   (lb::do-load :binary b-save type))
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



;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.testing -*-

#|

DESC: tests/save.lisp - testing code for save/load of game

|#

(in-package :org.langband.testing)

(def-lb-fixture save-fixture (in-game)
  ()
  (:documentation "Simple fixture for testing save/load."))


(defmethod perform-test ((fix save-fixture))
  (%save/load-test lb::*variant* lb::*player* lb::*level*))

(defun %save/load-test (var-obj pl lvl)

  (test-assert (lb::ok-object? var-obj :context :in-game :warn-on-failure t))
  ;;    (format t "OK~%")
  (test-assert (lb::ok-object? pl      :context :in-game :warn-on-failure t))
  (test-assert (lb::ok-object? lvl     :context :in-game :warn-on-failure t))

  (flet ((scat (a b)
	   (concatenate 'string (string a) (string b)))
	 (do-a-load (fname type)
	   (let* ((lb::*variant* nil)
		  (lb::*player* nil)
		  (lb::*level* nil))
	     (lb::load-a-saved-game nil fname type))))
      	   
    (let* ((type "full")
	   (lb::*variant* nil)
	   (lb::*player* nil)
	   (lb::*level* nil)
	    
	   (r-save (scat *dumps-directory* "rsave." type))
	   (b-save (scat *dumps-directory* "bsave." type))
	   )

      (lb::save-the-game var-obj pl lvl :fname r-save :format :readable)
      (lb::save-the-game var-obj pl lvl :fname b-save :format :binary)

      ;;	(trace do-a-load)
	
      (let ((readable (do-a-load r-save :readable))
	    (binary   (do-a-load b-save :binary))
	    (originals (list var-obj pl lvl))
	    )

	;;	  (format t "Readable is ~s~%" readable)
	;;	  (format t "Binary is ~s~%" binary)
	(assert (and (consp readable) (= (length readable) 3)))
	(assert (and (consp binary) (= (length readable) 3)))
	  
					#||
	(when (listp readable)
	(setf readable (car readable)))
	(when (listp binary)
	(setf binary (car binary)))
	||#
	;; a quick test
	(dolist (i readable)
	  (test-assert (lb::ok-object? i :context :in-game :warn-on-failure t)))
	(dolist (i binary)
	  (test-assert (lb::ok-object? i :context :in-game :warn-on-failure t)))

	(loop for read in readable
	      for bin in binary
	      do (test-assert (lb::lang-equal read bin)))
	(loop for read in readable
	      for orig in originals
	      do (test-assert (lb::lang-equal read orig)))
	(loop for bin in binary
	      for orig in originals
	      do (test-assert (lb::lang-equal bin orig)))

	  
					#||	
	(lb::%save-obj readable (scat "dumps/rafter." type))
	(lb::%save-obj binary   (scat "dumps/bafter." type))
	(lb::%save-obj original (scat "dumps/oafter." type))

;; the following two are not interesting anymore as we have lang-equal
	;;	(test-assert (not (equal readable binary)))
	;;	(test-assert (not (equalp readable binary)))
	(test-assert (lb::lang-equal readable binary))
	(test-assert (lb::lang-equal original readable))
	(test-assert (lb::lang-equal original binary)))
      ||#
      ))))

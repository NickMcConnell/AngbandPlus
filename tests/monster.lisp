;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND-TEST -*-

#|

DESC: tests/monster.lisp - testing code for monsters

|#

(in-package :lb-test)

(def-lb-fixture pre-monster-fixture (pre-variant)
  ()
  (:documentation "Simple fixture for testing monster."))


(def-lb-fixture in-monster-fixture (in-game)
  ()
  (:documentation "Simple fixture for testing monster."))


(defun %compatibility-read-test (var-obj lvl-obj)
  (let ((orig-mon (lb::variant.monsters var-obj))
	(orig-obj (lb::variant.objects var-obj))
	(lb::*variant* var-obj)
	(lb::*level* lvl-obj)
	)

    (unwind-protect
	 (let ((m-compat-table (make-hash-table :test #'eq))
	       (o-compat-table (make-hash-table :test #'eq))
	       (m-straight-table (make-hash-table :test #'eq))
	       (o-straight-table (make-hash-table :test #'eq))
	       )
	   
	   (setf (lb::variant.objects var-obj) o-compat-table
		 (lb::variant.monsters var-obj) m-compat-table)
	   (lb::van-register-levels! var-obj)
	   #+compatibility-objects
	   (lb::van-register-sorting-values! var-obj)
	   
	   #+compatibility-monsters
	   (lb::initialise-monsters& var-obj :old-file "r_info.txt")
	   #+compatibility-objects
	   (lb::initialise-objects& var-obj :old-file "k_info.txt")

   
	   (setf (lb::variant.objects var-obj) o-straight-table
		 (lb::variant.monsters var-obj) m-straight-table)
	   (lb::van-register-levels! var-obj)
	   #+compatibility-objects
	   (lb::van-register-sorting-values! var-obj)
	   
	   #+compatibility-monsters
	   (lb::initialise-monsters& var-obj :file "monsters.lisp")
	   #+compatibility-objects
	   (lb::initialise-objects& var-obj :file "objects.lisp")

	   (and t
		#+compatibility-monsters
		(test-assert (lb::lang-equal m-straight-table m-compat-table))
		#+compatibility-objects
		(test-assert (lb::lang-equal o-straight-table o-compat-table))
		))
      (setf (lb::variant.monsters var-obj) orig-mon
	    (lb::variant.objects var-obj) orig-obj)
    )))

(defun %monster-attack-conversion ()
  (let* ((attk-list '((<touch> :type <eat_gold> :damage nil)
		      (<beg> :type nil :damage nil)
		      (<hit> :damage (1 . 1))
		      ))
	 (attacks (lb::convert-obj attk-list :attacks))
	 )
    (and (every #'(lambda (x) (typep x 'lb::attack)) attacks)
	 (lb::lang-equal attacks (lb::convert-obj (lb::convert-obj attacks :attk-list)
						  :attacks))
	 )))


(defmethod perform-test ((fix pre-monster-fixture))
  (and (%monster-attack-conversion)
       (%compatibility-read-test (lb::van-make-variant-obj)
				 (lb::van-create-bare-town-level-obj))))

(defmethod perform-test ((fix in-monster-fixture))
  (and t
       #+(or compatibility-monsters compatibility-objects)
       (%compatibility-read-test lb::*variant* lb::*level*)))

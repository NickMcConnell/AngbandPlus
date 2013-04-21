;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla-testing -*-

#|

DESC: variants/vanilla/tests/object.lisp - testing code for game-objects

|#

(in-package :org.langband.vanilla-testing)

(lb-test:def-lb-fixture description-fixture (post-variant)
  ()
  (:documentation "Simple fixture for testing obj descriptions."))



(defmethod perform-test ((fix description-fixture))

  (labels ((%make-obj (var id)
	     (let ((kind (lb:get-object-kind var id)))
;;	       (format t "Have kind ~a~%" kind)
	       (lb:produce-active-object var kind)))
	   (%check-obj (var id wanted-str)
	     (let ((str (with-output-to-string (s)
			  (lb:write-obj-description var (%make-obj var id) s :store t))))
	       (lb:report-equal str wanted-str))))
	 
    
    (let* ((var-obj lb:*variant*);;(lb::van-make-variant-obj))
	   ;;(lb:*variant* var-obj)
	   (lb:*level* (lb::van-create-bare-town-level-obj)))
      
;;      (format t "Going checks on items.~%")
      ;; armour
      (%check-obj var-obj "chain-mail" "chain mail (-2) [14,+0]")
      
      ;; armour, plural
      (let ((item (%make-obj var-obj "chain-mail")))
	(test-assert (typep item 'active-object))
	(setf (lb:aobj.number item) 2)
	(lb:report-equal (with-output-to-string (s)
			   (write-obj-description var-obj item s :store t))
			 "2 chain mails (-2) [14,+0]"))

      ;; light-sources
      (%check-obj var-obj "lantern" "a brass lantern (half-full)")
      (%check-obj var-obj "torch" "a wooden torch (almost fresh)")

      (let ((item (%make-obj var-obj "torch")))
	(test-assert (typep item 'active-object))
	(setf (lb:aobj.number item) 2)
	(lb:report-equal (with-output-to-string (s)
			   (write-obj-description var-obj item s :store t))
			 "2 wooden torches (almost fresh)"))

      
;;      (format t "Going checks on rings.~%")
      ;; rings
      (let ((item (%make-obj var-obj "ring-dmg")))
	(test-assert (typep item 'active-object))
	(setf (lb:gval.dmg-modifier (lb:aobj.game-values item)) 7)
	(lb:report-equal (with-output-to-string (s)
			   (write-obj-description var-obj item s :store t))
			 "a ring of damage (+7)"))

      (let ((item (%make-obj var-obj "ring-protection")))
	(test-assert (typep item 'active-object))
	(setf (lb:gval.ac-modifier (lb:aobj.game-values item)) 7)
	(lb:report-equal (with-output-to-string (s)
			   (lb:write-obj-description var-obj item s :store t))
			 "a ring of protection [+7]"))

    
      t)))

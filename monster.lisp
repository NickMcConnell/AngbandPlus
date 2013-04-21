;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: monster.lisp - monster-code
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: The code which deals with critters you can meet in the dungeon.

|#

(in-package :langband)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defclass monster-kind ()
    (
     (id :accessor monster.id :initform nil)
     (name :accessor monster.name :initform nil)
     (desc :accessor monster.desc :initform nil)
     (symbol :accessor monster.symbol :initform nil)
     (colour :accessor monster.colour :initform nil)
     (alignment :accessor monster.alignment :initform nil)
     (type :accessor monster.type :initform nil)
     (level :accessor monster.level :initform nil)
     (rarity :accessor monster.rarity :initform nil)
     (hitpoints :accessor monster.hitpoints :initform nil)
     (armour :accessor monster.armour :initform nil)
     (speed :accessor monster.speed :initform nil)
     (xp :accessor monster.xp :initform 0)
     (abilities :accessor monster.abilities :initform nil)
     ;;   (resists :accessor monster.resists :initform nil)
     (immunities :accessor monster.immunities :initform nil)
     (vulnerabilities :accessor monster.vulnerabilities :initform nil)
     (alertness :accessor monster.alertness :initform nil)
     (vision :accessor monster.vision :initform nil)
     (attacks :accessor monster.attacks :initform nil)
     (treasures :accessor monster.treasures :initform nil)
     (sex :accessor monster.sex :initform nil)
     (special-abilities :accessor monster.sp-abilities :initform nil)
     )) 

  (defclass attack ()
    (
     (kind :accessor attack.kind :initform nil)
     (dmg-type :accessor attack.dmg-type :initform nil)
     (damage :accessor attack.damage :initform nil)
     ))

  )

(defun create-monster (id)
  "Returns an appropriate monster or NIL."
  (let ((kind (get-monster-kind id)))
    
    (when kind
      (let ((amon (make-instance 'active-monster :kind kind)))
	(setf (amon.cur-hp amon) 5)
	;; blah
	amon))))

(defun define-monster-kind (id name &key desc symbol colour
			    alignment type level
			    rarity hitpoints armour
			    speed xp abilities
			    immunities alertness
			    vulnerabilities
			    vision attacks sp-abilities
			    treasures sex)
  "Defines a critter you might bump into when you least expect it"
  
  (declare (ignore symbol colour alignment type level
		   rarity hitpoints armour speed xp abilities
		   immunities alertness vulnerabilities
		   vision attacks sp-abilities treasures sex))
  
  (warn "Creating monster ~a [~a]" name id)

  (let ((m-obj (make-instance 'Monster-kind)))

    (setf (monster.id m-obj) id)
    (setf (monster.name m-obj) name)

    (if (stringp desc)
	(setf (monster.desc m-obj) desc)
	(warn "No description for monster ~a found" id))

    (setf (get-monster-kind id) m-obj) 
    
    m-obj))


(defmethod print-object ((inst monster-kind) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S]" (class-name (class-of inst)) 
	   (monster.id inst)
	   (monster.name inst)))
  inst)


(defmethod print-object ((inst active-monster) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~S, (~s,~s)]" (class-name (class-of inst)) 
	   (amon.kind inst) (location-x inst) (location-y inst))
  inst))


(defmethod monster.name ((mon active-monster))
  (monster.name (amon.kind mon)))

(defun get-mkind-table ()
  (let* ((o-table (get-mtype-table *level* *variant*))
	 (table (gobj-table.obj-table o-table)))
    table))

(defun %get-mkind-alloc-tbl ()
  (let* ((o-table (get-mtype-table *level* *variant*))
	 (table (gobj-table.alloc-table o-table)))
    table))
 

(defun get-monster-kind (id)
  "Returns monster-kind or nil."
  (let ((table (get-mkind-table)))
    (gethash id table)))

(defun add-new-mkind! (obj id)
  ""
  (declare (ignore id))
  (apply-filters-on-obj :monsters *variant* obj))

(defun add-monster-kind-to-table! (monster-kind id table)
  ""
  (setf (gethash id table) monster-kind))

(defun (setf get-monster-kind) (monster-kind id)
  "establishes a monster-kind"

  (let ((table (get-mkind-table)))
    (add-monster-kind-to-table! monster-kind id table)))




(defmethod dump-object ((obj monster-kind) stream (style (eql :lispy)))
  
    (let* (;;(attacks (monster.attacks obj))
	   (clause `(define-monster-kind ,(monster.id obj)
		     ,(monster.name obj)
		     :desc ,(monster.desc obj)
		     :symbol ,(monster.symbol obj)
		     :colour ,(symbolify (monster.colour obj))
		     :alignment ,(symbolify (monster.alignment obj))
		     :type ,(symbolify (monster.type obj))
		     :level ,(monster.level obj)
		     :rarity ,(monster.rarity obj)
		     :hitpoints ,(monster.hitpoints obj)
		     :armour ,(monster.armour obj)
		     :speed ,(monster.speed obj)
		     :xp ,(monster.xp obj)
		     :alertness ,(monster.alertness obj)
		     :vision ,(monster.vision obj)

		     :abilities ,(symbolify (monster.abilities obj))
		     :sp-abilities ,(symbolify (monster.sp-abilities obj))
		     ;;		  :resists ,(monster.resists obj)
		     :immunities ,(symbolify (monster.immunities obj))
		     :vulnerabilities ,(symbolify (monster.vulnerabilities obj))
		     :attacks ,(symbolify (mon-attacks-as-list obj))
		     :treasures ,(symbolify (monster.treasures obj))
		     :sex ,(symbolify (monster.sex obj))
		     )
	  
	     ))
  
      (pprint clause stream)))

(defun mon-attacks-as-list (monster)
  (let ((attacks (monster.attacks monster)))
    (mapcar #'(lambda (x)
		(list (attack.kind x) :type (attack.dmg-type x) :damage (attack.damage x)))
	    attacks)))

(defun get-monster-list (&optional (var-obj *variant*))
  "returns a fresh list.  Remove me!"
  (declare (ignore var-obj))
  (let ((table (get-mkind-table)))
    (stable-sort (loop for v being each hash-value of table
		       collecting v)
		 #'string-equal
		 :key #'monster.id)))



#||
(defun dump-all-monsters (out-file monster-list style)
  (with-open-file (o-str (pathname out-file)
			 :direction :output 
			 :if-exists :supersede)
    (let ((*print-case* :downcase)
	  (*print-escape* t)))
    (dolist (i monster-list)
      (dump-object i o-str style))))

(defun print-all-monsters (&optional (var-obj *variant*))
  (let ((*print-readably* nil)
	(table (get-mkind-table)))
    
    (maphash #'(lambda (k v)
		 (format t "~a -> ~a~%" k v))
	     table)))
||#

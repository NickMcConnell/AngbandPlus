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
     (speed :accessor monster.speed :initform 0)
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


  )

(defmethod has-ability? ((mon monster-kind) ability)
  (dolist (i (monster.abilities mon))
    (cond ((and (symbolp i) (eq ability i))
	   (return-from has-ability? i))
	  ((and (consp i) (eq (car i) ability))
	   (return-from has-ability? i))))
  nil)
	  
(defmethod has-ability? ((mon active-monster) ability)
  (has-ability? (amon.kind mon) ability))

(defun create-monster (id)
  "Returns an appropriate monster or NIL."

  (assert (or (stringp id) (symbolp id)))
  
  (let ((kind (get-monster-kind (if (symbolp id)
				    (symbol-name id)
				    id))))
    
    (when kind
      (create-active-monster kind))))


(defun create-active-monster (kind)
  (assert (not (eq kind nil)))
  (let ((amon (make-instance 'active-monster :kind kind))
	(num-hitdice (car (monster.hitpoints kind)))
	(hitdice (cdr (monster.hitpoints kind))))
    
    (if (has-ability? kind '<max-hitpoints>)
	(setf (current-hp amon) (* num-hitdice hitdice))
	(setf (current-hp amon) (roll-dice num-hitdice hitdice)))

    (setf (get-creature-max-hp amon) (current-hp amon))
    (setf (get-creature-speed amon) (monster.speed kind))
;;    (warn "Monster ~a got ~a hp from ~a dice" (get-creature-name amon)
;;	  (current-hp amon) (monster.hitpoints kind))
    ;; blah
    amon))

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

    (assert (or (stringp id) (symbolp id)))
    (setf (monster.id m-obj) (if (symbolp id)
				 (symbol-name id)
				 id))
    (assert (stringp name))
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


(defmethod get-xp-value ((creature active-monster))
  (monster.xp (amon.kind creature)))

(defmethod get-creature-name ((creature active-monster))
  (monster.name (amon.kind creature)))

(defmethod get-creature-ac ((creature active-monster))
  (monster.armour (amon.kind creature)))


(defmethod monster.name ((creature active-monster)) ;; remove eventually
  (monster.name (amon.kind creature)))

(defmethod get-creature-name ((creature player))
  (player.name creature))

(defmethod increase-xp! ((mon active-monster) amount)
  (declare (ignore amount))
  nil)

(defmethod monster.attacks ((mon active-monster))
  (monster.attacks (amon.kind mon)))

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
  (assert (or (stringp id) (symbolp id)))
  (let ((table (get-mkind-table))
	(key (if (symbolp id) (symbol-name id) id)))
;;    (warn "htbl has test ~a" (hash-table-test table))
    (gethash key table)))

(defun add-new-mkind! (obj id)
  ""
  (declare (ignore id))
  (apply-filters-on-obj :monsters *variant* obj))

(defun (setf get-monster-kind) (monster-kind id)
  "establishes a monster-kind"

  (assert (or (stringp id) (symbolp id)))
    
  (let ((table (get-mkind-table))
	(key (if (symbolp id) (symbol-name id) id)))
    (setf (gethash key table) monster-kind)))



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


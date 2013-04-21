;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: monster.lisp - monster-code
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: The code which deals with critters you can meet in the dungeon.

|#

(in-package :langband)

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

(defclass l-attack ()
  (
   (kind :accessor attack.kind :initform nil)
   (dmg-type :accessor attack.dmg-type :initform nil)
   (damage :accessor attack.damage :initform nil)
   ))


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
   (format stream "~:(~S~) [~S]" (class-name (class-of inst)) 
	   (amon.kind inst))
  inst))


(defmethod monster.name ((mon active-monster))
  (monster.name (amon.kind mon)))

(defun get-monster-kind (id)
  "Returns monster-kind or nil."
  (gethash id *monster-kind-table*))

(defun (setf get-monster-kind) (monster-kind id)
  "establishes a monster-kind"
  (setf (gethash id *monster-kind-table*) monster-kind))

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

(defun get-monster-list ()
  "returns a fresh list.  Remove me!"
  (stable-sort (loop for v being each hash-value of *monster-kind-table*
		     collecting v)
	       #'string-equal
	       :key #'monster.id))


(defun allocate-monster! (dungeon player distance sleeping)
  "distance is a fixnum, sleeping is either NIL or T."

  (let ((x nil)
	(y nil)
	(dungeon-height (dungeon.height dungeon))
	(dungeon-width  (dungeon.width dungeon))
	(px (player.loc-x player))
	(py (player.loc-y player)))

    (loop named search-for-spot
	  do
	  (setq x (random dungeon-width)
		y (random dungeon-height))

	  (when (and (cave-boldly-naked? dungeon x y)
		     (> (distance x y px py) distance))
	    (return-from search-for-spot nil)))
    
  (if (place-monster! dungeon player x y sleeping t)
      t
      nil)
  ))

(defun place-single-monster! (dun pl mon x y sleeping)
  "blah.."
  (declare (ignore pl))
  
  (setf (cave-monsters dun x y) mon)
  
  )

(defun place-monster! (dungeon player x y sleeping group)
  "Tries to place a monster at given coordinates.."

  (declare (ignore group))
  
  (let ((possible-monster (get-monster-by-level (dungeon.level dungeon))))
    (when possible-monster
      (place-single-monster! dungeon player possible-monster x y sleeping)))

  
  )

(defun get-monster-kind-by-level (level)
  "Returns a monster-kind for the given level."

  ;; skipping boost
  
  (let ((total 0)
	(counter 0)
	(table *alloc-table-monsters*))

;;    (warn "Table is ~a" (length table))

    (loop named counting-area
	  for a-obj across table
	  do
;;	  (warn "Checking ~a" a-obj)
	  (when (> (alloc.level a-obj) level)
	    (return-from counting-area nil))
	  ;; skip chest-check
	  (setf (alloc.prob3 a-obj) (alloc.prob2 a-obj))
	  (incf total (alloc.prob3 a-obj))
	  (incf counter))


    (when (= 0 total)
      (warn "No suitable objects at level ~a" level)
      (return-from get-monster-kind-by-level nil))

    (let ((val (random total)))
;;      (warn "Counter is ~a and total is ~a and we got ~a" counter total val)

      ;; loop through objects and find one
      (loop for a-obj across table
	    do
	    (when (< val (alloc.prob3 a-obj))
	      (return-from get-monster-kind-by-level (alloc.obj a-obj)))
	    (decf val (alloc.prob3 a-obj)))
      
      ))
  
  nil)


(defun get-monster-by-level (level)
  "Returns an (active) object by level."
  (let ((the-kind (get-monster-kind-by-level level)))
    (if (not the-kind)
	nil
	(let ((a (make-instance 'active-monster :obj the-kind)))
	  (setf (amon.cur-hp a) 5)
	  a))))


#||
(defun dump-all-monsters (out-file monster-list style)
  (with-open-file (o-str (pathname out-file)
			 :direction :output 
			 :if-exists :supersede)
    (let ((*print-case* :downcase)
	  (*print-escape* t)))
    (dolist (i monster-list)
      (dump-object i o-str style))))

(defun print-all-monsters ()
  (let ((*print-readably* nil))
    (maphash #'(lambda (k v)
		 (format t "~a -> ~a~%" k v))
	     *monster-kind-table*)))
||#

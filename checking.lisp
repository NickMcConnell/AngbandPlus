;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: checking.lisp - equality and various predicates
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defgeneric lang-equal (first-obj second-obj)
    (:documentation "A recursive check for equality (along the lines of EQUAL)
but one that works with langband-objects.")))


(defmethod lang-equal (first-obj second-obj)
  (equal first-obj second-obj))

(defmethod lang-equal :around (x y)
  (unless (next-method-p)
     ;; this will never happen
     (lang-warn "Unable to find LANG-EQUAL for types ~a and ~a" (type-of x) (type-of y))
     (return-from lang-equal nil))
  
  (let ((retval (call-next-method)))
    (unless retval
      (typecase x
	(dungeon-coord
	 (lang-warn "Equality failed for ~s and ~s" (type-of x) (type-of y))
	 (describe x)
	 (describe y)
	 )

	(t
	 (lang-warn "Equality failed for ~s and ~s" (type-of x) (type-of y)))
	))
    retval))

#||
;; don't give this one anything circular
(defmethod lang-equal ((x cons) (y cons))
  (and (lang-equal (car x) (car y))
       (lang-equal (cdr x) (cdr y))))
||#

(defmethod lang-equal ((first-obj cons) (second-obj cons))
  (every #'lang-equal first-obj second-obj))


(defmethod lang-equal ((x array) (y array))
  (and (= (array-rank x) (array-rank y))
       (dotimes (axis (array-rank x) t)
	 (unless (= (array-dimension x axis)
		    (array-dimension y axis))
	   (return nil)))
       (dotimes (index (array-total-size x) t)
	 (let ((x-el (row-major-aref x index))
	       (y-el (row-major-aref y index)))
	   (unless (or (eq x-el y-el)
		       (lang-equal x-el y-el))
	     (return nil))))))

(defmethod lang-equal ((first-obj dungeon) (second-obj dungeon))

  (and (= (dungeon.depth first-obj)
	  (dungeon.depth second-obj))
       (= (dungeon.height first-obj)
	  (dungeon.height second-obj))
       (= (dungeon.width first-obj)
	  (dungeon.width second-obj))
       (lang-equal (dungeon.table first-obj)
		   (dungeon.table second-obj))
       (lang-equal (dungeon.monsters first-obj)
		   (dungeon.monsters second-obj))
       (lang-equal (dungeon.rooms first-obj)
		   (dungeon.rooms second-obj))
       (lang-equal (dungeon.triggers first-obj)
		   (dungeon.triggers second-obj))))
       

(defmethod lang-equal ((first-obj dungeon-coord) (second-obj dungeon-coord))
  (and (= (coord.feature first-obj)
	  (coord.feature second-obj))
       (= (coord.flags first-obj)
	  (coord.flags second-obj))
       (lang-equal (coord.objects first-obj)
		   (coord.objects second-obj))
       (lang-equal (coord.monsters first-obj)
		   (coord.monsters second-obj))))
 
(defmethod lang-equal ((x active-monster) (y active-monster))
  (and (amon.kind x)
       (amon.kind y)
       (equal (monster.id (amon.kind x))
	      (monster.id (amon.kind y)))
       (= (current-hp x)
	  (current-hp y))
       (= (get-creature-max-hp x)
	  (get-creature-max-hp y))
       (= (get-creature-speed x)
	  (get-creature-speed y))
       (= (get-creature-energy x)
	  (get-creature-energy y))
       (= (get-creature-mana x)
	  (get-creature-mana y))
       (= (location-x x)
	  (location-x y))
       (= (location-y x)
	  (location-y y))
       (equal (creature-alive? x)
	      (creature-alive? y))))
	  
(defmethod lang-equal ((x active-object) (y active-object))
  (and (aobj.kind x)
       (aobj.kind y)
       (equal (object.id (aobj.kind x))
	      (object.id (aobj.kind y)))
       (equal (aobj.inscr x)
	      (aobj.inscr y))
       (equal (aobj.number x)
	      (aobj.number y))
       (lang-equal (aobj.contains x)
		   (aobj.contains y))
       (lang-equal (aobj.events x)
		   (aobj.events y))
       (= (location-x x)
	  (location-x y))
       (= (location-y x)
	  (location-y y))))

(defmethod lang-equal ((x items-on-floor) (y items-on-floor))

  (and (= (items.cur-size x)
	  (items.cur-size y))
       (lang-equal (items.objs x)
		   (items.objs y))
       ;; skip dungeon as it is just a help-pointer
       (= (location-x x)
	  (location-x y))
       (= (location-y x)
	  (location-y y))))

(defmethod lang-equal ((x items-worn) (y items-worn))

  (and (= (items.cur-size x)
	  (items.cur-size y))
       (lang-equal (items.objs x)
		   (items.objs y))
       ))

(defmethod lang-equal ((x items-in-container) (y items-in-container))

  #||
  (unless (= (items.cur-size x)
	     (items.cur-size y))
    (error "CS"))
  (unless (= (items.max-size x)
	     (items.max-size y))
    (lang-warn "MS"))
  ||#
  
  (and (= (items.cur-size x)
	  (items.cur-size y))
       (lang-equal (items.objs x)
		   (items.objs y))
       (= (items.max-size x)
	  (items.max-size y))
       ))

(defmethod lang-equal ((x active-room) (y active-room))

  (and (room.type x)
       (room.type y)
       (equal (room-type.id (room.type x))
	      (room-type.id (room.type y)))

       (= (location-x x)
	  (location-x y))
       (= (location-y x)
	  (location-y y))))

(defmethod lang-equal ((x player) (y player))
  
  (and (equal (class.id (player.class x))    (class.id (player.class y)))
       (equal (race.id  (player.race x))     (race.id (player.race y)))

       (equal (player.name x) 	       (player.name y))
       (equal (player.sex x) 	       (player.sex y))

       (equalp (player.base-stats x)    (player.base-stats y))
       (equalp (player.curbase-stats x) (player.curbase-stats y))
       (equalp (player.modbase-stats x) (player.modbase-stats y))
       (equalp (player.active-stats x)  (player.active-stats y))

       (equal (player.view-x x)        (player.view-x y))
       (equal (player.view-y x)        (player.view-y y))

       (equal (player.level x)         (player.level y))
       (equal (player.max-level x)     (player.max-level y))
       
       (equal (player.max-hp x)        (player.max-hp y))
       (equal (player.max-mana x)      (player.max-mana y))
       (equalp (player.xp-table x)      (player.xp-table y))
       (equalp (player.hp-table x)      (player.hp-table y))

       (equal (player.energy-use x)    (player.energy-use y))
       (equal (player.leaving-p x)     (player.leaving-p y))
       
       (equal (player.dead-p x)        (player.dead-p y))
       (equal (player.speed x)         (player.speed y))
       
       (equal (player.loc-x x)         (player.loc-x y))
       (equal (player.loc-y x)         (player.loc-y y))

       (equal (player.depth x)         (player.depth y))
       (equal (player.max-depth x)     (player.max-depth y))
       (equal (player.max-xp x)        (player.max-xp y))
       (equal (player.cur-xp x)        (player.cur-xp y))
       (equal (player.fraction-xp x)   (player.fraction-xp y))
       (equal (player.cur-hp x)        (player.cur-hp y))
       (equal (player.fraction-hp x)   (player.fraction-hp y))
       (equal (player.cur-mana x)      (player.cur-mana y))
       (equal (player.fraction-mana x) (player.fraction-mana y))

       (equal (player.gold x)         (player.gold y))
       (equal (player.energy x)       (player.energy y))
       
       (equal (player.base-ac x)        (player.base-ac y))
       (equal (player.ac-bonus x)       (player.ac-bonus y))
       (equal (player.light-radius x)   (player.light-radius y))
       (equal (player.infravision x)    (player.infravision y))

       (lang-equal (player.equipment x) (player.equipment y))
       (lang-equal (player.inventory x) (player.inventory y))
       (lang-equal (player.skills x)    (player.skills y))
       ))


(defmethod lang-equal ((x skills) (y skills))
  
  (dolist (i (variant.skill-translations *variant*))
    (unless (= (slot-value x (cdr i))
	       (slot-value y (cdr i)))
      (return-from lang-equal nil)))
  t)

(defmethod lang-equal ((x level) (y level))

  (and (equal (level.id x) (level.id y))
       (equal (level.depth x) (level.depth y))
       (equal (level.rating x) (level.rating y))
       (lang-equal (level.dungeon x)
		   (level.dungeon y))
       ))

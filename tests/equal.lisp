;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: tests/equal.lisp - equality and various predicates
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)


(defmacro report-predicate (pred x y)
  (let ((retval (gensym))
	(x-val (gensym))
	(y-val (gensym)))
    `(let* ((,x-val ,x)
	    (,y-val ,y)
	    (,retval (,pred ,x-val ,y-val)))
      (if ,retval
	  ,retval
	  (progn
	    (format t "~&LANG-EQUAL: Check ~s ~s failed, values: ~s ~s~%" ',pred ',x ,x-val ,y-val)
	    ,retval)))))

(defmacro report-equal (x y)
  `(report-predicate equal ,x ,y))

(defmacro report-equalp (x y)
  `(report-predicate equalp ,x ,y))

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
	(cons
	 (lang-warn "Equality failed for ~s and ~s" x y))
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
       ;; add later
       ;;(lang-equal (dungeon.triggers first-obj)
	;;	   (dungeon.triggers second-obj))
       ))
       

(defmethod lang-equal ((first-obj dungeon-coord) (second-obj dungeon-coord))
  (and (= (coord.feature first-obj)
	  (coord.feature second-obj))
;;       (= (coord.flags first-obj)
;;	  (coord.flags second-obj))
       (= (logand (coord.flags first-obj) +saved-cave-flags+)
	  (logand (coord.flags second-obj) +saved-cave-flags+))
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
       (= (maximum-hp x)
	  (maximum-hp y))
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
	  (location-y y))
       (= (aobj.identify x)
	  (aobj.identify y))
       ))

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
  ;; add misc and abilities
  
  (and

   (report-equal (player.name x) 	       (player.name y))
   (report-equal (class.id (player.class x))   (class.id (player.class y)))
   (report-equal (race.id  (player.race x))    (race.id (player.race y)))
   (report-equal (player.sex x) 	       (player.sex y))

   (report-equalp (player.base-stats x)    (player.base-stats y))
   (report-equalp (player.cur-statmods x)  (player.cur-statmods y))
   
   (report-equalp (player.hp-table x)      (player.hp-table y))
   (lang-equal (player.equipment x) (player.equipment y))
   (lang-equal (player.misc x)      (player.misc y))

   (report-equal (player.dead-from x) 	       (player.dead-from y))

   (report-equal (location-x x)         (location-x y))
   (report-equal (location-y x)         (location-y y))
   (report-equal (player.view-x x)        (player.view-x y))
   (report-equal (player.view-y x)        (player.view-y y))

   (report-equal (player.depth x)         (player.depth y))
   (report-equal (player.max-depth x)     (player.max-depth y))

   (report-equal (player.max-xp x)        (player.max-xp y))
   (report-equal (player.cur-xp x)        (player.cur-xp y))
   (report-equal (player.fraction-xp x)   (player.fraction-xp y))
   
   (report-equal (current-hp x)           (current-hp y))
   (report-equal (player.fraction-hp x)   (player.fraction-hp y))

   (report-equal (player.cur-mana x)      (player.cur-mana y))
   (report-equal (player.fraction-mana x) (player.fraction-mana y))

   (report-equal (player.gold x)         (player.gold y))
   (report-equal (player.food x)         (player.food y))
   (report-equal (player.energy x)       (player.energy y))

   (report-equal (player.level x)         (player.level y))
   (report-equal (player.max-level x)     (player.max-level y))   


   (report-equal (maximum-hp x)           (maximum-hp y))
   (report-equal (player.max-mana x)      (player.max-mana y))
   (report-equalp (player.xp-table x)      (player.xp-table y))

   (report-equal (player.energy-use x)    (player.energy-use y))
   (report-equal (player.leaving-p x)     (player.leaving-p y))
       
   (report-equal (player.dead-p x)        (player.dead-p y))
   (report-equal (player.speed x)         (player.speed y))

   (report-equal (player.burden x)         (player.burden y))
   (report-equal (player.light-radius x)   (player.light-radius y))
   (report-equal (player.infravision x)    (player.infravision y))

   (lang-equal (player.inventory x) (player.inventory y))
   (lang-equal (player.skills x)    (player.skills y))

   (report-equalp (player.modbase-stats x) (player.modbase-stats y))
   (report-equalp (player.active-stats x)  (player.active-stats y))
       
   (lang-equal (player.actual-abilities x)     (player.actual-abilities y))
   (lang-equal (player.perceived-abilities x)  (player.perceived-abilities y))

   (report-equal (creature.resists x)    (creature.resists y))
   ))


(defmethod lang-equal ((x skills) (y skills))
  (let ((var-obj *variant*))
    (when var-obj ;; fix this?
      (dolist (i (variant.skill-translations var-obj))
	(unless (= (slot-value x (cdr i))
		   (slot-value y (cdr i)))
	  (return-from lang-equal nil))))
    t))

(defmethod lang-equal ((x level) (y level))

  (and (equal (level.id x) (level.id y))
       (equal (level.depth x) (level.depth y))
       (equal (level.rating x) (level.rating y))
       (lang-equal (level.dungeon x)
		   (level.dungeon y))
       ))

(defun %print-set-diff (x y)
  (let ((s-a (loop for xk being the hash-keys of x collecting xk))
	(s-b (loop for xk being the hash-keys of y collecting xk)))
    (warn "Set-diff ~s ~s" (set-difference s-a s-b :test #'equal) (set-difference s-b s-a :test #'equal))))

(defmethod lang-equal ((x hash-table) (y hash-table))
  (flet ((compare-tables (xtbl ytbl)
	   (maphash #'(lambda (x-key x-value)
			(multiple-value-bind (y-value foundp)
			    (gethash x-key ytbl)
			  (unless (and foundp (lang-equal x-value y-value))
			    (return-from compare-tables nil)))
			nil)
		    xtbl)
	   t))
    
    (and (report-equal (hash-table-test x) (hash-table-test y))
	 (let ((cnt-eql (report-equal (hash-table-count x) (hash-table-count y))))
	   (unless cnt-eql
	     (%print-set-diff x y))
	   cnt-eql)						 
	 (compare-tables x y)
	 )))

(defmethod lang-equal ((x game-obj-table) (y game-obj-table))

  (and (lang-equal (gobj-table.obj-table x) (gobj-table.obj-table y))
       (lang-equal (gobj-table.alloc-table x) (gobj-table.alloc-table y))
       (lang-equal (gobj-table.obj-table-by-lvl x) (gobj-table.obj-table-by-lvl y))))

(defmethod lang-equal ((x alloc-entry) (y alloc-entry))
  (and (lang-equal (alloc.obj x)   (alloc.obj y))
       (lang-equal (alloc.index x) (alloc.index y))
       (lang-equal (alloc.depth x) (alloc.depth y))
       ;;; ....
       ))
       

(defmethod lang-equal ((x monster-kind) (y monster-kind))
  (and (report-equal (monster.id x) (monster.id y))
       (report-equal (monster.name x) (monster.name y))
       (report-equal (monster.desc x) (monster.desc y))
       (report-equal (monster.symbol x) (monster.symbol y))
       (report-equal (monster.colour x) (monster.colour y))
       (report-equal (monster.alignment x) (monster.alignment y))
       (report-equal (monster.type x) (monster.type y))
       (report-equal (monster.depth x) (monster.depth y))
       (report-equal (monster.rarity x) (monster.rarity y))
       (report-equal (monster.hitpoints x) (monster.hitpoints y))
       (report-equal (monster.armour x) (monster.armour y))
       (report-equal (monster.speed x) (monster.speed y))
       (report-equal (monster.xp x) (monster.xp y))
       (report-equal (monster.sex x) (monster.sex y))

       (report-equal (monster.abilities x) (monster.abilities y))
       (report-equal (monster.immunities x) (monster.immunities y))
       (report-equal (monster.vulnerabilities x) (monster.vulnerabilities y))
       (report-equal (monster.alertness x) (monster.alertness y))
       (report-equal (monster.vision x) (monster.vision y))
       (lang-equal   (monster.attacks x) (monster.attacks y))
       (report-equal (monster.treasures x) (monster.treasures y))
       (report-equal (monster.sp-abilities x) (monster.sp-abilities y))
       ))
		   
(defmethod lang-equal ((x attack) (y attack))
  (and (report-equal (attack.kind x) (attack.kind y))
       (report-equal (attack.dmg-type x) (attack.dmg-type y))
       (report-equal (attack.damage x) (attack.damage y))
       ))

(defmethod lang-equal ((x object-kind) (y object-kind))
  (and (report-equal (object.id x) (object.id y))
       (report-equal (object.numeric-id x) (object.numeric-id y))
       (report-equal (object.name x) (object.name y))
       (report-equal (object.x-attr x) (object.x-attr y))
       (report-equal (object.x-char x) (object.x-char y))
       (report-equal (object.depth x) (object.depth y))
       (report-equal (object.rarity x) (object.rarity y))
       (report-equalp (object.chance x) (object.chance y))
       (report-equalp (object.locale x) (object.locale y))
       (report-equal (object.weight x) (object.weight y))
       (report-equal (object.cost x) (object.cost y))
       (report-equal (object.obj-type x) (object.obj-type y))
       (report-equal (object.flags x) (object.flags y))
       (lang-equal (object.game-values x) (object.game-values y))
       (report-equal (object.easy-know x) (object.easy-know y))
       (report-equal (object.aware x) (object.aware y))
       (report-equal (object.tried x) (object.tried y))
       (report-equal (object.flavour x) (object.flavour y))
       (report-equal (object.sort-value x) (object.sort-value y))
       ;; skip events
       ;; skip effects
       ))

(defmethod lang-equal ((x game-values) (y game-values))
  (and (report-equal (gval.base-ac x) (gval.base-ac y))
       (report-equal (gval.ac-modifier x) (gval.ac-modifier y))
       (report-equal (gval.base-dice x) (gval.base-dice y))
       (report-equal (gval.num-dice x) (gval.num-dice y))
       (report-equal (gval.tohit-modifier x) (gval.tohit-modifier y))
       (report-equal (gval.dmg-modifier x) (gval.dmg-modifier y))
       (report-equal (gval.mana x) (gval.mana y))
       (report-equal (gval.charges x) (gval.charges y))
       (report-equal (gval.food-value x) (gval.food-value y))
       (report-equal (gval.light-radius x) (gval.light-radius y))
       (report-equal (gval.tunnel x) (gval.tunnel y))
       (report-equal (gval.speed x) (gval.speed y))
       (report-equal (gval.skill-modifiers x) (gval.skill-modifiers y))
       (report-equal (gval.stat-modifiers x) (gval.stat-modifiers y))
       (report-equal (gval.ignores x) (gval.ignores y))
       (report-equal (gval.resists x) (gval.resists y))
       (report-equal (gval.immunities x) (gval.immunities y))
       (report-equal (gval.abilities x) (gval.abilities y))
       (report-equal (gval.sustains x) (gval.sustains y))
       (report-equal (gval.slays x) (gval.slays y))
       ))

(defmethod lang-equal ((x player-abilities) (y player-abilities))
  (and (report-equal (pl-ability.base-ac x)         (pl-ability.base-ac y))
       (report-equal (pl-ability.ac-modifier x)     (pl-ability.ac-modifier y))
       (report-equal (pl-ability.to-hit-modifier x) (pl-ability.to-hit-modifier y))
       (report-equal (pl-ability.to-dmg-modifier x) (pl-ability.to-dmg-modifier y))
       ))


(defmethod lang-equal ((x misc-player-info) (y misc-player-info))
  (and (report-equal (playermisc.age x)         (playermisc.age y))
       (report-equal (playermisc.status x)      (playermisc.status y))
       (report-equal (playermisc.height x)      (playermisc.height y))
       (report-equal (playermisc.weight x)      (playermisc.weight y))
       ))

(defmethod lang-equal ((x variant) (y variant))
  (and
   (report-equal  (variant.id x)       (variant.id y))
   (report-equal  (variant.name x)     (variant.name y))
   (report-equal  (variant.sys-file x) (variant.sys-file y))
   (report-equalp (variant.sexes x)    (variant.sexes y))
   (lang-equal    (variant.races x)    (variant.races y))
   ;; add more
   ))

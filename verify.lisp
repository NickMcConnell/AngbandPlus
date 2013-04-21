;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: verify.lisp - verification of objects
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

;; the verify-code should report which test screws up!

(defmacro verify-check (check)
  "Very hacky macro to report which checks fail in OK-OBJECT?"
  `(if ,check
    t
    (progn
      (when warn-on-failure
	(warn "~s failed in OK-OBJECT?" ',check))
      (return-from ok-object? nil))))
 


;; we assume things
(defmethod ok-object? (obj &key context warn-on-failure)
  (declare (ignore context))
  (when warn-on-failure
    (warn "The object ~s of type ~s failed on OK-OBJECT?" obj (class-of obj)))
  nil)

(defmethod ok-object? ((obj active-room) &key context warn-on-failure)
  (declare (ignore context))
  (verify-check (not (eq nil (room.type obj))))
  (verify-check (stringp (room-type.id (room.type obj))))
  (verify-check (not (minusp (location-x obj))))
  (verify-check (not (minusp (location-y obj))))
  t)

(defmethod ok-object? ((obj active-object) &key context warn-on-failure)

  (verify-check (ok-object? (aobj.kind obj) :context context :warn-on-failure warn-on-failure))
  (verify-check (stringp (aobj.inscr obj)))
  (verify-check (integerp (aobj.number obj)))
  (verify-check (>= (aobj.number obj) 0))
  (verify-check (not (minusp (location-x obj))))
  (verify-check (not (minusp (location-y obj))))
  t)

(defmethod ok-object? ((player player) &key context warn-on-failure)

    (verify-check (not (eq nil (player.inventory player))))
    (verify-check (not (eq nil (player.equipment player))))
    (verify-check (typep (player.equipment player) 'items-worn))
    (verify-check (eq (item-table-find (player.equipment player) 'eq.backpack)
		   (player.inventory player)))
	 
    (verify-check (stringp (player.name player)))
    (verify-check (typep (player.race player) 'character-race))
    (verify-check (typep (player.class player) 'character-class))

    (verify-check (ok-object? (player.misc player) :context context :warn-on-failure warn-on-failure))
    (verify-check (ok-object? (player.perceived-abilities player) :context context :warn-on-failure warn-on-failure))
    (verify-check (ok-object? (player.actual-abilities player) :context context :warn-on-failure warn-on-failure))

    (verify-check (integerp (maximum-hp player)))
    (verify-check (integerp (current-hp player)))
    (verify-check (<= 0 (maximum-hp player)))
    ;;(verify-check (<= 0 (current-hp player)))
	 
    ;;(verify-check (stringp (player.dead-from player)))

    (verify-check (<= 0 (player.burden player)))
    (verify-check (<= 0 (player.light-radius player)))
    (verify-check (<= 0 (player.infravision player)))
    (verify-check (<= 0 (player.see-invisible player)))
    
    (verify-check (arrayp (player.base-stats player)))
    (verify-check (arrayp (player.cur-statmods player)))
    (verify-check (arrayp (player.active-stats player)))
    (verify-check (arrayp (player.modbase-stats player)))
    
    (let ((bstat-table (player.base-stats player))
	  (cstat-table (player.cur-statmods player))
	  (mstat-table (player.modbase-stats player))
	  (astat-table (player.active-stats player)))
	   
      (verify-check (and (not (eq bstat-table cstat-table))
		      (not (eq bstat-table mstat-table))
		      (not (eq bstat-table astat-table))
		      (not (eq cstat-table mstat-table))
		      (not (eq cstat-table astat-table))
		      (not (eq mstat-table astat-table)))))
    )

(defmethod ok-object? ((obj level) &key context warn-on-failure)
  (declare (ignore context))
  (verify-check (not (eq (level.dungeon obj) nil)))
  (verify-check (numberp (level.depth obj)))
  (verify-check (numberp (dungeon.depth (level.dungeon obj))))
  (verify-check (= (level.depth obj) (dungeon.depth (level.dungeon obj))))
  (verify-check (numberp (level.rating obj))) ;; might need fixing later
  t)

(defmethod ok-object? ((obj l-event) &key context warn-on-failure)
  (declare (ignore context))
  (verify-check (stringp (event.id obj)))
  (verify-check (typep (event.return obj) 'return-actions))
  (verify-check (functionp (event.function obj)))
  (verify-check (symbolp (event.type obj)))
  t)

 
(defmethod ok-object? ((obj monster-kind) &key context warn-on-failure)

  (verify-check (stringp (monster.id obj)))
  (verify-check (stringp (monster.desc obj)))
;;  (verify-check (characterp (x-char obj)))
;;  (verify-check (characterp (x-attr obj)))
  ;; skip alignment
  ;; skip type
  (verify-check (listp (alloc-locations obj)))
  ;; skip hp
  (verify-check (integerp (monster.armour obj)))
  (verify-check (<= 0 (monster.armour obj)))
  (verify-check (integerp (monster.speed obj)))
  (verify-check (<= 0 (monster.speed obj)))
  (verify-check (integerp (monster.xp obj)))
  (verify-check (<= 0 (monster.xp obj)))
  ;; skip gender
  ;; skip abilities
  ;; skip immunities
  (verify-check (integerp (monster.alertness obj)))
  (verify-check (<= 0 (monster.alertness obj)))
  (verify-check (integerp (monster.vision obj)))
  (verify-check (<= 0 (monster.vision obj)))

  (dolist (attk (monster.attacks obj) t)
    (verify-check (ok-object? attk :context context :warn-on-failure warn-on-failure)))
  (dolist (treas (monster.treasures obj) t)
    (verify-check (ok-object? treas :context context :warn-on-failure warn-on-failure)))
  ;; skip vulnerabilities
  ;; skip special-abilities
  t)


(defmethod ok-object? ((ab player-abilities) &key context warn-on-failure)
  (declare (ignore context))
  (verify-check (integerp (pl-ability.base-ac ab)))
  (verify-check (>= (pl-ability.base-ac ab) 0))
  (verify-check (integerp (pl-ability.ac-modifier ab)))
  (verify-check (integerp (pl-ability.to-hit-modifier ab)))
  (verify-check (integerp (pl-ability.to-dmg-modifier ab)))
  t)

(defmethod ok-object? ((info misc-player-info) &key context warn-on-failure)
  (declare (ignore context))
  (verify-check (integerp (playermisc.age    info)))
  (verify-check (integerp (playermisc.status info)))
  (verify-check (integerp (playermisc.height info)))
  (verify-check (integerp (playermisc.weight info)))
  t)

(defmethod ok-object? ((dungeon dungeon) &key context warn-on-failure)
  (verify-check (integerp (dungeon.depth dungeon)))
  (verify-check (integerp (dungeon.height dungeon)))
  (verify-check (integerp (dungeon.width dungeon)))
  ;; skip table
  (dolist (mon (dungeon.monsters dungeon) t)
    (verify-check (ok-object? mon :context context :warn-on-failure warn-on-failure)))
  (dolist (obj (dungeon.objects dungeon) t)
    (verify-check (ok-object? obj :context context :warn-on-failure warn-on-failure)))
  (dolist (room (dungeon.rooms dungeon) t)
    (verify-check (ok-object? room :context context :warn-on-failure warn-on-failure)))
  ;; skip active
  ;; skip triggers
  t)

(defmethod ok-object? ((var-obj variant) &key context warn-on-failure)

  (verify-check (activated? var-obj))
  (verify-check (stringp (variant.id var-obj)))
  (verify-check (stringp (variant.name var-obj)))
  ;; config-path
  (dolist (gender (variant.genders var-obj) t)
    (verify-check (ok-object? gender :context context :warn-on-failure warn-on-failure)))
  (loop for race being the hash-values of (variant.races var-obj)
	do (verify-check (ok-object? race :context context :warn-on-failure warn-on-failure)))
  (loop for cclass being the hash-values of (variant.races var-obj)
	do (verify-check (ok-object? cclass :context context :warn-on-failure warn-on-failure)))
  (dolist (effect (variant.effects var-obj) t)
    (verify-check (ok-object? effect :context context :warn-on-failure warn-on-failure)))
  (dolist (element (variant.elements var-obj) t)
    (verify-check (ok-object? element :context context :warn-on-failure warn-on-failure)))

  (verify-check (integerp (variant.turn var-obj)))
  (verify-check (<= 0 (variant.turn var-obj)))
  ;; skip turn-events
  ;; skip level-builders
  ;; skip floor-features
  ;; skip room-builders
  
  (verify-check (integerp (variant.max-depth var-obj)))
  (verify-check (plusp (variant.max-depth var-obj)))
  (verify-check (integerp (variant.max-charlevel var-obj)))
  (verify-check (plusp (variant.max-charlevel var-obj)))
  (verify-check (arrayp (variant.xp-table var-obj))) ;; should also check that it's increasing

  ;; might want to check the content of these tables as well
  (verify-check (hash-table-p (variant.monsters var-obj)))
  (loop for mon being the hash-values of (variant.monsters var-obj)
	do (verify-check (ok-object? mon :context context :warn-on-failure warn-on-failure)))
  (verify-check (hash-table-p (variant.objects var-obj)))
  (loop for obj being the hash-values of (variant.objects var-obj)
	do (verify-check (ok-object? obj :context context :warn-on-failure warn-on-failure)))
  (verify-check (hash-table-p (variant.monsters-by-level var-obj)))
  (verify-check (hash-table-p (variant.objects-by-level var-obj)))
  (verify-check (hash-table-p (variant.filters var-obj)))

  ;; skip flavour-types
  ;; skip house-types
  ;; skip house-owners
  ;; skip skill-translations
  ;; skip attk-descs
  (verify-check (integerp (variant.day-length var-obj)))
  (verify-check (plusp (variant.day-length var-obj)))
  ;; skip help-topics
  t)

(defmethod ok-object? ((mon active-monster) &key context warn-on-failure)

  (verify-check (ok-object? (amon.kind mon) :context context :warn-on-failure warn-on-failure))
  (verify-check (integerp (current-hp mon)))
  (verify-check (integerp (maximum-hp mon)))
  (verify-check (integerp (get-creature-speed mon)))
  (verify-check (<= 0 (get-creature-speed mon)))
  (verify-check (integerp (get-creature-energy mon)))
  (verify-check (<= 0 (get-creature-energy mon)))
  (verify-check (integerp (get-creature-mana mon)))
  (verify-check (<= 0 (get-creature-mana mon)))
  ;; add more
  t)

(defmethod ok-object? ((obj attack) &key context warn-on-failure)
  (declare (ignore context))
  (verify-check (symbolp (attack.kind obj)))
  (verify-check (symbolp (attack.dmg-type obj)))
  (verify-check (not (eq nil (attack.kind obj))))
  ;; can be nil apparently, probably means normal hit
;;  (verify-check (not (eq nil (attack.dmg-type obj))))
  ;; skip damage
  t)

(defmethod ok-object? ((obj treasure-drop) &key context warn-on-failure)
  (declare (ignore context))
   (verify-check (numberp (drop.chance obj)))
   (verify-check (or (integerp (drop.amount obj))
		  (and (consp (drop.amount obj))
		       (integerp (car (drop.amount obj)))
		       (integerp (cdr (drop.amount obj))))))
		       
   (verify-check (symbolp (drop.quality obj)))
   (verify-check (symbolp (drop.type obj)))
   t)

(defmethod ok-object? ((obj gender) &key context warn-on-failure)
  (declare (ignore context))
  (verify-check (stringp (gender.id obj)))
  (verify-check (stringp (gender.name obj)))
  (verify-check (symbolp (gender.symbol obj)))
  (verify-check (stringp (gender.win-title obj)))
  t)

(defmethod ok-object? ((obj character-race) &key context warn-on-failure)
  (declare (ignore context))
  (verify-check (stringp (race.id obj)))
  (verify-check (stringp (race.name obj)))
  (verify-check (symbolp (race.symbol obj)))
  (verify-check (stringp (race.desc obj)))
  (verify-check (integerp (race.xp-extra obj)))
  (verify-check (integerp (race.hit-dice obj)))
  ;; skip stat-changes
  ;; skip abilities
  (verify-check (integerp (race.resists obj)))
  (verify-check (<= 0 (race.resists obj)))
  ;; skip classes
  ;; skip start-eq
  ;; skip skills
  t)

(defmethod ok-object? ((obj effect) &key context warn-on-failure)
  (declare (ignore context))
  (verify-check (stringp (effect.name obj)))
  (verify-check (nonboolsym? (effect.symbol obj)))
  (verify-check (integerp (effect.number obj)))
  (verify-check (integerp (effect.bit-flag obj)))
  (verify-check (<= 0 (effect.number obj)))
  (verify-check (<= 0 (effect.bit-flag obj)))
  t)

(defmethod ok-object? ((obj element) &key context warn-on-failure)
  (declare (ignore context))
  (verify-check (stringp (element.name obj)))
  (verify-check (nonboolsym? (element.symbol obj)))
  (verify-check (integerp (element.number obj)))
  (verify-check (integerp (element.bit-flag obj)))
  (verify-check (<= 0 (element.number obj)))
  (verify-check (<= 0 (element.bit-flag obj)))
  t)

(defmethod ok-object? ((obj object-kind) &key context warn-on-failure)
  (declare (ignore context))
  (verify-check (stringp (object.id obj)))
  (verify-check (integerp (object.sort-value obj)))
  (verify-check (listp (alloc-locations obj)))
  
  ;; improve flavour-testing later!!!
  #||
  (cond ((and (need-flavour? *variant* obj)
	      (eq context :in-game))
	 (verify-check (legal-flavour-obj? (object.flavour obj))))
	(t
	 (verify-check (eq (object.flavour obj) nil))))
||#		 
  t)


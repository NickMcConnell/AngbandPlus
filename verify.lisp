;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: verify.lisp - verification of objects
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

;; the verify-code should report which test screws up!

(defmacro %ok-check (check)
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
  (%ok-check (not (eq nil (room.type obj))))
  (%ok-check (stringp (room-type.id (room.type obj))))
  (%ok-check (not (minusp (location-x obj))))
  (%ok-check (not (minusp (location-y obj))))
  t)

(defmethod ok-object? ((obj active-object) &key context warn-on-failure)

  (%ok-check (ok-object? (aobj.kind obj) :context context :warn-on-failure warn-on-failure))
  (%ok-check (stringp (aobj.inscr obj)))
  (%ok-check (integerp (aobj.number obj)))
  (%ok-check (>= (aobj.number obj) 0))
  (%ok-check (not (minusp (location-x obj))))
  (%ok-check (not (minusp (location-y obj))))
  t)

(defmethod ok-object? ((player player) &key context warn-on-failure)

    (%ok-check (not (eq nil (player.inventory player))))
    (%ok-check (not (eq nil (player.equipment player))))
    (%ok-check (typep (player.equipment player) 'items-worn))
    (%ok-check (eq (item-table-find (player.equipment player) 'eq.backpack)
		   (player.inventory player)))
	 
    (%ok-check (stringp (player.name player)))
    (%ok-check (typep (player.race player) 'character-race))
    (%ok-check (typep (player.class player) 'character-class))

    (%ok-check (ok-object? (player.misc player) :context context :warn-on-failure warn-on-failure))
    (%ok-check (ok-object? (player.perceived-abilities player) :context context :warn-on-failure warn-on-failure))
    (%ok-check (ok-object? (player.actual-abilities player) :context context :warn-on-failure warn-on-failure))

    (%ok-check (integerp (maximum-hp player)))
    (%ok-check (integerp (current-hp player)))
    (%ok-check (<= 0 (maximum-hp player)))
    (%ok-check (<= 0 (current-hp player)))
	 
    (%ok-check (stringp (player.dead-from player)))

    (%ok-check (<= 0 (player.burden player)))
    (%ok-check (<= 0 (player.light-radius player)))
    (%ok-check (<= 0 (player.infravision player)))
    (%ok-check (<= 0 (player.see-invisible player)))
    
    (%ok-check (arrayp (player.base-stats player)))
    (%ok-check (arrayp (player.cur-statmods player)))
    (%ok-check (arrayp (player.active-stats player)))
    (%ok-check (arrayp (player.modbase-stats player)))
    
    (let ((bstat-table (player.base-stats player))
	  (cstat-table (player.cur-statmods player))
	  (mstat-table (player.modbase-stats player))
	  (astat-table (player.active-stats player)))
	   
      (%ok-check (and (not (eq bstat-table cstat-table))
		      (not (eq bstat-table mstat-table))
		      (not (eq bstat-table astat-table))
		      (not (eq cstat-table mstat-table))
		      (not (eq cstat-table astat-table))
		      (not (eq mstat-table astat-table)))))
    )

(defmethod ok-object? ((obj level) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (not (eq (level.dungeon obj) nil)))
  (%ok-check (numberp (level.depth obj)))
  (%ok-check (numberp (dungeon.depth (level.dungeon obj))))
  (%ok-check (= (level.depth obj) (dungeon.depth (level.dungeon obj))))
  (%ok-check (numberp (level.rating obj))) ;; might need fixing later
  t)

(defmethod ok-object? ((obj l-event) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (stringp (event.id obj)))
  (%ok-check (typep (event.return obj) 'return-actions))
  (%ok-check (functionp (event.function obj)))
  (%ok-check (symbolp (event.type obj)))
  t)

 
(defmethod ok-object? ((obj monster-kind) &key context warn-on-failure)

  (%ok-check (stringp (monster.id obj)))
  (%ok-check (stringp (monster.desc obj)))
;;  (%ok-check (characterp (x-char obj)))
;;  (%ok-check (characterp (x-attr obj)))
  ;; skip alignment
  ;; skip type
  (%ok-check (integerp (monster.depth obj)))
  (%ok-check (<= 0 (monster.depth obj)))
  (%ok-check (integerp (monster.rarity obj)))
  (%ok-check (<= 0 (monster.rarity obj)))
  ;; skip hp
  (%ok-check (integerp (monster.armour obj)))
  (%ok-check (<= 0 (monster.armour obj)))
  (%ok-check (integerp (monster.speed obj)))
  (%ok-check (<= 0 (monster.speed obj)))
  (%ok-check (integerp (monster.xp obj)))
  (%ok-check (<= 0 (monster.xp obj)))
  ;; skip gender
  ;; skip abilities
  ;; skip immunities
  (%ok-check (integerp (monster.alertness obj)))
  (%ok-check (<= 0 (monster.alertness obj)))
  (%ok-check (integerp (monster.vision obj)))
  (%ok-check (<= 0 (monster.vision obj)))

  (dolist (attk (monster.attacks obj) t)
    (%ok-check (ok-object? attk :context context :warn-on-failure warn-on-failure)))
  (dolist (treas (monster.treasures obj) t)
    (%ok-check (ok-object? treas :context context :warn-on-failure warn-on-failure)))
  ;; skip vulnerabilities
  ;; skip special-abilities
  t)


(defmethod ok-object? ((ab player-abilities) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (integerp (pl-ability.base-ac ab)))
  (%ok-check (>= (pl-ability.base-ac ab) 0))
  (%ok-check (integerp (pl-ability.ac-modifier ab)))
  (%ok-check (integerp (pl-ability.to-hit-modifier ab)))
  (%ok-check (integerp (pl-ability.to-dmg-modifier ab)))
  t)

(defmethod ok-object? ((info misc-player-info) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (integerp (playermisc.age    info)))
  (%ok-check (integerp (playermisc.status info)))
  (%ok-check (integerp (playermisc.height info)))
  (%ok-check (integerp (playermisc.weight info)))
  t)

(defmethod ok-object? ((dungeon dungeon) &key context warn-on-failure)
  (%ok-check (integerp (dungeon.depth dungeon)))
  (%ok-check (integerp (dungeon.height dungeon)))
  (%ok-check (integerp (dungeon.width dungeon)))
  ;; skip table
  (dolist (mon (dungeon.monsters dungeon) t)
    (%ok-check (ok-object? mon :context context :warn-on-failure warn-on-failure)))
  (dolist (obj (dungeon.objects dungeon) t)
    (%ok-check (ok-object? obj :context context :warn-on-failure warn-on-failure)))
  (dolist (room (dungeon.rooms dungeon) t)
    (%ok-check (ok-object? room :context context :warn-on-failure warn-on-failure)))
  ;; skip active
  ;; skip triggers
  t)

(defmethod ok-object? ((var-obj variant) &key context warn-on-failure)

  (%ok-check (activated? var-obj))
  (%ok-check (stringp (variant.id var-obj)))
  (%ok-check (stringp (variant.name var-obj)))
  ;; sys-file
  ;; config-path
  (dolist (gender (variant.genders var-obj) t)
    (%ok-check (ok-object? gender :context context :warn-on-failure warn-on-failure)))
  (loop for race being the hash-values of (variant.races var-obj)
	do (%ok-check (ok-object? race :context context :warn-on-failure warn-on-failure)))
  (loop for cclass being the hash-values of (variant.races var-obj)
	do (%ok-check (ok-object? cclass :context context :warn-on-failure warn-on-failure)))
  (dolist (effect (variant.effects var-obj) t)
    (%ok-check (ok-object? effect :context context :warn-on-failure warn-on-failure)))
  (dolist (element (variant.elements var-obj) t)
    (%ok-check (ok-object? element :context context :warn-on-failure warn-on-failure)))

  (%ok-check (integerp (variant.turn var-obj)))
  (%ok-check (<= 0 (variant.turn var-obj)))
  ;; skip turn-events
  ;; skip level-builders
  ;; skip floor-features
  ;; skip room-builders
  
  (%ok-check (integerp (variant.max-depth var-obj)))
  (%ok-check (plusp (variant.max-depth var-obj)))
  (%ok-check (integerp (variant.max-charlevel var-obj)))
  (%ok-check (plusp (variant.max-charlevel var-obj)))
  (%ok-check (arrayp (variant.xp-table var-obj))) ;; should also check that it's increasing

  ;; might want to check the content of these tables as well
  (%ok-check (hash-table-p (variant.monsters var-obj)))
  (loop for mon being the hash-values of (variant.monsters var-obj)
	do (%ok-check (ok-object? mon :context context :warn-on-failure warn-on-failure)))
  (%ok-check (hash-table-p (variant.objects var-obj)))
  (loop for obj being the hash-values of (variant.objects var-obj)
	do (%ok-check (ok-object? obj :context context :warn-on-failure warn-on-failure)))
  (%ok-check (hash-table-p (variant.monsters-by-level var-obj)))
  (%ok-check (hash-table-p (variant.objects-by-level var-obj)))
  (%ok-check (hash-table-p (variant.filters var-obj)))

  ;; skip flavour-types
  ;; skip house-types
  ;; skip house-owners
  ;; skip skill-translations
  ;; skip attk-descs
  (%ok-check (integerp (variant.day-length var-obj)))
  (%ok-check (plusp (variant.day-length var-obj)))
  ;; skip help-topics
  t)

(defmethod ok-object? ((mon active-monster) &key context warn-on-failure)

  (%ok-check (ok-object? (amon.kind mon) :context context :warn-on-failure warn-on-failure))
  (%ok-check (integerp (current-hp mon)))
  (%ok-check (integerp (maximum-hp mon)))
  (%ok-check (integerp (get-creature-speed mon)))
  (%ok-check (<= 0 (get-creature-speed mon)))
  (%ok-check (integerp (get-creature-energy mon)))
  (%ok-check (<= 0 (get-creature-energy mon)))
  (%ok-check (integerp (get-creature-mana mon)))
  (%ok-check (<= 0 (get-creature-mana mon)))
  ;; add more
  t)

(defmethod ok-object? ((obj attack) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (symbolp (attack.kind obj)))
  (%ok-check (symbolp (attack.dmg-type obj)))
  (%ok-check (not (eq nil (attack.kind obj))))
  ;; can be nil apparently, probably means normal hit
;;  (%ok-check (not (eq nil (attack.dmg-type obj))))
  ;; skip damage
  t)

(defmethod ok-object? ((obj treasure-drop) &key context warn-on-failure)
  (declare (ignore context))
   (%ok-check (numberp (drop.chance obj)))
   (%ok-check (or (integerp (drop.amount obj))
		  (and (consp (drop.amount obj))
		       (integerp (car (drop.amount obj)))
		       (integerp (cdr (drop.amount obj))))))
		       
   (%ok-check (symbolp (drop.quality obj)))
   (%ok-check (symbolp (drop.type obj)))
   t)

(defmethod ok-object? ((obj gender) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (stringp (gender.id obj)))
  (%ok-check (stringp (gender.name obj)))
  (%ok-check (symbolp (gender.symbol obj)))
  (%ok-check (stringp (gender.win-title obj)))
  t)

(defmethod ok-object? ((obj character-race) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (stringp (race.id obj)))
  (%ok-check (stringp (race.name obj)))
  (%ok-check (symbolp (race.symbol obj)))
  (%ok-check (stringp (race.desc obj)))
  (%ok-check (integerp (race.xp-extra obj)))
  (%ok-check (integerp (race.hit-dice obj)))
  ;; skip stat-changes
  ;; skip abilities
  (%ok-check (integerp (race.resists obj)))
  (%ok-check (<= 0 (race.resists obj)))
  ;; skip classes
  ;; skip start-eq
  ;; skip skills
  t)

(defmethod ok-object? ((obj effect) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (stringp (effect.name obj)))
  (%ok-check (nonboolsym? (effect.symbol obj)))
  (%ok-check (integerp (effect.number obj)))
  (%ok-check (integerp (effect.bit-flag obj)))
  (%ok-check (<= 0 (effect.number obj)))
  (%ok-check (<= 0 (effect.bit-flag obj)))
  t)

(defmethod ok-object? ((obj element) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (stringp (element.name obj)))
  (%ok-check (nonboolsym? (element.symbol obj)))
  (%ok-check (integerp (element.number obj)))
  (%ok-check (integerp (element.bit-flag obj)))
  (%ok-check (<= 0 (element.number obj)))
  (%ok-check (<= 0 (element.bit-flag obj)))
  t)

(defmethod ok-object? ((obj object-kind) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (stringp (object.id obj)))
  (%ok-check (integerp (object.sort-value obj)))

  ;; improve flavour-testing later!!!
  #||
  (cond ((and (need-flavour? *variant* obj)
	      (eq context :in-game))
	 (%ok-check (legal-flavour-obj? (object.flavour obj))))
	(t
	 (%ok-check (eq (object.flavour obj) nil))))
||#		 
  t)


;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: verify.lisp - verification of objects
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

;; we assume things
(defmethod ok-object? (obj)
  (declare (ignore obj))
  t)

(defmethod ok-object? ((obj active-room))
  
  (and (not (eq nil (room.type obj)))
       (stringp (room-type.id (room.type obj)))
       (not (minusp (location-x obj)))
       (not (minusp (location-y obj)))))

(defmethod ok-object? ((obj active-object))
  
    (and (stringp (object.id (aobj.kind obj)))
	 (stringp (aobj.inscr obj))
	 (not (minusp (aobj.number obj)))
	 (not (minusp (location-x obj)))
	 (not (minusp (location-y obj)))
	 ))

(defmethod ok-object? ((pl player))
  (let ((player pl)) ;; easier to cut'n paste from other code
    (and (not (eq nil (player.inventory pl)))
	 (not (eq nil (player.equipment pl)))
	 (typep (player.equipment pl) 'items-worn)
	 (eq (item-table-find (player.equipment pl) 'eq.backpack)
	     (player.inventory pl))
	 
	 (stringp (player.name player))
	 (typep (player.race pl) 'race)
	 (typep (player.class pl) 'character-class)

	 (stringp (player.dead-from player))
	 
	 (arrayp (player.base-stats player))
	 (arrayp (player.curbase-stats player))
	 (arrayp (player.active-stats player))
	 (arrayp (player.modbase-stats player))

	 (let ((bstat-table (player.base-stats player))
	       (cstat-table (player.curbase-stats player))
	       (mstat-table (player.modbase-stats player))
	       (astat-table (player.active-stats player)))
	   
	   (and (not (eq bstat-table cstat-table))
		(not (eq bstat-table mstat-table))
		(not (eq bstat-table astat-table))
		(not (eq cstat-table mstat-table))
		(not (eq cstat-table astat-table))
		(not (eq mstat-table astat-table))))
	 
	 )))

(defmethod ok-object? ((obj level))

  (and (not (eq (level.dungeon obj) nil))
       (numberp (level.depth obj))
       (numberp (dungeon.depth (level.dungeon obj)))
       (= (level.depth obj) (dungeon.depth (level.dungeon obj)))
       (numberp (level.rating obj)) ;; might need fixing later
       ))

(defmethod ok-object? ((obj l-event))

  (and (not (eq nil (event.id obj)))
       (or (stringp (event.id obj)) (symbolp (event.id obj)))
       (typep (event.return obj) 'return-actions)
       (functionp (event.function obj))
       (symbolp (event.type obj))
       ))
 
(defmethod ok-object? ((obj monster-kind))
  (and (numberp (monster.level obj))
       (numberp (monster.rarity obj))
       (symbolp (monster.id obj))))

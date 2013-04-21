;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: dump.lisp - code related to dumping various information-structures, ...
Copyright (c) 2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)


(defmethod get-loadable-form ((variant variant) (object game-values) &key)
  
  (let ((the-form '()))
    (flet ((possibly-add (initarg val &optional (def-val nil))
	     (unless (equal val def-val)
	       (setf the-form (nconc the-form (list initarg (loadable-val val)))))))
      (setf the-form (list 'make-game-values))
      
      (possibly-add :base-ac (gval.base-ac object) 0)
      (possibly-add :ac-modifier (gval.ac-modifier object) 0)
      (possibly-add :base-dice (gval.base-dice object) 0)
      (possibly-add :num-dice (gval.num-dice object) 0)
      (possibly-add :tohit-modifier (gval.tohit-modifier object) 0)
      (possibly-add :dmg-modifier (gval.dmg-modifier object) 0)
      (possibly-add :mana (gval.mana object) 0)
      (possibly-add :charges (gval.charges object) 0)
      (possibly-add :food-value (gval.food-value object) 0)
      (possibly-add :light-radius (gval.light-radius object) 0)
      (possibly-add :tunnel (gval.tunnel object) 0)
      (possibly-add :speed (gval.speed object) 0)
      (possibly-add :skill-modifiers (gval.skill-modifiers object))
      (possibly-add :stat-modifiers (gval.stat-modifiers object))
      (possibly-add :ignores (gval.ignores object) 0)
      (possibly-add :resists (gval.resists object) 0)
      (possibly-add :immunities (gval.immunities object) 0)
      (possibly-add :abilities (gval.abilities object))
      (possibly-add :sustains (gval.sustains object))
      (possibly-add :slays (gval.slays object))

      the-form)))


(defmethod get-loadable-form ((variant variant) (object object-kind) &key (full-dump nil))
  
  (let ((the-form '()))
    (flet ((possibly-add (initarg val &optional (def-val nil))
	     (unless (equal val def-val)
	       (setf the-form (nconc the-form (list initarg (loadable-val val)))))))
    (setf the-form (list 'define-object-kind 
			 (object.id object)
			 (object.name object)))
    (possibly-add :numeric-id (object.numeric-id object))
;;    (possibly-add :desc (object.desc object))
    (possibly-add :x-attr (convert-obj (object.x-attr object) :letter))
    (possibly-add :x-char (object.x-char object))
    (possibly-add :depth (object.depth object))
    (possibly-add :rarity (object.rarity object))
    (possibly-add :chance (object.chance object) #(0 0 0 0))
    (possibly-add :locale (object.locale object) #(0 0 0 0))
    (possibly-add :weight (object.weight object))
    (possibly-add :cost (object.cost object))
    (possibly-add :obj-type (object.obj-type object))
    (possibly-add :flags (object.flags object))
    (possibly-add :identified (object.tried object))
    (possibly-add :sort-value (object.sort-value object) 0)
    (possibly-add :easy-know (object.easy-know object))
    (possibly-add :the-kind (object.the-kind object))
    
    (when full-dump
      (possibly-add :flavour (object.flavour object)))


    (when-bind (gval (object.game-values object))
      (setf the-form (append the-form (list :game-values (get-loadable-form variant gval)))))
    
    the-form)))


(defmethod get-loadable-form ((variant variant) (object monster-kind) &key)
  (let ((the-form '()))
    (flet ((possibly-add (initarg val &optional (def-val nil))
	     (unless (equal val def-val)
	       (setf the-form (nconc the-form (list initarg (loadable-val val)))))))
      
      (setf the-form (list 'define-monster-kind 
			   (monster.id object)
			   (monster.name object)))
      
      (possibly-add :desc (monster.desc object))
      (possibly-add :symbol (monster.symbol object))
      (possibly-add :colour (convert-obj (monster.colour object) :letter))
      (possibly-add :alignment (monster.alignment object))
      (possibly-add :type (monster.type object))
      (possibly-add :depth (monster.depth object))
      (possibly-add :rarity (monster.rarity object))
      (possibly-add :hitpoints (monster.hitpoints object))
      (possibly-add :armour (monster.armour object))
      (possibly-add :speed (monster.speed object) 0)
      (possibly-add :xp (monster.xp object) 0)
      (possibly-add :abilities (monster.abilities object))
      (possibly-add :immunities (monster.immunities object))
      (possibly-add :vulnerabilities (monster.vulnerabilities object))
      (possibly-add :alertness (monster.alertness object))
      (possibly-add :vision (monster.vision object))
      (possibly-add :attacks (convert-obj (monster.attacks object) :attk-list))
      (possibly-add :treasures (monster.treasures object))
      (possibly-add :gender (monster.gender object))
      (possibly-add :special-abilities (monster.sp-abilities object))
      ;; in group, uncertain of this one
;;      (possibly-add :in-group (monster.in-group object))

      the-form)))

(defmethod print-object ((inst character-class) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A ~A]" (class-name (class-of inst))
	   (class.id inst)
	   (class.name inst)))
  inst)


(defmethod print-object ((inst character-race) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A ~A]" (class-name (class-of inst))
	   (race.id inst)
	   (race.name inst)))
  inst)

(defmethod print-object ((inst feature-type) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (class-name (class-of inst)) 
	   (feature.name inst)))
  inst)


(defmethod print-object ((inst house) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A ~A ~A]" (class-name (class-of inst))
	   (slot-value inst 'name)
	   (slot-value inst 'id)
	   (slot-value inst 'owner)
	   ))
	   
  inst)


(defmethod print-object ((inst owner) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A]" (class-name (class-of inst))
	   (slot-value inst 'name)))
	   
  inst)


(defmethod print-object ((inst object-kind) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S]" (class-name (class-of inst)) 
	   (object.name inst) (object.depth inst)))
  inst)

(defmethod print-object ((inst active-object) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~a ~S (~a,~a)]" (class-name (class-of inst)) 
	   (aobj.number inst) (aobj.kind inst) (location-x inst) (location-y inst))
  inst))



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

(defmethod print-object ((inst treasure-drop) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a ~a ~a]" (class-name (class-of inst)) 
	   (drop.chance inst) (drop.quality inst) (drop.amount inst) (drop.type inst))
   inst))

(defmethod print-object ((inst player-attribute) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a]" (class-name (class-of inst)) 
	   (attr.key inst) (attr.value inst))
   inst))

(defmethod print-object ((inst temp-player-attribute) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a - ~a]" (class-name (class-of inst)) 
	   (attr.key inst) (attr.value inst) (attr.duration inst))
   inst))

(defmethod print-object ((inst character-stat) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a]" (class-name (class-of inst)) 
	   (stat.symbol inst) (stat.number inst))
   inst))

(defmethod print-object ((inst gender) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a]" (class-name (class-of inst)) 
	   (gender.id inst) (gender.symbol inst))
   inst))


(defmethod print-object ((inst items-in-container) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A ~A]" (class-name (class-of inst))
	   (items.cur-size inst)
	   (items.max-size inst)))
  inst)

(defun dump-objects (out-file &optional object-list)
  (let ((obj-list (if object-list
		      object-list
		      (get-object-list)))
	(var-obj *variant*)
	(*print-case* :downcase)
	(*print-right-margin* 120))
    
    (with-open-file (ffile (pathname out-file)
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
      (pprint '(in-package :langband)
	      ffile)
      (terpri ffile)
      (dolist (x obj-list)
	(print (get-loadable-form var-obj x) ffile)
	(terpri ffile))
      (terpri ffile))))


;; turn into loadable forms
(defun dump-features (out-file &optional feature-list)
  (let* ((features (if feature-list
		       feature-list
		       (loop for x being the hash-values of (variant.floor-features *variant*)
			     collecting x)))
	 (sorted-features (sort (copy-list features) #'< :key #'feature.id)))

    (let ((*print-case* :downcase))
      (with-open-file (ffile (pathname out-file)
			     :direction :output
			     :if-exists :supersede
			     :if-does-not-exist :create)
	(loop for x in sorted-features
	      do
	      (pprint `(define-feature-type ,(feature.id x)
			,(feature.name x)
			,(feature.x-attr x)
			,(feature.x-char x)
			:mimic ,(feature.mimic x))
		      ffile))))))


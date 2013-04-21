;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/base.lisp - the base variant class for Vanilla
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)


(defclass vanilla-variant (variant)
  ((dawn-time     :initarg :dawntime   :initform 0    :accessor variant.dawn)
   (twilight-time :initarg :twilight   :initform 6000 :accessor variant.twilight)
   (town-seed         :initform nil  :accessor variant.town-seed)
   (used-scroll-names :initform (make-hash-table :test #'equal) :accessor variant.used-scroll-names)
   
   (legal-effects  :initarg :legal-effects
		   :initform nil
		   :accessor variant.legal-effects)
   (gold-table     :initarg :gold-table
		   :initform nil
		   :accessor variant.gold-table)
   (spells         :initarg :spells
		   :initform (make-hash-table :test #'equal)
		   :accessor variant.spells)

   (spellbooks     :initarg :spellbooks
		   :initform (make-hash-table :test #'equal)
		   :accessor variant.spellbooks)


;;   (object-effects :initarg :object-effects :initform (make-hash-table :test #'equal)
;;		   :accessor variant.object-effects)

   ))


(defclass van-town-level (themed-level)
  ((id     :initform "town-level")
   (symbol :initform 'town-level)
   (stores        :initarg :stores     :initform nil  :accessor level.stores)
   (num-stores    :initarg :num-stores :initform 8    :accessor level.num-stores)
   (home-num      :initarg :home-num   :initform 7    :accessor level.home-num))

  (:documentation "The Vanilla variant has a special town-level with
stores and special behaviour.  The class is used for dispatching."))


(defclass magic-spell ()
  ((name   :accessor spell.name
	   :initform nil
	   :initarg :name
	   :documentation "Name of the spell.")
   (id     :accessor spell.id
	   :initform nil
	   :initarg :id
	   :documentation "Id for the spell, everyone uses this id.")
   (effect :accessor spell.effect
	   :initform nil :initarg
	   :effect :documentation "A function which is invoked when the spell is cast."))
  (:documentation "A very simple wrapper about the spell and little else."))


(defclass spellbook ()
  ((name   :accessor spellbook.name
	   :initform nil
	   :initarg :name
	   :documentation "The name of the spellbook, used for listings.")
   (id     :accessor spellbook.id
	   :initform nil
	   :initarg :id
	   :documentation "The id for the spellbook, used for lookups, e.g from object-kinds.")
   (size   :accessor spellbook.size
	   :initform 6
	   :initarg :size
	   :documentation "The size of the spellbook, ie how many spells it _can_ take.") 
   (spells :accessor spellbook.spells
	   :initform nil
	   :initarg :spells
	   :documentation "An array of spell-objects."))
  (:documentation "Represents the spell-data of a spellbook, not the actual book."))

(defclass spell-classdata ()
  ((id      :initarg :id
	    :initform nil
	    :accessor spell.id
	    :documentation "Id to the real spell, used for lookup.")
   (level   :initarg :level
	    :initform nil
	    :accessor spell.level
	    :documentation "The level the caster must be.")
   (mana    :initarg :mana
	    :initform nil
	    :accessor spell.mana
	    :documentation "The mana the caster needs.")
   (failure :initarg :failure
	    :initform nil
	    :accessor spell.failure
	    :documentation "The base failure-rate in %.")
   (xp      :initarg :xp
	    :initform nil
	    :accessor spell.xp
	    :documentation "The xp given for first-time casting.")
   (tried   :initarg :tried
	    :initform nil
	    :accessor spell.tried
	    :documentation "This slot is saved with the player-object."))
  
  (:documentation "Information that a class have about a spell.  To cast a spell
the class needs this information.  This information varies from class to class, but
the spell is usually the same."))
   

(defclass spellcasting-class (character-class)
  ((spell-stat      :initarg :spell-stat
		    :initform nil
		    :accessor class.spell-stat
		    :documentation "What is the spell-stat?")
   (spells-at-level :initarg :spells-at-level
		    :initform 1
		    :accessor class.spells-at-level
		    :documentation "At what level does the spellcaster get spells?")
   (spells          :initarg :spells
		    :initform nil
		    :accessor class.spells
		    :documentation "An array with the possible spells (of type spell-classdata) the class can have.")
   (learnt-spells   :initarg :learnt-spells
		    :initform nil
		    :accessor class.learnt-spells
		    :documentation "An array with ids to learnt spells (in order).  Is saved with the
player-object."))
  (:documentation "A subclass of the class character-class.  Represents a class with spell-castin capabilities."))

;; this is an ugly hack, it's used for spell-effects as state-info
(defclass vanilla-monster-effect ()
  ((damage     :initform 0        :initarg :damage     :accessor meff.damage)
   (note       :initform nil      :initarg :note       :accessor meff.note)
   (seen       :initform nil      :initarg :seen       :accessor meff.seen)
   (obvious    :initform nil      :initarg :obvious    :accessor meff.obvious)
   (dying-note :initform " dies." :initarg :dying-note :accessor meff.dying-note)
   ))


(defclass black-market (store)
  ()
  (:documentation "A store with steep prices, used as a dispatch class."))

(defgeneric is-spellcaster? (obj)
  (:documentation "Returns T if the object/player is a spellcaster."))

(defgeneric apply-spell-effect! (variant type source target &key x y damage state-object)
  (:documentation "Applies a spell-effect of type TYPE from SOURCE on TARGET at coords (X,Y) for
DAMAGE damage.  The state-object may be used to pass info back to calling function.  The methods
should return NIL or the state-object (with possibly updated state."))

(defun van-make-variant-obj ()
  (make-instance 'vanilla-variant
		 :id "langband-vanilla"
		 :name "Vanilla"

		 :stat-length 6
		 
		 ;; only used by development
		 :sys-file "./variants/vanilla/langband-vanilla.system"
		 :config-path
		 #+langband-development
		 "./variants/vanilla/config"
		 #-langband-development
		 "/var/lib/games/langband/vanilla"))


(register-variant& "langband-vanilla" #'van-make-variant-obj)
	   

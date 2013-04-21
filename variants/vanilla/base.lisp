;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/base.lisp - the base variant class for Vanilla
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(defclass vanilla-variant (variant)
  ((dawn-time         :initform 25200 :accessor variant.dawn)
   (twilight-time     :initform 75600 :accessor variant.twilight)
   (town-seed         :initform nil   :accessor variant.town-seed)
   
   (used-scroll-names :initform (make-hash-table :test #'equal)
		      :accessor variant.used-scroll-names)
   
   (gold-table     :initarg :gold-table
		   :initform nil
		   :accessor variant.gold-table)
   (spells         :initarg :spells
		   :initform (make-hash-table :test #'equal)
		   :accessor variant.spells)

   (spellbooks     :initarg :spellbooks
		   :initform (make-hash-table :test #'equal)
		   :accessor variant.spellbooks)

   (skill-translations :accessor variant.skill-translations
		       :initform nil
		       :initarg :skill-translations)

   (ego-items :accessor variant.ego-items
	      :initform (make-hash-table :test #'equal))

   (ego-items-by-level :accessor variant.ego-items-by-level
		       :initform (make-hash-table :test #'equal))
   
   ;; 50 is max-level
   (max-charlevel :initform 50)
   ;; put xp-table here right away
   (xp-table :initform #1A(     10       25       45       70      100
			       140      200      280      380      500
			       650      850     1100     1400     1800
			      2300     2900     3600     4400     5400
			      6800     8400    10200    12500    17500
			     25000    35000    50000    75000   100000
			    150000   200000   275000   350000   450000
			    550000   700000   850000  1000000  1250000
			   1500000  1800000  2100000  2400000  2700000
			   3000000  3500000  4000000  4500000  5000000))

   (legal-effects :initarg :legal-effects
                  :initform '(:quaff :read :eat :create :add-magic :use)
                  :accessor variant.legal-effects)

   ))


(defclass van/town-level (themed-level)
  ((id     :initform "town-level")
   (symbol :initform 'town-level)
   (stores        :initarg :stores     :initform nil  :accessor level.stores)
   (num-stores    :initarg :num-stores :initform 8    :accessor level.num-stores)
   (home-num      :initarg :home-num   :initform 7    :accessor level.home-num))

  (:documentation "The Vanilla variant has a special town-level with
stores and special behaviour.  The class is used for dispatching."))

(defclass players-home (house)
  ())


(defclass ego-item ()
  ((id          :accessor ego.id          :initform ""  :initarg :id)
   (name        :accessor ego.name        :initform ""  :initarg :name)
   (numeric-id  :accessor ego.numeric-id  :initform -1  :initarg :numeric-id)
   (power-lvl   :accessor ego.power-lvl   :initform 0)
   (xtra        :accessor ego.xtra        :initform 0)
   (max-to-hit  :accessor ego.max-to-hit  :initform 0   :initarg :max-to-hit)
   (max-to-dmg  :accessor ego.max-to-dmg  :initform 0   :initarg :max-to-dmg)
   (max-to-ac   :accessor ego.max-to-ac   :initform 0   :initarg :max-to-ac)
   (pval        :accessor ego.pval        :initform 0   :initarg :pval)
   (locations   :accessor ego.locations   :initform '())
   (weight      :accessor ego.weight      :initform 0)
   (cost        :accessor ego.cost        :initform 0   :initarg :cost)
   (obj-types   :accessor ego.obj-types   :initform '())
   #||
   (tval        :accessor ego.tval        :initform 0   :initarg :tval)
   (min-sval    :accessor ego.min-sval    :initform 0   :initarg :min-sval)
   (max-sval    :accessor ego.max-sval    :initform 0   :initarg :max-sval)
   ||#
   (flags       :accessor ego.flags       :initform '() :initarg :flags)
   (game-values :accessor ego.game-values :initform nil :initarg :game-values)
   ))


(defclass magic-spell ()
  ((name   :accessor spell.name
	   :initform nil
	   :initarg :name
	   :documentation "Name of the spell.")

   (id     :accessor spell.id
	   :initform nil
	   :initarg :id
	   :documentation "Id for the spell, everyone uses this id.")

   ;; hack used for converting spells, use 'id'
   (numeric-id :accessor spell.numeric-id
	       :initform -1)
   
   (effect-type :accessor spell.effect-type
		:initform nil
		:documentation "pointer to a spell-effect with necessary info.")
   
   (effect :accessor spell.effect
	   :initform nil
	   :documentation "A function which is invoked when the spell is cast."))
  
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
  ((spell-stat        :initform nil
		      :accessor class.spell-stat
		      :documentation "What is the spell-stat?")
   
   (spells-at-level   :initform 1
		      :accessor class.spells-at-level
		      :documentation "At what level does the spellcaster get spells?")
   
   (max-armour-weight :initform -1
		      :accessor class.max-armour-weight
		      :documentation "What is the max weight of armour the mage can wear?")
   
   (spells            :initform nil
		      :accessor class.spells
		      :documentation "An array with the possible spells (of type spell-classdata) the class can have.")
   
   (learnt-spells     :initform nil
		      :accessor class.learnt-spells
		      :documentation "An array with ids to learnt spells (in order).  Is saved with the player-object."))
  (:documentation "A subclass of the class character-class.  Represents a class with spell-castin capabilities."))

(defclass van/spell-effect (visual-projectile)
  ((gfx-beam :initform 0 :accessor spell-effect.gfx-beam :documentation "gfx code for beam.")
   (text-beam :initform 0 :accessor spell-effect.text-beam :documentation "text code for beam.")
   ))


;; this is an ugly hack, it's used for spell-effects as state-info
(defclass vanilla-monster-effect ()
  ((damage     :initform 0        :initarg :damage     :accessor meff.damage)
   (note       :initform nil      :initarg :note       :accessor meff.note)
   (seen       :initform nil      :initarg :seen       :accessor meff.seen)
   (obvious    :initform nil      :initarg :obvious    :accessor meff.obvious)
   (dying-note :initform "dies"   :initarg :dying-note :accessor meff.dying-note)
   ))


(defclass black-market (store)
  ()
  (:documentation "A store with steep prices, used as a dispatch class."))

(defclass vanilla-skills ()
  ((fighting     :accessor skills.fighting      :initform 0)
   (shooting     :accessor skills.shooting      :initform 0)
   (searching    :accessor skills.searching     :initform 0)
   (saving-throw :accessor skills.saving-throw  :initform 0)
   (stealth      :accessor skills.stealth       :initform 0)
   (disarming    :accessor skills.disarming     :initform 0)
   (device       :accessor skills.device        :initform 0)
   (perception   :accessor skills.perception    :initform 0)
   ))

;; this is a dummy for classes, not objects.. the player will have numbers
(defstruct (van/skill (:conc-name van/skill.))
  (name "")
  (base 0)
  (lvl-gain 0));; this is for 10 levels, to allow for fractions


(defgeneric is-spellcaster? (obj)
  (:documentation "Returns T if the object/player is a spellcaster."))

(defgeneric apply-spell-effect! (variant type source target &key x y damage state-object)
  (:documentation "Applies a spell-effect of type TYPE from SOURCE on TARGET at coords (X,Y) for
DAMAGE damage.  The state-object may be used to pass info back to calling function.  The methods
should return NIL or the state-object (with possibly updated state."))

(defgeneric get-melee-weapon (creature)
  (:documentation "Returns the active-object that is used as a melee weapon."))

(defgeneric get-missile-weapon (creature)
  (:documentation "Returns the active-object that is used as missile-weapon."))

(defgeneric get-light-source (creature)
  (:documentation "Returns the light-source used."))

(defgeneric get-charge-status (object)
  (:documentation "Returns the charge-status of the given object or NIL."))

(defgeneric calculate-creature-mana! (variant creature)
  (:documentation "Does a walk-through of the creature and recalculates mana."))

(defgeneric produce-skills-object (variant &key default-value)
  (:documentation "Returns a skills-object for the given variant."))

(defgeneric build-skills-obj-from-list (variant skill-list)
  (:documentation "Returns a skill-object from a list of skill-info."))
  
(defgeneric get-skill-translation (variant key)
  (:documentation "Returns a skill-translation for the given KEY, I think."))

(defgeneric register-skill-translation& (variant translation)
  (:documentation "Registers a skill-translation with the variant."))

(defgeneric roll-saving-throw (creature attack-power)
  (:documentation "Rolls a saving throw for creature against an attack-power."))

;;; define relevant object-types for vanilla.
(define-object-type vanilla-object
    :aobj-slots ((ego :accessor aobj.ego :initform nil)))
(define-object-type weapon :key <weapon> :is vanilla-object
		    :kind-slots ((text-colour :initform +term-white+)))

(define-object-type melee-weapon :is weapon)
(define-object-type sword :key <sword> :is melee-weapon)
(define-object-type pole-arm :key <pole-arm> :is melee-weapon)
(define-object-type hafted :key <hafted> :is melee-weapon)

(define-object-type missile-weapon :key <missile-weapon> :is weapon) ;; clash with bow?
(define-object-type bow :is missile-weapon :key <bow>
	      :kind-slots ((multiplier :accessor object.multiplier :initform 1 :initarg :multiplier)))
(define-object-type ammo :is vanilla-object :key <ammo>
	      :kind-slots ((effect-type :accessor object.effect-type :initform nil)))
(define-object-type digger :is weapon :key <digger>)

(define-object-type armour :is vanilla-object :key <armour>
		    :kind-slots ((text-colour :initform +term-l-umber+)))

(define-object-type body-armour :is armour :key <body-armour>
		    :kind-slots ((text-colour :initform +term-l-white+)))
(define-object-type soft-body-armour :is body-armour :key <soft-body-armour>)
(define-object-type hard-body-armour :is body-armour :key <hard-body-armour>)
(define-object-type dragonscale-armour :is body-armour :key <dsm-armour>)
(define-object-type boots :is armour :key <boots>)
(define-object-type gloves :is armour :key <gloves>)
(define-object-type shield :is armour :key <shield>)
(define-object-type headgear :is armour :key <headgear>)
(define-object-type helmet :is headgear :key <helmet>)
(define-object-type crown :is headgear :key <crown>)
(define-object-type cloak :is armour :key <cloak>)

(define-object-type potion :is vanilla-object :key <potion>
		    :kind-slots ((text-colour :initform +term-l-blue+)))

(define-object-type money  :is vanilla-object :key <money>)
(define-object-type scroll :is vanilla-object :key <scroll>
		    :kind-slots ((text-colour :initform +term-white+)))
(define-object-type wand   :is vanilla-object :key <wand>
		    :kind-slots ((effect-type :accessor object.effect-type :initform nil)
				 (text-colour :initform +term-l-green+)))
(define-object-type staff  :is vanilla-object :key <staff>
		    :kind-slots ((text-colour :initform +term-yellow+)))

(define-object-type rod    :is vanilla-object :key <rod>
		    :kind-slots ((effect-type :accessor object.effect-type :initform nil)
				 (text-colour :initform +term-violet+)
				 (recharge-time :accessor object.recharge-time :initform 0))
		    :aobj-slots ((recharge-time :accessor aobj.recharge-time :initform 0)))
(define-object-type book   :is vanilla-object :key <book>)
(define-object-type spellbook :is book :key <spellbook>
		    :kind-slots ((text-colour :initform +term-l-red+)))
(define-object-type prayerbook :is book :key <prayerbook>
		    :kind-slots ((text-colour :initform +term-l-green+)))
(define-object-type ring   :is vanilla-object :key <ring>
		    :kind-slots ((text-colour :initform +term-l-red+)))

(define-object-type chest  :is vanilla-object  :key <chest>)
(define-object-type light-source :is vanilla-object :key <light-source>
		    :kind-slots ((status-descs :accessor object.status-descs :initform nil :initarg :status-descs)
				 (max-fuel     :accessor object.max-fuel     :initform nil :initarg :max-fuel)
				 (text-colour :initform +term-yellow+)))
	      
(define-object-type container :is vanilla-object :key <container>)

(define-object-type food :is vanilla-object :key <food>
		    :kind-slots ((text-colour :initform +term-l-umber+)))
(define-object-type mushroom :is food :key <mushroom>)
(define-object-type neckwear :is vanilla-object :key <neckwear>)
(define-object-type amulet :is neckwear :key <amulet>
		    :kind-slots ((text-colour :initform +term-orange+)))


(define-object-type junk :is vanilla-object :key <junk>)
(define-object-type skeleton :is junk :key <skeleton>)

;; move away later

(defvar *vanilla-images* #(
#| 0 |# "" 
        "" 
        "" 
        (engine-gfx "tiles/dg_armor32.png")
        (engine-gfx "tiles/dg_effects32.png")
#| 5 |# (engine-gfx "tiles/dg_food32.png")
        (engine-gfx "tiles/dg_classm32.png")
        (engine-gfx "tiles/dg_humans32.png")
        (engine-gfx "tiles/upd_jewels.png")
        (engine-gfx "tiles/dg_magic32.png") 
#| 10 |#(engine-gfx "tiles/dg_misc32.png")
        (engine-gfx "tiles/dg_potions32.png")
        (engine-gfx "tiles/dg_wands32.png")
        (engine-gfx "tiles/dg_weapons32.png")
        (engine-gfx "tiles/dg_people32.png")
#| 15 |#(engine-gfx "tiles/dg_dragon32.png")
        (engine-gfx "tiles/dg_monster132.png")
        (engine-gfx "tiles/dg_monster232.png")
        (engine-gfx "tiles/dg_monster332.png")
        (engine-gfx "tiles/dg_monster432.png")
#| 20 |#(engine-gfx "tiles/dg_monster532.png")
        (engine-gfx "tiles/dg_monster632.png")
        (engine-gfx "tiles/dg_monster732.png") 
        (engine-gfx "tiles/dg_undead32.png") 
        (engine-gfx "tiles/dg_uniques32.png")
#| 25 |#(engine-gfx "tiles/dg_dungeon32.png")
        (engine-gfx "tiles/dg_grounds32.png") 
        (engine-gfx "tiles/dg_extra132.png") 
        (engine-gfx "tiles/dg_town032.png")
        (engine-gfx "tiles/dg_town132.png")
#| 30 |#(engine-gfx "tiles/dg_town232.png")
        (engine-gfx "tiles/dg_town332.png")
        (engine-gfx "tiles/dg_town432.png")
        (engine-gfx "tiles/dg_town532.png")
        (engine-gfx "tiles/dg_town632.png") 
#| 35 |#(engine-gfx "tiles/dg_town732.png") 
        (engine-gfx "tiles/dg_town832.png")
        (engine-gfx "tiles/dg_town932.png")
	(engine-gfx "tiles/buttons.png")
	""
#| 40 |#(engine-gfx "tiles/crosshair.png")
	(engine-gfx "tiles/states.png")		   
	))




;; path tweaking needed!!!
(defun van-make-variant-obj ()
  (make-instance 'vanilla-variant
		 :id "vanilla"
		 :name "Vanilla"
		 :version "0.1.6"
		 :num-version 15
		 :stat-length 6
		 
		 :config-path
		 #+langband-development
		 "./variants/vanilla/config/"
		 #-langband-development
		 "/var/games/langband-vanilla/"))


(register-variant& "vanilla" #'van-make-variant-obj
		   :desc "Vanilla is a plain simulation of the regular/vanilla
Angband written in C.  It's main purpose is to be a reference point for new
variant plugins to the langband engine.")
   

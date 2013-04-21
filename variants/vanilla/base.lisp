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
  ((dawn-time     :initarg :dawntime   :initform 0    :accessor variant.dawn)
   (twilight-time :initarg :twilight   :initform 6000 :accessor variant.twilight)
   (town-seed         :initform nil  :accessor variant.town-seed)
   (used-scroll-names :initform (make-hash-table :test #'equal) :accessor variant.used-scroll-names)
   
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

(defclass players-home (house)
  ())

(defclass magic-spell ()
  ((name   :accessor spell.name
	   :initform nil
	   :initarg :name
	   :documentation "Name of the spell.")

   (id     :accessor spell.id
	   :initform nil
	   :initarg :id
	   :documentation "Id for the spell, everyone uses this id.")

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

(defclass spell-effect (visual-projectile)
  ((gfx-beam :initform 0 :accessor spell-effect.gfx-beam :documentation "gfx code for beam.")
   (text-beam :initform 0 :accessor spell-effect.text-beam :documentation "text code for beam.")
   ))


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

;; change to (col row) format
(defclass vanilla-basic-frame-locations (basic-frame-locations)
  ((max-mana :initarg :max-mana :initform '(18 . 0))
   (cur-mana :initarg :cur-mana :initform '(19 . 0))

   (target   :initarg :target  :initform '(20 . 0))
   (cut      :initarg :cut     :initform '(21 . 0))
   (stun     :initarg :stun    :initform '(22 . 0))
   )
  (:documentation "Locations and various settings when printing stuff.
Each location should be a cons with (row . col)."))
  
(defclass vanilla-bottom-row-locations (bottom-row-locations)
  ((blind    :initarg :blind    :initform 7)
   (confused :initarg :confused :initform 13)
   (afraid   :initarg :afraid   :initform 22)
   (poisoned :initarg :poisoned :initform 29)
   (study    :initarg :study    :initform 64))
   
  (:documentation "Locations and various settings when printing stuff.
Each location is a fixnum with column in the last row."))

(defclass vanilla-birth (birth-settings)
  ())

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

(defgeneric print-cut (variant player settings)
  (:documentation "Prints cut-info according to settings."))

(defgeneric print-stun (variant player settings)
  (:documentation "Prints stun-info according to settings."))

(defgeneric print-poisoned (variant player settings)
  (:documentation "Prints poison-info according to settings."))

(defgeneric print-afraid (variant player settings)
  (:documentation "Prints fear-info according to settings."))

(defgeneric print-confused (variant player settings)
  (:documentation "Prints confusion-info according to settings."))

(defgeneric print-mana-points (variant creature setting)
  (:documentation "prints mana points according to setting."))

(defgeneric calculate-creature-mana! (variant creature)
  (:documentation "Does a walk-through of the creature and recalculates mana."))

(defgeneric print-can-study-more (variant player setting)
  (:documentation "Prints if the player can study more spells."))

(defgeneric produce-skills-object (variant &key default-value)
  (:documentation "Returns a skills-object for the given variant."))

(defgeneric build-skills-obj-from-list (variant skill-list)
  (:documentation "Returns a skill-object from a list of skill-info."))
  
(defgeneric get-skill-translation (variant key)
  (:documentation "Returns a skill-translation for the given KEY, I think."))

(defgeneric register-skill-translation& (variant translation)
  (:documentation "Registers a skill-translation with the variant."))


;;; define relevant object-types for vanilla.

(def-obj-type weapon :key <weapon>)
(def-obj-type melee-weapon :is weapon)
(def-obj-type sword :key <sword> :is melee-weapon)
(def-obj-type pole-arm :key <pole-arm> :is melee-weapon)
(def-obj-type hafted :key <hafted> :is melee-weapon)

(def-obj-type missile-weapon :is weapon) ;; clash with bow?
(def-obj-type bow :is missile-weapon :key <bow>
	      :kind-slots ((multiplier :accessor object.multiplier :initform 1 :initarg :multiplier)))
(def-obj-type ammo :key <ammo>
	      :kind-slots ((effect-type :accessor object.effect-type :initform nil)))
(def-obj-type digger :is weapon :key <digger>)

(def-obj-type armour :key <armour>)
(def-obj-type body-armour :is armour :key <body-armour>)
(def-obj-type dragonscale-armour :is body-armour :key <dsm-armour>)
(def-obj-type boots :is armour :key <boots>)
(def-obj-type gloves :is armour :key <gloves>)
(def-obj-type shield :is armour :key <shield>)
(def-obj-type headgear :is armour :key <headgear>)
(def-obj-type cloak :is armour :key <cloak>)

(def-obj-type potion :key <potion>)
(def-obj-type money :key <money>)
(def-obj-type scroll :key <scroll>)
(def-obj-type wand :key <wand>
	      :kind-slots ((effect-type :accessor object.effect-type :initform nil)))
(def-obj-type staff :key <staff>)
(def-obj-type rod :key <rod>
	      :kind-slots ((effect-type :accessor object.effect-type :initform nil))
	      :aobj-slots ((recharge-time :accessor aobj.recharge-time :initform 0 :initarg :recharge-time)))
(def-obj-type book :key <book>)
(def-obj-type spellbook :is book :key <spellbook>)
(def-obj-type prayerbook :is book :key <prayerbook>)
(def-obj-type ring :key <ring>)
(def-obj-type chest :key <chest>)
(def-obj-type light-source :key <light-source>
	      :kind-slots ((status-descs :accessor object.status-descs :initform nil :initarg :status-descs)
			   (max-fuel     :accessor object.max-fuel     :initform nil :initarg :max-fuel)))
	      
(def-obj-type container :key <container>)

(def-obj-type food :key <food>)
(def-obj-type mushroom :is food :key <mushroom>)
(def-obj-type neckwear :key <neckwear>)
(def-obj-type amulet :is neckwear :key <amulet>)

(def-obj-type junk :key <junk>)
(def-obj-type skeleton :is junk :key <skeleton>)

;; move away later

(defvar *vanilla-images* #(
#| 0 |# "" 
        "" 
        "" 
        "tiles/dg_armor32.bmp" 
        "tiles/dg_effects32.bmp" 
#| 5 |# "tiles/dg_food32.bmp" 
        "tiles/dg_classm32.bmp" 
        "tiles/dg_humans32.bmp" 
        "tiles/dg_jewls32.bmp" 
        "tiles/dg_magic32.bmp" 
#| 10 |#"tiles/dg_misc32.bmp" 
        "tiles/dg_potions32.bmp" 
        "tiles/dg_wands32.bmp" 
        "tiles/dg_weapons32.bmp" 
        "tiles/dg_people32.bmp" 
#| 15 |#"tiles/dg_dragon32.bmp" 
        "tiles/dg_monster132.bmp" 
        "tiles/dg_monster232.bmp" 
        "tiles/dg_monster332.bmp" 
        "tiles/dg_monster432.bmp" 
#| 20 |#"tiles/dg_monster532.bmp" 
        "tiles/dg_monster632.bmp" 
        "tiles/dg_monster732.bmp" 
        "tiles/dg_undead32.bmp" 
        "tiles/dg_uniques32.bmp" 
#| 25 |#"tiles/dg_dungeon32.bmp" 
        "tiles/dg_grounds32.bmp" 
        "tiles/dg_extra132.bmp" 
        "tiles/dg_town032.bmp" 
        "tiles/dg_town132.bmp" 
#| 30 |#"tiles/dg_town232.bmp" 
        "tiles/dg_town332.bmp" 
        "tiles/dg_town432.bmp" 
        "tiles/dg_town532.bmp" 
        "tiles/dg_town632.bmp" 
#| 35 |#"tiles/dg_town732.bmp" 
        "tiles/dg_town832.bmp" 
        "tiles/dg_town932.bmp"
	"tiles/button.bmp"
	"tiles/button2.bmp"
#| 40 |#"tiles/crosshair.png"
;;        "tiles/runes.png" 			   
	))




;; path tweaking needed!!!
(defun van-make-variant-obj ()
  (make-instance 'vanilla-variant
		 :id "vanilla"
		 :name "Vanilla"
		 :version "0.1.4"
		 :num-version 14
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
   

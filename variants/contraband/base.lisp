;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/base.lisp - the base variant class for contraband
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.contraband)

(defclass contraband (variant)
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

   ;; this length should be calculated accurately to allow for iteration
   (skills :initform (make-array 30 :initial-element nil)
	   :accessor variant.skills)


   (quests :initform (make-hash-table :test #'equal)
	   :accessor variant.quests)
   
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

(defclass con/monster-kind (monster-kind)
  ((picture :accessor monster.picture
	    :initform nil)))


(defstruct (con/skill (:conc-name con/skill.))
  id
  slot
  alias
  desc
  idx
  cost)
  
#||
;; see config/skills.lisp
(defclass con/skills ()
  ((smithing        :accessor skills.smithing        :initform 0)
   (leatherwork     :accessor skills.leatherwork     :initform 0)
   (woodcraft       :accessor skills.woodcraft       :initform 0)
   (gemcutting      :accessor skills.gemcutting      :initform 0)
   (mining          :accessor skills.mining          :initform 0)
   (appraising      :accessor skills.appraising      :initform 0)
   (trade           :accessor skills.trade           :initform 0)
   (conversation    :accessor skills.conversation    :initform 0)
   (monster-lore    :accessor skills.monster-lore    :initform 0)
   (object-lore     :accessor skills.object-lore     :initform 0)
   (history         :accessor skills.history         :initform 0)
   (intuition       :accessor skills.intuition       :initform 0)
   (languages       :accessor skills.languages       :initform 0)
   (animal-handling :accessor skills.animal-handling :initform 0)
   (riding          :accessor skills.riding          :initform 0)
   (tracking        :accessor skills.tracking        :initform 0)
   (alchemy         :accessor skills.alchemy         :initform 0)
   (magic-devices   :accessor skills.magic-devices   :initform 0)
   (pick-pocket     :accessor skills.pick-pocket     :initform 0)
   (security        :accessor skills.security        :initform 0)
   (mechanical      :accessor skills.mechanical      :initform 0)
   ;; combat
   (unarmed         :accessor skills.unarmed         :initform 0)
   (short-blades    :accessor skills.short-blades    :initform 0)
   (long-blades     :accessor skills.long-blades     :initform 0)
   (polearms        :accessor skills.polearms        :initform 0)
   (bludgeoning     :accessor skills.bludgeoning     :initform 0)
   (archery         :accessor skills.archery         :initform 0)
   (shield          :accessor skills.shield          :initform 0)
   (heavy-armour    :accessor skills.heavy-armour    :initform 0)
   (light-armour    :accessor skills.light-armour    :initform 0)
   (evasion         :accessor skills.evasion         :initform 0)
   ))
||#

(defclass con/town (themed-level)
  ((id     :initform "town-level")
   (symbol :initform 'town-level)
   (stores        :initarg :stores     :initform nil  :accessor level.stores)
   (num-stores    :initarg :num-stores :initform 8    :accessor level.num-stores)
   (home-num      :initarg :home-num   :initform 7    :accessor level.home-num))

  (:documentation "Contraband has a special (two) town-level with
stores and special behaviour.  The class is used for dispatching."))


;; change to (col row) format
(defclass con-basic-frame-locations (basic-frame-locations)
  ((max-mana :initarg :max-mana :initform '(18 . 0))
   (cur-mana :initarg :cur-mana :initform '(19 . 0))

   (target   :initarg :target  :initform '(20 . 0))
   (cut      :initarg :cut     :initform '(21 . 0))
   (stun     :initarg :stun    :initform '(22 . 0))
   )
  (:documentation "Locations and various settings when printing stuff.
Each location should be a cons with (row . col)."))
  
(defclass con-bottom-row-locations (bottom-row-locations)
  ((blind    :initarg :blind    :initform 7)
   (confused :initarg :confused :initform 13)
   (afraid   :initarg :afraid   :initform 22)
   (poisoned :initarg :poisoned :initform 29)
   (study    :initarg :study    :initform 64))
   
  (:documentation "Locations and various settings when printing stuff.
Each location is a fixnum with column in the last row."))

(defclass con-birth (birth-settings)
  ())


(defclass quest ()
  ((id    :accessor quest.id    :initform nil :initarg :id
	  :documentation "A string id.")
   (name  :accessor quest.name    :initform nil :initarg :id
	  :documentation "A name for the quest.")
   (desc  :accessor quest.desc  :initform nil
	  :documentation "A description of the quest to put on a quest page.")
   (state :accessor quest.state :initform :not-started
	  :documentation "what is the current state of the quest?")
   (step  :accessor quest.step  :initform :init
	  :documentation "specifies at what step we are at.. :init and :finish being special values.")
   (steps :accessor quest.steps :initform nil
	  :documentation "steps within a quest, typically pointers to subquests.")
   (giver :accessor quest.giver :initform nil
	  :documentation "Who gave this quest.")
   (taker :accessor quest.taker :initform nil
	  :documentation "Who is doing this quest")
   (parent :accessor quest.parent :initform nil
	   :documentation "If it is a subquest, PARENT should point to the parent quest.")
   ))


(defgeneric quest-available? (variant quest quest-giver quest-taker)
  (:documentation "Checks if a quest can be taken by (ie 'is available for') the quest-taker."))

(defgeneric quest-status (variant quest quest-taker)
  (:documentation "Returns the status of the quest.. :active, :not-started, :success, :failure being some possible
returned results."))
  
(defgeneric init-quest (variant quest quest-giver quest-taker)
  (:documentation "Initialisation of the quest, which does the init of all settings."))

(defgeneric advance-quest (variant quest quest-taker)
  (:documentation "Advances a quest to the next step, which might be the end."))

(defgeneric finish-quest (variant quest quest-taker)
  (:documentation "Cleanup actions for the quest."))

;;; define relevant object-types for contraband

(def-obj-type weapon :key <weapon>)
(def-obj-type melee-weapon :is weapon)
(def-obj-type long-blade :key <long-blade> :is melee-weapon)
(def-obj-type short-blade :key <short-blade> :is melee-weapon)

(def-obj-type armour :key <armour>)
(def-obj-type headgear :is armour :key <headgear>)
(def-obj-type cloak :is armour :key <cloak>)
(def-obj-type body-armour :is armour :key <body-armour>)
(def-obj-type gloves :is armour :key <gloves>)
(def-obj-type boots :is armour :key <boots>)

(def-obj-type letter :key <letter>)
(def-obj-type ring :key <ring>)
(def-obj-type neckwear :key <neckwear>)
(def-obj-type amulet :is neckwear :key <amulet>)

(def-obj-type container :key <container>)

#||
;; doesn't work!
;; hacks to let things load
(defmethod get-otype-table ((var-obj contraband) (level level))
  (%get-var-table var-obj "level" 'objects-by-level))

(defmethod get-mtype-table ((var-obj contraband) (level level))
  (%get-var-table var-obj "level" 'monsters-by-level))

(defmethod activate-object :before ((var-obj contraband) &key)
  (register-level! var-obj "level")
  )
||#  

(defvar *contraband-images* #(
#| 0 |# "" 
        "" 
        "" 
        (engine-gfx "tiles/dg_armor32.bmp") 
        (engine-gfx "tiles/dg_effects32.bmp") 
#| 5 |# (engine-gfx "tiles/dg_food32.bmp") 
        (engine-gfx "tiles/dg_classm32.bmp") 
        (engine-gfx "tiles/dg_humans32.bmp") 
        (engine-gfx "tiles/dg_jewls32.bmp") 
        (engine-gfx "tiles/dg_magic32.bmp") 
#| 10 |#(engine-gfx "tiles/dg_misc32.bmp") 
        (engine-gfx "tiles/dg_potions32.bmp") 
        (engine-gfx "tiles/dg_wands32.bmp") 
        (engine-gfx "tiles/dg_weapons32.bmp") 
        (engine-gfx "tiles/dg_people32.bmp") 
#| 15 |#(engine-gfx "tiles/dg_dragon32.bmp") 
        (engine-gfx "tiles/dg_monster132.bmp") 
        (engine-gfx "tiles/dg_monster232.bmp") 
        (engine-gfx "tiles/dg_monster332.bmp") 
        (engine-gfx "tiles/dg_monster432.bmp") 
#| 20 |#(engine-gfx "tiles/dg_monster532.bmp") 
        (engine-gfx "tiles/dg_monster632.bmp") 
        (engine-gfx "tiles/dg_monster732.bmp") 
        (engine-gfx "tiles/dg_undead32.bmp") 
        (engine-gfx "tiles/dg_uniques32.bmp") 
#| 25 |#(engine-gfx "tiles/dg_dungeon32.bmp") 
        (engine-gfx "tiles/dg_grounds32.bmp") 
        (engine-gfx "tiles/dg_extra132.bmp") 
        (engine-gfx "tiles/dg_town032.bmp") 
        (engine-gfx "tiles/dg_town132.bmp") 
#| 30 |#(engine-gfx "tiles/dg_town232.bmp") 
        (engine-gfx "tiles/dg_town332.bmp") 
        (engine-gfx "tiles/dg_town432.bmp") 
        (engine-gfx "tiles/dg_town532.bmp") 
        (engine-gfx "tiles/dg_town632.bmp") 
#| 35 |#(engine-gfx "tiles/dg_town732.bmp") 
        (engine-gfx "tiles/dg_town832.bmp")
        (engine-gfx "tiles/dg_town932.bmp")
	(engine-gfx "tiles/button.bmp")
	(engine-gfx "tiles/button2.bmp")
#| 40 |#(engine-gfx "tiles/crosshair.png")
	(engine-gfx "tiles/summer.png")
	(variant-gfx "tiles/greendress.png")
	(variant-gfx "tiles/facts.png")
	(variant-gfx "tiles/alphabet.png") ;; temporary one for runesystem
#| 45 |#(variant-gfx "tiles/keyrow.png") ;; temporary one for runesystem
	(variant-gfx "tiles/red-pointers.png") ;; temporary one for runesystem
	))


;; path tweaking needed!!!
(defun make-contraband-obj ()
  (make-instance 'contraband
		 :id "contraband"
		 :name "Contraband"
		 :num-version 7
		 :stat-length 6

		 :gfx-path
		 #+langband-development
		 "./variants/contraband/graphics/"
		 #-langband-development
		 "/var/games/contraband/graphics/"
		 
		 :config-path
		 #+langband-development
		 "./variants/contraband/config/"
		 #-langband-development
		 "/var/games/contraband/"))


(register-variant& "contraband" #'make-contraband-obj
		   :desc "Contraband is all about rum, smuggling and pretty girls.")

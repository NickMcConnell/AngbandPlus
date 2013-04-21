;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: classes.lisp - The major classes and structs for langband
Copyright (c) 2002-2003 - Stig Erik Sandø

This program is free software  ; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation	 ; either version 2 of the License, or
(at your option) any later version.

||#

(in-package :org.langband.engine)

;;; Classes in langband are the first-class objects, structs are considered
;;; for really simple "classes" but might be turned into classes later.
;;; The DUNGEON struct is a struct in the hope that it'll make it faster,
;;; as the DUNGEON is frequently accessed

;;; currently in no specified order

(defclass activatable ()
  ((activated :reader activated? :initform nil))
  (:documentation "Mixin-class for activatation of objects,
may be removed later for efficiency-reasons.  It enforces a
protocol that allows activated? to be set automagically after
a succesful ACTIVATE-OBJECT."))

(defclass variant (activatable)
  ((id        :accessor variant.id
	      :initform "lithping"
	      :initarg :id)
   
   (name      :accessor variant.name
	      :initform "lithping"
	      :initarg :name)

   (version   :accessor variant.version
	      :initform "1.0"
	      :documentation "A string describing the version, useful for displaying."
	      :initarg :version)

   (num-version :accessor variant.num-version
		:initform 100
		:initarg :num-version
		:documentation "A never-displayed version-number that code
can use for compatibility checks, savegames and internal use.  version is
for display, num-version for active-use. u16b should be enough.")
   
   (config-path :accessor variant.config-path
		:initform nil
		:initarg :config-path
		:documentation "where are the configuration-files?")
   
   (gfx-path :accessor variant.gfx-path
	     :initform nil
	     :initarg :gfx-path
	     :documentation "What is the path to variant specific graphics?")

   ;; the rest can be done lazily

   (genders   :accessor variant.genders
	      :initform '()
	      :documentation "List of legal genders for players and monsters.")
   
   (races     :accessor variant.races
	      :initform (make-hash-table :test #'equal))
   
   (classes   :accessor variant.classes
	      :initform (make-hash-table :test #'equal))

   (effects   :accessor variant.effects
	      :initform '()
	      :documentation "List of legal effects and effects to handle for variant.")
   
   (elements  :accessor variant.elements
	      :initform '()
	      :documentation "List of legal elements and elements to handle for variant.")
   
   (turn      :accessor variant.turn
	      :initform 0
	      :initarg :turn)

   (turn-events :accessor variant.turn-events
		:initform (make-hash-table :test #'equal))


   ;; a level builder is a constructor that must be funcalled
   ;; the key is the level-id
   (level-builders :accessor variant.level-builders
		   :initform (make-hash-table :test #'equal))

   (floor-types :accessor variant.floor-types
		:initform (make-hash-table :test #'equal))

   (room-builders  :accessor variant.room-builders
		   :initform (make-hash-table :test #'equal))

     
   (max-depth      :accessor variant.max-depth
		   :initform 128)
     
   (max-charlevel  :accessor variant.max-charlevel
		   :initform 50)
     
   (xp-table  :accessor variant.xp-table
	      ;; maybe have a default? or maybe not
	      ;; it should be an array of size max-charlevel
	      :initform nil)

   (stats :accessor variant.stats
	  :initform nil)
   
   (stat-length :accessor variant.stat-length
		:initarg :stat-length
		:initform 0)
   
   ;; these are just monster-types.. not actual monsters
   (monsters :accessor variant.monsters
	     :initform (make-hash-table :test #'equal)
	     :documentation "these are just monster-types.. not active monsters.")

   (objects :accessor variant.objects
	    :initform (make-hash-table :test #'equal)
	    :documentation "these are just object-types.. not active objects.")

   (monsters-by-level :accessor variant.monsters-by-level
		      :initform (make-hash-table :test #'equal)
		      :documentation "these are monster-types organised by levels.")

   (objects-by-level :accessor variant.objects-by-level
		     :initform (make-hash-table :test #'equal)
		     :documentation "these are object-types organised by levels.")
   
   (traps :accessor variant.traps
	  :initform (make-hash-table :test #'equal)
	  :documentation "A table with trap-types.")

   (doors :accessor variant.doors
	  :initform (make-hash-table :test #'equal)
	  :documentation "A table with door-types.")

   
   (filters :accessor variant.filters
	    :initform (make-hash-table :test #'equal))
     
   (flavour-types :accessor variant.flavour-types
		  :initform (make-hash-table :test #'equal))

   (house-types :accessor variant.house-types
		:initform (make-hash-table))
     
   (house-owners :accessor variant.house-owners
		 :initform (make-hash-table))

   (attk-descs :accessor variant.attk-descs
	       :initform (make-hash-table :test #'eq))
   
   (attack-types :accessor variant.attack-types
		 :initform (make-hash-table :test #'eq))

   (visual-effects :accessor variant.visual-effects
		   :initform (make-hash-table :test #'equal))
  
   (day-length      :accessor variant.day-length
		    :initform 10000)

   (help-topics :accessor variant.help-topics
		:initform (make-hash-table :test #'equal))

   (settings :accessor variant.settings
	     :initform (make-hash-table :test #'equal) ;; maybe #'eq is enough?
	     :documentation "table with settings for various parts of the code, see later")

   (event-types :accessor variant.event-types
		:initform (make-hash-table :test #'equal) ;; maybe #'eq is enough?
		:documentation "table with known events that can occur.")

   (worn-item-slots :accessor variant.worn-item-slots
		    :initform nil)

   (images :accessor variant.images
	   :initform nil
	   :documentation "An array of relevant images to a variant.") 
	   
   ;; this one is crucial, with lowercase string-keys it stores information that
   ;; is easy to check and save/load
   (information :accessor variant.information
		:initform (make-hash-table :test #'equal))
   
   ))

(defstruct worn-item-slot
  key
  desc
  types
  hidden)

(defstruct (dungeon-coord (:conc-name coord.))
;;  (floor 0 :type u16b)
  (floor nil)
  (flags 0 :type u16b)  ;; info-flag in angband
  (objects nil)
  (monsters nil)
  (decor nil)
  )

;; Remember to update [save|load|checking].lisp when updating this one
(defstruct (dungeon (:conc-name dungeon.))
  
  (depth 0 :type fixnum)  ; just a fixnum

  (height 0 :type u-fixnum) ;; height of table below
  (width  0 :type u-fixnum) ;; width of table below

  (table nil)


  ;; enable these two later if the need should arise
;;  (up-stairs-p nil)
;;  (down-stairs-p nil)

  (monsters nil)
  (objects nil)
  (rooms nil)
  (decor nil) ;; list of all decor
  (active nil)
  (triggers nil) ;; can't be used yet
  )


(defclass settings ()
  ((name   :accessor setting.name
	   :initform "No-name"
	   :initarg :name)
   (events :accessor setting.events
	   :initform nil
	   :initarg nil)))

(defclass birth-settings (settings)
  ((allow-all-classes :accessor birth.allow-classes
		      :initarg :allow-all-classes
		      :initform nil)
   (instr-x      :initarg :instr-x     :initform 23)
   (instr-y      :initarg :instr-y     :initform 1)
   (instr-w      :initarg :instr-w     :initform 75)
   (instr-attr   :initarg :instr-attr  :initform +term-white+)
   (info-x       :initarg :info-x      :initform 1)
   (info-y       :initarg :info-y      :initform 8)
   (info-attr    :initarg :info-attr   :initform +term-l-green+)
   (query-x      :initarg :query-x     :initform 2)
   (query-y      :initarg :query-y     :initform 21)
   (query-attr   :initarg :query-attr  :initform +term-l-red+)
   (query-reduced :initarg :query-reduced  :initform nil)
   (choice-x     :initarg :choice-x     :initform 1)
   (choice-y     :initarg :choice-y     :initform 2)
   (choice-tattr :initarg :choice-attr  :initform +term-white+)
   (choice-attr  :initarg :choice-tattr :initform +term-l-blue+)
   (text-x       :initarg :text-x       :initform 2)
   (text-y       :initarg :text-y       :initform 10)
   (text-w       :initarg :text-w       :initform 75)
   (text-attr    :initarg :text-attr    :initform +term-l-red+)
   (altern-cols  :initarg :altern-cols  :initform 5)
   (altern-attr  :initarg :altern-attr  :initform +term-white+)
   (altern-sattr :initarg :altern-sattr :initform +term-l-blue+)
   (note-colour  :initarg :note-colour  :initform +term-white+)

   )
   
  (:documentation "Settings when creating characters."))

(defclass chardisplay-settings (settings)
  ((title-x       :initarg :title-x       :initform 1)
   (title-y       :initarg :title-y       :initform 2)
   (title-attr    :initarg :title-attr    :initform +term-white+)
   (value-attr    :initarg :value-attr    :initform +term-l-blue+)
   (value-badattr :initarg :value-badattr :initform +term-yellow+)
   (picture-x     :initarg :picture-x     :initform 23)
   (picture-y     :initarg :picture-y     :initform 2)
   (extra-x       :initarg :extra-x       :initform 1)
   (extra-y       :initarg :extra-y       :initform 18)
   (elem-x        :initarg :elem-x        :initform 1)
   (elem-y        :initarg :elem-y        :initform 10)
   (combat-x      :initarg :combat-x      :initform 28)
   (combat-y      :initarg :combat-y      :initform 10)
   (stats-x       :initarg :stats-x       :initform 42)
   (stats-y       :initarg :stats-y       :initform 3)
   (stats-attr    :initarg :stats-attr    :initform +term-white+)
   (statok-attr   :initarg :statok-attr   :initform +term-l-green+)
   (statbad-attr  :initarg :statbad-attr  :initform +term-yellow+)
   (skills-x      :initarg :skills-x      :initform 50)
   (skills-y      :initarg :skills-y      :initform 10)
   
   ))

(defclass resistdisplay-settings (settings)
  ((title-x      :initarg :title-x     :initform 2)
   (title-y      :initarg :title-y     :initform 0)
   (title-attr   :initarg :title-attr  :initform +term-l-blue+)
   (list-x       :initarg :list-x      :initform 2)
   (list-y       :initarg :list-y      :initform 3)
   (res-attr     :initarg :res-attr    :initform +term-l-green+)
   (unres-attr   :initarg :unres-attr  :initform +term-l-red+)))


(defclass dungeon-settings (settings)
  ((max-width   :initarg :max-width   :initform 198)
   (max-height  :initarg :max-height  :initform 66)
   ;; how many rooms
   (room-number :initarg :room-number :initform 50)
   ;; ranges
   (stairs-down :initarg :stairs-down :initform '(3 4))
   (stairs-up   :initarg :stairs-up   :initform '(1 2)))
  (:documentation "A class I will be expanding later.."))

(defclass basic-frame-locations (settings)
  ((name                        :initform "Basic frame locations")
   (race     :initarg :race     :initform '(1 . 0))
   (class    :initarg :class    :initform '(2 . 0))
   (title    :initarg :title    :initform '(3 . 0))
   (level    :initarg :level    :initform '(4 . 0))
   (xp       :initarg :xp       :initform '(5 . 0))
   (gold     :initarg :gold     :initform '(6 . 0))

   (stat     :initarg :stat     :initform '(8 . 0))
   (ac       :initarg :ac       :initform '(15 . 0))
   (max-hp   :initarg :max-hp   :initform '(16 . 0))
   (cur-hp   :initarg :cur-hp   :initform '(17 . 0))
   ;; more slots?
   )
  (:documentation "Locations and various settings when printing stuff.
Each location should be a cons with (row . col)."))

(defclass bottom-row-locations (settings)
  ((name                        :initform "Bottom row locations")
   (hungry   :initarg :hungry   :initform 0)
   ;; more slots?
   (state    :initarg :state    :initform 38)
   (speed    :initarg :speed    :initform 49)
   ;; more?
   (depth    :initarg :depth    :initform 70))
   
  (:documentation "Locations and various settings when printing stuff.
Each location is a fixnum with column in the last row."))


(defclass effect ()
  ((symbol   :reader effect.symbol   :initarg :symbol)
   (name     :reader effect.name     :initarg :name)
   (bit-flag :reader effect.bit-flag :initarg :bit-flag)
   (number   :reader effect.number   :initarg :number)
   ))

;;; effects
;; fast, slow, blind, prot-evil, shielded, afraid, cut, stun, blessed,
;; hero, super-hero, berserk, poisoned, slow-digest, invulnerable,
;; hallucinate, confused, paralysed, telepathy, invisibility, see-inv,
;; random-teleport, hold-life, ... 


(defclass element ()
  ((symbol   :reader element.symbol   :initarg :symbol)
   (name     :reader element.name     :initarg :name)
   (bit-flag :reader element.bit-flag :initarg :bit-flag)
   (number   :reader element.number   :initarg :number)
   ))


(defclass creature-attribute ()
  ((name  :accessor attr.name  :initform ""  :initarg :name)
   (key   :accessor attr.key   :initform nil :initarg :key)
   (type  :accessor attr.type  :initform nil :initarg :type)
   (desc  :accessor attr.desc  :initform ""  :initarg :desc)
   (value :accessor attr.value :initform 0   :initarg :value)
   (value-type    :accessor attr.value-type
		  :initform 'boolean
		  :initarg :value-type)
   (default-value :accessor attr.default-value
                  :initform 0
		  :initarg :default-value)
   ))

(defclass temp-creature-attribute (creature-attribute)
  ((duration       :accessor attr.duration
		   :initform 0
		   :initarg :duration
		   :documentation "How long duration has the effect.")
   (turned-on-msg  :accessor attr.turned-on-msg
		   :initform nil
		   :initarg :turned-on-msg
		   :documentation "A string that is printed when the attribute/state is turned on.")
   (turned-off-msg :accessor attr.turned-off-msg
		   :initform nil
		   :initarg :turned-off-msg
		   :documentation "A string that is printed when the attribute/state is turned off.")
   (update-fun     :accessor attr.update-fun
		   :initform nil
		   :initarg :update-fun
		   :documentation "Function that is called whenever duration/state is altered.")
   (on-update      :accessor attr.on-update
	           :initform nil
	           :initarg :on-update
	           :documentation "If function exists, it is called after the state is changed off/on.")
   ))
  

(defclass misc-player-info ()
  ((age    :accessor playermisc.age    :initform 0)
   (status :accessor playermisc.status :initform 0)
   (height :accessor playermisc.height :initform 0)
   (weight :accessor playermisc.weight :initform 0))
  (:documentation "A helper-class for the player-object."))


(defclass player-abilities ()
  ((base-ac         :accessor pl-ability.base-ac
		    :initform 0
		    :documentation "integer, >= 0")
   (ac-modifier     :accessor pl-ability.ac-modifier
		    :initform 0
		    :documentation "integer")
   (to-hit-modifier :accessor pl-ability.to-hit-modifier
		    :initform 0
		    :documentation "integer")
   (to-dmg-modifier :accessor pl-ability.to-dmg-modifier
		    :initform 0
		    :documentation "integer"))
  
  (:documentation "A helper-class for the player-object."))

(defclass character-stat ()
  ((symbol        :accessor stat.symbol
		  :initarg :symbol)
   (name          :accessor stat.name
		  :initform ""
		  :initarg :name)
   (abbreviation  :accessor stat.abbreviation
		  :initform ""
		  :initarg :abbreviation)
   (positive-desc :accessor stat.positive-desc
		  :initform ""
		  :initarg :positive-desc)
   (negative-desc :accessor stat.negative-desc
		  :initform ""
		  :initarg :negative-desc)
   (number        :accessor stat.number
		  :initarg :number)
   (fields        :accessor stat.fields
		  :initform nil)
   (data          :accessor stat.data
		  :initform nil
		  :initarg :data)
   ))

;; this is a hack!
(defstruct stat-field
  lower
  upper
  data)

(defclass player ()

  (
    ;; === Need Special saving ===
  
   (name   :accessor player.name   :initform nil)
   (class  :accessor player.class  :initform nil)
   (race   :accessor player.race   :initform nil)
   (gender :accessor player.gender :initform nil)
   
   (base-stats    :accessor player.base-stats
		  :initform nil
		  :documentation "This is the base stats")
   (current-statmods :accessor player.cur-statmods
		     :initform nil
		     :documentation "This is the diff (possibly drained or raised values) of the base stats")
   (hp-table      :accessor player.hp-table
		  :initform nil
		  :documentation "Note: should be saved.")
   (equipment     :accessor player.equipment :initform nil)
   
   ;; add save-code for this as well
   (misc          :accessor player.misc
		  :initform nil
		  :documentation "An object with misc info about the player character.")

   
   (dead-from     :accessor player.dead-from
		  :initform ""
		  :documentation "who killed the player?")

   (monster-knowledge :accessor player.monster-knowledge
		      :initform (make-hash-table :test #'equal)
		      :documentation "Knowledge about monsters.")
   
   (object-knowledge :accessor player.object-knowledge
		     :initform (make-hash-table :test #'equal)
		     :documentation "Knowledge about objects.")
   
   
   ;; === Directly savable to binary ===
   
   (loc-x :accessor location-x :initform +illegal-loc-x+)
   (loc-y :accessor location-y :initform +illegal-loc-y+)
   
   (view-x :accessor player.view-x :initform +illegal-loc-x+);; wx
   (view-y :accessor player.view-y :initform +illegal-loc-y+);; wy
   
   (depth     :accessor player.depth     :initform 0)
   (max-depth :accessor player.max-depth :initform 0)
   
   (max-xp      :accessor player.max-xp      :initform 0)
   (cur-xp      :accessor player.cur-xp      :initform 0)
   (fraction-xp :accessor player.fraction-xp :initform 0) 
   
   (cur-hp      :accessor current-hp         :initform 0)
   (fraction-hp :accessor player.fraction-hp :initform 0)
   
   (cur-mana      :accessor current-mana         :initform 0)
   (fraction-mana :accessor player.fraction-mana :initform 0)
   
   (gold        :accessor player.gold   :initform 0)
   (food        :accessor player.food   :initform (1- +food-full+))
   (energy      :accessor player.energy :initform 0)
   
   ;; === The remaining values can be calculated from the above ===
   
   (level     :accessor player.level
	      :initform 1
	      :documentation "can be calculated from cur-xp")
   (max-level :accessor player.max-level
	      :initform 1
	      :documentation "can be calculated from max-xp")

   (max-hp    :accessor maximum-hp
	      :initform 0
	      :documentation "can be calculated")
   (max-mana  :accessor maximum-mana
	      :initform 0
	      :documentation "can be calculated")
   (xp-table  :accessor player.xp-table
	      :initform nil
	      :documentation "can be calculated")
   
   (energy-use :accessor player.energy-use
	       :initform 0
	       :documentation "is just a temp-variable")
   (leaving?   :accessor player.leaving?
	       :initform nil) ;; need to save it?
   (dead?      :accessor player.dead?
	       :initform nil) ;; need to save it?
   (speed      :accessor player.speed
	       :initform +speed-base+)  ;; does this change?
   

   (burden       :accessor player.burden       :initform 0
		 :documentation "Calculated value for how much player carries.")
   (light-radius :accessor player.light-radius :initform 0)

   ;; need infravision and see-inv here, because these are accessed in inner loops
   ;; and should be fast.  the engine also uses them.  
   (infravision :accessor player.infravision
		:initform 0
		:documentation "How far does infravision reach?  0 for no infravision.")
   (see-invisible :accessor player.see-invisible
		  :initform 0
		  :documentation "How far does 'see invisible' reach?  0 for no see-inv.")
   (inventory   :accessor player.inventory
		:initform nil
		:documentation "quick variable to equipment.backpack.content")
   
   (skills      :accessor player.skills
		:initform nil
		:documentation "Meaning depends entirely on variant, engine will not touch this.")
   
   (modbase-stats :accessor player.modbase-stats
		  :initform nil
		  :documentation "This is the modified base stats (base + race + class + eq)")
   (active-stats :accessor player.active-stats
		 :initform nil
		 :documentation "This is the current active stat-value, it's
value is calculated by: (base + curstatmods + race + class + eq)")

   (perceived-abilities :accessor player.perceived-abilities
			:initform nil
			:documentation "A player-abilities object with perceived abilties.")
   (actual-abilities :accessor player.actual-abilities
		     :initform nil
		     :documentation "A player-abilities object with actual abilties.")
   

   ;; an array with index for each element, each element is an integer which tells the power of the resist
   (resists :accessor player.resists
	    :initform nil
	    :documentation "What does the player resist and how much?
object is an array with index for each element, each element is an integer which tells the power of the resist")

   (stat-sustains :accessor player.stat-sustains
		  :initform nil
		  :documentation "array of stat-length with possible values T or NIL.")
   
   ;; should not be touched by engine
   (calculated-attributes :accessor player.calc-attrs
			  :initform nil
			  :documentation "Should be a hash-table with calculated attributes.")
   (temp-attributes :accessor player.temp-attrs
		    :initform nil
		    :documentation "Should be a hash-table with temporary attributes.")

   (target :accessor player.target
	   :initform nil
	   :documentation "Who/what is the player targeting?")
   
   ;; === the following does not need saving

   (x-attr :accessor x-attr
	   :initform +term-white+
	   :documentation "Attr for player on map.")

   (x-char :accessor x-char
	   :initform #.(char-code #\@)
	   :documentation "Char for player on map.")

   (text-attr :accessor text-attr
	      :initform +term-white+
	      :documentation "Attr for player on map.")

   (text-char :accessor text-char
	      :initform #.(char-code #\@)
	      :documentation "Char for player on map.")
   
   (gfx-sym :accessor gfx-sym
	    :initform (text-paint-value +term-white+ #\@)
	    :documentation "The gfx symbol for the player.")
   
   (text-sym :accessor text-sym
	     :initform (text-paint-value +term-white+ #\@)
	     :documentation "The gfx symbol for the player.")

   ))


(defclass game-values ()
  ((base-ac       :accessor gval.base-ac
		  :initarg :base-ac
		  :initform 0)
   (ac-modifier   :accessor gval.ac-modifier
		  :initarg :ac-modifier
		  :initform 0)
   (base-dice     :accessor gval.base-dice
		  :initarg :base-dice
		  :initform 0)
   (num-dice      :accessor gval.num-dice
		  :initarg :num-dice
		  :initform 0)
   (tohit-modifier :accessor gval.tohit-modifier
		   :initarg :tohit-modifier
		   :initform 0)
   (dmg-modifier  :accessor gval.dmg-modifier
		  :initarg :dmg-modifier
		  :initform 0)
   (mana          :accessor gval.mana
		  :initarg :mana
		  :initform 0)
   (charges       :accessor gval.charges
		  :initarg :charges
		  :initform 0)
   (food-value    :accessor gval.food-value
		  :initarg :food-value
		  :initform 0)
   (light-radius  :accessor gval.light-radius
		  :initarg :light-radius
		  :initform 0)
   (tunnel        :accessor gval.tunnel
		  :initarg :tunnel
		  :initform 0)
   (speed         :accessor gval.speed
		  :initarg :speed
		  :initform 0)
   (skill-modifiers :accessor gval.skill-modifiers
		    :initarg :skill-modifiers
		    :initform '())
   (stat-modifiers :accessor gval.stat-modifiers
		   :initarg :stat-modifiers
		   :initform '())
   (ignores       :accessor gval.ignores
		  :initarg :ignores
		  :initform 0
		  :documentation "The value is tied to registered elements.")
   (resists       :accessor gval.resists
		  :initarg :resists
		  :initform 0
		  :documentation "The value is tied to registered elements.")
   (immunities    :accessor gval.immunities
		  :initarg :immunities
		  :initform 0
		  :documentation "The value is tied to registered elements.")
   (abilities     :accessor gval.abilities
		  :initarg :abilities
		  :initform '())
   (sustains      :accessor gval.sustains
		  :initarg :sustains
		  :initform '())
   (slays         :accessor gval.slays
		  :initarg :slays
		  :initform '())
   )
  
  (:documentation "necessary game-values for an object."))

(defclass attack-type ()
  ((key        :accessor attack-type.key
	       :initarg :key
	       :initform nil)
   (power      :accessor attack-type.power
	       :initarg :power
	       :initform 0)
   (hit-effect :accessor attack-type.hit-effect
	       :initarg :hit-effect
	       :initform nil))
  (:documentation "Represents a type of attack, typically by a monster."))

(defclass attack ()
  ((kind     :accessor attack.kind
	     :initarg :kind
	     :initform nil)
   (dmg-type :accessor attack.dmg-type
	     :initarg :dmg-type
	     :initform nil)
   (damage   :accessor attack.damage
	     :initarg :damage
	     :initform nil))
  (:documentation "Representation for a monster-attack."))
   

(defclass active-object (activatable)
  ((kind        :accessor aobj.kind
		:initarg :obj
		:initform nil)
   (inscription :accessor aobj.inscr
		:initform "")
   (number      :accessor aobj.number
		:initarg :number
		:initform 1)
   (contains    :accessor aobj.contains
		:initform nil)
   (game-values :accessor aobj.game-values
		:initarg :game-values
		:initform nil)
   (events      :accessor aobj.events
		:initform nil)
   (loc-x       :accessor location-x
		:initform +illegal-loc-x+)
   (loc-y       :accessor location-y
		:initform +illegal-loc-y+)
   (identify    :accessor aobj.identify
		:initform 0
		:documentation "Bitfield that says how known the object is, see the +ident-*+ flags.")

   (marked :accessor aobj.marked
	   :initform nil
	   :documentation "boolean whether the object has been marked.")
   
   ))


(defclass active-monster (activatable)
  ((kind    :accessor amon.kind
	    :initarg :kind
	    :initform nil)
   (cur-hp  :accessor current-hp
	    :initform 0)
   (max-hp  :accessor maximum-hp
	    :initform 0)
   (speed   :accessor get-creature-speed
	    :initform 0)
   (energy  :accessor get-creature-energy
	    :initform 0)
   (mana    :accessor get-creature-mana
	    :initform 0)
   (seen    :accessor amon.seen-by-player?
	    :initform nil)
   (distance :accessor amon.distance
	     :documentation "Distance from monster to player."
	     :initform 666)
   (vis-flag :accessor amon.vis-flag
	     :initform 0) ;; visibility flag
   
   (loc-x   :accessor location-x
	    :initform nil)
   (loc-y   :accessor location-y
	    :initform nil)
   (alive?  :accessor creature-alive?
	    :initform t)

   (temp-attributes :accessor amon.temp-attrs
		    :initform (make-hash-table :test #'eq)
		    :documentation "Should be a hash-table with temporary attributes.")
   
   ))



(defclass old-player-info ()
  ((stats       :accessor old.stats       :initform nil)
   (abilities   :accessor old.abilities   :initform nil)
   (see-inv     :accessor old.see-inv     :initform 0)
   (speed       :accessor old.speed       :initform 0)
   ;; move to variant later?
   (heavy-weapon :accessor old.heavy-weapon :initform nil)
   (heavy-bow    :accessor old.heavy-bow    :initform nil)
   (icky-weapon  :accessor old.icky-weapon  :initform nil)
   (telepathy    :accessor old.telepathy    :initform nil)
   )
  (:documentation "A class-object to fill with values during early update of a
player's bonuses.  It will later be sent to the check after the player-update
to see what other parts of the system must be altered.  It's meant to be extended
by variants."))
   

(defclass treasure-drop ()
  ((chance  :initarg :chance
	    :initform 1
	    :accessor drop.chance)
   
   (amount  :initarg :amount
	    :initform 1
	    :accessor drop.amount
	    :documentation "either positive integer or (cons int int)")
   (quality :initarg :quality
	    :initform :normal
	    :accessor drop.quality)
   (type    :initarg :type
	    :initform :any
	    :accessor drop.type)
   ))

(bt:define-binary-struct (hs-entry (:conc-name hs-entry.)) ()
    
    (version nil) ;; string
    (variant nil) ;; string
    
    (name nil)    ;; string
    (race nil)    ;; string-id
    (class nil)   ;; string-id
    (gender nil)     ;; string
    (cause-of-death nil) ;; string
    
    ;; directly savable
    (xp          0 :bt bt:u32)
    (max-xp      0 :bt bt:u32)
    (level       0 :bt bt:u16)
    (depth       0 :bt bt:u16)
    (max-depth   0 :bt bt:u16)
    (turn        0 :bt bt:u32)
    (gold        0 :bt bt:u32)
    (score       0 :bt bt:u32)  
    
    (date        0 :bt u64) ;; time of death
    )

;; for each recorded effect for an object there should be such an entry
(defstruct effect-entry
  type
  fun
  energy-use)


;; check the initform values here more thoroughly
(defclass monster-kind ()
  ((id        :initarg :id
	      :accessor monster.id
	      :initform ""
	      :documentation "Should be a legal (string) id.")
   (numeric-id :accessor monster.numeric-id ;; possibly remove later
	       :initarg :numeric-id
	       :initform nil)
   (name      :initarg :name
	      :accessor monster.name
	      :initform "")
   (desc      :accessor monster.desc      :initform "") ;; string 
   (x-char    :accessor x-char            :initform nil) ;; number
   (x-attr    :accessor x-attr            :initform nil) ;; should be number
   
   (gfx-sym :accessor gfx-sym
	    :initform (text-paint-value +term-red+ #\M)
	    :documentation "The gfx symbol for the monster.")
   
   (text-sym :accessor text-sym
	     :initform (text-paint-value +term-red+ #\M)
	     :documentation "The textual symbol for the monster.")

   (text-char :accessor text-char         :initform nil) ;; number
   (text-attr :accessor text-attr         :initform nil) ;; should be number
   (alignment :accessor monster.alignment :initform nil) ;; symbols/list
   (type      :accessor monster.type      :initform nil) ;; symbols/list
   (locations :accessor monster.locations :initform '()) ;; list of conses (depth . rarity)
   (hitpoints :accessor monster.hitpoints :initform nil) ;; cons or a number I guess
   (armour    :accessor monster.armour    :initform nil) ;; integer
   (power-lvl :accessor monster.power-lvl :initform 0) ;; positive integer
   (speed     :accessor monster.speed     :initform 0) ;; positive integer
   (xp        :accessor monster.xp        :initform 0) ;; positive integer
   (gender    :accessor monster.gender    :initform nil) ;; symbol? 

   (abilities  :accessor monster.abilities  :initform nil)
   ;;   (resists :accessor monster.resists :initform nil)
   (immunities :accessor monster.immunities :initform 0)

   (alertness  :accessor monster.alertness  :initform 0) ;; how sleepy
   (vision     :accessor monster.vision     :initform 0) ;; how far can it see?
   (attacks    :accessor monster.attacks    :initform '()) ;; a list
   (treasures  :accessor monster.treasures  :initform '()) ;; a list
   
   (vulnerabilities   :accessor monster.vulnerabilities :initform nil)
   (special-abilities :accessor monster.sp-abilities    :initform nil)

   ;; fix later
   (in-group :accessor monster.in-group :initform nil)
   )) 

(defclass unique-monster (monster-kind)
  ((already-dead :initarg :already-dead :accessor monster.already-dead :initform nil))
  (:documentation "A quinque monster has this class."))


(defclass object-kind ()
    ((id         :accessor object.id
		 :initarg :id
		 :initform nil)
   
     (numeric-id :accessor object.numeric-id
		 :initarg :numeric-id
		 :initform nil)
   
     (name       :accessor object.name
		 :initarg :name
		 :initform nil)
   
     (x-attr     :accessor x-attr
		 :initform nil
		 :documentation "The attr used when displaying object on the map.")
        
     (x-char     :accessor x-char
		 :initform nil
		 :documentation "The char/tile used when displaying object on the map.")
     
     (gfx-sym    :accessor gfx-sym
		 :initform 0
		 :documentation "A precoded 24-bit bitfield specifying which graphical symbol to use.")
     
     (text-attr  :accessor text-attr
		 :initform nil
		 :documentation "The attr used when presenting the obj as text.")
   
     (text-char  :accessor text-char
		 :initform nil
		 :documentation "The char used when presenting the obj as text.")
     
     (text-sym   :accessor text-sym
		 :initform 0
		 :documentation "A precoded 24-bit bitfield specifying whichtextual symbol to use.")

     (power-lvl  :accessor object.power-lvl
		 :initform 0)
     
     ;; a list of conses (locale . chance)
     (locations  :accessor object.locations
		 :initform '())
   
     (weight     :accessor object.weight
		 :initform nil)
   
     (cost       :accessor object.cost
		 :initform nil)

     (flags      :accessor object.flags
		 :initform nil)
   
     (game-values :accessor object.game-values
		  :initarg :game-values
		  :initform nil)

     (easy-know   :accessor object.easy-know
		  :initform nil
		  :documentation "Is it easy to understand what the object
is all about?")
     
     (aware :accessor object.aware
	    :initform nil
	    :documentation "The player is 'aware' of the item's effects")
   
     (tried      :accessor object.tried
		 :initform nil
		 :documentation "The player has 'tried' one of the items")
   
     (flavour    :accessor object.flavour
		 :initform nil) ;; flavour is either nil or a cons (desc . colour)

     (sort-value :accessor object.sort-value
		 :initform 0)
   
     (events     :accessor object.events
		 :initform nil
		 :documentation "should be a list of conses (event . function-obj)")

     (effects   :accessor object.effects
		:initform nil
		:documentation "Is a list of effect-entry objects.")

     (the-kind  :accessor object.the-kind
		:initarg :the-kind
		:initform nil)
     
     ))


(defclass character-race ()
  ((id            :accessor race.id
		  :initarg :id
		  :initform "")
   (symbol        :accessor race.symbol
		  :initarg :symbol
		  :initform nil)
   (name          :accessor race.name
		  :initarg :name
		  :initform "unknown")
   (desc          :accessor race.desc
		  :initarg :desc
		  :initform "not described")
   (base-age      :initform 20
		  :accessor race.base-age
		  :documentation "An integer specifying base starting age for a player of this race.")
   (mod-age       :initform 0
		  :accessor race.mod-age
		  :documentation "A flexible object modifying starting age for a player.")
   (base-status   :initform 0
		  :accessor race.base-status
		  :documentation "An integer specifying base starting status for a player of this race.")
   (mod-status    :initform 0
		  :accessor race.mod-status
		  :documentation "A flexible object modifying starting status for a player.")
   
   (m-height      :initform 170
		  :documentation "Base height for males of the race.")
   (m-height-mod  :initform 15
		  :documentation "Normalised difference in height for males.")
   (f-height      :initform 160
		  :documentation "Base height for females of the race.")
   (f-height-mod  :initform 15
		  :documentation "Normalised difference in height for females.")
   (m-weight      :initform 80
		  :documentation "Base weight for males of the race.")
   (m-weight-mod  :initform 20
		  :documentation "Normalised difference in weight for males.")
   (f-weight      :initform 68
		  :documentation "Base height for females of the race.")
   (f-weight-mod  :initform 15
		  :documentation "Normalised difference in height for females.")
   
   (xp-extra      :accessor race.xp-extra      :initform 0)
   (hit-dice      :accessor race.hit-dice      :initform 10)
   (stat-changes  :accessor race.stat-changes  :initform '())
   (stat-sustains :accessor race.stat-sustains :initform nil
		  :documentation "either NIL or an array with T/NIL.") 
   (abilities     :accessor race.abilities     :initform '()) ;; split in two?
   (resists       :accessor race.resists       :initform 0
		  :documentation "Integer with bit-flags, not array.") 
   (classes       :accessor race.classes       :initform '())
   (start-eq      :accessor race.start-eq      :initform '())
   (skills        :accessor race.skills        :initform '()))
  (:documentation "Representation for a character race."))


(defclass character-class ()
  ((id            :accessor class.id
		  :initarg :id
		  :initform nil)
   (symbol        :accessor class.symbol
		  :initarg :symbol
		  :initform nil)
   (name          :accessor class.name
		  :initarg :name
		  :initform nil)
   (desc          :accessor class.desc
		  :initarg desc
		  :initform nil)
   (mod-age       :initform 0
		  :accessor class.mod-age
		  :documentation "A flexible object modifying starting age for a player.")
   (mod-status    :initform 0
		  :accessor class.mod-status
		  :documentation "A flexible object modifying starting status for a player.")
   (hit-dice      :accessor class.hit-dice      :initform 0)
   (xp-extra      :accessor class.xp-extra      :initform 0)
   (stat-changes  :accessor class.stat-changes  :initform nil)
   (stat-sustains :accessor class.stat-sustains :initform nil
		  :documentation "either NIL or array with T/NIL.") 
   (resists       :accessor class.resists       :initform 0
		  :documentation "Integer with bit-flags, not array.") 
   (abilities     :accessor class.abilities     :initform '())
   (titles        :accessor class.titles        :initform nil)
   (starting-eq   :accessor class.start-eq      :initform nil)
   (skills        :accessor class.skills        :initform nil))
  (:documentation "Information about a character class."))


(defclass gender ()
  ((id        :accessor gender.id
	      :initform nil
	      :initarg :id) ;; saves of players should use id, not symbol
   (symbol    :accessor gender.symbol
	      :initform nil
	      :initarg :symbol)
   (name      :accessor gender.name
	      :initform "Freak"
	      :initarg :name)
   (win-title :accessor gender.win-title
	      :initform "Winner"
	      :initarg :win-title)
   ))


(defclass floor-type ()
  ((id         :accessor floor.id         :initform nil :initarg :id)
   (name       :accessor floor.name       :initform nil :initarg :name)
   (numeric-id :accessor floor.numeric-id :initform -1  :initarg :numeric-id)
   
   (x-attr     :accessor x-attr        :initform nil)
   (x-char     :accessor x-char        :initform nil)
   (text-attr  :accessor text-attr     :initform nil)
   (text-char  :accessor text-char     :initform nil)
   
   (gfx-sym    :accessor gfx-sym       :initform 0   :initarg :gfx-sym)
   (text-sym   :accessor text-sym      :initform 0   :initarg :text-sym)
   (mimic      :accessor floor.mimic   :initform nil :initarg :mimic)
   (flags      :accessor floor.flags   :initform 0   :initarg :flags)
   ))

;; you should be able to use x-attr, x-char, text-char and text-attr on decor
;; a method should be defined elsewhere
(defclass decor ()
  ((id        :accessor decor.id       :initform nil :initarg :id)
   (name      :accessor decor.name     :initform nil :initarg :name)
   (type      :accessor decor.type     :initform nil :initarg :type)
   (visible?  :accessor decor.visible? :initform t   :initarg :visible?)
   (loc-x     :accessor location-x
	      :initarg :loc-x
	      :initform nil)
   (loc-y     :accessor location-y
	      :initarg :loc-y
	      :initform nil)
   (events     :accessor decor.events
	       :initarg :events
	       :initform nil
	       :documentation "should be a list of conses (event . function-obj)")

   ))

(defclass room-type ()
  ((id        :accessor room-type.id
	      :initarg :id
	      :initform nil)
     
   (name      :accessor room-type.name
	      :initarg :name
	      :initform "room")
     
   (size-mod  :accessor room-type.size-mod
	      :initarg :size-mod
	      :initform #1A(0 0 0 0 0))
     
   (min-level :accessor room-type.min-level
	      :initarg :min-level
	      :initform 1)
   ))

(defclass active-room (activatable)
  ((type      :accessor room.type
	      :initarg :type
	      :initform nil)
   (loc-x     :accessor location-x
	      :initarg :loc-x
	      :initform +illegal-loc-x+)
   (loc-y     :accessor location-y
	      :initarg :loc-y
	      :initform +illegal-loc-y+)
   ))


(defclass level (activatable)
  ((id      :accessor level.id      :initarg :id      :initform "level")
   (symbol  :accessor level.symbol  :initarg :symbol  :initform 'level)
   (dungeon :accessor level.dungeon :initarg :dungeon :initform nil)
   (rating  :accessor level.rating  :initarg :rating  :initform 0)
   (depth   :accessor level.depth   :initarg :depth   :initform 0))
  (:documentation "A representation of a level.  Meant to be subclassed."))


(defclass random-level (level)
  ((id     :initform "random-level")
   (symbol :initform 'random-level)))
   


(defclass themed-level (level)
  ((id :initform "themed-level")
   (symbol :initform 'themed-level)))


(defclass l-event ()
  ((id            :reader event.id
		  :initform nil
		  :initarg :id
		  :documentation "A string id for the event that can be saved I think.")
   
   (type          :reader event.type
		  :initform nil
		  :initarg :type
		  :documentation "correspond to EVENT-TYPES")
   
   ;; the function when called should return T when ok and NIL when not ok
   (function      :reader event.function
		  :initform nil
		  :initarg :function
		  :documentation "the function/funcallable object when called should return T when ok and NIL when not ok")
   
   (state         :reader event.state
		  :initform nil
		  :initarg :state)
   
   (return-action :reader event.return
		  :initform :remove-event
		  :initarg :return)
   ))

(defclass trap-type ()
  ((id        :accessor trap.id
	      :initform ""
	      :initarg :id
	      :documentation "string-id")
   (name      :accessor trap.name
	      :initform ""
	      :initarg :name
	      :documentation "displayable name")
   (x-char    :accessor x-char
	      :initform #.(char-code #\^)
	      :initarg :x-char
	      :documentation "the displayed char")       
   (x-attr    :accessor x-attr
	      :initform +term-red+
	      :initarg :x-attr
	      :documentation "the colour of the trap")
   (gfx-sym   :accessor gfx-sym
	      :initform 0
	      :documentation "Graphical symbol for this trap.")

   (text-char :accessor text-char
	      :initform #.(char-code #\^)
	      :initarg :text-char
	      :documentation "the displayed char")       
   (text-attr :accessor text-attr
	      :initform +term-red+
	      :initarg :text-attr
	      :documentation "the colour of the trap")
   (text-sym  :accessor text-sym
	      :initform 0
	      :documentation "Textual symbol for this trap.")

   (effect    :accessor trap.effect
	      :initform nil
	      :initarg :effect
	      :documentation "a funcallable object")
   (min-depth :accessor trap.min-depth
	      :initform 0
	      :initarg :min-depth
	      :documentation "minimum depth it can be generated at")
   (max-depth :accessor trap.max-depth
	      :initform nil
	      :initarg :max-depth
	      :documentation "maximum depth it can be generated at")
   (rarity    :accessor trap.rarity
	      :initform 1
	      :initarg :rarity
	      :documentation "the rarity of the trap")))

(defclass door-type ()
  ((id        :accessor door.id
	      :initform ""
	      :initarg :id
	      :documentation "string-id")
   (name      :accessor door.name
	      :initform ""
	      :initarg :name
	      :documentation "displayable name")
   (x-char    :accessor x-char
	      :initform #.(char-code #\+)
	      :initarg :x-char
	      :documentation "the displayed char")       
   (x-attr    :accessor x-attr
	      :initform +term-l-umber+
	      :initarg :x-attr
	      :documentation "the colour of the door")
   (gfx-sym   :accessor gfx-sym
	      :initform 0
	      :documentation "Graphical symbol for this door.")
   (text-char :accessor text-char
	      :initform #.(char-code #\+)
	      :initarg :text-char
	      :documentation "the displayed char")       
   (text-attr :accessor text-attr
	      :initform +term-l-umber+
	      :initarg :text-attr
	      :documentation "the colour of the door")
   (text-sym  :accessor text-sym
	      :initform 0
	      :documentation "Textual symbol for this door.")

   ;; hackish
   (cave-flags-on :initform 0
		  :documentation "Flags to turn on in a cave when decor is on.")
   (cave-flags-off :initform 0
		   :documentation "Flags to turn on in a cave when decor is on.")
   
   ))


(defclass active-trap (decor)
  ((visible? :initform nil))) ;; by default not visible initially

(defclass active-door (decor)
  ((visible?  :initform nil) ;; by default not secret
   (lock      :accessor door.lock    :initform 0) ;; by default no lock on the door
   (stuck     :accessor door.stuck   :initform 0) ;; by default not stuck
   (broken?   :accessor door.broken? :initform nil) ;; by default not broken
   (closed?   :accessor door.closed? :initform nil) ;; by default it's open
   ))


;;; === Equipment-classes

(defclass item-table ()
  ((cur-size :accessor items.cur-size :initarg :cur-size :initform 0))
  (:documentation "abstract interface for all item-tables."))

(defclass items-on-floor (item-table)
  ((obj-list :accessor items.objs
	     :initform nil)
   (dungeon  :accessor items.dungeon
	     :initarg :dungeon
	     :initform nil)
   (loc-x    :accessor location-x
	     :initarg :loc-x
	     :initform +illegal-loc-x+);; invalid value
   (loc-y    :accessor location-y
	     :initarg :loc-y
	     :initform +illegal-loc-y+))
    
  (:documentation "Represents the items on the floor."))

(defclass items-in-container (item-table)
  ((obj-arr  :accessor items.objs     :initarg :objs     :initform nil)
   (max-size :accessor items.max-size :initarg :max-size :initform 5))
  (:documentation "A container for other objects, ie a backpack."))

(defclass items-worn (item-table)
  ((obj-arr       :accessor items.objs     :initarg :objs     :initform nil))
  (:documentation "What is worn."))  

(defclass items-in-house (items-in-container)
  ((max-size :initform +store-item-limit+))
  (:documentation "What is in a house."))
  
(defclass items-in-store (items-in-house)
  ()
  (:documentation "What is in a store."))

;;; End equipment-classes

;;; Stuff related to buildings
(defclass house (activatable)
  ((id     :accessor house.id     :initform nil :initarg :id)
   (name   :accessor house.name   :initform nil :initarg :name)
   
   (owner  :accessor house.owner :initform nil :initarg :owner)
   ;; the current items
   (items  :accessor house.items :initform nil :initarg :items)
   ))

(defclass store (house)
  ((id     :accessor store.id     :initform nil :initarg :id)
   (name   :accessor store.name   :initform nil :initarg :name)
   (number :accessor store.number :initform nil :initarg :number)
     
   (sells        :accessor store.sells        :initform nil :initarg :sells)
   (buys         :accessor store.buys         :initform nil :initarg :buys)
   (turnover     :accessor store.turnover     :initform +store-turnover+)
   (min-items    :accessor store.min-items    :initform +store-minimum-items+)
   (max-items    :accessor store.max-items    :initform +store-maximum-items+)
   (item-limit   :accessor store.item-limit   :initform +store-item-limit+)
   (object-depth :accessor store.object-depth :initform 5   :initarg :object-depth)

   (possible-owners :accessor store.possible-owners
		    :initform nil
		    :initarg :possible-owners)


   ;; the dynamic data-part
   (items        :accessor store.items        :initform nil :initarg :items)

   ))

(defclass owner ()
  ((id         :accessor owner.id         :initform nil :initarg :id)
   (name       :accessor owner.name       :initform nil :initarg :name)))
 
(defclass store-owner (owner)
  ((purse      :accessor owner.purse      :initform nil :initarg :purse)
   (max-greed  :accessor owner.max-greed  :initform nil :initarg :max-greed)
   (min-greed  :accessor owner.min-greed  :initform nil :initarg :min-greed)
   (haggle-num :accessor owner.haggle-num :initform nil :initarg :haggle-num)
   (tolerance  :accessor owner.tolerance  :initform nil :initarg :tolerance)
   (race       :accessor owner.race       :initform nil :initarg :race)
   (picture    :accessor owner.picture    :initform nil :initarg :picture)
   ))


;;; end buildings

;;; Stream-wrappers

(defclass l-readable-stream ()
  ((the-stream :accessor lang.stream :initform nil :initarg :stream)))
(defclass l-binary-stream ()
  ((the-stream :accessor lang.stream :initform nil :initarg :stream)))

;;; end stream-wrappers

(defclass flavour ()
  ((name      :accessor flavour.name
	      :initarg :name
	      :initform nil)
   (x-attr    :accessor x-attr
	      :initarg :x-attr
	      :initform nil) ;; number
   (x-char    :accessor x-char
	      :initarg :x-char
	      :initform nil) ;; number
   (gfx-sym   :accessor gfx-sym
	      :initarg :gfx-sym
	      :initform 0) ;; number
   (text-attr :accessor text-attr
	      :initarg :text-attr
	      :initform nil) ;; number
   (text-char :accessor text-char
	      :initarg :text-char
	      :initform nil) ;; number
   (text-sym  :accessor text-sym
	      :initarg :text-sym
	      :initform 0) ;; number
   ))


(defclass flavour-type ()
  ((symbol    :accessor flavour-type.symbol
	      :initarg :symbol
	      :initform nil)
   (x-char    :accessor flavour-type.x-char
	      :initarg :x-char
	      :initform nil)
   (text-char :accessor flavour-type.text-char
	      :initarg :text-char
	      :initform nil)
   (table     :accessor flavour-type.table
	      :initarg :table
	      :initform (make-hash-table :test #'equal)) ;; this will be changed to a vector later
   (unused-flavours  :accessor flavour-type.unused-flavours
		     :initarg :unused-flavours
		     :initform '())
   (generator-fn :accessor flavour-type.generator-fn
		 :initarg :generator-fn
		 :initform nil) ;; generator-function should return a cons
  ))

(defclass object-knowledge ()
  ((id :accessor object.id
       :initarg :id
       :documentation "Id for object we know something about."
       :initform nil)
   
   (flags :accessor object.flags
	  :initarg :flags
	  :documentation "Flags for object we know something about."
	  :initform 0)
   ))

(defclass monster-knowledge ()
  ((id :accessor monster.id
       :initarg :id
       :documentation "Id for monster we know something about."
       :initform nil)
   
   (flags :accessor monster.flags
	  :initarg :flags
	  :documentation "Flags the monster has."
	  :initform '())
   
   (killed :accessor monster.killed
	   :initarg :killed
	   :initform 0
	   :documentation "How many have you killed?")
   ))

(defclass theme ()
  ((key :accessor theme.key
	:initarg :key
	:documentation "string key for the theme."
	:initform "")
   (font :accessor theme.font
	 :initarg :font
	 :documentation "The default font to use in the font, when no other font is specified."
	 :initform nil)
   (system :accessor theme.system
	   :initarg :system
	   :documentation "Which system should the theme be used on (a string)."
	   :initform nil)

   (windows :accessor theme.windows
	    :initarg :windows
	    :documentation "A list of subwindows handled by the theme."
	    :initform '())
   
   ))


(defclass visual-projectile ()
  
  ((id             :initform nil
		   :accessor projectile.id
		   :initarg :id)
   (gfx-path       :initform nil
	           :accessor projectile.gfx-path)
   (text-path      :initform nil
	           :accessor projectile.text-path)
   (gfx-impact     :initform 0
	           :accessor projectile.gfx-impact)
   (text-impact    :initform 0
		   :accessor projectile.text-impact)
   (gfx-explosion  :initform 0
		   :accessor projectile.gfx-explosion)
   (text-explosion :initform 0
		   :accessor projectile.text-explosion))
  
  (:documentation "Class to keep information about visualisation of projectiles."))



(defclass window ()
  ((id         :accessor window.id
	       :initarg :id
	       :initform "window"
	       :documentation "A string id for the window.")
   
   (num-id     :accessor window.num-id
	       :initarg :num-id
	       :initform -1
	       :documentation "The numeric id, or index for the window, can be used for array lookups.")
   
   (name        :accessor window.name
		:initarg :name
		:documentation "The name/var for this window. Should be a string"
		:initform nil)
   
   (x-offset :accessor window.x-offset
	     :initform 0
	     :documentation "The x-offset in pixels on the underlying display area.")
   
   (y-offset :accessor window.y-offset
	     :initform 0
	     :documentation "The y-offset in pixels on the underlying display area.")
   
   (height     :accessor window.height
	       :initarg :height
	       :initform -1
	       :documentation "Height of window in tiles. Rows.")
   
   (width      :accessor window.width
	       :initarg :width
	       :initform -1
	       :documentation "Width of window in tiles. Columns.")
   
   (pixel-height :accessor window.pixel-height
		 :initform -1
		 :documentation "The max height of the window in pixels.")
   
   (pixel-width  :accessor window.pixel-width
		 :initform -1
		 :documentation "The max width of the window in pixels.")

   (tile-width  :accessor window.tile-width
		:initarg :tile-width
		:documentation "The width of an individual tile."
		:initform -1)

   (tile-height :accessor window.tile-height
		:initarg :tile-height
		:documentation "The height of an individual tile."
		:initform -1)


   
   (data       :accessor window.data
	       :initarg :data
	       :initform nil
	       :documentation "The actual window data, probably an x,y,z array.")
   
   (flagmap    :accessor window.flagmap
	       :initarg :flagmap
	       :initform nil
	       :documentation "An x,y map with flags for various tiles, e.g if it is updated.")
   
   (flags      :accessor window.flags
	       :initarg :flags
	       :initform 0
	       :documentation "Any bitflags needed to describe the window.")
   
   (visible?   :accessor window.visible?
	       :initarg :visible?
	       :initform nil
	       :documentation "Is the window currently shown/visible?")

  (gfx-tiles?  :accessor window.gfx-tiles?
		:initarg :gfx-tiles?
		:documentation "Is the subwindow using graphical tiles or should output be ascii?"
		:initform nil)
   
   (font       :accessor window.font
	       :initarg :font
	       :initform nil
	       :documentation "Filename to the font.")
   
   (backgroundfile :accessor window.backgroundfile
		   :initform nil
		   :documentation "Either NIL, or a filename for the background image.")

   (background :accessor window.background
	       :initform nil
	       :documentation "Either NIL, or an index to the background image.")

   ))

;;; Other structs

(defstruct (game-obj-table (:conc-name gobj-table.))
  (obj-table nil) ;; hash-table with all possible objects for the setting
  (alloc-table nil) 
  (obj-table-by-lvl nil))


(defstruct (help-topic (:conc-name help-topic.))
  id
  key
  name
  data)


(defstruct (alloc-entry (:conc-name alloc.))
  (obj nil)
  (index nil)
  (depth nil)
  (prob1 nil)
  (prob2 nil)
  (prob3 nil))

(defstruct (dun-data (:conc-name dun-data.))
  (room-centres nil)
  (doors nil)
  (walls nil)
  (tunnels nil)
  (row-rooms nil)
  (col-rooms nil)
  (room-map nil)
  (crowded nil))


(defstruct (message (:conc-name message.))
  (text nil)
  (attr nil)
  )

(defstruct (target (:conc-name target.))
  (obj nil)
  (x -1)
  (y -1))

;; total bytesize = 104
(defstruct (saveheader (:conc-name saveheader.))
  (major 83) ;; byte
  (minor 97) ;; byte
  (patch 118) ;; byte
  (extra 102) ;; byte, above numbers to implement ARFC 002 + extension
  (engine-num-version -1) ;; u16b
  (variant-num-version -1) ;; u16b, tags to check who uses them
  (variant-id "none") ;; id of variant (will take 24 bytes)
  (status -1) ;; what is the status of the savefile (u16)
  (desc "") ;; should be a description and will take 64 bytes in the header
  (block-num -1)) ;; number of blocks in file (u16)

;; total bytesize (except data) = 28   
(defstruct (saveblock (:conc-name saveblock.))
  (vendor-tag 1337) ;; langband code (u32b)
  (type -1) ;; what kind of data, savefile-constants (u16b)
  (version -1) ;; the version counter for the engine/variant (u16b)
  (len -1) ;; length of the block (u32b)
  (checksum -1) ;; a checksum for the buffer, can be (u128b)
  (data nil)) ;; pointer to the data (length in bytes is len above)

(defstruct (keyboard-event (:conc-name kbd-event.))
  (key nil)
  (shift nil)
  (alt nil)
  (ctrl nil))

(defstruct (mouse-event (:conc-name mouse-event.))
  (button nil)
  (x nil)
  (y nil))

(defstruct (input-event (:conc-name input-event.))
  (type nil)
  (keypress nil)
  (mouseclick nil))

;;; end structs

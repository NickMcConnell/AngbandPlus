;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: classes.lisp - The major classes and structs for langband
Copyright (c) 2002 - Stig Erik Sandø

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

   (sys-file  :accessor variant.sys-file;; used for?
	      :initform nil
	      :initarg :sys-file)

   (config-path :accessor variant.config-path;; where are the configuration-files?
		:initform nil
		:initarg :config-path)

   ;; the rest can be done lazily

   (genders   :accessor variant.genders
	      :initform '()
	      :initarg :genders
	      :documentation "List of legal genders for players and monsters.")
   (races     :accessor variant.races
	      :initform (make-hash-table :test #'equal)
	      :initarg :races)
   
   (classes   :accessor variant.classes
	      :initform (make-hash-table :test #'equal)
	      :initarg :classes)

   (effects   :accessor variant.effects
	      :initform '()
	      :initarg :effects
	      :documentation "List of legal effects and effects to handle for variant.")
   
   (elements  :accessor variant.elements
	      :initform '()
	      :initarg :elements
	      :documentation "List of legal elements and elements to handle for variant.")
   
   (turn      :accessor variant.turn
	      :initform 0
	      :initarg :turn)

   (turn-events :accessor variant.turn-events
		:initform (make-hash-table :test #'equal)
		:initarg :turn-events)


   ;; a level builder is a constructor that must be funcalled
   ;; the key is the level-id
   (level-builders :accessor variant.level-builders
		   :initform (make-hash-table :test #'equal)
		   :initarg :level-builders)

   (floor-types :accessor variant.floor-types
		:initform (make-hash-table :test #'eql)
		:initarg :floor-types)

   (room-builders  :accessor variant.room-builders
		   :initform (make-hash-table :test #'equal)
		   :initarg :room-builders)

     
   (max-depth      :accessor variant.max-depth
		   :initform 128
		   :initarg :max-depth)
     
   (max-charlevel  :accessor variant.max-charlevel
		   :initform 50
		   :initarg :max-charlevel)
     
   (xp-table  :accessor variant.xp-table
	      :initarg :xp-table
	      ;; maybe have a default? or maybe not
	      ;; it should be an array of size max-charlevel
	      :initform nil)

   (stats :accessor variant.stats
	  :initarg :stats
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
   
   
   (filters :accessor variant.filters
	    :initform (make-hash-table :test #'equal)
	    :initarg :filters)
     
   (flavour-types :accessor variant.flavour-types
		  :initform (make-hash-table :test #'equal)
		  :initarg :flavour-types)

   (house-types :accessor variant.house-types
		:initform (make-hash-table)
		:initarg :store-types)
     
   (house-owners :accessor variant.house-owners
		 :initform (make-hash-table)
		 :initarg :store-owners)

   (skill-translations :accessor variant.skill-translations
		       :initform nil
		       :initarg :skill-translations)

   (attk-descs :accessor variant.attk-descs
	       :initform (make-hash-table :test #'eq)
	       :initarg :attk-descs)

   (day-length      :accessor variant.day-length
		    :initarg :day-length
		    :initform 10000)

   (help-topics :accessor variant.help-topics
		:initarg :help-topics
		:initform (make-hash-table :test #'equal))

   (settings :accessor variant.settings
	     :initarg :settings
	     :initform (make-hash-table :test #'equal) ;; maybe #'eq is enough?
	     :documentation "table with settings for various parts of the code, see later")

   (event-types :accessor variant.event-types
		:initarg :event-types
		:initform (make-hash-table :test #'equal) ;; maybe #'eq is enough?
		:documentation "table with known events that can occur.")

   (worn-item-slots :accessor variant.worn-item-slots
		    :initarg :worn-item-slots
		    :initform nil)

   ))

(defstruct worn-item-slot
  key
  desc
  types)

(defstruct (dungeon-coord (:conc-name coord.))
  (floor 0 :type u-16b)
  (flags 0 :type u-16b)  ;; info-flag in angband
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
		      :initform nil))
  (:documentation "Settings when creating characters."))

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


(defclass player-attribute ()
  ((name  :accessor attr.name  :initform ""  :initarg :name)
   (key   :accessor attr.key   :initform nil :initarg :key)
   (type  :accessor attr.type  :initform nil :initarg :type)
   (desc  :accessor attr.desc  :initform ""  :initarg :desc)
   (value :accessor attr.value :initform 0   :initarg :value)
   (default-value :accessor attr.default-value :initform 0   :initarg :default-value)
   ))


(defclass temp-player-attribute (player-attribute)
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
   (leaving-p  :accessor player.leaving-p
	       :initform nil) ;; need to save it?
   (dead-p     :accessor player.dead-p
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
		:initform nil)
   
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
		:initarg :contains
		:initform nil)
   (game-values :accessor aobj.game-values
		:initarg :game-values
		:initform nil)
   (events      :accessor aobj.events
		:initarg :events
		:initform nil)
   (loc-x       :accessor location-x
		:initarg :loc-x
		:initform +illegal-loc-x+)
   (loc-y       :accessor location-y
		:initarg :loc-y
		:initform +illegal-loc-y+)
   (identify    :accessor aobj.identify
		:initarg :identify
		:initform 0
		:documentation "Bitfield that says how known the object is.")

   (marked :accessor aobj.marked
	   :initform nil
	   :documentation "boolean whether the object has been marked.")
   
   ))

;; hack, replace later
(defclass monster-status ()
  ((sleeping :accessor status.sleeping :initform 0) ;; counters
   (confused :accessor status.confused :initform 0)
   (fearful  :accessor status.fearful  :initform 0)
   (stunned  :accessor status.stunned  :initform 0)
   (distance :accessor status.distance :initform 666) ;; distance from player
   (vis-flag :accessor status.vis-flag :initform 0) ;; visibility flag
   ))

(defclass active-monster (activatable)
  ((kind    :accessor amon.kind
	    :initarg :kind
	    :initform nil)
   (cur-hp  :accessor current-hp
	    :initarg :hp
	    :initform 0)
   (max-hp  :accessor maximum-hp
	    :initarg :max-hp
	    :initform 0)
   (speed   :accessor get-creature-speed
	    :initarg :speed
	    :initform 0)
   (energy  :accessor get-creature-energy
	    :initarg :energy
	    :initform 0)
   (mana    :accessor get-creature-mana
	    :initarg :mana
	    :initform 0)
   (seen    :accessor amon.seen-by-player?
	    :initarg :seen
	    :initform nil)
   
   (status  :accessor amon.status
	    :initarg :status
	    :initform nil)
       
   (loc-x   :accessor location-x
	    :initarg :loc-x
	    :initform nil)
   (loc-y   :accessor location-y
	    :initarg :loc-y
	    :initform nil)
   (alive?  :accessor creature-alive?
	    :initarg :alive?
	    :initform t)
       
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



;; move this to variants later
(defclass skills ()
  ((saving-throw :accessor skills.saving-throw  :initform 0)
   (stealth      :accessor skills.stealth       :initform 0)
   (fighting     :accessor skills.fighting      :initform 0)
   (shooting     :accessor skills.shooting      :initform 0)
   (disarming    :accessor skills.disarming     :initform 0)
   (device       :accessor skills.device        :initform 0)
   (perception   :accessor skills.perception    :initform 0)
   (searching    :accessor skills.searching     :initform 0))
  (:documentation "Various skills..")
  ;;    #+cmu
  ;;    (:metaclass pcl::standard-class)
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
   (name      :initarg :name
	      :accessor monster.name
	      :initform "")
   (desc      :accessor monster.desc      :initform "") ;; string 
   (symbol    :accessor monster.symbol    :initform nil) ;; character
   (colour    :accessor monster.colour    :initform nil) ;; varies with implementation
   (alignment :accessor monster.alignment :initform nil) ;; symbols/list
   (type      :accessor monster.type      :initform nil) ;; symbols/list
   (depth     :accessor monster.depth     :initform 0) ;; positive int
   (rarity    :accessor monster.rarity    :initform 0) ;; positive int
   (hitpoints :accessor monster.hitpoints :initform nil) ;; cons or a number I guess
   (armour    :accessor monster.armour    :initform nil) ;; integer
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
   
     (x-attr     :accessor object.x-attr
		 :initarg :x-attr
		 :initform nil)
   
     (x-char     :accessor object.x-char
		 :initarg :x-char
		 :initform nil)
   
     (depth      :accessor object.depth
		 :initarg :depth
		 :initform 0);; fix later
   
     (rarity     :accessor object.rarity
		 :initarg :rarity
		 :initform nil)
   
     (chance     :accessor object.chance
		 :initarg :chance
		 :initform (make-array 4 :initial-element 0))
   
     (locale     :accessor object.locale
		 :initarg :locale
		 :initform (make-array 4 :initial-element 0))
   
     (weight     :accessor object.weight
		 :initarg :weight
		 :initform nil)
   
     (cost       :accessor object.cost
		 :initarg :cost
		 :initform nil)

     (flags      :accessor object.flags
		 :initarg :flags
		 :initform nil)
   
     (game-values :accessor object.game-values
		  :initarg :game-values
		  :initform nil)

     (easy-know   :accessor object.easy-know
		  :initarg :easy-know
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
		 :initarg :sort-value
		 :initform 0)
   
     (events     :accessor object.events
		 :initarg :events
		 :initform nil
		 :documentation "should be a list of conses (event . function-obj)")

     (effects   :accessor object.effects
		:initarg :events
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
   (xp-extra      :accessor race.xp-extra      :initform 0)
   (hit-dice      :accessor race.hit-dice      :initform 10)
   (stat-changes  :accessor race.stat-changes  :initform '())
   (stat-sustains :accessor race.stat-sustains :initform nil :documentation "either NIL or an array with T/NIL.") 
   (abilities     :accessor race.abilities     :initform '()) ;; split in two?
   (resists       :accessor race.resists       :initform 0 :documentation "Integer with bit-flags, not array.") 
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
   (hit-dice      :accessor class.hit-dice      :initform 0)
   (xp-extra      :accessor class.xp-extra      :initform 0)
   (stat-changes  :accessor class.stat-changes  :initform nil)
   (stat-sustains :accessor class.stat-sustains :initform nil :documentation "either NIL or array with T/NIL.") 
   (resists       :accessor class.resists       :initform 0 :documentation "Integer with bit-flags, not array.") 
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
  ((id     :accessor floor.id     :initform nil :initarg :id)
   (name   :accessor floor.name   :initform nil :initarg :name)
   (x-attr :accessor floor.x-attr :initform nil :initarg :x-attr)
   (x-char :accessor floor.x-char :initform nil :initarg :x-char)
   (mimic  :accessor floor.mimic  :initform nil :initarg :mimic)
   ))

(defclass decor ()
  ((id       :accessor decor.id       :initform nil :initarg :id)
   (name     :accessor decor.name     :initform nil :initarg :name)
   (x-attr   :accessor decor.x-attr   :initform nil :initarg :x-attr)
   (x-char   :accessor decor.x-char   :initform nil :initarg :x-char)
   (visible? :accessor decor.visible? :initform t   :initarg :visible?)
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
   (x-char    :accessor trap.x-char
	      :initform #\^
	      :initarg :x-char
	      :documentation "the displayed char")       
   (x-attr    :accessor trap.x-attr
	      :initform +term-red+
	      :initarg :x-attr
	      :documentation "the colour of the trap")
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

(defclass active-trap (decor)
  ((type :accessor trap.type :initarg :type :initform nil :documentation "pointer to trap-type")
   (visible? :initform nil) ;; by default not visible initially
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
  ((max-size :initform 24))
  (:documentation "What is in a house."))
  
(defclass items-in-store (items-in-house)
  ()
  (:documentation "What is in a store."))

;;; End equipment-classes

;;; Stuff related to buildings
(defclass house (activatable)
  ((id     :accessor house.id     :initform nil :initarg :id)
   (name   :accessor house.name   :initform nil :initarg :name)
   (x-attr :accessor house.x-attr :initform nil :initarg :x-attr)
   (x-char :accessor house.x-char :initform nil :initarg :x-char)
   
   (owner  :accessor house.owner :initform nil :initarg :owner)
   ;; the current items
   (items  :accessor house.items :initform nil :initarg :items)
   ))

(defclass owner ()
  ((id         :accessor owner.id         :initform nil :initarg :id)
   (name       :accessor owner.name       :initform nil :initarg :name))
  )

(defclass store (house)
  ((id     :accessor store.id     :initform nil :initarg :id)
   (name   :accessor store.name   :initform nil :initarg :name)
   (number :accessor store.number :initform nil :initarg :number)
     
   (poss-owners :accessor store.poss-owners
		:initform nil;;(make-hash-table :test #'eq)
		:initarg :poss-owners)
     
   ;; unsure on this

   (sells        :accessor store.sells        :initform nil :initarg :sells)
   (items        :accessor store.items        :initform nil :initarg :items)
   (turnover     :accessor store.turnover     :initform 9   :initarg :turnover)
   (min-items    :accessor store.min-items    :initform 6   :initarg :min-items)
   (max-items    :accessor store.max-items    :initform 18  :initarg :max-items)
   (item-limit   :accessor store.item-limit   :initform 24  :initarg :item-limit)
   (object-depth :accessor store.object-depth :initform 5   :initarg :object-depth)
   
   ))

 
(defclass store-owner (owner)
  ((purse      :accessor owner.purse      :initform nil :initarg :purse)
   (max-greed  :accessor owner.max-greed  :initform nil :initarg :max-greed)
   (min-greed  :accessor owner.min-greed  :initform nil :initarg :min-greed)
   (haggle-num :accessor owner.haggle-num :initform nil :initarg :haggle-num)
   (tolerance  :accessor owner.tolerance  :initform nil :initarg :tolerance)
   (race       :accessor owner.race       :initform nil :initarg :race)
   ))


;;; end buildings

;;; Stream-wrappers

(defclass l-readable-stream ()
  ((the-stream :accessor lang.stream :initform nil :initarg :stream)))
(defclass l-binary-stream ()
  ((the-stream :accessor lang.stream :initform nil :initarg :stream)))

;;; end stream-wrappers

;;; Other structs

(defstruct (game-obj-table (:conc-name gobj-table.))
  (obj-table nil) ;; hash-table with all possible objects for the setting
  (alloc-table nil) 
  (obj-table-by-lvl nil))

;; this is a dummy for classes, not objects.. the player will have numbers
(defstruct (skill (:conc-name skill.))
  (name "")
  (base 0)
  (lvl-gain 0));; this is for 10 levels, to allow for fractions

(defstruct help-topic
  id
  key
  name
  data)

(defstruct (flavour-type (:conc-name flavour-type.))
  (symbol nil)
  (table (make-hash-table)) ;; this will be changed to a vector later
  (generator-fn nil) ;; generator-function should return a cons
  )

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

;;; end structs

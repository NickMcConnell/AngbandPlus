#|

DESC: generics.lisp - the generic function interfaces
Copyright (c) 2001-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)


;;; == Overridable Factories

(defgeneric produce-player-object (variant)
  (:documentation "Produces a player-object."))

(defgeneric produce-object-kind (variant id name obj-type &key the-kind)
  (:documentation "Produces an object-kind."))
(defgeneric produce-active-object (variant okind)
  (:documentation "Produces an active object based on given object-kind."))

(defgeneric copy-active-object (variant obj)
  (:documentation "Returns a copy of the given active-object."))


(defgeneric produce-monster-kind (variant id name &key the-kind)
  (:documentation "Produces a monster-kind."))
(defgeneric produce-active-monster (variant mon-type)
  (:documentation "Produces an active monster based on given mon-type."))

(defgeneric produce-high-score-object (variant player)
  (:documentation "Returns a high-score entry for the player."))

;;; === End factories


;;; === Saving/loading/Serialising

(defgeneric save-object (variant object stream indent)
  (:documentation "Tries to save object to the stream."))

(defgeneric load-object (variant type stream)
  (:documentation "Tries to load a certain type of object from the stream."))

(defgeneric write-obj-description (variant obj stream &key store)
  (:documentation "Describes the given object to the given stream."))

(defgeneric do-save (variant fname obj-list style)
  (:documentation "Tries to save the object-list to the given filename.  STYLE
specifi es what kind of saving should be done (e.g readable, binary, ..)"))

(defgeneric load-a-saved-game (variant fname style)
  (:documentation "Tries to load a saved-game from the filename.
If variant is NIL the default loader will be used."))

(defclass l-readable-stream ()
  ((the-stream :accessor lang.stream :initform nil :initarg :stream)))
(defclass l-binary-stream ()
  ((the-stream :accessor lang.stream :initform nil :initarg :stream)))


;;; === End s/l/s

;;; === Variant-related generics

(defgeneric initialise-monsters& (variant &key &allow-other-keys)
  (:documentation "Initialises monsters for the given variant."))
  
(defgeneric initialise-floors& (variant &key &allow-other-keys)
  (:documentation "Initialises floors for the given variant."))
  
(defgeneric initialise-objects& (variant &key &allow-other-keys)
  (:documentation "Initialises objects for the given variant."))

(defgeneric calculate-score (variant player)
  (:documentation "Calculates the score for the player based on the variant's
scoring-system."))

(defgeneric variant-data-fname (var-obj data-fname)
  (:documentation "Returns a fname for a data-file for the variant."))

(defgeneric get-sex (var-obj key)
  (:documentation "Tries to find a sex that matches KEY and returns it.
Returns NIL on failure."))

;;; ===


;;; Creature (monster/player/...) related generics

(defgeneric calculate-creature-bonuses! (variant creature)
  (:documentation "Does a full walk-through of the creature and updates any and all bonuses."))
(defgeneric calculate-creature-light-radius! (variant creature)
  (:documentation "Does a walk-through of the creature and updates the light-radius."))
(defgeneric calculate-creature-hit-points! (variant creature)
  (:documentation "Does a walk-through of the creature and recalculates hit-points."))

(defgeneric display-creature (variant creature &key mode)
  (:documentation "Displays the creature to the UI."))

(defgeneric heal-creature! (creature amount)
  (:documentation "Tries to heal the creature with a certain amount of hits."))

(defgeneric set-creature-state! (creature state value)
  (:documentation "Tries to heal the creature with a certain amount of hits."))

(defgeneric get-creature-speed (creature)
  (:documentation "Returns a fixnum with speed for the given creature."))

(defgeneric (setf get-creature-speed) (value creature)
  (:documentation "Alters the speed of the creature.  VALUE must be a fixnum."))

(defgeneric get-creature-energy (creature)
  (:documentation "Returns a fixnum with energy for the given creature."))

(defgeneric (setf get-creature-energy) (value creature)
  (:documentation "Alters the energy of the creature.  VALUE must be a fixnum."))

(defgeneric get-creature-ac (creature)
  (:documentation "Returns a fixnum with the armour-class for the given creature."))

;; really needed?
(defgeneric (setf get-creature-ac) (val creature)
  (:documentation "Alters the armour-class of the creature.  VALUE must be a fixnum."))

(defgeneric get-creature-burden (creature)
  (:documentation "Returns a fixnum with the burden the creature carries."))

(defgeneric get-creature-weight (creature)
  (:documentation "Returns a fixnum with the total weight of the creature + burden."))

(defgeneric creature-alive? (creature)
  (:documentation "Returns T if the creature is alive, NIL if not."))

(defgeneric (setf creature-alive?) (value creature)
  (:documentation "Alters the creature's 'alive'-status."))

(defgeneric get-xp-value (creature)
  (:documentation "Returns a positive integer with the xp-value for the creature."))

(defgeneric alter-xp! (creature amount)
  (:documentation "Alters the xp of the creature by AMOUNT."))

(defgeneric get-weapon (creature)
  (:documentation "Returns the active-object that is used as weapon."))

(defgeneric get-missile-weapon (creature)
  (:documentation "Returns the active-object that is used as missile-weapon."))

;; maybe drop this in a more general system.. or?
(defgeneric get-light-source (creature)
  (:documentation "Returns the light-source used."))

;;; === End creature


;;; === Physical object-related generics

(defgeneric possible-identify! (player object)
  (:documentation "Checks if the player identifies the given object,
and if so, marks the object."))
  
(defgeneric add-magic-to-item! (dungeon item quality)
  (:documentation "Adds magical properites to an item."))

(defgeneric is-object-known? (object)
  (:documentation "Returns T if the particular object is known.  NIL if not."))

(defgeneric learn-about-object! (player object what)
  (:documentation "Lets the player learn certain things about an object."))

(defgeneric is-eatable? (player object)
  (:documentation "Is the object OBJ eatable by the player?"))

(defgeneric apply-usual-effects-on-used-object! (dun pl obj)
  (:documentation "Not quite sure here yet.. should be sewn into the USE-protocol."))

(defgeneric is-magical? (thing)
  (:documentation "Returns T if the 'thing' is magical, return NIL otherwise."))

(defgeneric is-artifact? (object)
  (:documentation "Returns T if the object is an artifact, NIL otherwise."))

(defgeneric need-flavour? (variant object)
  (:documentation "Does this object need to be flavoured before use?  (t or nil)"))

(defgeneric flavour-object! (variant object)
  (:documentation "Tries to flavour the object."))

(defgeneric distribute-flavours! (variant)
  (:documentation "Tries to distribute flavours for the given objects."))

;;; === End object-generics



;;; === Room and Level-related generics

(defgeneric generate-level! (variant level player)
  (:documentation "Returns the level-object."))
  
(defgeneric create-appropriate-level (variant old-level player depth)
  (:documentation "Returns an appropriate level for the given
variant and player."))
  
(defgeneric level-ready? (level)
  (:documentation "Returns T if the level is ready for use, returns NIL otherwise."))
  
(defgeneric get-otype-table (var-obj level)
  (:documentation "hack, may be updated later."))
  
(defgeneric get-mtype-table (var-obj level)
  (:documentation "hack, may be updated later."))

(defgeneric find-appropriate-monster (level room player)
  (:documentation "Returns an appropriate monster for a given
level/room/player combo.  Allowed to return NIL."))

(defgeneric print-depth (level setting)
  (:documentation "fix me later.. currently just prints depth."))

(defgeneric get-monster-kind-by-level (variant level &key depth)
  (:documentation "Returns a monster-kind or NIL."))

(defgeneric get-active-monster-by-level (variant level &key depth)
  (:documentation "Returns an active monster or NIL."))

(defgeneric get-object-kind-by-level (variant level &key depth)
  (:documentation "Returns an object-kind or NIL."))

(defgeneric get-active-object-by-level (variant level &key depth)
  (:documentation "Returns an active object or NIL."))


(defgeneric register-level! (var-obj level-key &key object-filter monster-filter &allow-other-keys)
  (:documentation "Registers a level-key in the variant as a later place-hanger for code."))

(defgeneric build-room! (room dungeon player where-x where-y)
  (:documentation "Builds given room in the dungeon at [where-x, where-y]."))
  
(defgeneric find-appropriate-room (variant level player)
  (:documentation "Tries to find an appropriate room-type for given
dungeon."))
 
;;; ===  End level-related

;;; === Store and building-related

(defgeneric build-house! (level house topleft-x topleft-y &key)
  (:documentation "Builds a house on the given level at given coord."))

(defgeneric visit-house (level house)
  (:documentation "Visits a given house.."))

(defgeneric find-owner-for-house (level house &key)
  (:documentation "Tries to find an appropriate owner for the house."))

(defgeneric store-generate-object (variant the-store)
  (:documentation "Returns an object appropriate for the store, no side-effects."))

(defgeneric store-maintenance! (variant the-store)
  (:documentation "Does maintenance on a store, possibly changing it."))

(defgeneric store-mass-produce! (variant store object)
  (:documentation "Possibly mass-produces and alters the object, and may add discount."))
  
;;; === End store


;;; === Item-table related generics


(defgeneric item-table-add!       (table obj &optional key))  
(defgeneric item-table-remove!    (table key &key only-single-items))
(defgeneric item-table-clean!     (table))
(defgeneric item-table-find       (table key))
(defgeneric item-table-sort!      (table sorter))
(defgeneric item-table-iterate!   (table function)
  (:documentation "Function should take three arguments in order:
the table, the key and the object itself."))

(defgeneric item-table-verify-key (table key)
  (:documentation "Returns T when key is OK, and NIL when it is not."))

(defgeneric item-table-print (table &key show-pause start-x start-y &allow-other-keys)
  (:documentation "Returns T when key is OK, and NIL when it is not."))

(defgeneric item-table-more-room? (table &optional obj)
  (:documentation "Returns T if there is room for OBJ, NIL if there is not.
If OBJ is not supplied it checks if there is more room in general.  If OBJ
is supplied, stacking-rules will also be checked."))

(defgeneric get-item-table (dungeon player which-table &key x y)
  (:documentation "Returns item-table or NIL."))

;;; === End item-table

;;; === Miscellaneous

(defgeneric can-creature-drop? (variant creature)
  (:documentation "Checks if the creature can drop anything on death."))

(defgeneric creature-drop! (variant creatue dungeon)
  (:documentation "Makes the dead creature drop items on death."))

(defgeneric shoot-a-missile (dungeon player missile-weapon missile)
  (:documentation "Shoots an arrow, queries for direction."))

(defgeneric melee-hit-creature? (attacker target the-attack)
  (:documentation "will the attacker hit the target?"))

(defgeneric melee-inflict-damage! (attacker target the-attack)
  (:documentation "inflict some damage after a successful hit."))

(defgeneric missile-hit-creature? (attacker target missile-weapon missile)
  (:documentation "Returns T if the missile hit the target, NIL otherwise."))

(defgeneric missile-inflict-damage! (attacker target missile-weapon missile)
  (:documentation "Rolls and applies damage to the target."))

(defgeneric select-item (dungeon player allow-from &key prompt where selection-function)
  (:documentation "Interactive selection of an item."))

(defgeneric trigger-event (obj event arg-list)
  (:documentation "Triggers a given event-type on the object. Recursive."))

(defgeneric register-object-event! (obj event)
  (:documentation "Registers an event on the object."))

(defgeneric location-x (obj)
  (:documentation "Generic function for all things that have a location
in the game at some point."))
  
(defgeneric (setf location-x) (value obj)
  (:documentation "Sets the x-location for the object whenever possible."))
  
(defgeneric location-y (obj)
  (:documentation "Generic function for all things that have a location
in the game at some point."))
  
(defgeneric (setf location-y) (value obj)
  (:documentation "Sets the y-location for the object whenever possible."))
    
(defgeneric use-object! (variant dun pl the-object &key which-use)
  (:documentation "Applies the object on the player in the dungeon."))

(defgeneric get-price (object situation)
  (:documentation "Returns a number with the price for the object in the
given situation."))

(defgeneric get-offer (object situation)
  (:documentation "Returns a number with an offered price for an object
in a certain situation."))

(defgeneric get-attribute (object)
  (:documentation "Returns attribute for a given object."))

(defgeneric lang-equal (first-obj second-obj)
  (:documentation "A recursive check for equality (along the lines of EQUAL)
but one that works with langband-objects."))

(defgeneric get-loadable-form (object &key &allow-other-keys)
  (:documentation "Pretty much similar to MAKE-LOAD-FORM."))

;; overridable player interface
(defgeneric update-xp-table! (variant player)
  (:documentation "Updates the xp-table on the player, and returns updated player."))
(defgeneric update-max-hp! (variant player)
  (:documentation "Updates the max-hp to fit with the hp-table."))


(defgeneric generate-random-name (variant creature race)
  (:documentation "Returns a random name for a given creature of race 'race', or NIL on failure."))

(defgeneric interactive-creation-of-player (variant)
  (:documentation "Interactive creation of a player object.  Should return a
player object or NIL."))

(defgeneric drop-near-location! (variant dungeon object x y)
  (:documentation "Tries to drop an object at given locaton."))

;;; === End misc


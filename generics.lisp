#|

DESC: generics.lisp - the generic function interfaces
Copyright (c) 2001-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

;;; === Basic generics

(defgeneric activate-object (obj &key &allow-other-keys)
  (:documentation "Most objects in Langband is created lazily.
This means that an object may be created but may not be fully initialised
and filled with appropriate values right away.  The normal CL/CLOS mechanisms
deal with the actual creation of the bare object, but non-trivial objects
should also be \"activated\", ie get proper values on all variables.
The object in question must be returned, failure to do so may lead to a
situation where the system assumes the object is invalid."))  

(defgeneric ok-object? (obj &key warn-on-failure context)
  (:documentation "Checks to make sure the object is ok.  Should not halt
the program, just return NIL on failure.  Is allowed to print warnings."))

(defgeneric convert-obj (obj to &key &allow-other-keys)
  (:documentation "Tries to convert the OBJ to the TO form, in pretty
much the same way as COERCE."))

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

(defgeneric lang-equal (first-obj second-obj)
  (:documentation "A recursive check for equality (along the lines of EQUAL)
but one that works with langband-objects."))

(defgeneric get-loadable-form (variant object &key &allow-other-keys)
  (:documentation "Pretty much similar to MAKE-LOAD-FORM."))

;;; === End basic

;;; == Overridable Factories

(defgeneric produce-player-object (variant)
  (:documentation "Produces a player-object."))

(defgeneric produce-object-kind (variant id name &key the-kind)
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

(defgeneric produce-character-class (variant id name &key &allow-other-keys)
  (:documentation "Returns a character-class object."))

(defgeneric produce-character-race (variant id name &key &allow-other-keys)
  (:documentation "Returns a character-class object."))

;;; === End factories


;;; === Saving/loading/Serialising

(defgeneric save-object (variant object stream indent)
  (:documentation "Tries to save object to the stream."))

(defgeneric load-object (variant type stream)
  (:documentation "Tries to load a certain type of object from the stream."))

(defgeneric write-obj-description (variant obj stream &key store numeric-prefix verbosity)
  (:documentation "Describes the given object to the given stream."))

(defgeneric do-save (variant fname obj-list style)
  (:documentation "Tries to save the object-list to the given filename.  STYLE
specifies what kind of saving should be done (e.g readable, binary, ..)"))

(defgeneric load-a-saved-game (variant fname style)
  (:documentation "Tries to load a saved-game from the filename.
If variant is NIL the default loader will be used."))

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

(defgeneric get-gender (var-obj key)
  (:documentation "Tries to find a gender that matches KEY and returns it.
Returns NIL on failure."))

(defgeneric variant-home-path (variant)
  (:documentation "Returns the path to the given variant, possibly also for the
current version."))
;;; ===


;;; Creature (monster/player/...) related generics

(defgeneric calculate-creature-bonuses! (variant creature)
  (:documentation "Does a full walk-through of the creature and updates any and all bonuses."))
(defgeneric calculate-creature-light-radius! (variant creature)
  (:documentation "Does a walk-through of the creature and updates the light-radius."))
(defgeneric calculate-creature-hit-points! (variant creature)
  (:documentation "Does a walk-through of the creature and recalculates hit-points."))
(defgeneric calculate-creature-mana! (variant creature)
  (:documentation "Does a walk-through of the creature and recalculates mana."))


(defgeneric display-creature (variant creature &key mode)
  (:documentation "Displays the creature to the UI."))

(defgeneric heal-creature! (creature amount)
  (:documentation "Tries to heal the creature with a certain amount of hits."))

(defgeneric get-creature-state (creature state)
  (:documentation "Returns the value of the named state for the given creature."))

;; must be implemented by a variant
(defgeneric modify-creature-state! (creature state &key new-value add subtract)
  (:documentation "Modifies the creature-state appropriately."))


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


(defgeneric creature-alive? (creature)
  (:documentation "Returns T if the creature is alive, NIL if not."))

(defgeneric (setf creature-alive?) (value creature)
  (:documentation "Alters the creature's 'alive'-status."))

(defgeneric get-xp-value (creature)
  (:documentation "Returns a positive integer with the xp-value for the creature."))

(defgeneric alter-xp! (creature amount)
  (:documentation "Alters the xp of the creature by AMOUNT."))

(defgeneric appears-in-group? (variant level monster)
  (:documentation "Returns T if the particular monster should appear in a group."))

;;; === End creature


;;; === Physical object-related generics

(defgeneric possible-identify! (player object)
  (:documentation "Checks if the player identifies the given object,
and if so, marks the object."))

(defgeneric apply-magic! (variant obj base-level &key good-p great-p allow-artifact)
  (:documentation "Applies certain magic to a bare item."))

(defgeneric add-magic-to-item! (variant item depth quality)
  (:documentation "Adds magical properties to an item."))

(defgeneric is-object-known? (object)
  (:documentation "Returns T if the particular object is known.  NIL if not."))

(defgeneric learn-about-object! (player object what)
  (:documentation "Lets the player learn certain things about an object."))

(defgeneric is-eatable? (player object)
  (:documentation "Is the object OBJ eatable by the player?"))

(defgeneric apply-usual-effects-on-used-object! (dungeon player obj)
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

(defgeneric print-mana-points (variant creature setting)
  (:documentation "prints mana points according to setting."))

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
  (:documentation "Tries to find an appropriate room-type for given dungeon."))
 
;;; ===  End level-related

;;; === Store and building-related

(defgeneric build-house! (level house topleft-x topleft-y &key)
  (:documentation "Builds a house on the given level at given coord."))

(defgeneric visit-house (level house)
  (:documentation "Visits a given house.."))

(defgeneric find-owner-for-house (level house &key var-obj selection)
  (:documentation "Tries to find an appropriate owner for the house.
:var-obj may give a variant-object to the method, and :selection may have a keyword
value specifying how to select the owner, e.g :random "))

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

(defgeneric item-table-print (table &key show-pause start-x start-y print-selection &allow-other-keys)
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

(defgeneric use-object! (variant dungeon player the-object &key which-use)
  (:documentation "Applies the object on the player in the dungeon."))

(defgeneric get-price (object situation)
  (:documentation "Returns a number with the price for the object in the
given situation."))

(defgeneric get-offer (object situation)
  (:documentation "Returns a number with an offered price for an object
in a certain situation."))

(defgeneric get-colour (object)
  (:documentation "Returns the colour/attr for a given object."))

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

(defgeneric is-creatable? (variant kind)
  (:documentation "Is the kind creatable?"))

(defgeneric get-mkind-table (variant level)
  (:documentation "Returns appropriate monster-kind table for variant and level."))

(defgeneric kill-target! (dungeon attacker target x y)
  (:documentation "Kills the target in an appropriate and legal way."))

(defgeneric copy-player-abilities (variant player-abilities)
  (:documentation "Copies and returns a player-ability object."))

(defgeneric allocate-monster! (variant dungeon player distance sleeping)
  (:documentation "Allocates a monster in the given dungeon."))

(defgeneric cmb-describe-miss (attacker target)
  (:documentation "Describe a miss as a message."))

(defgeneric roll-up-character! (variant player)
  (:documentation "Rolls up a player-object and modifies it appropriately."))

(defgeneric query-for-character-basics! (variant the-player)
  (:documentation "Does queries for basic-info (race, class, ..) and alters the player-object."))

(defgeneric print-tomb (variant player)
  (:documentation "Prints a tomb for the given (dead) player."))

(defgeneric organise-death& (variant player)
  (:documentation "Organises complete funeral for the dead player."))

(defgeneric create-gold (variant dungeon)
  (:documentation "Creates gold in the dungeon."))

(defgeneric place-gold! (variant dungeon x y)
  (:documentation "Places some gold in the dungeon at x,y."))

(defgeneric create-object (variant dungeon good-p great-p)
  (:documentation "Creates an object and puts it in the dungeon."))

(defgeneric place-object! (variant dungeon x y good-p great-p)
  (:documentation "Place an object at the given coordinate."))

(defgeneric allocate-object! (variant dungeon set type number)
  (:documentation "Allocates an object in the dungeon?"))

(defgeneric has-ability? (something ability)
  (:documentation "Shecks if SOMETHING has the given ability."))

(defgeneric get-creature-name (creature)
  (:documentation "Returns a string with the name of the creature."))

(defgeneric get-monster-kind (variant id)
  (:documentation "Returns a monster-kind with given id for given variant."))

(defgeneric get-monster-list (variant &key sort-key)
  (:documentation "Returns a list of monsters for the given variant."))

(defgeneric get-mkind-alloc-table (variant level)
  (:documentation "Returns an allocation table for monster-kinds for
the given variant and given level."))

(defgeneric fill-player-abilities! (variant target source)
  (:documentation "Fills the target with values from source."))

(defgeneric get-okind-table (variant level)
  (:documentation "Returns the object-kind table for the given variant and level."))

(defgeneric get-okind-alloc-table (variant level)
  (:documentation "Returns the object-kind allocation table for the given variant and level."))

(defgeneric get-object-kind (variant id)
  (:documentation "Returns an object-kind with given id for given variant."))

(defgeneric get-setting (variant key)
  (:documentation "Gets a setting from the variant identified by the KEY."))

(defgeneric (setf get-setting) (value variant key)
  (:documentation "Sets the setting identified by KEY for the VARIANT to VALUE."))

(defgeneric place-monster! (variant dungeon player x y sleeping group)
  (:documentation "Places a monster on the given coordinate."))

(defgeneric cmb-describe-hit (attacker target the-attack)
  (:documentation "Describe a hit from ATTACKER on TARGET."))

(defgeneric cmb-describe-death (attacker target)
  (:documentation "Describe death-blow."))

(defgeneric equip-character! (variant player settings)
  (:documentation "Equips character according to settings and variant."))

(defgeneric copy-game-values (variant game-values-obj)
  (:documentation "Copies the game-values-object and returns a new object."))

(defgeneric produce-game-values-object (variant)
  (:documentation "Returns a new game-values objedct for the variant."))

(defgeneric produce-skills-object (variant &key default-value)
  (:documentation "Returns a skills-object for the given variant."))

(defgeneric build-skills-obj-from-list (variant skill-list)
  (:documentation "Returns a skill-object from a list of skill-info."))
  
(defgeneric get-skill-translation (variant key)
  (:documentation "Returns a skill-translation for the given KEY, I think."))

(defgeneric register-skill-translation& (variant translation)
  (:documentation "Registers a skill-translation with the variant."))

(defgeneric create-alloc-table-objects (variant obj-table)
  (:documentation "Returns an allocation table for the given object-table."))

(defgeneric create-alloc-table-monsters (variant mon-table)
  (:documentation "Returns an allocation table for the given monster-table."))

(defgeneric get-high-scores (variant fname)
  (:documentation "Returns a sorted list of high-scores."))

(defgeneric display-high-scores (variant highscore-list &key current use-term)
  (:documentation "Displays high-scores in some way."))

(defgeneric save-high-score& (variant hs fname)
  (:documentation "Saves a given high-score entry to the given high-score file."))

(defgeneric update-monster! (variant active-monster full-update?)
  (:documentation "Update monster in various ways, mostly in relation to view."))

(defgeneric update-monsters! (variant dungeon full-update?)
  (:documentation "Update all monsters in dungeon in various ways, mostly in relation to view."))

(defgeneric make-stat-array (variant)
  (:documentation "Returns an array suitable for holding all the stats."))

(defgeneric is-stat-array? (variant obj)
  (:documentation "Returns T if the object is a stat-array."))

(defgeneric do-projection (source target-x target-y flag &key effect damage radius range)
  (:documentation "Does a general projection."))

(defgeneric apply-projection-effect! (variant source path-array
					     &key explosion-area flag 
					     distance damage effect)
  (:documentation "Tries to apply projection effect to the path-array and possibly to explosion-area."))

(defgeneric apply-projection-effect-to-target! (variant source target
							&key x y damage effect distance)
  (:documentation "Applies a projection effect to a specific target."))

(defgeneric damaged-by-element? (variant target element)
  (:documentation "Returns T if the target can be damaged, NIL if it immune/ignoring
it, and :vulnerable if it is especially vulnerable."))

(defgeneric deliver-damage! (variant source target amount &key note dying-note)
  (:documentation "Delivers AMOUNT damage to the TARGET, and prints either a note or a dying-note."))


;;; === End misc

;;; === Player-protocol

(defgeneric reset-player-object! (variant player)
  (:documentation "Resets all values on the player-object."))

(defgeneric calculate-abilities! (variant player object)
  (:documentation "Calculates abilities on the player based on given object (race, class, ...)"))

(defgeneric get-weight (object)
  (:documentation "Returns a fixnum with the total weight of the given object (creature, backpack, ...)."))

(defgeneric get-old-player-info (variant player &key reuse-object)
  (:documentation "Gets an object with info on the player that can later be used to track
changes to important parts of a player-object, e.g for updating right places of the screen.
The :REUSE-OBJECT keyword gives an object that can be reused for values, alleviating the
need to cons up a new object."))

(defgeneric handle-player-updates! (variant player old-info)
  (:documentation "Updates screens, windows, ... after a recalculation of the player.
It is passed the object returned by GET-OLD-PLAYER-INFO at start of recalculation."))

;;; === End player-protocol

(defgeneric place-rubble! (variant dungeon x y)
  (:documentation "Places rubble at the given coord."))

(defgeneric find-random-trap (variant dungeon x y)
  (:documentation "Finds a random trap and returns it initialised."))

(defgeneric place-trap! (variant dungeon x y)
  (:documentation "Places a trap at the given coord."))

(defgeneric deliver-elemental-damage! (variant source target element damage)
  (:documentation "Gives out decent elemental damage to a target."))

(defgeneric get-object-effect (variant the-object effect)
  (:documentation "Returns the wanted effect (or NIL) from the object."))

(defgeneric initialise-object-kind! (variant object keyword-arguments)
  (:documentation "Initialises an object-kind object with given keyword-arguments.
The method is responsible for checking legality of args.  The method should return
the object."))

;; possibly change name later
(defgeneric process-world& (variant dungeon player)
  (:documentation "Every tenth turn important calculations might need to be done.
These calculations are typically those that needs to be done fairly frequently,
but does not need to be 100% updated always."))


(defgeneric print-speed (variant player setting)
  (:documentation "prints speed-info."))

(defgeneric print-state (variant player setting)
  (:documentation "prints state-info."))

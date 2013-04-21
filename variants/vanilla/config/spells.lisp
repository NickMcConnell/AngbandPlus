;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/spells.lisp - definition of spells and spellbooks
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

;; beam-chances are wrong, needs revising later

;;; === Mage/ranger/rogue spells

(define-spell "Magic Missile"  "magic-missile"
  :effect-type "magic-missile"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.level player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-bolt-or-beam! player (- plvl 10) dir (get-spell-effect '<magic-missile>)
					(roll-dice (+ 3 (int-/ (1- plvl) 5)) 4)
					:projected-object spell)
		))))


(define-spell "Detect Monsters" "mage-detect-monsters")
(define-spell "Phase Door" "phase-door"
  :effect-type '<teleport>
  :effect (spell-effect (dungeon player spell)
	       (teleport-creature! dungeon player player 10)))

(define-spell "Light Area" "light-area"
  :effect-type '<light>
  :effect (spell-effect (dungeon player spell)
	      (let ((plvl (player.level player)))
		(light-area! dungeon player (roll-dice 2 (int-/ plvl 2))
			     (1+ (int-/ plvl 10)))
		)))
						   

(define-spell "Treasure Detection" "treasure-detection")
(define-spell "Cure Light Wounds" "mage-cure-light-wounds"
  :effect-type '<healing>
  :effect (spell-effect (dungeon player spell)
	    (let ((amount (roll-dice 2 8)))
		(heal-creature! player amount)
		(modify-creature-state! player '<cut> :subtract '<light>)
		)))
		
(define-spell "Object Detection" "object-detection")
(define-spell "Find Hidden Traps/Doors" "find-traps/doors")
(define-spell "Stinking Cloud" "stinking-cloud"
  :effect-type "poison"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.level player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<poison>) (+ 10 (int-/ plvl 2)) 2
				:projected-object spell)
		))))


(define-spell "Confuse Monster" "confuse-monster")
(define-spell "Lightning Bolt" "lightning-bolt"
  :effect-type "electricity"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.level player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-bolt-or-beam! player (- plvl 10) dir (get-spell-effect '<electricity>)
					(roll-dice (+ 3 (int-/ (- plvl 5) 4)) 8)
					:projected-object spell)
		))))

(define-spell "Trap/Door Destruction" "trap/door-destruction")
(define-spell "Sleep I" "sleep-1")
(define-spell "Cure Poison" "cure-poison"
  :effect-type '<healing>
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<poisoned> :new-value nil)))

(define-spell "Teleport Self" "mage-teleport-self"
  :effect-type '<teleport>
  :effect (spell-effect (dungeon player spell)
	    (teleport-creature! dungeon player player (* (player.level player) 5))
	    ))

(define-spell "Spear of Light" "spear-of-light"
  :effect-type '<light>
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.level player)))
	      ;; fix later
	      (declare (ignore plvl))
		(when-bind (dir (get-aim-direction))
		  (print-message! "A line of blue shimmering light appears.")
		  ;; add light-line call

		  ))))

(define-spell "Frost Bolt" "frost-bolt"
  :effect-type "cold"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.level player)))
		(when-bind (dir (get-aim-direction))
		  (van-fire-bolt-or-beam! player (- plvl 10) dir (get-spell-effect '<cold>)
					  (roll-dice (+ 5 (int-/ (- plvl 5) 4)) 8))
		  ))))

(define-spell "Turn Stone to Mud" "stone-to-mud")


(define-spell "Satisfy Hunger" "mage-satisfy-hunger"
  :effect-type '<healing>
  :effect (spell-effect (dungeon player spell)
	    (alter-food! player (1- +food-max+))))
(define-spell "Recharge Item I" "recharge-item-1")
(define-spell "Sleep II" "sleep-2")
(define-spell "Polymorph Other" "polymorph-other")
(define-spell "Identify" "identify"
  :effect-type '<divination>
  :effect (spell-effect (dungeon player spell)
	    (interactive-identify-object! dungeon player :type '<normal)))

(define-spell "Sleep III" "sleep-3")
(define-spell "Fire Bolt" "fire-bolt"
  :effect-type "fire"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.level player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-bolt-or-beam! player (- plvl 10) dir (get-spell-effect '<fire>)
					(roll-dice (+ 8 (int-/ (- plvl 5) 4)) 8))
		))))

(define-spell "Slow Monster" "slow-monster")


(define-spell "Frost Ball" "frost-ball"
  :effect-type "cold"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.level player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<frost>) (+ 30 plvl) 2)
		))))

(define-spell "Recharge Item II" "recharge-item-2")
(define-spell "Teleport Other" "mage-teleport-other")
(define-spell "Haste Self" "haste-self"
  :effect-type '<enhance>
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<hasted> :add (+ (player.level player) (random 20)))))
	    
(define-spell "Fire Ball" "fire-ball"
  :effect-type "fire"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.level player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<fire>) (+ 55 plvl) 2)
		))))

(define-spell "Word of Destruction" "mage-word-of-destruction")
(define-spell "Xenocide" "xenocide-1")


(define-spell "Door Creation" "door-creation")
(define-spell "Stair Creation" "stair-creation")
(define-spell "Teleport Level" "mage-teleport-level")
(define-spell "Earthquake" "mage-earthquake")
(define-spell "Word of Recall" "mage-word-of-recall")


(define-spell "Acid Bolt" "acid-bolt"
  :effect-type "acid"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.level player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-bolt-or-beam! player plvl dir (get-spell-effect '<acid>)
					(roll-dice (+ 6 (int-/ (- plvl 5) 4)) 8))
		))))

(define-spell "Cloud Kill" "cloud-kill"
  :effect-type "poison"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.level player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<poison>) (+ 20 (int-/ plvl 2)) 3)
		))))

(define-spell "Acid Ball" "acid-ball"
  :effect-type "acid"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.level player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<acid>) (+ 40 plvl) 2)
		))))

(define-spell "Ice Storm" "ice-storm"
  :effect-type "cold"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.level player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<cold>) (+ 70 plvl) 3)
		))))
  
(define-spell "Meteor Swarm" "meteor-swarm"
  :effect-type '<meteor>
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.level player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<meteor>) (+ 65 plvl) 3)
		))))

(define-spell "Mana Storm" "mana-storm"
  :effect-type '<mana>
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.level player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<mana>) (+ 300 (* plvl 2)) 3)
		))))



(define-spell "Detect Evil" "mage-detect-evil")
(define-spell "Detect Enchantment" "detect-enchantment")
(define-spell "Recharge Item III" "recharge-item-3")
(define-spell "Xenocide" "xenocide-2")
(define-spell "Mass Xenocide" "mass-xenocide")


(define-spell "Resist Fire" "resist-fire"
  :effect-type '<enhance>
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<resist-fire> :add (+ 20 (random 20)))))

(define-spell "Resist Cold" "resist-cold"
  :effect-type '<enhance>
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<resist-cold> :add (+ 20 (random 20)))))

(define-spell "Resist Acid" "resist-acid"
  :effect-type '<enhance>
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<resist-acid> :add (+ 20 (random 20)))))

(define-spell "Resist Poison" "resist-poison"
  :effect-type '<enhance>
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<resist-poison> :add (+ 20 (random 20)))))

(define-spell "Resistance" "resistance"
  :effect-type '<enhance>
  :effect (spell-effect (dungeon player spell)
	    (let ((time (+ 20 (randint 20))))
	      (modify-creature-state! player '<resist-fire> :add time)
	      (modify-creature-state! player '<resist-cold> :add time)
	      (modify-creature-state! player '<resist-acid> :add time)
	      (modify-creature-state! player '<resist-elec> :add time)
	      (modify-creature-state! player '<resist-poison> :add time)
	      )))
		



(define-spell "Heroism" "heroism"
  :effect-type '<enhance>
  :effect (spell-effect (dungeon player spell)
	    (heal-creature! player 10)
	    (modify-creature-state! player '<fear> :new-value nil)
	    (modify-creature-state! player '<heroic> :add (+ 25 (random 25)))
	    ))


(define-spell "Shield" "shield"
  :effect-type '<enhance>
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<shielded> :add (+ 30 (random 20)))))

(define-spell "Berserker" "berserker"
  :effect-type '<enhance>
  :effect (spell-effect (dungeon player spell)
	    (heal-creature! player 30)
	    (modify-creature-state! player '<fear> :new-value nil)
	    (modify-creature-state! player '<berserk> :add (+ 25 (random 25)))
	    ))

(define-spell "Essence of Speed" "essence-of-speed"
  :effect-type '<enhance>
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<hasted> :add (+ 30 (player.level player) (random 30)))
	    ))

(define-spell "Globe of Invulnerability" "globe-of-invulnerability"
  :effect-type '<enhance>
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<invulnerable> :add (+ 8 (randint 8)))
	    ))

;;; === Priest/paladin spells

(define-spell "Detect Evil" "priest-detect-evil")
(define-spell "Cure Light Wounds" "priest-cure-light-wounds"
  :effect-type '<healing>
  :effect (spell-effect (dungeon player spell)
	    (let ((amount (roll-dice 2 10)))
	      (heal-creature! player amount)
	      (modify-creature-state! player '<cut> :subtract '<light>) ;; fix
	      )))

(define-spell "Bless" "bless"
  :effect-type '<enhance>
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<blessed> :add (+ 12 (random 12)))
	    ))

(define-spell "Remove Fear" "remove-fear"
  :effect-type '<healing>
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<fear> :new-value nil)))

(define-spell "Call Light" "call-light"
  :effect-type '<light>
  :effect (spell-effect (dungeon player spell)
	      (let ((plvl (player.level player)))
		(light-area! dungeon player (roll-dice 2 (int-/ plvl 2))
			     (1+ (int-/ plvl 10)))
		)))

(define-spell "Find Traps" "find-traps")

(define-spell "Detect Doors/Stairs" "detect-doors/stairs")

(define-spell "Slow Poison" "slow-poison"
  :effect-type '<healing>
  :effect (spell-effect (dungeon player spell)
	    ;; FIX!
	    (modify-creature-state! player '<poisoned> :subtract '<half>)
	    ))



(define-spell "Scare Monster" "scare-monster")
(define-spell "Portal" "portal"
  :effect-type '<teleport>
  :effect (spell-effect (dungeon player spell)
	      (teleport-creature! dungeon player player (* (player.level player) 3))
	      ))

(define-spell "Cure Serious Wounds" "cure-serious-wounds"
  :effect-type '<healing>
  :effect (spell-effect (dungeon player spell)
	      (heal-creature! player (roll-dice 4 10))
	      (modify-creature-state! player '<cut> :subtract '<serious>)
	      ))

(define-spell "Chant" "chant"
  :effect-type '<enhance>
   :effect (spell-effect (dungeon player spell)
	     (modify-creature-state! player '<blessed> :add (+ 24 (random 24)))
	    ))

(define-spell "Sanctuary" "sanctuary")
(define-spell "Satisfy Hunger" "priest-satisfy-hunger"
  :effect-type '<healing>
  :effect (spell-effect (dungeon player spell)
	    (alter-food! player (1- +food-max+))))

(define-spell "Remove Curse" "remove-curse")

(define-spell "Resist Heat and Cold" "resist-heat-and-cold"
  :effect-type '<enhance>
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<resist-fire> :add (+ 10 (random 10)))
	    (modify-creature-state! player '<resist-cold> :add (+ 10 (random 10)))
	    ))


(define-spell "Neutralize Poison" "neutralize-poison"
  :effect-type '<healing>
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<poisoned> :new-value nil)))

(define-spell "Orb of Draining" "orb-of-draining")

(define-spell "Cure Critical Wounds" "cure-critical-wounds"
  :effect-type '<healing>
  :effect (spell-effect (dungeon player spell)
	       (heal-creature! player (roll-dice 6 10))
	       (modify-creature-state! player '<cut> :new-value nil)))

(define-spell "Sense Invisible" "sense-invisible"
  :effect-type '<enhance>
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<see-invisible> :add (+ 24 (random 24)))
	    ))

(define-spell "Protection from Evil" "protection-from-evil"
  :effect-type '<enhance>
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<prot-from-evil> :add (+ (* 3 (player.level player)) (random 25)))
	    ))

(define-spell "Earthquake" "priest-earthquake")

(define-spell "Sense Surroundings" "sense-surroundings")

(define-spell "Cure Mortal Wounds" "cure-mortal-wounds"
  :effect-type '<healing>
  :effect (spell-effect (dungeon player spell)
	    (heal-creature! player (roll-dice 6 10))
	    (modify-creature-state! player '<stun> :new-value nil)
	    (modify-creature-state! player '<cut>  :new-value nil)))

(define-spell "Turn Undead" "turn-undead")


(define-spell "Prayer" "prayer"
  :effect-type '<healing>
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<blessed> :add (+ 48 (random 48)))
	    ))

(define-spell "Dispel Undead" "dispel-undead")

(define-spell "Heal" "heal"
  :effect-type '<healing>
  :effect (spell-effect (dungeon player spell)
	    (heal-creature! player 300)
	    (modify-creature-state! player '<stun> :new-value nil)
	    (modify-creature-state! player '<cut>  :new-value nil)
	    ))

(define-spell "Dispel Evil" "dispel-evil")
(define-spell "Glyph of Warding" "glyph-of-warding")
(define-spell "Holy Word" "holy-word")


(define-spell "Detect Monsters" "priest-detect-monsters")
(define-spell "Detection" "detection")
(define-spell "Perception" "perception"
  :effect-type '<divination>
  :effect (spell-effect (dungeon player spell)
	    (interactive-identify-object! dungeon player :type '<normal)))

(define-spell "Probing" "probing")
(define-spell "Clairvoyance" "clairvoyance")


(define-spell "Cure Serious Wounds" "cure-serious-wounds-2"
  :effect-type '<healing>
  :effect (spell-effect (dungeon player spell)
	    (heal-creature! player (roll-dice 4 10))
	    (modify-creature-state! player '<cut> :new-value nil)
	    ))

(define-spell "Cure Mortal Wounds" "cure-mortal-wounds-2"
  :effect-type '<healing>
  :effect (spell-effect (dungeon player spell)
	    (heal-creature! player (roll-dice 8 10))
	    (modify-creature-state! player '<stun> :new-value nil)
	    (modify-creature-state! player '<cut>  :new-value nil)))

(define-spell "Healing" "healing"
  :effect-type '<healing>
  :effect (spell-effect (dungeon player spell)
	    (heal-creature! player 2000)
	    (modify-creature-state! player '<stun> :new-value nil)
	    (modify-creature-state! player '<cut> :new-value nil)))

(define-spell "Restoration" "restoration"
  :effect-type '<healing>
  :effect (spell-effect (dungeon player spell)
	    (update-player-stat! player '<str> '<restore>)
	    (update-player-stat! player '<dex> '<restore>)
	    (update-player-stat! player '<con> '<restore>)
	    (update-player-stat! player '<int> '<restore>)
	    (update-player-stat! player '<wis> '<restore>)
	    (update-player-stat! player '<chr> '<restore>)
	    ))

(define-spell "Remembrance" "remembrance")


(define-spell "Dispel Undead" "dispel-undead-2")
(define-spell "Dispel Evil" "dispel-evil-2")
(define-spell "Banishment" "banishment")
(define-spell "Word of Destruction" "priest-word-of-destruction")
(define-spell "Annihilation" "annihilation")

(define-spell "Unbarring Ways" "unbarring-ways")
(define-spell "Recharging" "recharging")
(define-spell "Dispel Curse" "dispel-curse")
(define-spell "Enchant Weapon" "enchant-weapon")
(define-spell "Enchant Armour" "enchant-armour")
(define-spell "Elemental Brand" "elemental-brand")

(define-spell "Blink" "blink"
  :effect-type '<teleport>
  :effect (spell-effect (dungeon player spell)
	      (teleport-creature! dungeon player player 10)
	      ))

(define-spell "Teleport Self" "priest-teleport-self"
  :effect-type '<teleport>
  :effect (spell-effect (dungeon player spell)
	    (teleport-creature! dungeon player player (* (player.level player) 8))
	    ))

(define-spell "Teleport Other" "priest-teleport-other")
(define-spell "Teleport Level" "priest-teleport-level")
(define-spell "Word of Recall" "priest-word-of-recall")
(define-spell "Alter Reality" "alter-reality")


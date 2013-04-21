;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/spells.lisp - definition of spells and spellbooks
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

;; spells are 2.9.3 version
;; beam-chances are wrong, needs revising later

;;; === Mage/ranger/rogue spells

(define-spell "Magic Missile"  "magic-missile"
  :numeric-id 0
  :effect-type "magic-missile"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.power-lvl player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-bolt-or-beam! player (- plvl 10) dir (get-spell-effect '<magic-missile>)
					(roll-dice (+ 3 (int-/ (1- plvl) 5)) 4)
					:projected-object spell)
		))))


(define-spell "Detect Monsters" "mage-detect-monsters"
  :numeric-id 1
  :effect-type "divination"
  :effect (spell-effect (dungeon player spell)
	    (detect-monsters! dungeon player spell)))
	    
(define-spell "Phase Door" "phase-door"
  :numeric-id 2
  :effect-type "teleport"
  :effect (spell-effect (dungeon player spell)
	       (teleport-creature! dungeon player player 10)))

(define-spell "Light Area" "light-area"
  :numeric-id 3
  :effect-type "light"
  :effect (spell-effect (dungeon player spell)
	      (let ((plvl (player.power-lvl player)))
		(light-area! dungeon player (location-x player)
			     (location-y player)
			     (roll-dice 2 (int-/ plvl 2))
			     (1+ (int-/ plvl 10))
			     :projected-object spell
			     :type '<light>)
		)))
						   

(define-spell "Treasure Detection" "treasure-detection"
  :numeric-id 4
  :effect-type "divination"
  :effect (spell-effect (dungeon player spell)
	    (detect-gold! dungeon player spell)
	    ))

(define-spell "Cure Light Wounds" "mage-cure-light-wounds"
  :numeric-id 5
  :effect-type "healing"
  :effect (spell-effect (dungeon player spell)
	    (let ((amount (roll-dice 2 8)))
		(heal-creature! player amount)
		(modify-creature-state! player '<cut> :subtract '<light>)
		)))
		
(define-spell "Object Detection" "object-detection"
  :numeric-id 6
  :effect-type "divination"
  :effect (spell-effect (dungeon player spell)
	    (detect-normal-objects! dungeon player spell)
	    ))


(define-spell "Find Hidden Traps/Doors" "find-traps/doors"
  :numeric-id 7
  :effect-type "divination"
  :effect (spell-effect (dungeon player spell)
	    (detect-traps! dungeon player spell)
	    (detect-doors! dungeon player spell)
	    ))


(define-spell "Stinking Cloud" "stinking-cloud"
  :numeric-id 8
  :effect-type "poison"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.power-lvl player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<poison>) (+ 10 (int-/ plvl 2)) 2
				:projected-object spell)
		))))


(define-spell "Confuse Monster" "confuse-monster"
  :numeric-id 9)

(define-spell "Lightning Bolt" "lightning-bolt"
  :numeric-id 10
  :effect-type "electricity"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.power-lvl player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-bolt-or-beam! player (- plvl 10) dir (get-spell-effect '<electricity>)
					(roll-dice (+ 3 (int-/ (- plvl 5) 4)) 8)
					:projected-object spell)
		))))

(define-spell "Trap/Door Destruction" "trap/door-destruction"
  :numeric-id 11)

(define-spell "Sleep I" "sleep-1"
  :numeric-id 12)

(define-spell "Cure Poison" "cure-poison"
  :numeric-id 13
  :effect-type "healing"
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<poisoned> :new-value nil)))

(define-spell "Teleport Self" "mage-teleport-self"
  :numeric-id 14
  :effect-type "teleport"
  :effect (spell-effect (dungeon player spell)
	    (teleport-creature! dungeon player player (* (player.power-lvl player) 5))
	    ))

(define-spell "Spear of Light" "spear-of-light"
  :numeric-id 15
  :effect-type "light"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.power-lvl player)))
	      ;; fix later
	      (declare (ignore plvl))
		(when-bind (dir (get-aim-direction))
		  (print-message! "A line of blue shimmering light appears.")
		  ;; add light-line call

		  ))))

(define-spell "Frost Bolt" "frost-bolt"
  :numeric-id 16
  :effect-type "cold"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.power-lvl player)))
		(when-bind (dir (get-aim-direction))
		  (van-fire-bolt-or-beam! player (- plvl 10) dir (get-spell-effect '<cold>)
					  (roll-dice (+ 5 (int-/ (- plvl 5) 4)) 8))
		  ))))

(define-spell "Turn Stone to Mud" "stone-to-mud"
  :numeric-id 17)


(define-spell "Satisfy Hunger" "mage-satisfy-hunger"
  :numeric-id 18
  :effect-type "healing"
  :effect (spell-effect (dungeon player spell)
	    (let ((curamount (player.satiation player)))
	      (when (< curamount +food-max+)
		(modify-satiation! player (- +food-max+ curamount 1))))))


(define-spell "Recharge Item I" "recharge-item-1"
  :numeric-id 19)

(define-spell "Sleep II" "sleep-2"
  :numeric-id 20)

(define-spell "Polymorph Other" "polymorph-other"
  :numeric-id 21)

(define-spell "Identify" "identify"
  :numeric-id 22
  :effect-type "divination"
  :effect (spell-effect (dungeon player spell)
	    (interactive-identify-object! dungeon player :type '<normal)))

(define-spell "Sleep III" "sleep-3"
  :numeric-id 23)

(define-spell "Fire Bolt" "fire-bolt"
  :numeric-id 24
  :effect-type "fire"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.power-lvl player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-bolt-or-beam! player (- plvl 10) dir (get-spell-effect '<fire>)
					(roll-dice (+ 8 (int-/ (- plvl 5) 4)) 8))
		))))

(define-spell "Slow Monster" "slow-monster"
  :numeric-id 25)


(define-spell "Frost Ball" "frost-ball"
  :numeric-id 26
  :effect-type "cold"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.power-lvl player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<frost>) (+ 30 plvl) 2)
		))))

(define-spell "Recharge Item II" "recharge-item-2"
  :numeric-id 27)

(define-spell "Teleport Other" "mage-teleport-other"
  :numeric-id 28)

(define-spell "Haste Self" "haste-self"
  :numeric-id 29
  :effect-type "enhance"
  :effect (spell-effect (dungeon player spell)
	    (haste-creature! player +10 (+ (player.power-lvl player) (random 20)))))
	    
(define-spell "Fire Ball" "fire-ball"
  :numeric-id 30
  :effect-type "fire"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.power-lvl player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<fire>) (+ 55 plvl) 2)
		))))

(define-spell "Word of Destruction" "mage-word-of-destruction"
  :numeric-id 31)

(define-spell "Xenocide" "xenocide-1"
  :numeric-id 32)


(define-spell "Door Creation" "door-creation"
  :numeric-id 33)

(define-spell "Stair Creation" "stair-creation"
  :numeric-id 34)

(define-spell "Teleport Level" "mage-teleport-level"
  :numeric-id 35)

(define-spell "Earthquake" "mage-earthquake"
  :numeric-id 36)

(define-spell "Word of Recall" "mage-word-of-recall"
  :numeric-id 37
  :effect-type "teleport"
  :effect (spell-effect (dungeon player spell)
	    (toggle-word-of-recall! player)))

(define-spell "Acid Bolt" "acid-bolt"
  :numeric-id 38
  :effect-type "acid"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.power-lvl player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-bolt-or-beam! player plvl dir (get-spell-effect '<acid>)
					(roll-dice (+ 6 (int-/ (- plvl 5) 4)) 8))
		))))

(define-spell "Cloud Kill" "cloud-kill"
  :numeric-id 39
  :effect-type "poison"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.power-lvl player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<poison>) (+ 20 (int-/ plvl 2)) 3)
		))))

(define-spell "Acid Ball" "acid-ball"
  :numeric-id 40
  :effect-type "acid"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.power-lvl player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<acid>) (+ 40 plvl) 2)
		))))

(define-spell "Ice Storm" "ice-storm"
  :numeric-id 41
  :effect-type "cold"
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.power-lvl player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<cold>) (+ 70 plvl) 3)
		))))
  
(define-spell "Meteor Swarm" "meteor-swarm"
  :numeric-id 42
  :effect-type '<meteor>
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.power-lvl player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<meteor>) (+ 65 plvl) 3)
		))))

(define-spell "Mana Storm" "mana-storm"
  :numeric-id 43
  :effect-type '<mana>
  :effect (spell-effect (dungeon player spell)
	    (let ((plvl (player.power-lvl player)))
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<mana>) (+ 300 (* plvl 2)) 3)
		))))



(define-spell "Detect Evil" "mage-detect-evil"
  :numeric-id 44
  :effect-type "divination"
  :effect (spell-effect (dungeon player spell)
	    (detect-evil-monsters! dungeon player spell)))

(define-spell "Detect Enchantment" "detect-enchantment"
  :numeric-id 45
  :effect-type "divination"
  :effect (spell-effect (dungeon player spell)
	    (detect-normal-objects! dungeon player spell) ;; should do enchantments!
	    ))

(define-spell "Recharge Item III" "recharge-item-3"
  :numeric-id 46)

(define-spell "Xenocide" "xenocide-2"
  :numeric-id 47)

(define-spell "Mass Xenocide" "mass-xenocide"
  :numeric-id 48)


(define-spell "Resist Fire" "resist-fire"
  :numeric-id 49
  :effect-type "enhance"
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<resist-fire> :add (+ 20 (random 20)))))

(define-spell "Resist Cold" "resist-cold"
  :numeric-id 50
  :effect-type "enhance"
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<resist-cold> :add (+ 20 (random 20)))))

(define-spell "Resist Acid" "resist-acid"
  :numeric-id 51
  :effect-type "enhance"
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<resist-acid> :add (+ 20 (random 20)))))

(define-spell "Resist Poison" "resist-poison"
  :numeric-id 52
  :effect-type "enhance"
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<resist-poison> :add (+ 20 (random 20)))))

(define-spell "Resistance" "resistance"
  :numeric-id 53
  :effect-type "enhance"
  :effect (spell-effect (dungeon player spell)
	    (let ((time (+ 20 (randint 20))))
	      (modify-creature-state! player '<resist-fire> :add time)
	      (modify-creature-state! player '<resist-cold> :add time)
	      (modify-creature-state! player '<resist-acid> :add time)
	      (modify-creature-state! player '<resist-elec> :add time)
	      (modify-creature-state! player '<resist-poison> :add time)
	      )))
		



(define-spell "Heroism" "heroism"
  :numeric-id 54
  :effect-type "enhance"
  :effect (spell-effect (dungeon player spell)
	    (heal-creature! player 10)
	    (modify-creature-state! player '<fear> :new-value nil)
	    (modify-creature-state! player '<heroic> :add (+ 25 (random 25)))
	    ))


(define-spell "Shield" "shield"
  :numeric-id 55
  :effect-type "enhance"
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<shielded> :add (+ 30 (random 20)))))

(define-spell "Berserker" "berserker"
  :numeric-id 56
  :effect-type "enhance"
  :effect (spell-effect (dungeon player spell)
	    (heal-creature! player 30)
	    (modify-creature-state! player '<fear> :new-value nil)
	    (modify-creature-state! player '<berserk> :add (+ 25 (random 25)))
	    ))

(define-spell "Essence of Speed" "essence-of-speed"
  :numeric-id 57
  :effect-type "enhance"
  :effect (spell-effect (dungeon player spell)
	    (haste-creature! player +10 (+ 30 (player.power-lvl player) (random 30)))
	    ))

(define-spell "Globe of Invulnerability" "globe-of-invulnerability"
  :numeric-id 58
  :effect-type "enhance"
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<invulnerable> :add (+ 8 (randint 8)))
	    ))

;;; === Priest/paladin spells

(define-spell "Detect Evil" "priest-detect-evil"
  :numeric-id 100
  :effect-type "divination"
  :effect (spell-effect (dungeon player spell)
	    (detect-evil-monsters! dungeon player spell)))

(define-spell "Cure Light Wounds" "priest-cure-light-wounds"
  :numeric-id 101
  :effect-type "healing"
  :effect (spell-effect (dungeon player spell)
	    (let ((amount (roll-dice 2 10)))
	      (heal-creature! player amount)
	      (modify-creature-state! player '<cut> :subtract '<light>) ;; fix
	      )))

(define-spell "Bless" "bless"
  :numeric-id 102
  :effect-type "enhance"
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<blessed> :add (+ 12 (random 12)))
	    ))

(define-spell "Remove Fear" "remove-fear"
  :numeric-id 103
  :effect-type "healing"
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<fear> :new-value nil)))

(define-spell "Call Light" "call-light"
  :numeric-id 104
  :effect-type "light"
  :effect (spell-effect (dungeon player spell)
	      (let ((plvl (player.power-lvl player)))
		(light-area! dungeon player
			     (location-x player)
			     (location-y player)
			     (roll-dice 2 (int-/ plvl 2))
			     (1+ (int-/ plvl 10))
			     :type '<light>
			     :projected-object spell)
		)))

(define-spell "Find Traps" "find-traps"
  :numeric-id 105
  :effect-type "divination"
  :effect (spell-effect (dungeon player spell)
	    (detect-traps! dungeon player spell)))


(define-spell "Detect Doors/Stairs" "detect-doors/stairs"
  :numeric-id 106
  :effect-type "divination"
  :effect (spell-effect (dungeon player spell)
	    (detect-doors! dungeon player spell)
	    (detect-stairs! dungeon player spell)))


(define-spell "Slow Poison" "slow-poison"
  :numeric-id 107
  :effect-type "healing"
  :effect (spell-effect (dungeon player spell)
	    ;; FIX!
	    (modify-creature-state! player '<poisoned> :subtract '<half>)
	    ))



(define-spell "Scare Monster" "scare-monster"
  :numeric-id 108)

(define-spell "Portal" "portal"
  :numeric-id 109
  :effect-type "teleport"
  :effect (spell-effect (dungeon player spell)
	      (teleport-creature! dungeon player player (* (player.power-lvl player) 3))
	      ))

(define-spell "Cure Serious Wounds" "cure-serious-wounds"
  :numeric-id 110
  :effect-type "healing"
  :effect (spell-effect (dungeon player spell)
	      (heal-creature! player (roll-dice 4 10))
	      (modify-creature-state! player '<cut> :subtract '<serious>)
	      ))

(define-spell "Chant" "chant"
  :numeric-id 111
  :effect-type "enhance"
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<blessed> :add (+ 24 (random 24)))
	    ))

(define-spell "Sanctuary" "sanctuary"
  :numeric-id 112)

(define-spell "Satisfy Hunger" "priest-satisfy-hunger"
  :numeric-id 113
  :effect-type "healing"
  :effect (spell-effect (dungeon player spell)
	    (let ((curamount (player.satiation player)))
	      (when (< curamount +food-max+)
		(modify-satiation! player (- +food-max+ curamount 1))))))

(define-spell "Remove Curse" "remove-curse"
  :numeric-id 114
  :effect-type "enchant"
  :effect (spell-effect (dungeon player spell)
	    (remove-curse! player :light)
	    ))


(define-spell "Resist Heat and Cold" "resist-heat-and-cold"
  :numeric-id 115
  :effect-type "enhance"
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<resist-fire> :add (+ 10 (random 10)))
	    (modify-creature-state! player '<resist-cold> :add (+ 10 (random 10)))
	    ))


(define-spell "Neutralize Poison" "neutralize-poison"
  :numeric-id 116
  :effect-type "healing"
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<poisoned> :new-value nil)))

(define-spell "Orb of Draining" "orb-of-draining"
  :numeric-id 117)

(define-spell "Cure Critical Wounds" "cure-critical-wounds"
  :numeric-id 118
  :effect-type "healing"
  :effect (spell-effect (dungeon player spell)
	       (heal-creature! player (roll-dice 6 10))
	       (modify-creature-state! player '<cut> :new-value nil)))

(define-spell "Sense Invisible" "sense-invisible"
  :numeric-id 119
  :effect-type "enhance"
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<see-invisible> :add (+ 24 (random 24)))
	    ))

(define-spell "Protection from Evil" "protection-from-evil"
  :numeric-id 120
  :effect-type "enhance"
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<prot-from-evil> :add (+ (* 3 (player.power-lvl player)) (random 25)))
	    ))

(define-spell "Earthquake" "priest-earthquake"
  :numeric-id 121)

(define-spell "Sense Surroundings" "sense-surroundings"
  :numeric-id 122)

(define-spell "Cure Mortal Wounds" "cure-mortal-wounds"
  :numeric-id 123
  :effect-type "healing"
  :effect (spell-effect (dungeon player spell)
	    (heal-creature! player (roll-dice 6 10))
	    (modify-creature-state! player '<stun> :new-value nil)
	    (modify-creature-state! player '<cut>  :new-value nil)))

(define-spell "Turn Undead" "turn-undead"
  :numeric-id 124)


(define-spell "Prayer" "prayer"
  :numeric-id 125
  :effect-type "healing"
  :effect (spell-effect (dungeon player spell)
	    (modify-creature-state! player '<blessed> :add (+ 48 (random 48)))
	    ))

(define-spell "Dispel Undead" "dispel-undead"
  :numeric-id 126)

(define-spell "Heal" "heal"
  :numeric-id 127
  :effect-type "healing"
  :effect (spell-effect (dungeon player spell)
	    (heal-creature! player 300)
	    (modify-creature-state! player '<stun> :new-value nil)
	    (modify-creature-state! player '<cut>  :new-value nil)
	    ))

(define-spell "Dispel Evil" "dispel-evil"
  :numeric-id 128)

(define-spell "Glyph of Warding" "glyph-of-warding"
  :numeric-id 129)

(define-spell "Holy Word" "holy-word"
  :numeric-id 130)


(define-spell "Detect Monsters" "priest-detect-monsters"
  :numeric-id 131
  :effect (spell-effect (dungeon player spell)
	    (detect-monsters! dungeon player spell)))


(define-spell "Detection" "detection"
  :numeric-id 132
  :effect-type "divination"
  :effect (spell-effect (dungeon player spell)
	    (detect-all! dungeon player spell)))


(define-spell "Perception" "perception"
  :numeric-id 133
  :effect-type "divination"
  :effect (spell-effect (dungeon player spell)
	    (interactive-identify-object! dungeon player :type '<normal)))

(define-spell "Probing" "probing"
  :numeric-id 134)

(define-spell "Clairvoyance" "clairvoyance"
  :numeric-id 135)


(define-spell "Cure Serious Wounds" "cure-serious-wounds-2"
  :numeric-id 136
  :effect-type "healing"
  :effect (spell-effect (dungeon player spell)
	    (heal-creature! player (roll-dice 4 10))
	    (modify-creature-state! player '<cut> :new-value nil)
	    ))

(define-spell "Cure Mortal Wounds" "cure-mortal-wounds-2"
  :numeric-id 137
  :effect-type "healing"
  :effect (spell-effect (dungeon player spell)
	    (heal-creature! player (roll-dice 8 10))
	    (modify-creature-state! player '<stun> :new-value nil)
	    (modify-creature-state! player '<cut>  :new-value nil)))

(define-spell "Healing" "healing"
  :numeric-id 138
  :effect-type "healing"
  :effect (spell-effect (dungeon player spell)
	    (heal-creature! player 2000)
	    (modify-creature-state! player '<stun> :new-value nil)
	    (modify-creature-state! player '<cut> :new-value nil)))

(define-spell "Restoration" "restoration"
  :numeric-id 139
  :effect-type "healing"
  :effect (spell-effect (dungeon player spell)
	    (update-player-stat! player '<str> '<restore>)
	    (update-player-stat! player '<dex> '<restore>)
	    (update-player-stat! player '<con> '<restore>)
	    (update-player-stat! player '<int> '<restore>)
	    (update-player-stat! player '<wis> '<restore>)
	    (update-player-stat! player '<chr> '<restore>)
	    ))

(define-spell "Remembrance" "remembrance"
  :numeric-id 140)


(define-spell "Dispel Undead" "dispel-undead-2"
  :numeric-id 141)

(define-spell "Dispel Evil" "dispel-evil-2"
  :numeric-id 142)
(define-spell "Banishment" "banishment"
  :numeric-id 143)
(define-spell "Word of Destruction" "priest-word-of-destruction"
  :numeric-id 144)
(define-spell "Annihilation" "annihilation"
  :numeric-id 145)

(define-spell "Unbarring Ways" "unbarring-ways"
  :numeric-id 146)
(define-spell "Recharging" "recharging"
  :numeric-id 147)

(define-spell "Dispel Curse" "dispel-curse"
  :numeric-id 148
  :effect-type "enchant"
  :effect (spell-effect (dungeon player spell)
	    (remove-curse! player :heavy)
	    ))


(define-spell "Enchant Weapon" "enchant-weapon"
  :numeric-id 149)
(define-spell "Enchant Armour" "enchant-armour"
  :numeric-id 150)
(define-spell "Elemental Brand" "elemental-brand"
  :numeric-id 151)

(define-spell "Blink" "blink"
  :numeric-id 152
  :effect-type "teleport"
  :effect (spell-effect (dungeon player spell)
	      (teleport-creature! dungeon player player 10)
	      ))

(define-spell "Teleport Self" "priest-teleport-self"
  :numeric-id 153
  :effect-type "teleport"
  :effect (spell-effect (dungeon player spell)
	    (teleport-creature! dungeon player player (* (player.power-lvl player) 8))
	    ))

(define-spell "Teleport Other" "priest-teleport-other"
  :numeric-id 154)

(define-spell "Teleport Level" "priest-teleport-level"
  :numeric-id 155)

(define-spell "Word of Recall" "priest-word-of-recall"
  :numeric-id 156
  :effect-type "teleport"
  :effect (spell-effect (dungeon player spell)
	    (toggle-word-of-recall! player)))


(define-spell "Alter Reality" "alter-reality"
  :numeric-id 157)

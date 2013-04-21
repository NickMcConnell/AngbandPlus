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
  :effect #'(lambda (dun player)
	      (declare (ignore dun))
	      (let ((plvl (player.level player)))
		(when-bind (dir (%read-direction))
		  (van-fire-bolt-or-beam! player (- plvl 10) dir (get-spell-effect '<magic-missile>)
					  (roll-dice (+ 3 (int-/ (1- plvl) 5)) 4))
		  ))))


(define-spell "Detect Monsters" "mage-detect-monsters")
(define-spell "Phase Door" "phase-door"
  :effect #'(lambda (dun player)
	       (teleport-creature! dun player player 10)))

(define-spell "Light Area" "light-area"
  :effect #'(lambda (dun player)
	      (let ((plvl (player.level player)))
		(light-area! dun player (roll-dice 2 (int-/ plvl 2))
			     (1+ (int-/ plvl 10)))
		)))
						   

(define-spell "Treasure Detection" "treasure-detection")
(define-spell "Cure Light Wounds" "mage-cure-light-wounds"
  :effect #'(lambda (dun player)
	      (declare (ignore dun))
	      (let ((amount (roll-dice 2 8)))
		(heal-creature! player amount)
		(setf (get-creature-state player :heal-cut) '<light>)
		)))
		
(define-spell "Object Detection" "object-detection")
(define-spell "Find Hidden Traps/Doors" "find-traps/doors")
(define-spell "Stinking Cloud" "stinking-cloud"
  :effect #'(lambda (dun player)
	      (declare (ignore dun))
	      (let ((plvl (player.level player)))
		(when-bind (dir (%read-direction))
		  (van-fire-ball! player dir (get-spell-effect '<poison>) (+ 10 (int-/ plvl 2)) 2)
		  ))))


(define-spell "Confuse Monster" "confuse-monster")
(define-spell "Lightning Bolt" "lightning-bolt"
  :effect #'(lambda (dun player)
	      (declare (ignore dun))
	      (let ((plvl (player.level player)))
		(when-bind (dir (%read-direction))
		  (van-fire-bolt-or-beam! player (- plvl 10) dir (get-spell-effect '<electricity>)
					  (roll-dice (+ 3 (int-/ (- plvl 5) 4)) 8))
		  ))))

(define-spell "Trap/Door Destruction" "trap/door-destruction")
(define-spell "Sleep I" "sleep-1")
(define-spell "Cure Poison" "cure-poison"
  :effect #'(lambda (dun player)
	      (declare (ignore dun))
	      (setf (get-creature-state player '<poisoned>) nil)))

(define-spell "Teleport Self" "mage-teleport-self"
  :effect #'(lambda (dun player)
	      (teleport-creature! dun player player (* (player.level player) 5))
	      ))

(define-spell "Spear of Light" "spear-of-light"
  :effect #'(lambda (dun player)
	      (declare (ignore dun))
	      (let ((plvl (player.level player)))
		(when-bind (dir (%read-direction))
		  (print-message! "A line of blue shimmering light appears.")
		  ;; add light-line call

		  ))))

(define-spell "Frost Bolt" "frost-bolt"
  :effect #'(lambda (dun player)
	      (declare (ignore dun))
	      (let ((plvl (player.level player)))
		(when-bind (dir (%read-direction))
		  (van-fire-bolt-or-beam! player (- plvl 10) dir (get-spell-effect '<cold>)
					  (roll-dice (+ 5 (int-/ (- plvl 5) 4)) 8))
		  ))))

(define-spell "Turn Stone to Mud" "stone-to-mud")


(define-spell "Satisfy Hunger" "mage-satisfy-hunger")
(define-spell "Recharge Item I" "recharge-item-1")
(define-spell "Sleep II" "sleep-2")
(define-spell "Polymorph Other" "polymorph-other")
(define-spell "Identify" "identify")
(define-spell "Sleep III" "sleep-3")
(define-spell "Fire Bolt" "fire-bolt"
  :effect #'(lambda (dun player)
	      (declare (ignore dun))
	      (let ((plvl (player.level player)))
		(when-bind (dir (%read-direction))
		  (van-fire-bolt-or-beam! player (- plvl 10) dir (get-spell-effect '<fire>)
					  (roll-dice (+ 8 (int-/ (- plvl 5) 4)) 8))
		  ))))

(define-spell "Slow Monster" "slow-monster")


(define-spell "Frost Ball" "frost-ball"
  :effect #'(lambda (dun player)
	      (declare (ignore dun))
	      (let ((plvl (player.level player)))
		(when-bind (dir (%read-direction))
		  (van-fire-ball! player dir (get-spell-effect '<frost>) (+ 30 plvl) 2)
		  ))))

(define-spell "Recharge Item II" "recharge-item-2")
(define-spell "Teleport Other" "mage-teleport-other")
(define-spell "Haste Self" "haste-self")
(define-spell "Fire Ball" "fire-ball"
  :effect #'(lambda (dun player)
	      (declare (ignore dun))
	      (let ((plvl (player.level player)))
		(when-bind (dir (%read-direction))
		  (van-fire-ball! player dir (get-spell-effect '<fire>) (+ 55 plvl) 2)
		  ))))

(define-spell "Word of Destruction" "mage-word-of-destruction")
(define-spell "Genocide" "genocide-1")


(define-spell "Door Creation" "door-creation")
(define-spell "Stair Creation" "stair-creation")
(define-spell "Teleport Level" "mage-teleport-level")
(define-spell "Earthquake" "mage-earthquake")
(define-spell "Word of Recall" "mage-word-of-recall")


(define-spell "Acid Bolt" "acid-bolt"
  :effect #'(lambda (dun player)
	      (declare (ignore dun))
	      (let ((plvl (player.level player)))
		(when-bind (dir (%read-direction))
		  (van-fire-bolt-or-beam! player plvl dir (get-spell-effect '<acid>)
					  (roll-dice (+ 6 (int-/ (- plvl 5) 4)) 8))
		  ))))

(define-spell "Cloud Kill" "cloud-kill"
  :effect #'(lambda (dun player)
	      (declare (ignore dun))
	      (let ((plvl (player.level player)))
		(when-bind (dir (%read-direction))
		  (van-fire-ball! player dir (get-spell-effect '<poison>) (+ 20 (int-/ plvl 2)) 3)
		  ))))

(define-spell "Acid Ball" "acid-ball"
  :effect #'(lambda (dun player)
	      (declare (ignore dun))
	      (let ((plvl (player.level player)))
		(when-bind (dir (%read-direction))
		  (van-fire-ball! player dir (get-spell-effect '<acid>) (+ 40 plvl) 2)
		  ))))

(define-spell "Ice Storm" "ice-storm"
  :effect #'(lambda (dun player)
	      (declare (ignore dun))
	      (let ((plvl (player.level player)))
		(when-bind (dir (%read-direction))
		  (van-fire-ball! player dir (get-spell-effect '<cold>) (+ 70 plvl) 3)
		  ))))
  
(define-spell "Meteor Swarm" "meteor-swarm"
  :effect #'(lambda (dun player)
	      (declare (ignore dun))
	      (let ((plvl (player.level player)))
		(when-bind (dir (%read-direction))
		  (van-fire-ball! player dir (get-spell-effect '<meteor>) (+ 65 plvl) 3)
		  ))))

(define-spell "Mana Storm" "mana-storm"
  :effect #'(lambda (dun player)
	      (declare (ignore dun))
	      (let ((plvl (player.level player)))
		(when-bind (dir (%read-direction))
		  (van-fire-ball! player dir (get-spell-effect '<mana>) (+ 300 (* plvl 2)) 3)
		  ))))



(define-spell "Detect Evil" "mage-detect-evil")
(define-spell "Detect Enchantment" "detect-enchantment")
(define-spell "Recharge Item III" "recharge-item-3")
(define-spell "Genocide" "genocide-2")
(define-spell "Mass Genocide" "mass-genocide")


(define-spell "Resist Fire" "resist-fire")
(define-spell "Resist Cold" "resist-cold")
(define-spell "Resist Acid" "resist-acid")
(define-spell "Resist Poison" "resist-poison")
(define-spell "Resistance" "resistance")


(define-spell "Heroism" "heroism")
(define-spell "Shield" "shield")
(define-spell "Berserker" "berserker")
(define-spell "Essence of Speed" "essence-of-speed")
(define-spell "Globe of Invulnerability" "globe-of-invulnerability")

;;; === Priest/paladin spells

(define-spell "Detect Evil" "priest-detect-evil")
(define-spell "Cure Light Wounds" "priest-cure-light-wounds"
  :effect #'(lambda (dun player)
	      (declare (ignore dun))
	      (let ((amount (roll-dice 2 10)))
		(heal-creature! player amount)
		(setf (get-creature-state player :heal-cut) '<light>) ;; fix
		)))

(define-spell "Bless" "bless")
(define-spell "Remove Fear" "remove-fear")
(define-spell "Call Light" "call-light"
  :effect #'(lambda (dun player)
	      (let ((plvl (player.level player)))
		(light-area! dun player (roll-dice 2 (int-/ plvl 2))
			     (1+ (int-/ plvl 10)))
		)))

(define-spell "Find Traps" "find-traps")
(define-spell "Detect Doors/Stairs" "detect-doors/stairs")
(define-spell "Slow Poison" "slow-poison")


(define-spell "Scare Monster" "scare-monster")
(define-spell "Portal" "portal"
  :effect #'(lambda (dun player)
	      (teleport-creature! dun player player (* (player.level player) 3))
	      ))

(define-spell "Cure Serious Wounds" "cure-serious-wounds")
(define-spell "Chant" "chant")
(define-spell "Sanctuary" "sanctuary")
(define-spell "Satisfy Hunger" "priest-satisfy-hunger")
(define-spell "Remove Curse" "remove-curse")
(define-spell "Resist Heat and Cold" "resist-heat-and-cold")


(define-spell "Neutralize Poison" "neutralize-poison")
(define-spell "Orb of Draining" "orb-of-draining")
(define-spell "Cure Critical Wounds" "cure-critical-wounds")
(define-spell "Sense Invisible" "sense-invisible")
(define-spell "Protection from Evil" "protection-from-evil")
(define-spell "Earthquake" "priest-earthquake")
(define-spell "Sense Surroundings" "sense-surroundings")
(define-spell "Cure Mortal Wounds" "cure-mortal-wounds")
(define-spell "Turn Undead" "turn-undead")


(define-spell "Prayer" "prayer")
(define-spell "Dispel Undead" "dispel-undead")
(define-spell "Heal" "heal")
(define-spell "Dispel Evil" "dispel-evil")
(define-spell "Glyph of Warding" "glyph-of-warding")
(define-spell "Holy Word" "holy-word")


(define-spell "Detect Monsters" "priest-detect-monsters")
(define-spell "Detection" "detection")
(define-spell "Perception" "perception")
(define-spell "Probing" "probing")
(define-spell "Clairvoyance" "clairvoyance")


(define-spell "Cure Serious Wounds" "cure-serious-wounds-2")
(define-spell "Cure Mortal Wounds" "cure-mortal-wounds-2")
(define-spell "Healing" "healing")
(define-spell "Restoration" "restoration")
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
  :effect #'(lambda (dun player)
	      (teleport-creature! dun player player 10)
	      ))

(define-spell "Teleport Self" "priest-teleport-self"
  :effect #'(lambda (dun player)
	      (teleport-creature! dun player player (* (player.level player) 8))
	      ))

(define-spell "Teleport Other" "priest-teleport-other")
(define-spell "Teleport Level" "priest-teleport-level")
(define-spell "Word of Recall" "priest-word-of-recall")
(define-spell "Alter Reality" "alter-reality")



;;; === Mage/ranger/rogue spellbooks
(define-spellbook "Magic for Beginners" "magic-beginner" :size 9
		  :spells '("magic-missile"
			    "mage-detect-monsters"
			    "phase-door"
			    "light-area"
			    "treasure-detection"
			    "mage-cure-light-wounds"
			    "object-detection"
			    "find-traps/doors"
			    "stinking-cloud"
			    ))

(define-spellbook "Conjurings and Tricks" "conjurings-and-tricks" :size 9
		  :spells '("confuse-monster"
			    "lightning-bolt"
			    "trap/door-destruction"
			    "sleep-1"
			    "cure-poison"
			    "mage-teleport-self"
			    "spear-of-light"
			    "frost-bolt"
			    "stone-to-mud"
			    ))
(define-spellbook "Incantation and Illusions" "incantations" :size 8
		  :spells '("mage-satisfy-hunger"
			    "recharge-item-1"
			    "sleep-2"
			    "polymorph-other"
			    "identify"
			    "sleep-3"
			    "fire-bolt"
			    "slow-monster"
			    ))

(define-spellbook "Sorcery and Evocations" "sorcery-evocations" :size 7
		  :spells '("frost-ball"
			    "recharge-item-2"
			    "mage-teleport-other"
			    "haste-self"
			    "fire-ball"
			    "mage-word-of-destruction"
			    "genocide-1"
			    ))

(define-spellbook "Mordenkainen's Escapes" "mordenkainen-escapes" :size 5
		  :spells '("door-creation"
			    "stair-creation"
			    "mage-teleport-level"
			    "mage-earthquake"
			    "mage-word-of-recall"
			    ))

(define-spellbook "Raal's Tome of Destruction" "raals-tome" :size 6
		  :spells '("acid-bolt"
			    "cloud-kill"
			    "acid-ball"
			    "ice-storm"
			    "meteor-swarm"
			    "mana-storm"
			    ))

(define-spellbook "Kelek's Grimoire of Power" "keleks-grimoire" :size 5
		  :spells '("mage-detect-evil"
			    "detect-enchantment"
			    "recharge-item-3"
			    "genocide-2"
			    "mass-genocide"
			    ))

(define-spellbook "Resistance of Scarabtarices" "resistance-scarab" :size 5
		  :spells '("resist-fire"
			    "resist-cold"
			    "resist-acid"
			    "resist-poison"
			    "resistance"
			    ))

(define-spellbook "Tenser's transformations" "tensers-transformations" :size 5
		  :spells '("heroism"
			    "shield"
			    "berserker"
			    "essence-of-speed"
			    "globe-of-invulnerability"
			    ))

;;; === Prayerbooks for priests/paladins

(define-spellbook "Beginners Handbook" "beginner-handbook" :size 8
		  :spells '("priest-detect-evil"
			    "priest-cure-light-wounds"
			    "bless"
			    "remove-fear"
			    "call-light"
			    "find-traps"
			    "detect-doors/stairs"
			    "slow-poison"
			    ))

(define-spellbook "Words of Wisdom" "words-wisdom" :size 8
		  :spells '("scare-monster"
			    "portal"
			    "cure-serious-wounds"
			    "chant"
			    "sanctuary"
			    "priest-satisfy-hunger"
			    "remove-curse"
			    "resist-heat-and-cold"
			    ))

(define-spellbook "Chants and Blessings" "chants-blessings" :size 9
		  :spells '("neutralize-poison"
			    "orb-of-draining"
			    "cure-critical-wounds"
			    "sense-invisible"
			    "protection-from-evil"
			    "priest-earthquake"
			    "sense-surroundings"
			    "cure-mortal-wounds"
			    "turn-undead"
			    ))

(define-spellbook "Exorcism and Dispelling" "exorcism-dispelling" :size 6
		  :spells '("prayer"
			    "dispel-undead"
			    "heal"
			    "dispel-evil"
			    "glyph-of-warding"
			    "holy-word"
			    ))

(define-spellbook "Godly Insights" "godly-insights" :size 5
		  :spells '("priest-detect-monsters"
			    "detection"
			    "perception"
			    "probing"
			    "clairvoyance"
			    ))

(define-spellbook "Purifications and Healing" "purifications" :size 5
		  :spells '("cure-serious-wounds-2"
			    "cure-mortal-wounds-2"
			    "healing"
			    "restoration"
			    "remembrance"
			    ))

(define-spellbook "Wrath of God" "wrath-of-god" :size 5
		  :spells '("dispel-undead-2"
			    "dispel-evil-2"
			    "banishment"
			    "priest-word-of-destruction"
			    "annihilation"
			    ))

(define-spellbook "Holy Infusions" "holy-infusions" :size 6
		  :spells '("unbarring-ways"
			    "recharging"
			    "dispel-curse"
			    "enchant-weapon"
			    "enchant-armour"
			    "elemental-brand"
			    ))

(define-spellbook "Ethereal openings" "ethereal-openings" :size 6
		  :spells '("blink"
			    "priest-teleport-self"
			    "priest-teleport-other"
			    "priest-teleport-level"
			    "priest-word-of-recall"
			    "alter-reality"
			    ))

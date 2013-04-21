;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/classes.lisp - classes for vanilla variant
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)


(define-character-class "warrior" "Warrior"
  :symbol '<warrior>
  :desc "
               A Warrior is a hack-and-slash character, who solves most of
               his problems by cutting them to pieces, but will occasionally
               fall back on the help of a magical device.  His prime
               stats are Strength and Constitution, and a good Dexterity
               can really help at times.  A Warrior will be good at Fighting
               and Throwing/Bows, but bad at most other skills."

  :xp-extra 0
  :stat-changes '((<str> +5) (<int> -2) (<wis> -2)
		  (<dex> +2) (<con> +2) (<chr> -1))
  :abilities '(;; need to be handled better, is now hard-coded in vanilla/player.lisp
	       ;;(<resist> <fear> :level 30)
	       )
  :hit-dice 9
  :titles '(
	    "Rookie"
	    "Soldier"
	    "Mercenary"
	    "Veteran"
	    "Swordsman"
	    "Champion"
	    "Hero"
	    "Baron"
	    "Duke"
	    "Lord")

  :starting-equipment '((obj :id "broad-sword")
			(obj :id "chain-mail")
			(obj :id "potion-berserk")
			#||
			(obj :id "lantern")
			(obj :id "mithril-plate")
			(obj :id "broken-stick" :amount 5)
			(obj :id "ring-resist-fire" :amount 7)
			(obj :id "magic-beginner" :amount 5)
			(obj :id "scroll-identify" :amount 80)
			(obj :id "wand-fire-bolts" :amount 5)
			(obj :id "rod-lightning-bolts" :amount 5)
			(obj :id "scroll-illumination" :amount 80)
			(obj :id "potion-poison" :amount 5)
			
			(obj :id "amulet-slow-digest")
			(obj :id "ring-resist-poison")
			(obj :id "amulet-resist-acid")
			
			
			(obj :id "oil-flask")
			(obj :id "empty-bottle")
			
			(obj :id "wand-lightning-bolt")
			(obj :id "wand-stinking-cloud")
			(obj :id "rod-lightning-bolts")
			(obj :id "ring-stupid")
			

			(obj :id "ring-teleport")
			(obj :id "potion-confusion" :amount 2)

			;; various objects I use when testing stuff
;;			(obj :id "ring-feather-fall")
;;			
;;			(obj :id "ring-woe")
;;			
			(obj :id "staff-identify")
			(obj :id "potion-speed" :amount 9)
			
			;;(obj :id "potion-heroism" :amount 2)
			;;(obj :id "potion-reduce-str")
			;;(obj :id "potion-reduce-dex")
			;;(obj :id "potion-poison" :amount 2)
			;;(obj :id "potion-slow-poison" :amount 2)
			;;(obj :id "potion-sleep" :amount 3)
			;;(obj :id "scroll-teleport-lvl" :amount 5)
			;;
			;;(obj :id "potion-str" :amount 7)
			

			||#
			
			)
			

  
  :skills '((<disarming> 25 10)
	    (<device> 18 7)
	    (<saving-throw> 18 10)
	    (<stealth> 1 0)
	    (<search> 14 0)
	    (<perception> 2 0)
	    (<fighting> 70 45)
	    (<shooting> 55 45))
  )

(define-character-class "paladin" "Paladin"
  :symbol '<paladin>
  :desc "
               A Paladin is a warrior/priest.  He is a very good fighter,
               second only to the warrior class, but not very good at missile
               weapons.  He receives prayers at a slower pace then the
               priest, but can even receive the most powerful prayers.
               Because a paladin is really a dual class character, it
               requires more experience  to advance him.  A paladin lacks
               much in the way of abilities.  He is poor at stealth,
               perception, searching, and magical devices.  He has a decent
               saving throw due to his divine alliance.  His primary stats
               are Strength and Charisma."
 
  :xp-extra 35
  :hit-dice 6
  :stat-changes '((<str> +3) (<int> -3) (<wis> +1)
		  (<dex> 0) (<con> +2) (<chr> +2))
;;  :abilities nil
  :titles '(
	   "Gallant"
	   "Keeper"
	   "Protector"
	   "Defender"
	   "Warder"
	   "Knight"
	   "Guardian"
	   "Low Paladin"
	   "High Paladin"
	   "Paladin Lord"
	   )
  
    :starting-equipment '((obj :id "broad-sword")
			  (obj :id "scroll-protect-from-evil")
			  (obj :id "beginner-handbook")
			  #||
			  ;; hacks
			  (obj :id "potion-poison" :amount 5)
			  (obj :id "potion-confusion" :amount 5)
			  (obj :id "potion-blindness" :amount 5)
			  (obj :id "potion-speed" :amount 5)
			  (obj :id "potion-slowness" :amount 5)
			  (obj :id "potion-sleep" :amount 5)
			  (obj :id "potion-restore-mana" :amount 5)
			  (obj :id "ring-stupid")
			  (obj :id "scroll-remove-curse" :amount 5)
			  (obj :id "scroll-wor" :amount 5)
			  ||#
			  )
    

    :skills '((<disarming> 20 7)
	      (<device> 24 10)
	      (<saving-throw> 25 11)
	      (<stealth> 1 0)
	      (<search> 12 0)
	      (<perception> 2 0)
	      (<fighting> 68 35)
	      (<shooting> 40 30))
    
    :magic-abilities '(:spell-stat <wis> :spells-at-level 1) ;; unsure on starting level
    :spells '((:id "priest-detect-evil" :level 1 :mana 1 :fail 30 :xp 4) 
	      (:id "priest-cure-light-wounds" :level 2 :mana 2 :fail 35 :xp 4) 
	      (:id "bless" :level 3 :mana 3 :fail 35 :xp 4) 
	      (:id "remove-fear" :level 5 :mana 3 :fail 35 :xp 4) 
	      (:id "call-light" :level 5 :mana 4 :fail 35 :xp 4) 
	      (:id "find-traps" :level 7 :mana 5 :fail 40 :xp 3) 
	      (:id "detect-doors/stairs" :level 7 :mana 5 :fail 40 :xp 3) 
	      (:id "slow-poison" :level 9 :mana 7 :fail 40 :xp 3) 
	      (:id "scare-monster" :level 9 :mana 7 :fail 40 :xp 3) 
	      (:id "portal" :level 9 :mana 8 :fail 40 :xp 3) 
	      (:id "cure-serious-wounds" :level 11 :mana 9 :fail 40 :xp 3) 
	      (:id "chant" :level 11 :mana 10 :fail 45 :xp 3) 
	      (:id "sanctuary" :level 11 :mana 10 :fail 45 :xp 3) 
	      (:id "priest-satisfy-hunger" :level 13 :mana 10 :fail 45 :xp 3) 
	      (:id "remove-curse" :level 13 :mana 11 :fail 45 :xp 4) 
	      (:id "resist-heat-and-cold" :level 15 :mana 13 :fail 45 :xp 4) 
	      (:id "neutralize-poison" :level 15 :mana 15 :fail 50 :xp 4) 
	      (:id "orb-of-draining" :level 17 :mana 15 :fail 50 :xp 4) 
	      (:id "cure-critical-wounds" :level 17 :mana 15 :fail 50 :xp 4) 
	      (:id "sense-invisible" :level 19 :mana 15 :fail 50 :xp 4) 
	      (:id "protection-from-evil" :level 19 :mana 15 :fail 50 :xp 4) 
	      (:id "priest-earthquake" :level 21 :mana 17 :fail 50 :xp 3) 
	      (:id "sense-surroundings" :level 23 :mana 17 :fail 50 :xp 3) 
	      (:id "cure-mortal-wounds" :level 25 :mana 20 :fail 50 :xp 3) 
	      (:id "turn-undead" :level 27 :mana 21 :fail 50 :xp 3) 
	      (:id "prayer" :level 29 :mana 22 :fail 50 :xp 3) 
	      (:id "dispel-undead" :level 31 :mana 24 :fail 60 :xp 3) 
	      (:id "heal" :level 33 :mana 28 :fail 60 :xp 3) 
	      (:id "dispel-evil" :level 35 :mana 32 :fail 70 :xp 4) 
	      (:id "glyph-of-warding" :level 37 :mana 70 :fail 90 :xp 5) 
	      (:id "holy-word" :level 39 :mana 38 :fail 95 :xp 10) 
	      (:id "priest-detect-monsters" :level 5 :mana 5 :fail 50 :xp 1) 
	      (:id "detection" :level 15 :mana 15 :fail 80 :xp 12) 
	      (:id "perception" :level 25 :mana 25 :fail 80 :xp 16) 
	      (:id "probing" :level 30 :mana 15 :fail 80 :xp 135) 
	      (:id "clairvoyance" :level 37 :mana 55 :fail 80 :xp 215) 
	      (:id "cure-serious-wounds-2" :level 17 :mana 15 :fail 50 :xp 25) 
	      (:id "cure-mortal-wounds-2" :level 23 :mana 25 :fail 60 :xp 35) 
	      (:id "healing" :level 35 :mana 60 :fail 80 :xp 115) 
	      (:id "restoration" :level 40 :mana 80 :fail 90 :xp 200) 
	      (:id "remembrance" :level 40 :mana 80 :fail 90 :xp 250) 
	      (:id "dispel-undead-2" :level 20 :mana 13 :fail 70 :xp 20) 
	      (:id "dispel-evil-2" :level 30 :mana 20 :fail 75 :xp 40) 
	      (:id "banishment" :level 30 :mana 35 :fail 80 :xp 200) 
	      (:id "priest-word-of-destruction" :level 40 :mana 40 :fail 80 :xp 100) 
	      (:id "annihilation" :level 47 :mana 70 :fail 75 :xp 250) 
	      (:id "unbarring-ways" :level 10 :mana 16 :fail 50 :xp 20) 
	      (:id "recharging" :level 25 :mana 30 :fail 80 :xp 15) 
	      (:id "dispel-curse" :level 30 :mana 50 :fail 80 :xp 130) 
	      (:id "enchant-weapon" :level 40 :mana 70 :fail 80 :xp 200) 
	      (:id "enchant-armour" :level 42 :mana 80 :fail 85 :xp 250) 
	      (:id "elemental-brand" :level 47 :mana 95 :fail 85 :xp 250) 
	      (:id "blink" :level 7 :mana 7 :fail 50 :xp 2) 
	      (:id "priest-teleport-self" :level 20 :mana 20 :fail 50 :xp 4) 
	      (:id "priest-teleport-other" :level 25 :mana 25 :fail 80 :xp 12) 
	      (:id "priest-teleport-level" :level 35 :mana 50 :fail 75 :xp 115) 
	      (:id "priest-word-of-recall" :level 40 :mana 60 :fail 75 :xp 10) 
	      (:id "alter-reality" :level 45 :mana 70 :fail 75 :xp 250) 
	      ))

(define-character-class "ranger" "Ranger"
  :symbol '<ranger>
  :desc "
               A Ranger is a warrior/mage.  He is a good fighter, and the
               best of the classes with a missile weapon such as a bow.
               The ranger learns spells much more slowly than a mage, but
               is capable of learning all but the most powerful spells.
               Because a ranger is really a dual class character, more
               experience is required for him to advance.  A ranger has a
               good stealth, good perception, good searching, a good saving
               throw, and is good with magical devices.  His primary stats
               are Intelligence and Dexterity."

  :xp-extra 30
  :hit-dice 4
  :stat-changes '((<str> +2) (<int> +2) (<wis> 0)
		  (<dex> +1) (<con> +1) (<chr> +1))
;;  :abilities nil
  :titles '(
	    "Runner"
	    "Strider"
	    "Scout"
	    "Courser"
	    "Tracker"
	    "Guide"
	    "Pathfinder"
	    "Low Ranger"
	    "High Ranger"
	    "Ranger Lord"
	    )
  
    :starting-equipment '((obj :id "magic-beginner")
			  (obj :id "short-sword")
			  (obj :id "long-bow")
			  (obj :id "arrow" :amount "6d6")
			  #||
			  ;; hackish, for testing
			  (obj :id "potion-resist-heat" :amount 2)
			  (obj :id "potion-resist-cold" :amount 2)
			  ||#
			  )
    
    :skills '((<disarming> 30 8)
	      (<device> 32 10)
	      (<saving-throw> 28 10)
	      (<stealth> 3 0)
	      (<search> 24 0)
	      (<perception> 16 0)
	      (<fighting> 56 30)
	      (<shooting> 72 45))
    
    :magic-abilities '(:spell-stat <int> :spells-at-level 3)
    :spells '((:id "magic-missile" :level 3 :mana 1 :fail 30 :xp 1) 
	      (:id "mage-detect-monsters" :level 3 :mana 2 :fail 35 :xp 2) 
	      (:id "phase-door" :level 3 :mana 2 :fail 35 :xp 2) 
	      (:id "light-area" :level 5 :mana 3 :fail 35 :xp 1) 
	      (:id "mage-cure-light-wounds" :level 5 :mana 3 :fail 40 :xp 1) 
	      (:id "find-traps/doors" :level 5 :mana 4 :fail 45 :xp 2) 
	      (:id "stinking-cloud" :level 7 :mana 5 :fail 40 :xp 3) 
	      (:id "confuse-monster" :level 7 :mana 6 :fail 40 :xp 2) 
	      (:id "lightning-bolt" :level 9 :mana 7 :fail 40 :xp 3) 
	      (:id "trap/door-destruction" :level 9 :mana 8 :fail 45 :xp 3) 
	      (:id "sleep-1" :level 11 :mana 8 :fail 40 :xp 3) 
	      (:id "cure-poison" :level 11 :mana 9 :fail 45 :xp 3) 
	      (:id "mage-teleport-self" :level 13 :mana 10 :fail 45 :xp 3) 
	      (:id "spear-of-light" :level 13 :mana 11 :fail 55 :xp 4) 
	      (:id "frost-bolt" :level 15 :mana 12 :fail 50 :xp 4) 
	      (:id "stone-to-mud" :level 15 :mana 13 :fail 50 :xp 4) 
	      (:id "mage-satisfy-hunger" :level 17 :mana 17 :fail 55 :xp 3) 
	      (:id "recharge-item-1" :level 17 :mana 17 :fail 90 :xp 4) 
	      (:id "sleep-2" :level 21 :mana 17 :fail 55 :xp 3) 
	      (:id "polymorph-other" :level 21 :mana 19 :fail 60 :xp 3) 
	      (:id "identify" :level 23 :mana 25 :fail 90 :xp 3) 
	      (:id "sleep-3" :level 23 :mana 20 :fail 60 :xp 3) 
	      (:id "fire-bolt" :level 25 :mana 20 :fail 60 :xp 3) 
	      (:id "slow-monster" :level 25 :mana 21 :fail 65 :xp 3) 
	      (:id "frost-ball" :level 27 :mana 21 :fail 65 :xp 3) 
	      (:id "recharge-item-2" :level 29 :mana 23 :fail 95 :xp 3) 
	      (:id "mage-teleport-other" :level 31 :mana 25 :fail 70 :xp 3) 
	      (:id "haste-self" :level 33 :mana 25 :fail 75 :xp 4) 
	      (:id "fire-ball" :level 35 :mana 25 :fail 80 :xp 5) 
	      (:id "mage-word-of-destruction" :level 37 :mana 30 :fail 95 :xp 10) 
	      (:id "door-creation" :level 8 :mana 17 :fail 20 :xp 25) 
	      (:id "stair-creation" :level 19 :mana 22 :fail 40 :xp 21) 
	      (:id "mage-teleport-level" :level 25 :mana 27 :fail 60 :xp 17) 
	      (:id "mage-earthquake" :level 30 :mana 28 :fail 60 :xp 16) 
	      (:id "mage-word-of-recall" :level 35 :mana 35 :fail 75 :xp 13) 
	      (:id "acid-bolt" :level 20 :mana 16 :fail 50 :xp 6) 
	      (:id "cloud-kill" :level 22 :mana 19 :fail 60 :xp 9) 
	      (:id "acid-ball" :level 30 :mana 25 :fail 70 :xp 13) 
	      (:id "ice-storm" :level 33 :mana 35 :fail 75 :xp 25) 
	      (:id "meteor-swarm" :level 35 :mana 45 :fail 85 :xp 35) 
	      (:id "mage-detect-evil" :level 10 :mana 15 :fail 50 :xp 4) 
	      (:id "detect-enchantment" :level 15 :mana 20 :fail 70 :xp 25) 
	      (:id "recharge-item-3" :level 35 :mana 60 :fail 95 :xp 115) 
	      (:id "resist-fire" :level 8 :mana 15 :fail 50 :xp 30) 
	      (:id "resist-cold" :level 8 :mana 15 :fail 50 :xp 30) 
	      (:id "resist-acid" :level 8 :mana 15 :fail 50 :xp 30) 
	      (:id "resist-poison" :level 16 :mana 25 :fail 75 :xp 50) 
	      (:id "resistance" :level 25 :mana 40 :fail 85 :xp 70) 
	      (:id "heroism" :level 10 :mana 15 :fail 50 :xp 40) 
	      (:id "shield" :level 15 :mana 20 :fail 75 :xp 80) 
	      (:id "berserker" :level 25 :mana 30 :fail 80 :xp 125) 
	      (:id "essence-of-speed" :level 32 :mana 50 :fail 50 :xp 250)
	      ))

(define-character-class "priest" "Priest"
  :symbol '<priest>
  :desc "
               A Priest is a character of holy devotion.  They explore the
               dungeon only to destroy the evil that lurks within, and if
               treasure just happens to fall into their packs, well, so
               much more to the glory of their church!  Priests receive
               their spells from a deity, and therefore do not choose which
               spells they will learn.  They are familiar with magical devices,
               preferring to call them instruments of god,  but are
               not as good as a mage in their use.  Priests have good saving
               throws, and make decent fighters, preferring blunt
               weapons over edged ones.  Wisdom and Charisma are the
               priest's primary stats."

  :xp-extra 20
  :hit-dice 2
  :stat-changes '((<str> -1) (<int> -3) (<wis> +3)
		  (<dex> -1) (<con> 0) (<chr> +2))
;;  :abilities nil
  :titles '(
	    "Believer"
	    "Acolyte"
	    "Adept"
	    "Curate"
	    "Canon"
	    "Lama"
	    "Patriarch"
	    "Priest"
	    "High Priest"
	    "Priest Lord"
	    )
  
  :starting-equipment '((obj :id "mace")
			(obj :id "beginner-handbook")
			(obj :id "potion-healing")
			)

  :skills '((<disarming> 25 7)
	    (<device> 30 10)
	    (<saving-throw> 32 12)
	    (<stealth> 2 0)
	    (<search> 16 0)
	    (<perception> 8 0)
	    (<fighting> 48 20)
	    (<shooting> 35 20))

  :magic-abilities '(:spell-stat <wis> :spells-at-level 1)
  :spells '((:id "priest-detect-evil"       :level 1 :mana 1 :fail 10 :xp 4)
	    (:id "priest-cure-light-wounds" :level 1 :mana 2 :fail 15 :xp 4)
	    (:id "bless"                    :level 1 :mana 2 :fail 20 :xp 4)
	    (:id "remove-fear"              :level 1 :mana 2 :fail 25 :xp 4)
	    (:id "call-light"               :level 3 :mana 2 :fail 25 :xp 1)
	    (:id "find-traps"               :level 3 :mana 3 :fail 27 :xp 2)
	    (:id "detect-doors/stairs"      :level 3 :mana 3 :fail 27 :xp 2)
	    (:id "slow-poison"              :level 3 :mana 3 :fail 28 :xp 4)

	    (:id "scare-monster"         :level 5 :mana 4 :fail 29 :xp 3)
	    (:id "portal"                :level 5 :mana 4 :fail 30 :xp 4)
	    (:id "cure-serious-wounds"   :level 5 :mana 4 :fail 32 :xp 4)
	    (:id "chant"                 :level 5 :mana 5 :fail 34 :xp 4)
	    (:id "sanctuary"             :level 7 :mana 5 :fail 36 :xp 3)
	    (:id "priest-satisfy-hunger" :level 7 :mana 5 :fail 38 :xp 4)
	    (:id "remove-curse"          :level 7 :mana 6 :fail 38 :xp 5)
	    (:id "resist-heat-and-cold"  :level 7 :mana 7 :fail 38 :xp 5)

	    (:id "neutralize-poison" :level 9 :mana 6 :fail 38 :xp 4) 
	    (:id "orb-of-draining" :level 9 :mana 7 :fail 38 :xp 4) 
	    (:id "cure-critical-wounds" :level 9 :mana 7 :fail 40 :xp 4) 
	    (:id "sense-invisible" :level 11 :mana 8 :fail 42 :xp 4) 
	    (:id "protection-from-evil" :level 11 :mana 8 :fail 42 :xp 4) 
	    (:id "priest-earthquake" :level 11 :mana 9 :fail 55 :xp 5) 
	    (:id "sense-surroundings" :level 13 :mana 10 :fail 45 :xp 4) 
	    (:id "cure-mortal-wounds" :level 13 :mana 11 :fail 45 :xp 4) 
	    (:id "turn-undead" :level 15 :mana 12 :fail 50 :xp 5) 
	    (:id "prayer" :level 15 :mana 14 :fail 50 :xp 5) 
	    (:id "dispel-undead" :level 17 :mana 14 :fail 55 :xp 7) 
	    (:id "heal" :level 21 :mana 16 :fail 60 :xp 7) 
	    (:id "dispel-evil" :level 25 :mana 20 :fail 70 :xp 12) 
	    (:id "glyph-of-warding" :level 33 :mana 55 :fail 90 :xp 15) 
	    (:id "holy-word" :level 39 :mana 32 :fail 95 :xp 20) 
	    (:id "priest-detect-monsters" :level 3 :mana 3 :fail 50 :xp 2) 
	    (:id "detection" :level 10 :mana 10 :fail 80 :xp 20) 
	    (:id "perception" :level 20 :mana 20 :fail 80 :xp 20) 
	    (:id "probing" :level 25 :mana 10 :fail 80 :xp 150) 
	    (:id "clairvoyance" :level 35 :mana 50 :fail 80 :xp 230) 
	    (:id "cure-serious-wounds-2" :level 15 :mana 5 :fail 50 :xp 25) 
	    (:id "cure-mortal-wounds-2" :level 17 :mana 7 :fail 60 :xp 45) 
	    (:id "healing" :level 30 :mana 50 :fail 80 :xp 130) 
	    (:id "restoration" :level 35 :mana 70 :fail 90 :xp 230) 
	    (:id "remembrance" :level 35 :mana 70 :fail 90 :xp 250) 
	    (:id "dispel-undead-2" :level 15 :mana 7 :fail 70 :xp 25) 
	    (:id "dispel-evil-2" :level 20 :mana 10 :fail 75 :xp 60) 
	    (:id "banishment" :level 25 :mana 25 :fail 80 :xp 250) 
	    (:id "priest-word-of-destruction" :level 35 :mana 35 :fail 80 :xp 115) 
	    (:id "annihilation" :level 45 :mana 60 :fail 75 :xp 250) 
	    (:id "unbarring-ways" :level 5 :mana 6 :fail 50 :xp 40) 
	    (:id "recharging" :level 15 :mana 20 :fail 80 :xp 25) 
	    (:id "dispel-curse" :level 25 :mana 40 :fail 80 :xp 160) 
	    (:id "enchant-weapon" :level 35 :mana 50 :fail 80 :xp 230) 
	    (:id "enchant-armour" :level 37 :mana 60 :fail 85 :xp 250) 
	    (:id "elemental-brand" :level 45 :mana 95 :fail 85 :xp 250) 
	    (:id "blink" :level 3 :mana 3 :fail 50 :xp 6) 
	    (:id "priest-teleport-self" :level 10 :mana 10 :fail 50 :xp 8) 
	    (:id "priest-teleport-other" :level 20 :mana 20 :fail 80 :xp 16) 
	    (:id "priest-teleport-level" :level 30 :mana 40 :fail 75 :xp 133) 
	    (:id "priest-word-of-recall" :level 35 :mana 50 :fail 75 :xp 11) 
	    (:id "alter-reality" :level 40 :mana 60 :fail 75 :xp 250)
	    ))


(define-character-class "mage" "Mage"
  :symbol '<mage>
  :desc "
               A Mage must live by his wits.  He cannot hope to simply hack
               his way through the dungeon, and so must therefore use his
               magic to defeat, deceive, confuse, and escape.  A mage is
               not really complete without an assortment of magical devices
               to use in addition to his spells.  He can master the higher
               level magical devices far easier than anyone else, and has
               the best saving throw to resist effects of spells cast at
               him.  Intelligence and Dexterity are his primary stats.
               There is no rule that says a mage cannot become a good
               fighter, but spells are his true realm."

  :xp-extra 30
  :hit-dice 0
  :stat-changes '((<str> -5) (<int> +3) (<wis> 0)
		  (<dex> +1) (<con> -2) (<chr> +1))
;;  :abilities nil
  :titles '(
	    "Novice"
	    "Apprentice"
	    "Trickster"
	    "Illusionist"
	    "Spellbinder"
	    "Evoker"
	    "Conjurer"
	    "Warlock"
	    "Sorcerer"
	    "Mage Lord"
	    )
  
    :starting-equipment '((obj :id "dagger")
			  (obj :id "magic-beginner")
			  (obj :id "scroll-wor")
			  #||
			  (obj :id "leather-gloves")
			  (obj :id "iron-helm")
			  (obj :id "full-plate")
			  (obj :id "metal-boots")
			  (obj :id "chain-mail")
			  
			  ;; hacks
			  (obj :type (<scroll> <illuminate>) :amount 2)
			  (obj :type (<scroll> <enchant> <weapon> <to-hit>))
			  (obj :type (<scroll> <enchant> <weapon> <powerful>))
			  (obj :type (<scroll> <phase-door>))
			  ||#
			  )

    :skills '((<disarming> 30 7)
	      (<device> 36 13)
	      (<saving-throw> 30 9)
	      (<stealth> 2 0)
	      (<search> 16 0)
	      (<perception> 20 0)
	      (<fighting> 34 15)
	      (<shooting> 20 15))

    :abilities '(<cumbered-by-gloves>)
    :magic-abilities '(:spell-stat <int> :spells-at-level 1 :max-armour-weight 300)
    
    :spells '((:id "magic-missile"          :level 1 :mana 1 :fail 22 :xp 4) 
	      (:id "mage-detect-monsters"   :level 1 :mana 1 :fail 23 :xp 4)
	      (:id "phase-door"             :level 1 :mana 2 :fail 24 :xp 4)
	      (:id "light-area"             :level 1 :mana 2 :fail 26 :xp 4)
	      ;; does not have treasure detection
	      (:id "mage-cure-light-wounds" :level 3 :mana 3 :fail 25 :xp 3)
	      ;; no object-detection
	      (:id "find-traps/doors"       :level 3 :mana 3 :fail 25 :xp 1)
	      (:id "stinking-cloud"         :level 3 :mana 3 :fail 27 :xp 3)

	      (:id "confuse-monster"       :level 3 :mana 4 :fail 30 :xp 1)
	      (:id "lightning-bolt"        :level 5 :mana 4 :fail 30 :xp 4)
	      (:id "trap/door-destruction" :level 5 :mana 5 :fail 30 :xp 6)
	      (:id "sleep-1"               :level 5 :mana 5 :fail 30 :xp 4)
	      (:id "cure-poison"           :level 5 :mana 5 :fail 35 :xp 4)
	      (:id "mage-teleport-self"    :level 7 :mana 6 :fail 35 :xp 5)
	      (:id "spear-of-light"        :level 7 :mana 6 :fail 30 :xp 5)
	      (:id "frost-bolt"            :level 7 :mana 6 :fail 40 :xp 6)
	      (:id "stone-to-mud"          :level 9 :mana 7 :fail 44 :xp 8)

	      (:id "mage-satisfy-hunger" :level  9 :mana 7 :fail 45 :xp 8)
	      (:id "recharge-item-1"     :level  9 :mana 7 :fail 75 :xp 9)
	      (:id "sleep-2"             :level  9 :mana 7 :fail 45 :xp 8)
	      (:id "polymorph-other"     :level 11 :mana 7 :fail 45 :xp 9)
	      (:id "identify"            :level 11 :mana 7 :fail 75 :xp 6)
	      (:id "sleep-3"             :level 13 :mana 7 :fail 50 :xp 6)
	      (:id "fire-bolt"           :level 15 :mana 9 :fail 50 :xp 6)
	      (:id "slow-monster"        :level 17 :mana 9 :fail 50 :xp 7)

	      (:id "frost-ball"               :level 19 :mana 12 :fail 55 :xp 8)
	      (:id "recharge-item-2"          :level 21 :mana 12 :fail 90 :xp 8)
	      (:id "mage-teleport-other"      :level 23 :mana 12 :fail 60 :xp 8)
	      (:id "haste-self"               :level 25 :mana 12 :fail 65 :xp 10)
	      (:id "fire-ball"                :level 29 :mana 18 :fail 65 :xp 12)
	      (:id "mage-word-of-destruction" :level 33 :mana 21 :fail 80 :xp 15)
	      (:id "xenocide-1"               :level 37 :mana 25 :fail 95 :xp 21)

	      (:id "door-creation"       :level 7  :mana 7  :fail 20 :xp 28)
	      (:id "stair-creation"      :level 9  :mana 12 :fail 40 :xp 44)
	      (:id "mage-teleport-level" :level 15 :mana 17 :fail 60 :xp 29)
	      (:id "mage-earthquake"     :level 20 :mana 18 :fail 60 :xp 24)
	      (:id "mage-word-of-recall" :level 25 :mana 25 :fail 75 :xp 19)

	      (:id "acid-bolt"    :level 10 :mana 6  :fail 50 :xp 12)
	      (:id "cloud-kill"   :level 12 :mana 9  :fail 60 :xp 16)
	      (:id "acid-ball"    :level 20 :mana 15 :fail 70 :xp 20)
	      (:id "ice-storm"    :level 27 :mana 25 :fail 75 :xp 29)
	      (:id "meteor-swarm" :level 35 :mana 35 :fail 85 :xp 34)
	      (:id "mana-storm"   :level 42 :mana 45 :fail 95 :xp 200)

	      (:id "mage-detect-evil" :level 5 :mana 5 :fail 50 :xp 8) 
	      (:id "detect-enchantment" :level 10 :mana 10 :fail 70 :xp 40) 
	      (:id "recharge-item-3" :level 25 :mana 30 :fail 95 :xp 160) 
	      (:id "xenocide-2" :level 30 :mana 50 :fail 70 :xp 40) 
	      (:id "mass-xenocide" :level 40 :mana 75 :fail 80 :xp 100) 
	      (:id "resist-fire" :level 4 :mana 5 :fail 50 :xp 20) 
	      (:id "resist-cold" :level 4 :mana 5 :fail 50 :xp 20) 
	      (:id "resist-acid" :level 4 :mana 5 :fail 50 :xp 20) 
	      (:id "resist-poison" :level 8 :mana 10 :fail 75 :xp 40) 
	      (:id "resistance" :level 15 :mana 20 :fail 85 :xp 60) 
	      (:id "heroism" :level 5 :mana 5 :fail 50 :xp 80) 
	      (:id "shield" :level 10 :mana 12 :fail 75 :xp 120) 
	      (:id "berserker" :level 15 :mana 20 :fail 80 :xp 200) 
	      (:id "essence-of-speed" :level 22 :mana 30 :fail 50 :xp 250) 
	      (:id "globe-of-invulnerability" :level 45 :mana 70 :fail 75 :xp 250) 
	      ))


(define-character-class "rogue" "Rogue"
  :symbol '<rogue>
  :desc "
               A Rogue is a character that prefers to live by his cunning,
               but is capable of fighting his way out of a tight spot.  He
               is the master of traps and locks, no device being impossible
               for him to overcome.  A rogue has a high stealth allowing
               him to sneak around many creatures without having to fight,
               or sneak up and get the first blow.  A rogue's perception is
               higher than any other class, and many times he will notice a
               trap or secret door before having to search.  A rogue is
               better than warriors or paladins with magical devices, but
               still can not rely on their performance.  Rogues can also
               learn a few spells, but not the  powerful offensive spells
               magi can use.  A rogue's primary stats are Intelligence and
               Dexterity."

  :xp-extra 25
  :hit-dice 6
  :stat-changes '((<str> +2) (<int> +1) (<wis> -2)
		  (<dex> +3) (<con> +1) (<chr> -1))
  :abilities '(<cumbered-by-gloves>)

  :titles '(
	    "Vagabond"
	    "Cutpurse"
	    "Robber"
	    "Burglar"
	    "Filcher"
	    "Sharper"
	    "Low Thief"
	    "High Thief"
	    "Master Thief"
	    "Assassin"
	    )
  
  :starting-equipment '((obj :id "magic-beginner")
			(obj :id "small-sword")
			(obj :id "soft-leather-armour")
			)
  
  :skills '((<disarming> 45 15)
	    (<device> 32 10)
	    (<saving-throw> 28 10)
	    (<stealth> 5 0)
	    (<search> 32 0)
	    (<perception> 24 0)
	    (<fighting> 60 40)
	    (<shooting> 66 30))
  
  :magic-abilities '(:spell-stat <int> :spells-at-level 5)
  :spells '((:id "mage-detect-monsters" :level 5 :mana 1 :fail 50 :xp 1) 
	    (:id "phase-door" :level 7 :mana 2 :fail 55 :xp 1) 
	    (:id "light-area" :level 9 :mana 3 :fail 60 :xp 1) 
	    (:id "treasure-detection" :level 10 :mana 3 :fail 60 :xp 1) 
	    (:id "mage-cure-light-wounds" :level 11 :mana 4 :fail 65 :xp 1) 
	    (:id "object-detection" :level 12 :mana 4 :fail 65 :xp 1) 
	    (:id "find-traps/doors" :level 13 :mana 5 :fail 70 :xp 1) 
	    (:id "confuse-monster" :level 15 :mana 6 :fail 75 :xp 1) 
	    (:id "trap/door-destruction" :level 17 :mana 7 :fail 80 :xp 1) 
	    (:id "sleep-1" :level 19 :mana 8 :fail 85 :xp 1) 
	    (:id "cure-poison" :level 21 :mana 9 :fail 90 :xp 1) 
	    (:id "mage-teleport-self" :level 22 :mana 9 :fail 50 :xp 1) 
	    (:id "spear-of-light" :level 23 :mana 10 :fail 95 :xp 1) 
	    (:id "stone-to-mud" :level 24 :mana 11 :fail 70 :xp 1) 
	    (:id "mage-satisfy-hunger" :level 25 :mana 12 :fail 95 :xp 1) 
	    (:id "recharge-item-1" :level 27 :mana 15 :fail 99 :xp 1) 
	    (:id "identify" :level 28 :mana 18 :fail 50 :xp 2) 
	    (:id "recharge-item-2" :level 30 :mana 20 :fail 99 :xp 6) 
	    (:id "haste-self" :level 32 :mana 25 :fail 70 :xp 6) 
	    (:id "door-creation" :level 7 :mana 7 :fail 20 :xp 25) 
	    (:id "stair-creation" :level 9 :mana 12 :fail 40 :xp 45) 
	    (:id "mage-teleport-level" :level 15 :mana 17 :fail 60 :xp 30) 
	    (:id "mage-word-of-recall" :level 30 :mana 35 :fail 75 :xp 15) 
	    (:id "acid-bolt" :level 13 :mana 16 :fail 50 :xp 10) 
	    (:id "cloud-kill" :level 18 :mana 20 :fail 60 :xp 10) 
	    (:id "mage-detect-evil" :level 5 :mana 5 :fail 50 :xp 10) 
	    (:id "detect-enchantment" :level 10 :mana 10 :fail 70 :xp 40) 
	    (:id "recharge-item-3" :level 35 :mana 40 :fail 95 :xp 100) 
	    (:id "resist-fire" :level 10 :mana 12 :fail 50 :xp 40) 
	    (:id "resist-cold" :level 10 :mana 12 :fail 50 :xp 40) 
	    (:id "resist-acid" :level 10 :mana 12 :fail 50 :xp 40) 
	    (:id "resist-poison" :level 15 :mana 20 :fail 75 :xp 60) 
	    (:id "resistance" :level 25 :mana 30 :fail 85 :xp 80) 
	    (:id "heroism" :level 10 :mana 11 :fail 50 :xp 40) 
	    (:id "shield" :level 15 :mana 20 :fail 75 :xp 80) 
	    (:id "berserker" :level 20 :mana 25 :fail 80 :xp 160) 
	    (:id "essence-of-speed" :level 26 :mana 30 :fail 50 :xp 250)))

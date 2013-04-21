;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/stores.lisp - stores and owners for vanilla variant
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(define-store '<general-store>
    :name "General Store"
    :number 1
    :sells '((obj :id "food-ration" :weight 5) ;; how weighted is the outcome, default 1
	     (obj :id "biscuit")
	     (obj :id "beef-jerky" :weight 2)
	     (obj :id "pint-wine")
	     (obj :id "pint-ale")
	     (obj :id "torch" :weight 4)
	     (obj :id "lantern" :weight 2)
	     (obj :id "oil-flask" :weight 6)
	     ;;(obj :id "iron-spike" :weight 2)
	     (obj :id "bolt" :weight 2)
	     (obj :id "arrow" :weight 2)
	     (obj :id "iron-shot" :weight 2)
	     (obj :id "shovel")
	     (obj :id "pick")
	     (obj :id "cloak" :weight 3)
	     )
    :buys '(<food> <light-source> <ammo> <digger> <cloak>)) ;; add spikes and flasks too


(define-store '<armoury>
    :name "Armoury"
    :number 2
    :sells '((obj :id "soft-leather-boots" :weight 2)
	     (obj :id "hard-leather-boots" :weight 2)
	     (obj :id "hard-leather-cap" :weight 2)
	     (obj :id "metal-cap")
	     (obj :id "iron-helm")
	     (obj :id "robe" :weight 2)
	     (obj :id "soft-leather-armour" :weight 2)
	     (obj :id "hard-leather-armour" :weight 2)
	     (obj :id "hard-studded-leather" :weight 2)
	     (obj :id "leather-scale-mail" :weight 2)
	     (obj :id "metal-scale-mail")
	     (obj :id "chain-mail" :weight 2)
	     (obj :id "augm-chain-mail")
	     (obj :id "bar-chain-mail")
	     (obj :id "double-chain-mail")
	     (obj :id "metal-brigandine")
	     (obj :id "leather-gloves" :weight 2)
	     (obj :id "gauntlets")
	     (obj :id "small-leather-shield" :weight 2)
	     (obj :id "large-leather-shield")
	     (obj :id "small-metal-shield")
	     )
    :buys #'(lambda (obj store)
	      (declare (ignore store))
	      (when (typep obj 'active-object/armour)
		t)))

(define-store '<weapon-smith>
    :name "Weapon Smith"
    :number 3
    :sells '((obj :id "dagger")
	     (obj :id "dirk")
	     (obj :id "rapier")
	     (obj :id "small-sword")
	     (obj :id "short-sword")
	     (obj :id "sabre")
	     (obj :id "cutlass")
	     (obj :id "tulwar")
	     (obj :id "broad-sword")
	     (obj :id "long-sword")
	     (obj :id "scimitar")
	     (obj :id "katana")
	     (obj :id "bastard-sword")
	     (obj :id "spear")
	     (obj :id "awl-pike")
	     (obj :id "trident")
	     (obj :id "pike")
	     (obj :id "beaked-axe")
	     (obj :id "broad-axe")
	     (obj :id "lance")
	     (obj :id "battle-axe")
	     (obj :id "sling")
	     (obj :id "short-bow")
	     (obj :id "long-bow")
	     (obj :id "light-xbow")
	     (obj :id "iron-shot" :weight 3)
	     (obj :id "arrow" :weight 3)
	     (obj :id "bolt" :weight 3)
	     )
    :buys #'(lambda (obj store)
	      (declare (ignore store))
	      (when (or (typep obj 'active-object/weapon)
			(typep obj 'active-object/ammo))
		t)))


(define-store '<temple>
    :name "Temple"
    :number 4
    :sells '((obj :id "whip")
	     (obj :id "quarterstaff")
	     (obj :id "mace" :weight 2)
	     (obj :id "ball-and-chain")
	     (obj :id "war-hammer")
	     (obj :id "lucerne-hammer")
	     (obj :id "morning-star")
	     (obj :id "flail" :weight 2)
	     (obj :id "lead-mace")
	     (obj :id "scroll-remove-curse")
	     (obj :id "scroll-blessing")
	     (obj :id "scroll-holy-chant")
	     (obj :id "potion-boldness")
	     (obj :id "potion-heroism")
	     (obj :id "potion-cure-light")
	     (obj :id "potion-cure-serious" :weight 2)
	     (obj :id "potion-cure-critical" :weight 2)
	     (obj :id "potion-restore-xp" :weight 3)
	     ;; prayer books
	     )
    :buys #'(lambda (obj store)
	      (declare (ignore store))
	      (cond ((or (typep obj 'active-object/hafted)
			 (typep obj 'active-object/scroll)
			 (typep obj 'active-object/potion)
			 (typep obj 'active-object/prayerbook))
		     t)

		    ((or (typep obj 'active-object/weapon))
		     ;; check if blessed
		     nil)
		    
		    (t
		     nil))))


(define-store '<alchemist>
    :name "Alchemist"
    :number 5
    :sells '((obj :id "scroll-enchant-wpn-hit")
	     (obj :id "scroll-enchant-wpn-dmg")
	     (obj :id "scroll-enchant-armour")
	     (obj :id "scroll-identify" :weight 4)
	     (obj :id "scroll-illumination")
	     (obj :id "scroll-phase-door" :weight 3)
	     (obj :id "scroll-monster-confusion")
	     (obj :id "scroll-mapping" :weight 2)
	     (obj :id "scroll-det-gold")
	     (obj :id "scroll-det-item")
	     (obj :id "scroll-det-trap")
	     (obj :id "scroll-det-door")
	     (obj :id "scroll-det-inv")
	     (obj :id "scroll-recharging")
	     (obj :id "scroll-satisfy-hunger")
	     (obj :id "scroll-wor" :weight 4)
	     (obj :id "potion-resist-heat")
	     (obj :id "potion-resist-cold")
	     (obj :id "potion-restore-str")
	     (obj :id "potion-restore-int")
	     (obj :id "potion-restore-wis")
	     (obj :id "potion-restore-dex")
	     (obj :id "potion-restore-con")
	     (obj :id "potion-restore-chr")
	     )
    :buys '(<potion> <scroll>))
	     

(define-store '<magic-shop>
    :name "Magic Shop"
    :number 6
    :sells '((obj :id "ring-searching")
	     (obj :id "ring-feather-fall")
	     (obj :id "ring-protection")
	     (obj :id "amulet-chr")
	     (obj :id "amulet-slow-digest")
	     (obj :id "amulet-resist-acid")
	     (obj :id "wand-slow-monster")
	     (obj :id "wand-confuse-monster")
	     (obj :id "wand-sleep-monster")
	     (obj :id "wand-magic-missile")
	     (obj :id "wand-stinking-cloud")
	     (obj :id "wand-wonder")
	     (obj :id "staff-light")
	     (obj :id "staff-mapping")
	     (obj :id "staff-det-trap")
	     (obj :id "staff-det-door")
	     (obj :id "staff-det-gold")
	     (obj :id "staff-det-item")
	     (obj :id "staff-det-inv")
	     (obj :id "staff-det-evil")
	     (obj :id "staff-teleport" :weight 2)
	     (obj :id "staff-identify" :weight 2)
	     ;; spellbooks
	     )
    :buys '(<potion> <scroll> <ring> <amulet> <staff> <rod> <wand> <spellbook>))

(define-store '<black-market>
    :name "Black Market"
    :type 'black-market
    :number 7
    :buys t)

(define-house '<home>
    :name "Player's home"
    :type 'players-home
    :number 8
    :owner :player)


;;; ===== general stores =====
(define-store-owner
    :store-type '<general-store>
    :id "bilbo"
    :name "Bilbo the Friendly"
    :purse 200
    :max-greed 175
    :min-greed 108
    :haggle-num 4
    :tolerance 12
    :picture "people/male-hobbit-rogue.png"
    :race '<hobbit>)

(define-store-owner
    :store-type '<general-store>
    :id '"rincewind"
    :name "Rincewind the Chicken"
    :purse 200
    :max-greed 175
    :min-greed 108
    :haggle-num 4
    :tolerance 12
    :picture "people/male-human-warrior.png"
    :race '<human>)

(define-store-owner
    :store-type '<general-store>
    :id "snafu"
    :name "Snafu the Midget"
    :purse 300
    :max-greed 170
    :min-greed 107
    :haggle-num 5
    :tolerance 15
    :picture "people/male-dwarf-warrior.png"
    :race '<dwarf>)

(define-store-owner
    :store-type '<general-store>
    :id "lyar"
    :name "Lyar-el the Comely"
    :purse 300
    :max-greed 165
    :min-greed 107
    :haggle-num 6
    :tolerance 18
    :picture "people/female-elf-rogue.png"
    :race '<elf>)

;;; ===== armouries =====

(define-store-owner
    :store-type '<armoury>
    :id "kon-dar"
    :name "Kon-Dar the Ugly"
    :purse 5000
    :max-greed 210
    :min-greed 115
    :haggle-num 5
    :tolerance 7
    :picture "people/male-halforc-ranger.png"
    :race '<half-orc>)

(define-store-owner
    :store-type '<armoury>
    :id "darg-low"
    :name "Darg-Low the Grim"
    :purse 10000
    :max-greed 190
    :min-greed 111
    :haggle-num 4
    :tolerance 9
    :picture "people/male-human-warrior.png"
    :race '<human>)

(define-store-owner
    :store-type '<armoury>
    :id "decado"
    :name "Decado the Handsome"
    :purse 25000
    :max-greed 200
    :min-greed 112
    :haggle-num 4
    :tolerance 10
    :picture "people/male-human-warrior.png"
    :race '<dunedan>)

(define-store-owner
    :store-type '<armoury>
    :id "mauglin"
    :name "Mauglin the Grumpy"
    :purse 30000
    :max-greed 200
    :min-greed 112
    :haggle-num 4
    :tolerance 5
    :picture "people/male-dwarf-warrior.png"
    :race '<dwarf>)

;;; ===== weapon-smiths =====

(define-store-owner
    :store-type '<weapon-smith>
    :id "ithyl"
    :name "Ithyl-Mak the Beastly"
    :purse 5000
    :max-greed 210
    :min-greed 115
    :haggle-num 6
    :tolerance 6
    :picture "people/male-halftroll-warrior.png"
    :race '<half-troll>)

(define-store-owner
    :store-type '<weapon-smith>
    :id "arndal"
    :name "Arndal Beast-Slayer"
    :purse 10000
    :max-greed 185
    :min-greed 110
    :haggle-num 5
    :tolerance 9
    :picture "people/male-elf-warrior.png"
    :race '<elf>)

(define-store-owner
    :store-type '<weapon-smith>
    :id "tarl"
    :name "Tarl Beast-Master"
    :purse 25000
    :max-greed 190
    :min-greed 115
    :haggle-num 5
    :tolerance 7
    :picture "people/male-hobbit-rogue.png"
    :race '<hobbit>)

(define-store-owner
    :store-type '<weapon-smith>
    :id "oglign"
    :name "Oglign Dragon-Slayer"
    :purse 30000
    :max-greed 195
    :min-greed 112
    :haggle-num 4
    :tolerance 8
    :picture "people/male-dwarf-warrior.png"
    :race '<dwarf>)

;;; ===== temples =====

(define-store-owner
    :store-type '<temple>
    :id "ludwig"
    :name "Ludwig the Humble"
    :purse 15000
    :max-greed 175
    :min-greed 109
    :haggle-num 6
    :tolerance 15
    :picture "people/male-human-warrior.png"
    :race '<human>)

(define-store-owner
    :store-type '<temple>
    :id "gunnar"
    :name "Gunnar the Paladin"
    :purse 20000
    :max-greed 185
    :min-greed 110
    :haggle-num 5
    :tolerance 23
    :picture "people/male-human-warrior.png"
    :race '<human>)

(define-store-owner
    :store-type '<temple>
    :id "delilah"
    :name "Delilah the Pure"
    :purse 25000
    :max-greed 180
    :min-greed 107
    :haggle-num 6
    :tolerance 20
    :picture "people/female-elf-rogue.png"
    :race '<elf>)

(define-store-owner
    :store-type '<temple>
    :id "bosk"
    :name "Bosk the Wise"
    :purse 30000
    :max-greed 185
    :min-greed 109
    :haggle-num 5
    :tolerance 15
    :picture "people/male-dwarf-warrior.png"
    :race '<dwarf>)

;;; ===== alchemists =====

(define-store-owner
    :store-type '<alchemist>
    :id "mauser"
    :name "Mauser the Chemist"
    :purse 10000
    :max-greed 190
    :min-greed 111
    :haggle-num 5
    :tolerance 8
    :picture "people/male-highelf-mage.png"
    :race '<half-elf>)

(define-store-owner
    :store-type '<alchemist>
    :id "wizzle"
    :name "Wizzle the Chaotic"
    :purse 10000
    :max-greed 190
    :min-greed 111
    :haggle-num 6
    :tolerance 8
    :picture "people/male-hobbit-rogue.png"
    :race '<hobbit>)

(define-store-owner
    :store-type '<alchemist>
    :id "ganat"
    :name "Ga-nat the Greedy"
    :purse 15000
    :max-greed 200
    :min-greed 116
    :haggle-num 6
    :tolerance 9
    :picture "people/male-gnome-rogue.png"
    :race '<gnome>)

(define-store-owner
    :store-type '<alchemist>
    :id "vella"
    :name "Vella the Slender"
    :purse 15000
    :max-greed 220
    :min-greed 111
    :haggle-num 4
    :tolerance 9
    :picture "people/female-human-bard.png"
    :race '<human>)


;;; ===== magic shops =====

(define-store-owner
    :store-type '<magic-shop>
    :id "ariel"
    :name "Ariel the Sorceress"
    :purse 15000
    :max-greed 200
    :min-greed 110
    :haggle-num 7
    :tolerance 8
    :picture "people/female-halfelf-ranger.png"
    :race '<half-elf>)

(define-store-owner
    :store-type '<magic-shop>
    :id "buggerby"
    :name "Buggerby the Great"
    :purse 20000
    :max-greed 215
    :min-greed 113
    :haggle-num 6
    :tolerance 10
    :picture "people/male-gnome-rogue.png"
    :race '<gnome>)

(define-store-owner
    :store-type '<magic-shop>
    :id "inglorian"
    :name "Inglorian the Mage"
    :purse 25000
    :max-greed 200
    :min-greed 110
    :haggle-num 7
    :tolerance 10
    :picture "people/male-human-warrior.png"
    :race '<human>)

(define-store-owner
    :store-type '<magic-shop>
    :id "luthien"
    :name "Luthien Starshine"
    :purse 30000
    :max-greed 175
    :min-greed 110
    :haggle-num 5
    :tolerance 11
    :picture "people/female-highelf-ranger.png"
    :race '<high-elf>)

;;; ===== black markets =====

(define-store-owner
    :store-type '<black-market>
    :id "lohak"
    :name "Lo-Hak the Awful"
    :purse 15000
    :max-greed 250
    :min-greed 150
    :haggle-num 10
    :tolerance 5
    :picture "people/male-halftroll-warrior.png"
    :race '<half-troll>)

(define-store-owner
    :store-type '<black-market>
    :id "histor"
    :name "Histor the Brute"
    :purse 20000
    :max-greed 250
    :min-greed 150
    :haggle-num 10
    :tolerance 5
    :picture "people/male-halforc-ranger.png"
    :race '<half-orc>)

(define-store-owner
    :store-type '<black-market>
    :id "durwin"
    :name "Durwin the Shifty"
    :purse 30000
    :max-greed 250
    :min-greed 150
    :haggle-num 10
    :tolerance 5
    :picture "people/male-dwarf-warrior.png"
    :race '<dwarf>)

(define-store-owner
    :store-type '<black-market>
    :id "drago"
    :name "Drago the Fair"
    :purse 30000
    :max-greed 250
    :min-greed 150
    :haggle-num 10
    :tolerance 5
    :picture "people/male-highelf-mage.png"
    :race '<elf>)

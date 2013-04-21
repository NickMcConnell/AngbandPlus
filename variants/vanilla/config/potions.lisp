;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

					#|

DESC: variants/vanilla/config/potions.lisp - potions for vanilla variant
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software  ; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation	 ; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(define-object-kind "slime-mold-juice" "slime mold juice"
  :numeric-id 222
  :text-attr #\d
  :text-char #\!
  :power-lvl 0
  :locations '((0 . 1))
  :weight 4
  :cost 2
  :sort-value 5502
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (print-message! "You feel less thirsty!")
	      (possible-identify! player item)
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 400)) 

(define-object-kind "apple-juice" "apple juice"
  :numeric-id 223
  :text-attr #\d
  :text-char #\!
  :power-lvl 0
  :locations '((0 . 1))
  :weight 4
  :cost 1
  :sort-value 5501
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (print-message! "You feel less thirsty!")
	      (possible-identify! player item)
	      :used)
  
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 250)) 

(define-object-kind "water" "water"
  :numeric-id 224
  :text-attr #\d
  :text-char #\!
  :power-lvl 0
  :locations '((0 . 1))
  :weight 4
  :cost 1
  :sort-value 5500
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (print-message! "You feel less thirsty!")
	      (possible-identify! player item)
	      :used)
  
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 200)) 

(define-object-kind "potion-str" "strength"
  :numeric-id 225
  :text-attr #\d
  :text-char #\!
  :power-lvl 30
  :locations '((30 . 1))
  :weight 4
  :cost 8000
  :sort-value 5548
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<str> '<increase>)
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-reduce-str" "weakness"
  :numeric-id 226
  :text-attr #\d
  :text-char #\!
  :power-lvl 3
  :locations '((3 . 1))
  :weight 4
  :cost 0
  :sort-value 5516
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<str> '<reduce>)
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-restore-str" "restore strength"
  :numeric-id 227
  :text-attr #\d
  :text-char #\!
  :power-lvl 25
  :locations '((25 . 1))
  :weight 4
  :cost 300
  :sort-value 5542
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<str> '<restore>)
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-int" "intelligence"
  :numeric-id 228
  :text-attr #\d
  :text-char #\!
  :power-lvl 30
  :locations '((30 . 1))
  :weight 4
  :cost 8000
  :sort-value 5549
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<str> '<increase>)
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-reduce-int" "stupidity"
  :numeric-id 229
  :text-attr #\d
  :text-char #\!
  :power-lvl 20
  :locations '((20 . 1))
  :weight 4
  :cost 0
  :sort-value 5517
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<int> '<reduce>)
		(possible-identify! player item))
	      :used)
  
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-restore-int" "restore intelligence"
  :numeric-id 230
  :text-attr #\d
  :text-char #\!
  :power-lvl 25
  :locations '((25 . 1))
  :weight 4
  :cost 300
  :sort-value 5543
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<int> '<restore>)
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-wis" "wisdom"
  :numeric-id 231
  :text-attr #\d
  :text-char #\!
  :power-lvl 30
  :locations '((30 . 1))
  :weight 4
  :cost 8000
  :sort-value 5550
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<wis> '<increase>)
		(possible-identify! player item))
	      :used)
  
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-reduce-wis" "naivety"
  :numeric-id 232
  :text-attr #\d
  :text-char #\!
  :power-lvl 20
  :locations '((20 . 1))
  :weight 4
  :cost 0
  :sort-value 5518
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<wis> '<reduce>)
		(possible-identify! player item))
	      :used)
  
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-restore-wis" "restore wisdom"
  :numeric-id 233
  :text-attr #\d
  :text-char #\!
  :power-lvl 25
  :locations '((25 . 1))
  :weight 4
  :cost 300
  :sort-value 5544
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<wis> '<restore>)
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-chr" "charisma"
  :numeric-id 234
  :text-attr #\d
  :text-char #\!
  :power-lvl 20
  :locations '((20 . 1))
  :weight 4
  :cost 1000
  :sort-value 5553
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<str> '<increase>)
		(possible-identify! player item))
	      :used)
  
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-reduce-chr" "ugliness"
  :numeric-id 235
  :text-attr #\d
  :text-char #\!
  :power-lvl 20
  :locations '((20 . 1))
  :weight 4
  :cost 0
  :sort-value 5521
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<chr> '<reduce>)
		(possible-identify! player item))
	      :used)
  
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-restore-chr" "restore charisma"
  :numeric-id 236
  :text-attr #\d
  :text-char #\!
  :power-lvl 20
  :locations '((20 . 1))
  :weight 4
  :cost 300
  :sort-value 5547
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<chr> '<restore>)
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-cure-light" "cure light wounds"
  :numeric-id 237
  :text-attr #\d
  :text-char #\!
  :power-lvl 0
  :locations '((0 . 1) (1 . 1) (3 . 1))
  :weight 4
  :cost 15
  :sort-value 5534
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (let ((amount (roll-dice 2 8)))
		(when (heal-creature! player amount)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<blindness> :new-value nil)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<cut> :subtract '<light>)
		  (possible-identify! player item))
		:used))
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 50)) 

(define-object-kind "potion-reduce-dex" "clumsiness"
  :numeric-id 238
  :text-attr #\d
  :text-char #\!
  :power-lvl 5
  :locations '((5 . 1))
  :weight 4
  :cost 0
  :sort-value 5519
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<dex> '<reduce>)
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-reduce-con" "sickliness"
  :numeric-id 239
  :text-attr #\d
  :text-char #\!
  :power-lvl 10
  :locations '((10 . 1))
  :weight 4
  :cost 0
  :sort-value 5520
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<con> '<reduce>)
		(possible-identify! player item))
	      :used)

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-cure-serious" "cure serious wounds"
  :numeric-id 240
  :text-attr #\d
  :text-char #\!
  :power-lvl 3
  :locations '((3 . 1))
  :weight 4
  :cost 40
  :sort-value 5535
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (let ((amount (roll-dice 4 8)))
		(when (heal-creature! player amount)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<blindness> :new-value nil)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<confusion> :new-value nil)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<cut> :subtract '<serious>)
		  (possible-identify! player item))
		:used))
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 100)) 

(define-object-kind "potion-cure-critical" "cure critical wounds"
  :numeric-id 241
  :text-attr #\d
  :text-char #\!
  :power-lvl 5
  :locations '((5 . 1))
  :weight 4
  :cost 100
  :sort-value 5536
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (let ((amount (roll-dice 6 8)))
		(when (heal-creature! player amount)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<blindness> :new-value nil)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<confusion> :new-value nil)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<poisoned> :new-value nil)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<cut> :new-value nil)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<stun> :new-value nil)
		  (possible-identify! player item))
		:used))
  
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 100)) 

(define-object-kind "potion-healing" "healing"
  :numeric-id 242
  :text-attr #\d
  :text-char #\!
  :power-lvl 15
  :locations '((15 . 1))
  :weight 4
  :cost 300
  :sort-value 5537
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (let ((amount 300))
		(when (heal-creature! player amount)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<blindness> :new-value nil)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<confusion> :new-value nil)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<poisoned>  :new-value nil)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<cut> :new-value nil)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<stun> :new-value nil)
		  (possible-identify! player item))
		:used))
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 200)) 

(define-object-kind "potion-con" "constitution"
  :numeric-id 243
  :text-attr #\d
  :text-char #\!
  :power-lvl 30
  :locations '((30 . 1))
  :weight 4
  :cost 8000
  :sort-value 5552
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<con> '<increase>)
		(possible-identify! player item))
	      :used)
  
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-xp" "experience"
  :numeric-id 244
  :text-attr #\d
  :text-char #\!
  :power-lvl 65
  :locations '((65 . 1))
  :weight 4
  :cost 25000
  :sort-value 5559
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-sleep" "sleep"
  :numeric-id 245
  :text-attr #\d
  :text-char #\!
  :power-lvl 0
  :locations '((0 . 1))
  :weight 4
  :cost 0
  :sort-value 5511
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (unless (eq t (get-creature-state player '<free-action>))
		(when (modify-creature-state! player '<paralysed> :add (+ 4 (randint 4)))
		  (possible-identify! player item)))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 100)) 

(define-object-kind "potion-blindness" "blindness"
  :numeric-id 246
  :text-attr #\d
  :text-char #\!
  :power-lvl 0
  :locations '((0 . 1))
  :weight 4
  :cost 0
  :sort-value 5507
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (unless (resists-element? player '<blindness>)
		(when (modify-creature-state! player '<blindness> :add (+ 100 (random 100)))
		  (possible-identify! player item)))
	      :used)

  :game-values (make-game-values :base-dice 1 :num-dice 1))

(define-object-kind "potion-confusion" "confusion"
  :numeric-id 247
  :text-attr #\d
  :text-char #\!
  :power-lvl 0
  :locations '((0 . 1))
  :weight 4
  :cost 0
  :sort-value 5509
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (unless (resists-element? player '<confusion>)
		(when (modify-creature-state! player '<confusion> :add (+ 15 (random 20)))
		  (possible-identify! player item)))
	      :used)
  
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 50)) 

(define-object-kind "potion-poison" "poison"
  :numeric-id 248
  :text-attr #\d
  :text-char #\!
  :power-lvl 3
  :locations '((3 . 1))
  :weight 4
  :cost 0
  :sort-value 5506
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (unless (resists-element? player '<poison>)
		(when (modify-creature-state! player '<poisoned> :add (+ 10 (random 15)))
		  (possible-identify! player item)))
	      :used)
  
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-speed" "speed"
  :numeric-id 249
  :text-attr #\d
  :text-char #\!
  :power-lvl 1
  :locations '((1 . 1) (40 . 1))
  :weight 4
  :cost 75
  :sort-value 5529
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      ;; FIX, check if he's already hasted.  also fix for potion and staff
	      (when (haste-creature! player +10 (+ 15 (random 25)))
		(possible-identify! player item))
	      :used)
  
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-slowness" "slowness"
  :numeric-id 250
  :text-attr #\d
  :text-char #\!
  :power-lvl 1
  :locations '((1 . 1))
  :weight 4
  :cost 0
  :sort-value 5504
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (modify-creature-state! player '<slowed> :add (+ 15 (random 25)))
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 50)) 

(define-object-kind "potion-dex" "dexterity"
  :numeric-id 251
  :text-attr #\d
  :text-char #\!
  :power-lvl 30
  :locations '((30 . 1))
  :weight 4
  :cost 8000
  :sort-value 5551
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<dex> '<increase>)
		(possible-identify! player item))
	      :used)
  
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-restore-dex" "restore dexterity"
  :numeric-id 252
  :text-attr #\d
  :text-char #\!
  :power-lvl 25
  :locations '((25 . 1))
  :weight 4
  :cost 300
  :sort-value 5545
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<dex> '<restore>)
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-restore-con" "restore constitution"
  :numeric-id 253
  :text-attr #\d
  :text-char #\!
  :power-lvl 25
  :locations '((25 . 1))
  :weight 4
  :cost 300
  :sort-value 5546
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<con> '<restore>)
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-lose-memory" "lose memories"
  :numeric-id 254
  :text-attr #\d
  :text-char #\!
  :power-lvl 10
  :locations '((10 . 1))
  :weight 4
  :cost 0
  :sort-value 5513
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "salt-water" "salt water"
  :numeric-id 255
  :text-attr #\d
  :text-char #\!
  :power-lvl 0
  :locations '((0 . 1))
  :weight 4
  :cost 0
  :sort-value 5505
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-self-know" "enlightenment"
  :numeric-id 256
  :text-attr #\d
  :text-char #\!
  :power-lvl 25
  :locations '((25 . 1))
  :weight 4
  :cost 800
  :sort-value 5556
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-heroism" "heroism"
  :numeric-id 257
  :text-attr #\d
  :text-char #\!
  :power-lvl 1
  :locations '((1 . 1))
  :weight 4
  :cost 35
  :sort-value 5532
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (heal-creature! player 10)
		(possible-identify! player item))
	      (when (modify-creature-state! player '<fear> :new-value nil)
		(possible-identify! player item))
	      (when (modify-creature-state! player '<heroic> :add (+ 25 (randint 25))) ;; increase effect
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-berserk" "berserk strength"
  :numeric-id 258
  :text-attr #\d
  :text-char #\!
  :power-lvl 3
  :locations '((3 . 1))
  :weight 4
  :cost 100
  :sort-value 5533
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (heal-creature! player 30)
		(possible-identify! player item))
	      (when (modify-creature-state! player '<fear> :new-value nil)
		(possible-identify! player item))
	      (when (modify-creature-state! player '<berserk> :add (+ 25 (randint 25))) ;; increase effect
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-boldness" "boldness"
  :numeric-id 259
  :text-attr #\d
  :text-char #\!
  :power-lvl 1
  :locations '((1 . 1))
  :weight 4
  :cost 10
  :sort-value 5528
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (modify-creature-state! player '<fear> :new-value nil)
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-restore-xp" "restore life levels"
  :numeric-id 260
  :text-attr #\d
  :text-char #\!
  :power-lvl 40
  :locations '((40 . 1))
  :weight 4
  :cost 400
  :sort-value 5541
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-resist-heat" "resist heat"
  :numeric-id 261
  :text-attr #\d
  :text-char #\!
  :power-lvl 1
  :locations '((1 . 1))
  :weight 4
  :cost 30
  :sort-value 5530
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (modify-creature-state! player '<resist-fire> :add (+ 10 (random 10)))
		(possible-identify! player item))
	      :used)

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-resist-cold" "resist cold"
  :numeric-id 262
  :text-attr #\d
  :text-char #\!
  :power-lvl 1
  :locations '((1 . 1))
  :weight 4
  :cost 30
  :sort-value 5531
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (modify-creature-state! player '<resist-cold> :add (+ 10 (random 10)))
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-det-inv" "detect invisible"
  :numeric-id 263
  :text-attr #\d
  :text-char #\!
  :power-lvl 3
  :locations '((3 . 1))
  :weight 4
  :cost 50
  :sort-value 5525
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (modify-creature-state! player '<see-invisible> :add (+ 12 (random 12)))
		(possible-identify! player item))
	      :used)
  
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-slow-poison" "slow poison"
  :numeric-id 264
  :text-attr #\d
  :text-char #\!
  :power-lvl 1
  :locations '((1 . 1))
  :weight 4
  :cost 25
  :sort-value 5526
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (modify-creature-state! player '<poisoned> :subtract '<half>)
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-cure-poison" "neutralize poison"
  :numeric-id 265
  :text-attr #\d
  :text-char #\!
  :power-lvl 5
  :locations '((5 . 1))
  :weight 4
  :cost 75
  :sort-value 5527
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when  (modify-creature-state! player '<poisoned> :new-value nil)
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-restore-mana" "restore mana"
  :numeric-id 266
  :text-attr #\d
  :text-char #\!
  :power-lvl 25
  :locations '((25 . 1))
  :weight 4
  :cost 350
  :sort-value 5540
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (< (current-mana player) (maximum-mana player))
		(setf (current-mana player) (maximum-mana player))
		(setf (player.fraction-mana player) 0)
		(print-message! "You feel your head clear.")
		(bit-flag-add! *redraw* +print-mana+)
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-infravision" "infravision"
  :numeric-id 267
  :text-attr #\d
  :text-char #\!
  :power-lvl 3
  :locations '((3 . 1))
  :weight 4
  :cost 20
  :sort-value 5524
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (modify-creature-state! player '<infravision> :add (+ 100 (random 100)))
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-death" "death"
  :numeric-id 415
  :text-attr #\d
  :text-char #\!
  :power-lvl 55
  :locations '((55 . 4))
  :weight 4
  :cost 0
  :sort-value 5523
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-ruination" "ruination"
  :numeric-id 416
  :text-attr #\d
  :text-char #\!
  :power-lvl 40
  :locations '((40 . 8))
  :weight 4
  :cost 0
  :sort-value 5515
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-detonations" "detonations"
  :numeric-id 417
  :text-attr #\d
  :text-char #\!
  :power-lvl 60
  :locations '((60 . 8))
  :weight 4
  :cost 10000
  :sort-value 5522
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 25 :num-dice 25)) 

(define-object-kind "potion-augmentation" "augmentation"
  :numeric-id 418
  :text-attr #\d
  :text-char #\!
  :power-lvl 40
  :locations '((40 . 16))
  :weight 4
  :cost 60000
  :sort-value 5555
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (when (update-player-stat! player '<str> '<increase>)
		(possible-identify! player item))
	      (when (update-player-stat! player '<dex> '<increase>)
		(possible-identify! player item))
	      (when (update-player-stat! player '<con> '<increase>)
		(possible-identify! player item))
	      (when (update-player-stat! player '<int> '<increase>)
		(possible-identify! player item))
	      (when (update-player-stat! player '<wis> '<increase>)
		(possible-identify! player item))
	      (when (update-player-stat! player '<chr> '<increase>)
		(possible-identify! player item))
	      :used)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-*healing*" "*healing*"
  :numeric-id 419
  :text-attr #\d
  :text-char #\!
  :power-lvl 40
  :locations '((40 . 4))
  :weight 4
  :cost 1500
  :sort-value 5538
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (let ((amount 1200))
		(when (heal-creature! player amount)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<blindness> :new-value nil)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<confusion> :new-value nil)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<poisoned> :new-value nil)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<cut> :new-value nil)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<stun> :new-value nil)
		  (possible-identify! player item))
		:used))  
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-life" "life"
  :numeric-id 420
  :text-attr #\d
  :text-char #\!
  :power-lvl 60
  :locations '((60 . 4))
  :weight 4
  :cost 5000
  :sort-value 5539
  :the-kind '<potion>
  :on-quaff (object-effect (dungeon player item)
	      (let ((amount 5000))
		(print-message! "You feel life flow through your body.")
		;; fix, add restore levels
		(heal-creature! player amount)
		
		(modify-creature-state! player '<blindness>   :new-value nil)
		(modify-creature-state! player '<confusion>   :new-value nil)
		(modify-creature-state! player '<poisoned>    :new-value nil)
		(modify-creature-state! player '<hallucinate> :new-value nil)
		
		(modify-creature-state! player '<cut>  :new-value nil)
		(modify-creature-state! player '<stun> :new-value nil)
		     
		
		(update-player-stat! player '<str> '<restore>)
		(update-player-stat! player '<dex> '<restore>)
		(update-player-stat! player '<con> '<restore>)
		(update-player-stat! player '<int> '<restore>)
		(update-player-stat! player '<wis> '<restore>)
		(update-player-stat! player '<chr> '<restore>)
		(possible-identify! player item)
		:used))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-self-knowledge" "self knowledge"
  :numeric-id 421
  :text-attr #\d
  :text-char #\!
  :power-lvl 40
  :locations '((40 . 1))
  :weight 4
  :cost 2000
  :sort-value 5558
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-*enlightenment*" "*enlightenment*"
  :numeric-id 422
  :text-attr #\d
  :text-char #\!
  :power-lvl 70
  :locations '((70 . 4))
  :weight 4
  :cost 80000
  :sort-value 5557
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/sticks.lisp - wands/rods/staves for vanilla variant
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

;;; Wands

(define-object-kind "wand-light" "light"
  :numeric-id 269
  :x-attr #\d
  :x-char #\-
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 10
  :cost 200
  :sort-value 4807
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 10) 6)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-lightning-bolt" "lightning bolts"
  :numeric-id 270
  :x-attr #\d
  :x-char #\-
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 10
  :cost 600
  :sort-value 4817
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 6)))

  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-bolt-or-beam! player 20 dir (get-spell-effect '<electricity>)
					(roll-dice 6 6))
		(possible-identify! player item)
		:still-useful))

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-frost-bolt" "frost bolts"
  :numeric-id 271
  :x-attr #\d
  :x-char #\-
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 10
  :cost 800
  :sort-value 4819
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		   (add-charges! item (+ (randint 5) 6)))

  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-bolt-or-beam! player 20 dir (get-spell-effect '<cold>)
					(roll-dice 6 8))
		(possible-identify! player item)
		:still-useful))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-fire-bolts" "fire bolts"
  :numeric-id 272
  :x-attr #\d
  :x-char #\-
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 10
  :cost 1000
  :sort-value 4818
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 6)))

  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-bolt-or-beam! player 20 dir (get-spell-effect '<fire>)
					(roll-dice 12 8))
		(possible-identify! player item)
		:still-useful))

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-stone-mud" "stone to mud"
  :numeric-id 273
  :x-attr #\d
  :x-char #\-
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 10
  :cost 300
  :sort-value 4806
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 4) 3)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-polymorph" "polymorph"
  :numeric-id 274
  :x-attr #\d
  :x-char #\-
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 10
  :cost 400
  :sort-value 4813
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 6)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-heal-monster" "heal monster"
  :numeric-id 275
  :x-attr #\d
  :x-char #\-
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 10
  :cost 0
  :sort-value 4800
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 20) 8)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-haste-monster" "haste monster"
  :numeric-id 276
  :x-attr #\d
  :x-char #\-
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 10
  :cost 0
  :sort-value 4801
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 20) 8)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-slow-monster" "slow monster"
  :numeric-id 277
  :x-attr #\d
  :x-char #\-
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 10
  :cost 500
  :sort-value 4809
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 10) 6)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-confuse-monster" "confuse monster"
  :numeric-id 278
  :x-attr #\d
  :x-char #\-
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 10
  :cost 500
  :sort-value 4810
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 12) 6)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-sleep-monster" "sleep monster"
  :numeric-id 279
  :x-attr #\d
  :x-char #\-
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 10
  :cost 500
  :sort-value 4808
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 15) 8)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-drain-life" "drain life"
  :numeric-id 280
  :x-attr #\d
  :x-char #\-
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 10
  :cost 1200
  :sort-value 4812
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 3)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-destroy-door" "trap/door destruction"
  :numeric-id 281
  :x-attr #\d
  :x-char #\-
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 10
  :cost 100
  :sort-value 4805
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 6)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-magic-missile" "magic missile"
  :numeric-id 282
  :x-attr #\d
  :x-char #\-
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 10
  :cost 200
  :sort-value 4815
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 10) 6)))

  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-bolt-or-beam! player 20 dir (get-spell-effect '<magic-missile>)
					(roll-dice 3 4))
		(possible-identify! player item)
		:still-useful))

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-clone-monster" "clone monster"
  :numeric-id 283
  :x-attr #\d
  :x-char #\-
  :depth 15
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(15 50 0 0)
  :weight 10
  :cost 0
  :sort-value 4802
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 3)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-scare-monster" "scare monster"
  :numeric-id 284
  :x-attr #\d
  :x-char #\-
  :depth 10
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(10 0 0 0)
  :weight 10
  :cost 500
  :sort-value 4811
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 3)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-teleport-other" "teleport other"
  :numeric-id 285
  :x-attr #\d
  :x-char #\-
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 10
  :cost 350
  :sort-value 4803
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 6)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-disarming" "disarming"
  :numeric-id 286
  :x-attr #\d
  :x-char #\-
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 10
  :cost 700
  :sort-value 4804
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 4)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-lightning-balls" "lightning balls"
  :numeric-id 287
  :x-attr #\d
  :x-char #\-
  :depth 35
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(35 0 0 0)
  :weight 10
  :cost 1200
  :sort-value 4821
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 4)))

  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-ball! player dir (get-spell-effect '<electricity>) 64 2)
		(possible-identify! player item)
		))
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<electricity>))) 

(define-object-kind "wand-cold-balls" "cold balls"
  :numeric-id 288
  :x-attr #\d
  :x-char #\-
  :depth 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 10
  :cost 1500
  :sort-value 4823
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 6) 2)))

  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-ball! player dir (get-spell-effect '<cold>) 96 2)
		(possible-identify! player item)
		))
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold>))) 

(define-object-kind "wand-fire-balls" "fire balls"
  :numeric-id 289
  :x-attr #\d
  :x-char #\-
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 10
  :cost 1800
  :sort-value 4822
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 4) 2)))

  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-ball! player dir (get-spell-effect '<fire>) 144 2)
		(possible-identify! player item)
		))
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<fire>))) 

(define-object-kind "wand-stinking-cloud" "stinking cloud"
  :numeric-id 290
  :x-attr #\d
  :x-char #\-
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 10
  :cost 400
  :sort-value 4814
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 6)))

  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-ball! player dir (get-spell-effect '<poison>) 12 2)
		(possible-identify! player item)
		))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-acid-balls" "acid balls"
  :numeric-id 291
  :x-attr #\d
  :x-char #\-
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 10
  :cost 1650
  :sort-value 4820
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 2)))
  
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-ball! player dir (get-spell-effect '<acid>) 120 2)
		(possible-identify! player item)
		))
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<acid>))) 

(define-object-kind "wand-wonder" "wonder"
  :numeric-id 292
  :x-attr #\d
  :x-char #\-
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 10
  :cost 250
  :sort-value 4824
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 15) 8)))
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "wand-acid-bolts" "acid bolts"
  :numeric-id 294
  :x-attr #\d
  :x-char #\-
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 10
  :cost 950
  :sort-value 4816
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 6)))

  :on-zap (object-effect (dungeon player item)
	     (when-bind (dir (%read-direction))
		(van-fire-bolt-or-beam! player 20 dir (get-spell-effect '<acid>)
					(roll-dice 10 8))
		(possible-identify! player item)
		:still-useful))

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-dragon-flame" "dragon's flame"
  :numeric-id 295
  :x-attr #\d
  :x-char #\-
  :depth 50
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(50 0 0 0)
  :weight 10
  :cost 2400
  :sort-value 4826
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 1)))
  
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-ball! player dir (get-spell-effect '<fire>) 200 3)
		(possible-identify! player item)
		))

  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "wand-dragon-frost" "dragon's frost"
  :numeric-id 296
  :x-attr #\d
  :x-char #\-
  :depth 50
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(50 0 0 0)
  :weight 10
  :cost 2400
  :sort-value 4827
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 1)))
  
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-ball! player dir (get-spell-effect '<cold>) 160 3)
		(possible-identify! player item)
		))

  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "wand-dragon-breath" "dragon's breath"
  :numeric-id 297
  :x-attr #\d
  :x-char #\-
  :depth 60
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(60 0 0 0)
  :weight 10
  :cost 2400
  :sort-value 4828
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 1)))
  
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(let ((which (randint 5)))
		  (ecase which
		    (1 (van-fire-ball! player dir (get-spell-effect '<acid>) 200 3))
		    (2 (van-fire-ball! player dir (get-spell-effect '<electricity>) 160 3))
		    (3 (van-fire-ball! player dir (get-spell-effect '<fire>) 200 3))
		    (4 (van-fire-ball! player dir (get-spell-effect '<cold>) 160 3))
		    (5 (van-fire-ball! player dir (get-spell-effect '<poison>) 120 3))))
		(possible-identify! player item)
		))

  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "wand-annihilation" "annihilation"
  :numeric-id 298
  :x-attr #\d
  :x-char #\-
  :depth 60
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(60 0 0 0)
  :weight 10
  :cost 3000
  :sort-value 4825
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 2) 1)))
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>)))

;;; Staves

(define-object-kind "staff-det-trap" "trap location"
  :numeric-id 300
  :x-attr #\d
  :x-char #\_
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 50
  :cost 350
  :sort-value 4612
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 6)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-det-gold" "treasure location"
  :numeric-id 301
  :x-attr #\d
  :x-char #\_
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 50
  :cost 200
  :sort-value 4610
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 20) 8)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-det-item" "object location"
  :numeric-id 302
  :x-attr #\d
  :x-char #\_
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 50
  :cost 200
  :sort-value 4611
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 15) 6)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-teleport" "teleportation"
  :numeric-id 303
  :x-attr #\d
  :x-char #\_
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 50
  :cost 2000
  :sort-value 4604
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 4) 5)))
  :on-zap (object-effect (dungeon player item)
	    (teleport-creature! dungeon player player 100)
	    (possible-identify! player item)
	    :still-useful)
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-earthquakes" "earthquakes"
  :numeric-id 304
  :x-attr #\d
  :x-char #\_
  :depth 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 50
  :cost 350
  :sort-value 4628
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 3)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-summoning" "summoning"
  :numeric-id 305
  :x-attr #\d
  :x-char #\_
  :depth 10
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(10 50 0 0)
  :weight 50
  :cost 0
  :sort-value 4603
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 1)))
  :on-zap (object-effect (dungeon player item)
	     (dotimes (i (randint 4))
	       (when (summon-monster dungeon
				     (location-x player) (location-y player)
				     (dungeon.depth dungeon) :type :any)
		 (possible-identify! player item))))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-light" "light"
  :numeric-id 306
  :x-attr #\d
  :x-char #\_
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 50
  :cost 250
  :sort-value 4608
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 20) 8)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-*destruction*" "*destruction*"
  :numeric-id 307
  :x-attr #\d
  :x-char #\_
  :depth 50
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(50 70 0 0)
  :weight 50
  :cost 2500
  :sort-value 4629
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 1)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-starlight" "starlight"
  :numeric-id 308
  :x-attr #\d
  :x-char #\_
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 50
  :cost 800
  :sort-value 4607
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 6)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-haste-monsters" "haste monsters"
  :numeric-id 309
  :x-attr #\d
  :x-char #\_
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 50
  :cost 0
  :sort-value 4602
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 8)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-slow-monsters" "slow monsters"
  :numeric-id 310
  :x-attr #\d
  :x-char #\_
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 50
  :cost 800
  :sort-value 4621
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 6)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-sleep-monsters" "sleep monsters"
  :numeric-id 311
  :x-attr #\d
  :x-char #\_
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 50
  :cost 700
  :sort-value 4620
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 6)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-cure-light" "cure light wounds"
  :numeric-id 312
  :x-attr #\d
  :x-char #\_
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 50
  :cost 350
  :sort-value 4616
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 6)))
  :on-zap (object-effect (dungeon player item)
	    (when (heal-creature! player  (randint 8))
	      (possible-identify! player item))
	    :still-useful)

  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-det-inv" "detect invisible"
  :numeric-id 313
  :x-attr #\d
  :x-char #\_
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 50
  :cost 200
  :sort-value 4614
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 15) 8)))
  :on-zap (object-effect (dungeon player item)
	    (when (detect-invisible! dungeon player (location-x player) (location-y player)
				     +default-detect-radius+)
	      (possible-identify! player item))
	    :still-useful)

  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-speed" "speed"
  :numeric-id 314
  :x-attr #\d
  :x-char #\_
  :depth 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 50
  :cost 1000
  :sort-value 4622
  :the-kind '<staff>
  :on-zap (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<hasted> :add (+ 15 (random 25)))
	      (possible-identify! player item))
	    :still-useful)
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 4)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-slowness" "slowness"
  :numeric-id 315
  :x-attr #\d
  :x-char #\_
  :depth 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 50
  :cost 0
  :sort-value 4601
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		   (add-charges! item (+ (randint 8) 8)))

  :on-zap (object-effect (dungeon player item)
	      (when (modify-creature-state! player '<slowed> :add (+ 15 (random 30)))
		(possible-identify! player item)
		:still-useful))

  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-det-door" "door/stair location"
  :numeric-id 316
  :x-attr #\d
  :x-char #\_
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 50
  :cost 350
  :sort-value 4613
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 6)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-remove-curse" "remove curse"
  :numeric-id 317
  :x-attr #\d
  :x-char #\_
  :depth 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 50
  :cost 500
  :sort-value 4606
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 4)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-det-evil" "detect evil"
  :numeric-id 318
  :x-attr #\d
  :x-char #\_
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 50
  :cost 350
  :sort-value 4615
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 15) 8)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-curing" "curing"
  :numeric-id 319
  :x-attr #\d
  :x-char #\_
  :depth 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 50
  :cost 1000
  :sort-value 4617
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 4)))
  :on-zap (object-effect (dungeon player item)
	    (modify-creature-state! player '<blindness> :new-value nil)
	    (modify-creature-state! player '<confusion> :new-value nil)
	    (modify-creature-state! player '<poisoned>  :new-value nil)
	    (modify-creature-state! player '<cut>       :new-value nil)
	    (modify-creature-state! player '<stun>      :new-value nil)
	    
	    (possible-identify! player item)
	    :still-useful)
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-dispel-evil" "dispel evil"
  :numeric-id 320
  :x-attr #\d
  :x-char #\_
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 50
  :cost 1200
  :sort-value 4624
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 4)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-probing" "probing"
  :numeric-id 321
  :x-attr #\d
  :x-char #\_
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 50
  :cost 2000
  :sort-value 4623
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 6) 2)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-darkness" "darkness"
  :numeric-id 322
  :x-attr #\d
  :x-char #\_
  :depth 5
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(5 50 0 0)
  :weight 50
  :cost 0
  :sort-value 4600
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 8)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-xenocide" "xenocide"
  :numeric-id 323
  :x-attr #\d
  :x-char #\_
  :depth 70
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(70 0 0 0)
  :weight 50
  :cost 3500
  :sort-value 4627
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 2) 1)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-power" "power"
  :numeric-id 324
  :x-attr #\d
  :x-char #\_
  :depth 70
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(70 0 0 0)
  :weight 50
  :cost 4000
  :sort-value 4625
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		  (add-charges! item (+ (randint 3) 1)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-magi" "the magi"
  :numeric-id 325
  :x-attr #\d
  :x-char #\_
  :depth 70
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(70 0 0 0)
  :weight 50
  :cost 4500
  :sort-value 4619
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 2) 2)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-identify" "perception"
  :numeric-id 326
  :x-attr #\d
  :x-char #\_
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 50
  :cost 400
  :sort-value 4605
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		  (add-charges! item (+ (randint 15) 5)))
  :on-zap (object-effect (dungeon player item)
	    (interactive-identify-object! dungeon player)
	    (possible-identify! player item)
	    :still-useful)

  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-holiness" "holiness"
  :numeric-id 327
  :x-attr #\d
  :x-char #\_
  :depth 70
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(70 0 0 0)
  :weight 50
  :cost 4500
  :sort-value 4626
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 2) 2)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-mapping" "enlightenment"
  :numeric-id 328
  :x-attr #\d
  :x-char #\_
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 50
  :cost 750
  :sort-value 4609
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 5)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-healing" "healing"
  :numeric-id 329
  :x-attr #\d
  :x-char #\_
  :depth 70
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(70 0 0 0)
  :weight 50
  :cost 5000
  :sort-value 4618
  :the-kind '<staff>
  :on-zap (object-effect (dungeon player item)
	    (let ((amount 300))
	      (when (heal-creature! player amount)
		(possible-identify! player item))
	      (when (modify-creature-state! player '<blindness> :new-value nil)
		(possible-identify! player item))
	      (when (modify-creature-state! player '<confusion> :new-value nil)
		(possible-identify! player item))
	      (when (modify-creature-state! player '<poisoned> :new-value nil)
		(possible-identify! player item))
	      (when (modify-creature-state! player '<cut>      :new-value nil)
		(possible-identify! player item))
	      (when (modify-creature-state! player '<stun>     :new-value nil)
		(possible-identify! player item))
	      :still-useful))
  
  :on-add-magic (magic-add (item depth status)
		  (add-charges! item (+ (randint 2) 1)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

;;; rods

(define-object-kind "rod-door-loc" "door/stair location"
  :numeric-id 351
  :x-attr #\d
  :x-char #\-
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 15
  :cost 1000
  :sort-value 4501
  :the-kind '<rod>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-trap-loc" "trap location"
  :numeric-id 352
  :x-attr #\d
  :x-char #\-
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 15
  :cost 100
  :sort-value 4500
  :the-kind '<rod>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-probing" "probing"
  :numeric-id 353
  :x-attr #\d
  :x-char #\-
  :depth 40
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(40 0 0 0)
  :weight 15
  :cost 4000
  :sort-value 4507
  :the-kind '<rod>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-recall" "recall"
  :numeric-id 354
  :x-attr #\d
  :x-char #\-
  :depth 30
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(30 0 0 0)
  :weight 15
  :cost 4000
  :sort-value 4503
  :the-kind '<rod>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-illumination" "illumination"
  :numeric-id 355
  :x-attr #\d
  :x-char #\-
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 15
  :cost 1000
  :sort-value 4504
  :the-kind '<rod>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-light" "light"
  :numeric-id 356
  :x-attr #\d
  :x-char #\-
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 15
  :cost 500
  :sort-value 4515
  :the-kind '<rod>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-lightning-bolts" "lightning bolts"
  :numeric-id 357
  :x-attr #\d
  :x-char #\-
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 15
  :cost 2000
  :sort-value 4521
  :the-kind '<rod>
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-bolt-or-beam! player 10 dir (get-spell-effect '<electricity>)
					(roll-dice 6 6))
		(possible-identify! player item)
		(setf (aobj.recharge-time item) 11))
	      :still-useful)

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-frost-bolts" "frost bolts"
  :numeric-id 358
  :x-attr #\d
  :x-char #\-
  :depth 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 15
  :cost 2500
  :sort-value 4523
  :the-kind '<rod>
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-bolt-or-beam! player 10 dir (get-spell-effect '<cold>)
					(roll-dice 10 8))
		(possible-identify! player item)
		(setf (aobj.recharge-time item) 13))
	      :still-useful)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-fire-bolts" "fire bolts"
  :numeric-id 359
  :x-attr #\d
  :x-char #\-
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 15
  :cost 3000
  :sort-value 4522
  :the-kind '<rod>
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-bolt-or-beam! player 10 dir (get-spell-effect '<fire>)
					(roll-dice 16 8))
		(possible-identify! player item)
		(setf (aobj.recharge-time item) 15))
	      :still-useful)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-polymorph" "polymorph"
  :numeric-id 360
  :x-attr #\d
  :x-char #\-
  :depth 35
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(35 0 0 0)
  :weight 15
  :cost 1200
  :sort-value 4519
  :the-kind '<rod>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-slow-monster" "slow monster"
  :numeric-id 361
  :x-attr #\d
  :x-char #\-
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 15
  :cost 1500
  :sort-value 4517
  :the-kind '<rod>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-sleep-monster" "sleep monster"
  :numeric-id 362
  :x-attr #\d
  :x-char #\-
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 15
  :cost 1500
  :sort-value 4516
  :the-kind '<rod>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-drain-life" "drain life"
  :numeric-id 363
  :x-attr #\d
  :x-char #\-
  :depth 75
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(75 0 0 0)
  :weight 15
  :cost 3600
  :sort-value 4518
  :the-kind '<rod>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-teleport-other" "teleport other"
  :numeric-id 364
  :x-attr #\d
  :x-char #\-
  :depth 45
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(45 0 0 0)
  :weight 15
  :cost 1400
  :sort-value 4513
  :the-kind '<rod>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-disarming" "disarming"
  :numeric-id 365
  :x-attr #\d
  :x-char #\-
  :depth 35
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(35 0 0 0)
  :weight 15
  :cost 2100
  :sort-value 4514
  :the-kind '<rod>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-lightning-balls" "lightning balls"
  :numeric-id 366
  :x-attr #\d
  :x-char #\-
  :depth 55
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(55 0 0 0)
  :weight 15
  :cost 4000
  :sort-value 4525
  :the-kind '<rod>
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-ball! player dir (get-spell-effect '<electricity>) 64 2)
		(possible-identify! player item)
		(setf (aobj.recharge-time item) 23))
	      :still-useful)

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-cold-balls" "cold balls"
  :numeric-id 367
  :x-attr #\d
  :x-char #\-
  :depth 60
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(60 0 0 0)
  :weight 15
  :cost 4500
  :sort-value 4527
  :the-kind '<rod>
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-ball! player dir (get-spell-effect '<cold>) 96 2)
		(possible-identify! player item)
		(setf (aobj.recharge-time item) 25))
	      :still-useful)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-fire-balls" "fire balls"
  :numeric-id 368
  :x-attr #\d
  :x-char #\-
  :depth 75
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(75 0 0 0)
  :weight 15
  :cost 5000
  :sort-value 4526
  :the-kind '<rod>
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-ball! player dir (get-spell-effect '<fire>) 144 2)
		(possible-identify! player item)
		(setf (aobj.recharge-time item) 30))
	      :still-useful)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-acid-balls" "acid balls"
  :numeric-id 369
  :x-attr #\d
  :x-char #\-
  :depth 70
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(70 0 0 0)
  :weight 15
  :cost 5500
  :sort-value 4524
  :the-kind '<rod>
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-ball! player dir (get-spell-effect '<acid>) 120 2)
		(possible-identify! player item)
		(setf (aobj.recharge-time item) 27))
	      :still-useful)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-acid-bolts" "acid bolts"
  :numeric-id 370
  :x-attr #\d
  :x-char #\-
  :depth 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 15
  :cost 3500
  :sort-value 4520
  :the-kind '<rod>
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (%read-direction))
		(van-fire-bolt-or-beam! player 10 dir (get-spell-effect '<acid>)
					(roll-dice 12 8))
		(possible-identify! player item)
		(setf (aobj.recharge-time item) 12))
	      :still-useful)

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-enlightenment" "enlightenment"
  :numeric-id 371
  :x-attr #\d
  :x-char #\-
  :depth 65
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(65 0 0 0)
  :weight 15
  :cost 10000
  :sort-value 4505
  :the-kind '<rod>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-perception" "perception"
  :numeric-id 372
  :x-attr #\d
  :x-char #\-
  :depth 50
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(50 0 0 0)
  :weight 15
  :cost 13000
  :sort-value 4502
  :the-kind '<rod>
  :on-zap (object-effect (dungeon player item)
	    (interactive-identify-object! dungeon player)
	    (possible-identify! player item)
	    (setf (aobj.recharge-time item) 10)
	    :still-useful)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-curing" "curing"
  :numeric-id 373
  :x-attr #\d
  :x-char #\-
  :depth 65
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(65 0 0 0)
  :weight 15
  :cost 15000
  :sort-value 4508
  :the-kind '<rod>
  :on-zap (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<blindness> :new-value nil)
	      (possible-identify! player item))
	    (when (modify-creature-state! player '<confusion> :new-value nil)
	      (possible-identify! player item))
	    (when (modify-creature-state! player '<poisoned> :new-value nil)
	      (possible-identify! player item))
	    (when (modify-creature-state! player '<cut>  :new-value nil)
	      (possible-identify! player item))
	    (when (modify-creature-state! player '<stun> :new-value nil)
	      (possible-identify! player item))
	    (setf (aobj.recharge-time item) 999)
	    :still-useful)

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-healing" "healing"
  :numeric-id 374
  :x-attr #\d
  :x-char #\-
  :depth 80
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(80 0 0 0)
  :weight 15
  :cost 20000
  :sort-value 4509
  :the-kind '<rod>
  :on-zap (object-effect (dungeon player item)
	      (let ((amount 500))
		(when (heal-creature! player amount)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<cut> :new-value nil)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<stun> :new-value nil)
		  (possible-identify! player item))
		(setf (aobj.recharge-time item) 999)
		:still-useful))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-detection" "detection"
  :numeric-id 375
  :x-attr #\d
  :x-char #\-
  :depth 30
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(30 0 0 0)
  :weight 15
  :cost 5000
  :sort-value 4506
  :the-kind '<rod>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-restoration" "restoration"
  :numeric-id 376
  :x-attr #\d
  :x-char #\-
  :depth 80
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(80 0 0 0)
  :weight 15
  :cost 25000
  :sort-value 4510
  :the-kind '<rod>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-speed" "speed"
  :numeric-id 377
  :x-attr #\d
  :x-char #\-
  :depth 95
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(95 0 0 0)
  :weight 15
  :cost 50000
  :sort-value 4511
  :the-kind '<rod>
  :on-zap (object-effect (dungeon player item)
	    ;; FIX, check if he's already hasted.  also fix for potion and staff
	    (when (modify-creature-state! player '<hasted> :add (+ 15 (random 30)))
	      (possible-identify! player item))
	    (setf (aobj.recharge-time item) 99)
	    :still-useful)

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

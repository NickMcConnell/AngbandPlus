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
  :text-attr #\d
  :text-char #\-
  :power-lvl 3
  :locations '((3 . 1))
  :weight 10
  :cost 200
  :sort-value 4807
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 10) 6)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-lightning-bolt" "lightning bolts"
  :numeric-id 270
  :text-attr #\d
  :text-char #\-
  :power-lvl 15
  :locations '((15 . 1))
  :weight 10
  :cost 600
  :sort-value 4817
  :the-kind '<wand>
  :effect-type "electricity"
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 6)))

  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-bolt-or-beam! player 20 dir (get-spell-effect '<electricity>)
					(roll-dice 6 6) :projected-object item)
		(possible-identify! player item)
		:still-useful))

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-frost-bolt" "frost bolts"
  :numeric-id 271
  :text-attr #\d
  :text-char #\-
  :power-lvl 20
  :locations '((20 . 1))
  :weight 10
  :cost 800
  :sort-value 4819
  :the-kind '<wand>
  :effect-type "cold"
  :on-add-magic (magic-add (item depth status)
		   (add-charges! item (+ (randint 5) 6)))

  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-bolt-or-beam! player 20 dir (get-spell-effect '<cold>)
					(roll-dice 6 8) :projected-object item)
		(possible-identify! player item)
		:still-useful))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-fire-bolts" "fire bolts"
  :numeric-id 272
  :text-attr #\d
  :text-char #\-
  :power-lvl 30
  :locations '((30 . 1))
  :weight 10
  :cost 1000
  :sort-value 4818
  :the-kind '<wand>
  :effect-type "fire"
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 6)))

  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-bolt-or-beam! player 20 dir (get-spell-effect '<fire>)
					(roll-dice 12 8) :projected-object item)
		(possible-identify! player item)
		:still-useful))

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-stone-mud" "stone to mud"
  :numeric-id 273
  :text-attr #\d
  :text-char #\-
  :power-lvl 10
  :locations '((10 . 1))
  :weight 10
  :cost 300
  :sort-value 4806
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 4) 3)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-polymorph" "polymorph"
  :numeric-id 274
  :text-attr #\d
  :text-char #\-
  :power-lvl 20
  :locations '((20 . 1))
  :weight 10
  :cost 400
  :sort-value 4813
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 6)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-heal-monster" "heal monster"
  :numeric-id 275
  :text-attr #\d
  :text-char #\-
  :power-lvl 3
  :locations '((3 . 1))
  :weight 10
  :cost 0
  :sort-value 4800
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 20) 8)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-haste-monster" "haste monster"
  :numeric-id 276
  :text-attr #\d
  :text-char #\-
  :power-lvl 3
  :locations '((3 . 1))
  :weight 10
  :cost 0
  :sort-value 4801
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 20) 8)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-slow-monster" "slow monster"
  :numeric-id 277
  :text-attr #\d
  :text-char #\-
  :power-lvl 5
  :locations '((5 . 1))
  :weight 10
  :cost 500
  :sort-value 4809
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 10) 6)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-confuse-monster" "confuse monster"
  :numeric-id 278
  :text-attr #\d
  :text-char #\-
  :power-lvl 5
  :locations '((5 . 1))
  :weight 10
  :cost 500
  :sort-value 4810
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 12) 6)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-sleep-monster" "sleep monster"
  :numeric-id 279
  :text-attr #\d
  :text-char #\-
  :power-lvl 5
  :locations '((5 . 1))
  :weight 10
  :cost 500
  :sort-value 4808
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 15) 8)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-drain-life" "drain life"
  :numeric-id 280
  :text-attr #\d
  :text-char #\-
  :power-lvl 50
  :locations '((50 . 1))
  :weight 10
  :cost 1200
  :sort-value 4812
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 3)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-destroy-door" "trap/door destruction"
  :numeric-id 281
  :text-attr #\d
  :text-char #\-
  :power-lvl 10
  :locations '((10 . 1))
  :weight 10
  :cost 100
  :sort-value 4805
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 6)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-magic-missile" "magic missile"
  :numeric-id 282
  :text-attr #\d
  :text-char #\-
  :power-lvl 3
  :locations '((3 . 1))
  :weight 10
  :cost 200
  :sort-value 4815
  :the-kind '<wand>
  :effect-type "magic-missile"
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 10) 6)))

  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-bolt-or-beam! player 20 dir (get-spell-effect '<magic-missile>)
					(roll-dice 3 4) :projected-object item)
		(possible-identify! player item)
		:still-useful))

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-clone-monster" "clone monster"
  :numeric-id 283
  :text-attr #\d
  :text-char #\-
  :power-lvl 15
  :locations '((15 . 1) (50 . 1))
  :weight 10
  :cost 0
  :sort-value 4802
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 3)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-scare-monster" "scare monster"
  :numeric-id 284
  :text-attr #\d
  :text-char #\-
  :power-lvl 10
  :locations '((10 . 1)) ;; was rarity 4, too scarce
  :weight 10
  :cost 500
  :sort-value 4811
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 3)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-teleport-other" "teleport other"
  :numeric-id 285
  :text-attr #\d
  :text-char #\-
  :power-lvl 20
  :locations '((20 . 1))
  :weight 10
  :cost 350
  :sort-value 4803
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 6)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-disarming" "disarming"
  :numeric-id 286
  :text-attr #\d
  :text-char #\-
  :power-lvl 20
  :locations '((20 . 1))
  :weight 10
  :cost 700
  :sort-value 4804
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 4)))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-lightning-balls" "lightning balls"
  :numeric-id 287
  :text-attr #\d
  :text-char #\-
  :power-lvl 35
  :locations '((35 . 1))
  :weight 10
  :cost 1200
  :sort-value 4821
  :the-kind '<wand>
  :effect-type "electricity"
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 4)))

  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<electricity>) 64 2 :projected-object item)
		(possible-identify! player item)
		:still-useful))
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<electricity>))) 

(define-object-kind "wand-cold-balls" "cold balls"
  :numeric-id 288
  :text-attr #\d
  :text-char #\-
  :power-lvl 40
  :locations '((40 . 1))
  :weight 10
  :cost 1500
  :sort-value 4823
  :the-kind '<wand>
  :effect-type "cold"
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 6) 2)))

  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<cold>) 96 2 :projected-object item)
		(possible-identify! player item)
		:still-useful))
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold>))) 

(define-object-kind "wand-fire-balls" "fire balls"
  :numeric-id 289
  :text-attr #\d
  :text-char #\-
  :power-lvl 50
  :locations '((50 . 1))
  :weight 10
  :cost 1800
  :sort-value 4822
  :the-kind '<wand>
  :effect-type "fire"
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 4) 2)))

  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<fire>) 144 2 :projected-object item)
		(possible-identify! player item)
		:still-useful))
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<fire>))) 

(define-object-kind "wand-stinking-cloud" "stinking cloud"
  :numeric-id 290
  :text-attr #\d
  :text-char #\-
  :power-lvl 5
  :locations '((5 . 1))
  :weight 10
  :cost 400
  :sort-value 4814
  :the-kind '<wand>
  :effect-type "poison"
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 6)))

  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<poison>) 12 2 :projected-object item)
		(possible-identify! player item)
		:still-useful))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-acid-balls" "acid balls"
  :numeric-id 291
  :text-attr #\d
  :text-char #\-
  :power-lvl 50
  :locations '((50 . 1))
  :weight 10
  :cost 1650
  :sort-value 4820
  :the-kind '<wand>
  :effect-type "acid"
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 2)))
  
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<acid>) 120 2 :projected-object item)
		(possible-identify! player item)
		:still-useful))
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<acid>))) 

(define-object-kind "wand-wonder" "wonder"
  :numeric-id 292
  :text-attr #\d
  :text-char #\-
  :power-lvl 3
  :locations '((3 . 1))
  :weight 10
  :cost 250
  :sort-value 4824
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 15) 8)))
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "wand-acid-bolts" "acid bolts"
  :numeric-id 294
  :text-attr #\d
  :text-char #\-
  :power-lvl 30
  :locations '((30 . 1))
  :weight 10
  :cost 950
  :sort-value 4816
  :the-kind '<wand>
  :effect-type "acid"
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 6)))

  :on-zap (object-effect (dungeon player item)
	     (when-bind (dir (get-aim-direction))
		(van-fire-bolt-or-beam! player 20 dir (get-spell-effect '<acid>)
					(roll-dice 10 8) :projected-object item)
		(possible-identify! player item)
		:still-useful))

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "wand-dragon-flame" "dragon's flame"
  :numeric-id 295
  :text-attr #\d
  :text-char #\-
  :power-lvl 50
  :locations '((50 . 2)) ;; made less rare
  :weight 10
  :cost 2400
  :sort-value 4826
  :the-kind '<wand>
  :effect-type "fire"
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 1)))
  
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<fire>) 200 3 :projected-object item)
		(possible-identify! player item)
		:still-useful))

  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "wand-dragon-frost" "dragon's frost"
  :numeric-id 296
  :text-attr #\d
  :text-char #\-
  :power-lvl 50
  :locations '((50 . 2)) ;; less rare
  :weight 10
  :cost 2400
  :sort-value 4827
  :the-kind '<wand>
  :effect-type "cold"
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 1)))
  
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<cold>) 160 3 :projected-object item)
		(possible-identify! player item)
		:still-useful))

  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "wand-dragon-breath" "dragon's breath"
  :numeric-id 297
  :text-attr #\d
  :text-char #\-
  :power-lvl 60
  :locations '((60 . 2)) ;; less rare
  :weight 10
  :cost 2400
  :sort-value 4828
  :the-kind '<wand>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 1)))
  
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
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
  :text-attr #\d
  :text-char #\-
  :power-lvl 60
  :locations '((60 . 4))
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
  :text-attr #\d
  :text-char #\_
  :power-lvl 10
  :locations '((10 . 1))
  :weight 50
  :cost 350
  :sort-value 4612
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 6)))
  :on-zap (object-effect (dungeon player item)
	    (when (detect-traps! dungeon player item)
	      (possible-identify! player item))
	    :still-useful)

  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-det-gold" "treasure location"
  :numeric-id 301
  :text-attr #\d
  :text-char #\_
  :power-lvl 5
  :locations '((5 . 1))
  :weight 50
  :cost 200
  :sort-value 4610
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 20) 8)))
  :on-zap (object-effect (dungeon player item)
	    (when (detect-gold! dungeon player item)
	      (possible-identify! player item))
	    :still-useful)

  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-det-item" "object location"
  :numeric-id 302
  :text-attr #\d
  :text-char #\_
  :power-lvl 5
  :locations '((5 . 1))
  :weight 50
  :cost 200
  :sort-value 4611
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 15) 6)))
  :on-zap (object-effect (dungeon player item)
	    (when (detect-normal-objects! dungeon player item)
	      (possible-identify! player item))
	    :still-useful)

  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-teleport" "teleportation"
  :numeric-id 303
  :text-attr #\d
  :text-char #\_
  :power-lvl 20
  :locations '((20 . 1))
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
  :text-attr #\d
  :text-char #\_
  :power-lvl 40
  :locations '((40 . 1))
  :weight 50
  :cost 350
  :sort-value 4628
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 3)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-summoning" "summoning"
  :numeric-id 305
  :text-attr #\d
  :text-char #\_
  :power-lvl 10
  :locations '((10 . 1) (50 . 1))
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
  :text-attr #\d
  :text-char #\_
  :power-lvl 5
  :locations '((5 . 1))
  :weight 50
  :cost 250
  :sort-value 4608
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 20) 8)))
  :on-zap (object-effect (dungeon player item)
	    (when (light-area! dungeon player (location-x player)
			       (location-y player) (roll-dice 2 8)
			       2 :type '<light>) ;; 2d8 dmg, radius 2
	      (possible-identify! player item))
	    :still-useful)
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-*destruction*" "*destruction*"
  :numeric-id 307
  :text-attr #\d
  :text-char #\_
  :power-lvl 50
  :locations '((50 . 1) (70 . 1))
  :weight 50
  :cost 2500
  :sort-value 4629
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 1)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-starlight" "starlight"
  :numeric-id 308
  :text-attr #\d
  :text-char #\_
  :power-lvl 20
  :locations '((20 . 1))
  :weight 50
  :cost 800
  :sort-value 4607
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 6)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-haste-monsters" "haste monsters"
  :numeric-id 309
  :text-attr #\d
  :text-char #\_
  :power-lvl 10
  :locations '((10 . 1))
  :weight 50
  :cost 0
  :sort-value 4602
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 8)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-slow-monsters" "slow monsters"
  :numeric-id 310
  :text-attr #\d
  :text-char #\_
  :power-lvl 10
  :locations '((10 . 1))
  :weight 50
  :cost 800
  :sort-value 4621
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 6)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-sleep-monsters" "sleep monsters"
  :numeric-id 311
  :text-attr #\d
  :text-char #\_
  :power-lvl 10
  :locations '((10 . 1))
  :weight 50
  :cost 700
  :sort-value 4620
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 6)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-cure-light" "cure light wounds"
  :numeric-id 312
  :text-attr #\d
  :text-char #\_
  :power-lvl 5
  :locations '((5 . 1))
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
  :text-attr #\d
  :text-char #\_
  :power-lvl 5
  :locations '((5 . 1))
  :weight 50
  :cost 200
  :sort-value 4614
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 15) 8)))
  :on-zap (object-effect (dungeon player item)
	    (when (detect-invisible! dungeon player item +default-detect-radius+)
	      (possible-identify! player item))
	    :still-useful)

  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-speed" "speed"
  :numeric-id 314
  :text-attr #\d
  :text-char #\_
  :power-lvl 40
  :locations '((40 . 1))
  :weight 50
  :cost 1000
  :sort-value 4622
  :the-kind '<staff>
  :on-zap (object-effect (dungeon player item)
	    (when (haste-creature! player +10 (+ 15 (random 25)))
	      (possible-identify! player item))
	    :still-useful)
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 4)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-slowness" "slowness"
  :numeric-id 315
  :text-attr #\d
  :text-char #\_
  :power-lvl 40
  :locations '((40 . 1))
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
  :text-attr #\d
  :text-char #\_
  :power-lvl 10
  :locations '((10 . 1))
  :weight 50
  :cost 350
  :sort-value 4613
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 6)))
  :on-zap (object-effect (dungeon player item)
	    (detect-stairs! dungeon player item)
	    (detect-doors! dungeon player item)
	    (possible-identify! player item)
	    :still-useful)
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-remove-curse" "remove curse"
  :numeric-id 317
  :text-attr #\d
  :text-char #\_
  :power-lvl 40
  :locations '((40 . 1))
  :weight 50
  :cost 500
  :sort-value 4606
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 4)))
  :on-zap (object-effect (dungeon player item)
	    (when (plusp (remove-curse! player :light))
	      (unless (is-blind? player)
		(print-message! "The staff glows blue for a moment..")
		(possible-identify! player item)))
	    :still-useful)
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-det-evil" "detect evil"
  :numeric-id 318
  :text-attr #\d
  :text-char #\_
  :power-lvl 20
  :locations '((20 . 1))
  :weight 50
  :cost 350
  :sort-value 4615
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		  (add-charges! item (+ (randint 15) 8)))
  :on-zap (object-effect (dungeon player item)
	    (when (detect-evil-monsters! dungeon player item +default-detect-radius+)
	      (possible-identify! player item))
	    :still-useful)

  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-curing" "curing"
  :numeric-id 319
  :text-attr #\d
  :text-char #\_
  :power-lvl 25
  :locations '((25 . 1))
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
  :text-attr #\d
  :text-char #\_
  :power-lvl 50
  :locations '((50 . 1))
  :weight 50
  :cost 1200
  :sort-value 4624
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 3) 4)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-probing" "probing"
  :numeric-id 321
  :text-attr #\d
  :text-char #\_
  :power-lvl 30
  :locations '((30 . 1))
  :weight 50
  :cost 2000
  :sort-value 4623
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 6) 2)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-darkness" "darkness"
  :numeric-id 322
  :text-attr #\d
  :text-char #\_
  :power-lvl 5
  :locations '((5 . 1) (50 . 1))
  :weight 50
  :cost 0
  :sort-value 4600
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 8) 8)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-xenocide" "xenocide"
  :numeric-id 323
  :text-attr #\d
  :text-char #\_
  :power-lvl 70
  :locations '((70 . 4))
  :weight 50
  :cost 3500
  :sort-value 4627
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 2) 1)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-power" "power"
  :numeric-id 324
  :text-attr #\d
  :text-char #\_
  :power-lvl 70
  :locations '((70 . 2))
  :weight 50
  :cost 4000
  :sort-value 4625
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		  (add-charges! item (+ (randint 3) 1)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-magi" "the magi"
  :numeric-id 325
  :text-attr #\d
  :text-char #\_
  :power-lvl 70
  :locations '((70 . 2))
  :weight 50
  :cost 4500
  :sort-value 4619
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 2) 2)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-identify" "perception"
  :numeric-id 326
  :text-attr #\d
  :text-char #\_
  :power-lvl 10
  :locations '((10 . 1))
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
  :text-attr #\d
  :text-char #\_
  :power-lvl 70
  :locations '((70 . 2))
  :weight 50
  :cost 4500
  :sort-value 4626
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 2) 2)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-mapping" "enlightenment"
  :numeric-id 328
  :text-attr #\d
  :text-char #\_
  :power-lvl 20
  :locations '((20 . 1))
  :weight 50
  :cost 750
  :sort-value 4609
  :the-kind '<staff>
  :on-add-magic (magic-add (item depth status)
		    (add-charges! item (+ (randint 5) 5)))
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "staff-healing" "healing"
  :numeric-id 329
  :text-attr #\d
  :text-char #\_
  :power-lvl 70
  :locations '((70 . 1))
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

;;; === Rods

(define-object-kind "rod-door-loc" "door/stair location"
  :numeric-id 351
  :text-attr #\d
  :text-char #\-
  :power-lvl 15
  :locations '((15 . 1))
  :weight 15
  :cost 1000
  :sort-value 4501
  :the-kind '<rod>
  :recharge-time 70
  :on-zap (object-effect (dungeon player item)
	    (detect-doors! dungeon player item)
	    (detect-stairs! dungeon player item)
	    (possible-identify! player item)
	    :still-useful)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-trap-loc" "trap location"
  :numeric-id 352
  :text-attr #\d
  :text-char #\-
  :power-lvl 5
  :locations '((5 . 1))
  :weight 15
  :cost 100
  :sort-value 4500
  :the-kind '<rod>
  :recharge-time 50
  :on-zap (object-effect (dungeon player item)
	    (when (detect-traps! dungeon player item)
	      (possible-identify! player item))
	    :still-useful)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-probing" "probing"
  :numeric-id 353
  :text-attr #\d
  :text-char #\-
  :power-lvl 40
  :locations '((40 . 4))
  :weight 15
  :cost 4000
  :sort-value 4507
  :the-kind '<rod>
  :recharge-time 50
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-recall" "recall"
  :numeric-id 354
  :text-attr #\d
  :text-char #\-
  :power-lvl 30
  :locations '((30 . 4))
  :weight 15
  :cost 4000
  :sort-value 4503
  :the-kind '<rod>
  :recharge-time 60
  :on-zap (object-effect (dungeon player item)
	     (when (toggle-word-of-recall! player)
	       (possible-identify! player item))
	     :still-useful)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-illumination" "illumination"
  :numeric-id 355
  :text-attr #\d
  :text-char #\-
  :power-lvl 20
  :locations '((20 . 1))
  :weight 15
  :cost 1000
  :sort-value 4504
  :the-kind '<rod>
  :recharge-time 30
  :on-zap (object-effect (dungeon player item)
	    ;;(warn "illumination.")
	    (when (light-area! dungeon player (location-x player)
			       (location-y player) (roll-dice 2 8)
			       2 :type '<light>) ;; 2d8 dmg, radius 2
	      (possible-identify! player item))
	    :still-useful)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-light" "light"
  :numeric-id 356
  :text-attr #\d
  :text-char #\-
  :power-lvl 10
  :locations '((10 . 1))
  :weight 15
  :cost 500
  :sort-value 4515
  :the-kind '<rod>
  :recharge-time 9
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-lightning-bolts" "lightning bolts"
  :numeric-id 357
  :text-attr #\d
  :text-char #\-
  :power-lvl 20
  :locations '((20 . 1))
  :weight 15
  :cost 2000
  :sort-value 4521
  :the-kind '<rod>
  :effect-type "electricity"
  :recharge-time 11
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-bolt-or-beam! player 10 dir (get-spell-effect '<electricity>)
					(roll-dice 6 6) :projected-object item)
		(possible-identify! player item))
	      :still-useful)

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-frost-bolts" "frost bolts"
  :numeric-id 358
  :text-attr #\d
  :text-char #\-
  :power-lvl 25
  :locations '((25 . 1))
  :weight 15
  :cost 2500
  :sort-value 4523
  :the-kind '<rod>
  :effect-type "cold"
  :recharge-time 13
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-bolt-or-beam! player 10 dir (get-spell-effect '<cold>)
					(roll-dice 10 8) :projected-object item)
		(possible-identify! player item))
	      :still-useful)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-fire-bolts" "fire bolts"
  :numeric-id 359
  :text-attr #\d
  :text-char #\-
  :power-lvl 30
  :locations '((30 . 1))
  :weight 15
  :cost 3000
  :sort-value 4522
  :the-kind '<rod>
  :effect-type "fire"
  :recharge-time 15
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-bolt-or-beam! player 10 dir (get-spell-effect '<fire>)
					(roll-dice 16 8) :projected-object item)
		(possible-identify! player item))
	      :still-useful)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-polymorph" "polymorph"
  :numeric-id 360
  :text-attr #\d
  :text-char #\-
  :power-lvl 35
  :locations '((35 . 1))
  :weight 15
  :cost 1200
  :sort-value 4519
  :the-kind '<rod>
  :recharge-time 25
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-slow-monster" "slow monster"
  :numeric-id 361
  :text-attr #\d
  :text-char #\-
  :power-lvl 30
  :locations '((30 . 1))
  :weight 15
  :cost 1500
  :sort-value 4517
  :the-kind '<rod>
  :recharge-time 20
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-sleep-monster" "sleep monster"
  :numeric-id 362
  :text-attr #\d
  :text-char #\-
  :power-lvl 30
  :locations '((30 . 1))
  :weight 15
  :cost 1500
  :sort-value 4516
  :the-kind '<rod>
  :recharge-time 18
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-drain-life" "drain life"
  :numeric-id 363
  :text-attr #\d
  :text-char #\-
  :power-lvl 75
  :locations '((75 . 4))
  :weight 15
  :cost 3600
  :sort-value 4518
  :the-kind '<rod>
  :recharge-time 23
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-teleport-other" "teleport other"
  :numeric-id 364
  :text-attr #\d
  :text-char #\-
  :power-lvl 45
  :locations '((45 . 2))
  :weight 15
  :cost 1400
  :sort-value 4513
  :the-kind '<rod>
  :recharge-time 25
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-disarming" "disarming"
  :numeric-id 365
  :text-attr #\d
  :text-char #\-
  :power-lvl 35
  :locations '((35 . 1))
  :weight 15
  :cost 2100
  :sort-value 4514
  :the-kind '<rod>
  :recharge-time 30
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-lightning-balls" "lightning balls"
  :numeric-id 366
  :text-attr #\d
  :text-char #\-
  :power-lvl 55
  :locations '((55 . 1))
  :weight 15
  :cost 4000
  :sort-value 4525
  :the-kind '<rod>
  :recharge-time 23
  :effect-type "electricity"
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<electricity>) 64 2 :projected-object item)
		(possible-identify! player item))
	      :still-useful)

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-cold-balls" "cold balls"
  :numeric-id 367
  :text-attr #\d
  :text-char #\-
  :power-lvl 60
  :locations '((60 . 1))
  :weight 15
  :cost 4500
  :sort-value 4527
  :the-kind '<rod>
  :recharge-time 25
  :effect-type "cold"
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<cold>) 96 2 :projected-object item)
		(possible-identify! player item))
	      :still-useful)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-fire-balls" "fire balls"
  :numeric-id 368
  :text-attr #\d
  :text-char #\-
  :power-lvl 75
  :locations '((75 . 1))
  :weight 15
  :cost 5000
  :sort-value 4526
  :the-kind '<rod>
  :recharge-time 30
  :effect-type "fire"
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<fire>) 144 2 :projected-object item)
		(possible-identify! player item))
	      :still-useful)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-acid-balls" "acid balls"
  :numeric-id 369
  :text-attr #\d
  :text-char #\-
  :power-lvl 70
  :locations '((70 . 1))
  :weight 15
  :cost 5500
  :sort-value 4524
  :the-kind '<rod>
  :recharge-time 27
  :effect-type "acid"
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-ball! player dir (get-spell-effect '<acid>) 120 2 :projected-object item)
		(possible-identify! player item))
	      :still-useful)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-acid-bolts" "acid bolts"
  :numeric-id 370
  :text-attr #\d
  :text-char #\-
  :power-lvl 40
  :locations '((40 . 1))
  :weight 15
  :cost 3500
  :sort-value 4520
  :the-kind '<rod>
  :recharge-time 12
  :effect-type "acid"
  :on-zap (object-effect (dungeon player item)
	      (when-bind (dir (get-aim-direction))
		(van-fire-bolt-or-beam! player 10 dir (get-spell-effect '<acid>)
					(roll-dice 12 8) :projected-object item)
		(possible-identify! player item))
	      :still-useful)

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-enlightenment" "enlightenment"
  :numeric-id 371
  :text-attr #\d
  :text-char #\-
  :power-lvl 65
  :locations '((65 . 4))
  :weight 15
  :cost 10000
  :sort-value 4505
  :the-kind '<rod>
  :recharge-time 99
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-perception" "perception"
  :numeric-id 372
  :text-attr #\d
  :text-char #\-
  :power-lvl 50
  :locations '((50 . 8))
  :weight 15
  :cost 13000
  :sort-value 4502
  :the-kind '<rod>
  :recharge-time 10
  :on-zap (object-effect (dungeon player item)
	    (interactive-identify-object! dungeon player)
	    (possible-identify! player item)
	    :still-useful)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-curing" "curing"
  :numeric-id 373
  :text-attr #\d
  :text-char #\-
  :power-lvl 65
  :locations '((65 . 8))
  :weight 15
  :cost 15000
  :sort-value 4508
  :the-kind '<rod>
  :recharge-time 999
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
	    :still-useful)

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-healing" "healing"
  :numeric-id 374
  :text-attr #\d
  :text-char #\-
  :power-lvl 80
  :locations '((80 . 8))
  :weight 15
  :cost 20000
  :sort-value 4509
  :the-kind '<rod>
  :recharge-time 999
  :on-zap (object-effect (dungeon player item)
	      (let ((amount 500))
		(when (heal-creature! player amount)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<cut> :new-value nil)
		  (possible-identify! player item))
		(when (modify-creature-state! player '<stun> :new-value nil)
		  (possible-identify! player item))
		:still-useful))
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-detection" "detection"
  :numeric-id 375
  :text-attr #\d
  :text-char #\-
  :power-lvl 30
  :locations '((30 . 8))
  :weight 15
  :cost 5000
  :sort-value 4506
  :the-kind '<rod>
  :recharge-time 99
  :on-zap (object-effect (dungeon player item)
	    (when (detect-all! dungeon player item)
	      (possible-identify! player item))
	    :still-useful)
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-restoration" "restoration"
  :numeric-id 376
  :text-attr #\d
  :text-char #\-
  :power-lvl 80
  :locations '((80 . 16))
  :weight 15
  :cost 25000
  :sort-value 4510
  :the-kind '<rod>
  :recharge-time 999
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rod-speed" "speed"
  :numeric-id 377
  :text-attr #\d
  :text-char #\-
  :power-lvl 95
  :locations '((95 . 16))
  :weight 15
  :cost 50000
  :sort-value 4511
  :the-kind '<rod>
  :recharge-time 99
  :on-zap (object-effect (dungeon player item)
	    ;; FIX, check if he's already hasted.  also fix for potion and staff
	    (when (haste-creature! player +10 (+ 15 (random 30)))
	      (possible-identify! player item))
	    :still-useful)

  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

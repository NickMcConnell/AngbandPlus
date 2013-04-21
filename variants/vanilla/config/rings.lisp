;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/rings.lisp - rings for vanilla variant
Copyright (c) 2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(define-object-kind "ring-str" "strength"
  :numeric-id 132
  :x-attr #\d
  :x-char #\=
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 2
  :cost 500
  :obj-type '(<str> <ring>)
  :flags '(<hide-type>)
  :sort-value 4424
  :the-kind '<ring>
  :game-values (make-game-values :stat-modifiers '(<str>))) 

(define-object-kind "ring-dex" "dexterity"
  :numeric-id 133
  :x-attr #\d
  :x-char #\=
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 2
  :cost 500
  :obj-type '(<dex> <ring>)
  :flags '(<hide-type>)
  :sort-value 4426
  :the-kind '<ring>
  :game-values (make-game-values :stat-modifiers '(<dex>))) 

(define-object-kind "ring-con" "constitution"
  :numeric-id 134
  :x-attr #\d
  :x-char #\=
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 2
  :cost 500
  :obj-type '(<con> <ring>)
  :flags '(<hide-type>)
  :sort-value 4427
  :the-kind '<ring>
  :game-values (make-game-values :stat-modifiers '(<con>))) 

(define-object-kind "ring-int" "intelligence"
  :numeric-id 135
  :x-attr #\d
  :x-char #\=
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 2
  :cost 500
  :obj-type '(<int> <ring>)
  :flags '(<hide-type>)
  :sort-value 4425
  :the-kind '<ring>
  :game-values (make-game-values :stat-modifiers '(<int>))) 

(define-object-kind "ring-speed" "speed"
  :numeric-id 136
  :x-attr #\d
  :x-char #\=
  :depth 80
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(80 0 0 0)
  :weight 2
  :cost 100000
  :obj-type '(<speed> <ring>)
  :flags '(<hide-type>)
  :sort-value 4431
  :the-kind '<ring>
  :game-values (make-game-values :abilities '(<speed>))) 

(define-object-kind "ring-searching" "searching"
  :numeric-id 137
  :x-attr #\d
  :x-char #\=
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 2
  :cost 250
  :obj-type '(<searching> <ring>)
  :flags '(<hide-type>)
  :sort-value 4423
  :the-kind '<ring>
  :game-values (make-game-values :skill-modifiers '(<search>))) 

(define-object-kind "ring-teleport" "teleportation"
  :numeric-id 138
  :x-attr #\d
  :x-char #\=
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 2
  :cost 0
  :obj-type '(<teleport> <ring>)
  :flags '(<easy-know> <curse>)
  :sort-value 4404
  :the-kind '<ring>
  :game-values (make-game-values :abilities '(<teleport>))) 

(define-object-kind "ring-slow-digest" "slow digestion"
  :numeric-id 139
  :x-attr #\d
  :x-char #\=
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 2
  :cost 250
  :obj-type '(<slow-digestion> <ring>)
  :flags '(<easy-know>)
  :sort-value 4406
  :the-kind '<ring>
  :game-values (make-game-values :abilities '(<slow-digestion>))) 

(define-object-kind "ring-resist-fire" "resist fire"
  :numeric-id 140
  :x-attr #\d
  :x-char #\=
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 2
  :cost 250
  :obj-type '(<ring> <resist> <fire>)
  :flags '(<easy-know>)
  :sort-value 4408
  :the-kind '<ring>
  :game-values (make-game-values :ignores '(<fire>) :resists '(<fire>))) 

(define-object-kind "ring-resist-cold" "resist cold"
  :numeric-id 141
  :x-attr #\d
  :x-char #\=
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 2
  :cost 250
  :obj-type '(<ring> <resist> <cold>)
  :flags '(<easy-know>)
  :sort-value 4409
  :the-kind '<ring>
  :game-values (make-game-values :ignores '(<cold>) :resists '(<cold>))) 

(define-object-kind "ring-feather-fall" "feather falling"
  :numeric-id 142
  :x-attr #\d
  :x-char #\=
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 2
  :cost 200
  :obj-type '(<feather-fall> <ring>)
  :flags '(<easy-know>)
  :sort-value 4407
  :the-kind '<ring>
  :game-values (make-game-values :abilities '(<feather-fall>))) 

(define-object-kind "ring-resist-poison" "poison resistance"
  :numeric-id 143
  :x-attr #\d
  :x-char #\=
  :depth 40
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(40 0 0 0)
  :weight 2
  :cost 16000
  :obj-type '(<ring> <resist> <poison>)
  :flags '(<easy-know>)
  :sort-value 4420
  :the-kind '<ring>
  :game-values (make-game-values :resists '(<poison>))) 

(define-object-kind "ring-free-action" "free action"
  :numeric-id 144
  :x-attr #\d
  :x-char #\=
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 2
  :cost 1500
  :obj-type '(<free-action> <ring>)
  :flags '(<easy-know>)
  :sort-value 4421
  :the-kind '<ring>
  :game-values (make-game-values :abilities '(<free-action>))) 

(define-object-kind "ring-weakness" "weakness"
  :numeric-id 145
  :x-attr #\d
  :x-char #\=
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 2
  :cost 0
  :obj-type '(<weakness> <ring>)
  :flags '(<hide-type> <curse>)
  :sort-value 4402
  :the-kind '<ring>
  :game-values (make-game-values :stat-modifiers '(<str>))) 

(define-object-kind "ring-flames" "flames"
  :numeric-id 146
  :x-attr #\d
  :x-char #\=
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 2
  :cost 3000
  :obj-type '(<ring> <protection> <fire>)
  :sort-value 4418
  :the-kind '<ring>
  :game-values (make-game-values :ac-modifier 15 :ignores '(<fire>) :resists '(<fire>))) 

(define-object-kind "ring-acid" "acid"
  :numeric-id 147
  :x-attr #\d
  :x-char #\=
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 2
  :cost 3000
  :obj-type '(<ring> <protection> <acid>)
  :sort-value 4417
  :the-kind '<ring>
  :game-values (make-game-values :ac-modifier 15 :ignores '(<acid>) :resists '(<acid>))) 

(define-object-kind "ring-ice" "ice"
  :numeric-id 148
  :x-attr #\d
  :x-char #\=
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 2
  :cost 3000
  :obj-type '(<ring> <protection> <cold>)
  :sort-value 4419
  :the-kind '<ring>
  :game-values (make-game-values :ac-modifier 15 :ignores '(<cold>) :resists '(<cold>))) 

(define-object-kind "ring-woe" "woe"
  :numeric-id 149
  :x-attr #\d
  :x-char #\=
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 2
  :cost 0
  :obj-type '(<woe> <ring>)
  :flags '(<hide-type> <curse>)
  :sort-value 4400
  :the-kind '<ring>
  :on-add-magic #'(lambda (item depth status)
		    (declare (ignore status))
		    (bit-flag-add! (aobj.identify item) (logior +ident-cursed+ +ident-broken+))
		    (unless (aobj.game-values item)
			(setf (aobj.game-values item) (make-game-values)))
		    (let ((gvals (aobj.game-values item))
			  (stat-change (- -1 (magic-bonus-for-level 5 depth))))
		      (setf (gval.ac-modifier gvals) (- -5 (magic-bonus-for-level 10 depth)))
		      (setf (gval.stat-modifiers gvals)
			    (build-stat-table-from-symlist *variant*
							   (loop for i in (gval.stat-modifiers gvals)
								 collecting (list i stat-change))))
		      ;;(warn "stat-changes for woe ~s" (gval.stat-modifiers gvals))
		      ))


  :game-values (make-game-values :stat-modifiers '(<chr> <wis>) :abilities '(<teleport>))) 

(define-object-kind "ring-stupid" "stupidity"
  :numeric-id 150
  :x-attr #\d
  :x-char #\=
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 2
  :cost 0
  :obj-type '(<stupidity> <ring>)
  :flags '(<hide-type> <curse>)
  :sort-value 4403
  :the-kind '<ring>
  :game-values (make-game-values :stat-modifiers '(<int>))) 

(define-object-kind "ring-dmg" "damage"
  :numeric-id 151
  :x-attr #\d
  :x-char #\=
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 2
  :cost 500
  :obj-type '(<damage> <ring>)
  :sort-value 4429
  :on-add-magic #'(lambda (item depth status)
		    (let ((bonus (+ 5 (randint 3) (magic-bonus-for-level 7 depth))))
		      (when (eq status :cursed)
			(setf bonus (- 0 bonus))
			(bit-flag-add! (aobj.identify item) (logior +ident-cursed+ +ident-broken+))
			)
		      (unless (aobj.game-values item)
			(setf (aobj.game-values item) (make-game-values)))
		      ;;(warn "bonus is ~s" bonus)
		      (setf (gval.dmg-modifier (aobj.game-values item)) bonus)))
  :the-kind '<ring>) 

(define-object-kind "ring-to-hit" "accuracy"
  :numeric-id 152
  :x-attr #\d
  :x-char #\=
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 2
  :cost 500
  :obj-type '(<accuracy> <ring>)
  :sort-value 4428
  :on-add-magic #'(lambda (item depth status)
		    (let ((bonus (+ 5 (randint 3) (magic-bonus-for-level 7 depth))))
		      (when (eq status :cursed)
			(bit-flag-add! (aobj.identify item) (logior +ident-cursed+ +ident-broken+))
			(setf bonus (- 0 bonus)))
		      (unless (aobj.game-values item)
			(setf (aobj.game-values item) (make-game-values)))
		      ;;(warn "bonus is ~s" bonus)
		      (setf (gval.tohit-modifier (aobj.game-values item)) bonus)))
  :the-kind '<ring>) 

(define-object-kind "ring-protection" "protection"
  :numeric-id 153
  :x-attr #\d
  :x-char #\=
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 2
  :cost 500
  :obj-type '(<protection> <ring>)
  :sort-value 4416
  :the-kind '<ring>) 

(define-object-kind "ring-aggr-monster" "aggravate monster"
  :numeric-id 154
  :x-attr #\d
  :x-char #\=
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 2
  :cost 0
  :obj-type '(<aggravation> <ring>)
  :flags '(<easy-know> <curse>)
  :sort-value 4401
  :the-kind '<ring>
  :game-values (make-game-values :abilities '(<aggravate>))) 

(define-object-kind "ring-see-inv" "see invisible"
  :numeric-id 155
  :x-attr #\d
  :x-char #\=
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 2
  :cost 340
  :obj-type '(<see-invisible> <ring>)
  :flags '(<easy-know>)
  :sort-value 4422
  :the-kind '<ring>
  :game-values (make-game-values :abilities '(<see-invisible>))) 

(define-object-kind "ring-sust-str" "sustain strength"
  :numeric-id 156
  :x-attr #\d
  :x-char #\=
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 2
  :cost 750
  :obj-type '(<ring> <sustain> <str>)
  :flags '(<easy-know>)
  :sort-value 4410
  :the-kind '<ring>
  :game-values (make-game-values :sustains '(<str>))) 

(define-object-kind "ring-sust-int" "sustain intelligence"
  :numeric-id 157
  :x-attr #\d
  :x-char #\=
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 2
  :cost 600
  :obj-type '(<ring> <sustain> <int>)
  :flags '(<easy-know>)
  :sort-value 4411
  :the-kind '<ring>
  :game-values (make-game-values :sustains '(<int>))) 

(define-object-kind "ring-sust-wis" "sustain wisdom"
  :numeric-id 158
  :x-attr #\d
  :x-char #\=
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 2
  :cost 600
  :obj-type '(<ring> <sustain> <wis>)
  :flags '(<easy-know>)
  :sort-value 4412
  :the-kind '<ring>
  :game-values (make-game-values :sustains '(<wis>))) 

(define-object-kind "ring-sust-con" "sustain constitution"
  :numeric-id 159
  :x-attr #\d
  :x-char #\=
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 2
  :cost 750
  :obj-type '(<ring> <sustain> <dex>)
  :flags '(<easy-know>)
  :sort-value 4413
  :the-kind '<ring>
  :game-values (make-game-values :sustains '(<con>))) 

(define-object-kind "ring-sust-dex" "sustain dexterity"
  :numeric-id 160
  :x-attr #\d
  :x-char #\=
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 2
  :cost 750
  :obj-type '(<ring> <sustain> <con>)
  :flags '(<easy-know>)
  :sort-value 4414
  :the-kind '<ring>
  :game-values (make-game-values :sustains '(<dex>))) 

(define-object-kind "ring-sust-chr" "sustain charisma"
  :numeric-id 161
  :x-attr #\d
  :x-char #\=
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 2
  :cost 500
  :obj-type '(<ring> <sustain> <chr>)
  :flags '(<easy-know>)
  :sort-value 4415
  :the-kind '<ring>
  :game-values (make-game-values :sustains '(<chr>))) 

(define-object-kind "ring-slaying" "slaying"
  :numeric-id 162
  :x-attr #\d
  :x-char #\=
  :depth 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 2
  :cost 1000
  :obj-type '(<slaying> <ring>)
  :flags '(<show-modififers>)
  :sort-value 4430
  :the-kind '<ring>
  :game-values (make-game-values)) 

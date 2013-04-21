;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/food.lisp - eatable objects for vanilla variant
Copyright (c) 2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(define-object-kind "mushroom-blindness" "blindness"
  :numeric-id 1
  :text-attr #\d
  :text-char #\,
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 1
  :cost 0
  :sort-value 6001
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<blindness> :add (+ 200 (random 200)))
	      (possible-identify! player item))
	    :used)
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "mushroom-paranoia" "paranoia"
  :numeric-id 2
  :text-attr #\d
  :text-char #\,
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 1
  :cost 0
  :sort-value 6002
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<fear> :add (+ 10 (random 10)))
	      (possible-identify! player item))
	    :used)
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "mushroom-confusion" "confusion"
  :numeric-id 3
  :text-attr #\d
  :text-char #\,
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 1
  :cost 0
  :sort-value 6003
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<confusion> :add (+ 10 (random 10)))
	      (possible-identify! player item))
	    :used)
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "mushroom-hallucination" "hallucination"
  :numeric-id 4
  :text-attr #\d
  :text-char #\,
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 1
  :cost 0
  :sort-value 6004
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<hallucinate> :add (+ 250 (random 250)))
	      (possible-identify! player item))
	    :used)
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "mushroom-cure-poison" "cure poison"
  :numeric-id 5
  :text-attr #\d
  :text-char #\,
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 1
  :cost 60
  :sort-value 6012
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<poisoned> :new-value nil)
	      (possible-identify! player item))
	    :used)
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "mushroom-cure-blindness" "cure blindness"
  :numeric-id 6
  :text-attr #\d
  :text-char #\,
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 1
  :cost 50
  :sort-value 6013
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<blindness> :new-value nil)
	      (possible-identify! player item))
	    :used)
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "mushroom-cure-paranoia" "cure paranoia"
  :numeric-id 7
  :text-attr #\d
  :text-char #\,
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 1
  :cost 25
  :sort-value 6014
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<fear> :new-value nil)
	      (possible-identify! player item))
	    :used)

  :game-values (make-game-values :food-value 500)) 

(define-object-kind "mushroom-cure-confusion" "cure confusion"
  :numeric-id 8
  :text-attr #\d
  :text-char #\,
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 1
  :cost 50
  :sort-value 6015
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<confusion> :new-value nil)
	      (possible-identify! player item))
	    :used)
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "mushroom-weakness" "weakness"
  :numeric-id 9
  :text-attr #\d
  :text-char #\,
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 1
  :cost 0
  :sort-value 6006
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    ;; add damage and desc
	    (update-player-stat! player '<str> '<reduce>)
	    (possible-identify! player item)
	    :used)
  
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "mushroom-unhealth" "unhealth"
  :numeric-id 10
  :text-attr #\d
  :text-char #\,
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 1
  :cost 50
  :sort-value 6010
  :the-kind '<mushroom>
  :game-values (make-game-values :base-dice 10 :num-dice 10 :food-value 500)) 

(define-object-kind "mushroom-restore-con" "restore constitution"
  :numeric-id 11
  :text-attr #\d
  :text-char #\,
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 1
  :cost 350
  :sort-value 6018
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (update-player-stat! player '<con> '<restore>)
	      (possible-identify! player item))
	    :used)
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "mushroom-restoring" "restoring"
  :numeric-id 12
  :text-attr #\d
  :text-char #\,
  :depth 20
  :rarity 0
  :chance #(8 4 1 0)
  :locale #(20 30 40 0)
  :weight 1
  :cost 1000
  :sort-value 6019
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (update-player-stat! player '<str> '<restore>)
	    (update-player-stat! player '<dex> '<restore>)
	    (update-player-stat! player '<con> '<restore>)
	    (update-player-stat! player '<int> '<restore>)
	    (update-player-stat! player '<wis> '<restore>)
	    (update-player-stat! player '<chr> '<restore>)
	    (possible-identify! player item)
	    :used)
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "mushroom-stupidity" "stupidity"
  :numeric-id 13
  :text-attr #\d
  :text-char #\,
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 1
  :cost 0
  :sort-value 6008
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    ;; add damage and desc
	    (update-player-stat! player '<int> '<reduce>)
	    (possible-identify! player item)
	    :used)
  
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "mushroom-naivety" "naivety"
  :numeric-id 14
  :text-attr #\d
  :text-char #\,
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 1
  :cost 0
  :sort-value 6009
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    ;; add damage and desc
	    (update-player-stat! player '<wis> '<reduce>)
	    (possible-identify! player item)
	    :used)
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "mushroom-poison" "poison"
  :numeric-id 15
  :text-attr #\d
  :text-char #\,
  :depth 5
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(5 5 0 0)
  :weight 1
  :cost 0
  :sort-value 6000
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<poisoned> :add (+ 10 (random 10)))
	      (possible-identify! player item))
	    :used)
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "mushroom-sickness" "sickness"
  :numeric-id 16
  :text-attr #\d
  :text-char #\,
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 1
  :cost 0
  :sort-value 6007
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    ;; add damage and desc
	    (update-player-stat! player '<con> '<reduce>)
	    (possible-identify! player item)
	    :used)
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "mushroom-paralysis" "paralysis"
  :numeric-id 17
  :text-attr #\d
  :text-char #\,
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 1
  :cost 0
  :sort-value 6005
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<paralysed> :add (+ 10 (random 10)))
	      (possible-identify! player item))
	    :used)
  
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "mushroom-restore-str" "restore strength"
  :numeric-id 18
  :text-attr #\d
  :text-char #\,
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 1
  :cost 350
  :sort-value 6017
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (update-player-stat! player '<con> '<restore>)
	      (possible-identify! player item))
	    :used)
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "mushroom-disease" "disease"
  :numeric-id 19
  :text-attr #\d
  :text-char #\,
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 1
  :cost 50
  :sort-value 6011
  :the-kind '<mushroom>
  :on-eat #'%dummy-eat-fun
  :game-values (make-game-values :base-dice 10 :num-dice 10 :food-value 500)) 

(define-object-kind "mushroom-cure-serious" "cure serious wounds"
  :numeric-id 20
  :text-attr #\d
  :text-char #\,
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 2
  :cost 75
  :sort-value 6016
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (heal-creature! player (roll-dice 4 8))
	      (possible-identify! player item))
	    :used)
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "food-ration" "& ration~ of food"
  :numeric-id 21
  :text-attr #\U
  :text-char #\,
  :x-attr (tile-file 5) :x-char (tile-number 4)
  :depth 0
  :rarity 0
  :chance #(1 1 1 0)
  :locale #(0 5 10 0)
  :weight 10
  :cost 3
  :sort-value 6035
  :the-kind '<food>
  :on-eat #'%dummy-eat-fun
  :game-values (make-game-values :food-value 5000)) 

(define-object-kind "biscuit" "& hard biscuit~"
  :numeric-id 22
  :text-attr #\U
  :text-char #\,
  :x-attr (tile-file 5) :x-char (tile-number 2)
  :depth 0
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 1
  :sort-value 6032
  :the-kind '<food>
  :on-eat #'%dummy-eat-fun
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "beef-jerky" "& strip~ of beef jerky"
  :numeric-id 23
  :text-attr #\u
  :text-char #\,
  :x-attr (tile-file 5) :x-char (tile-number 3)
  :depth 0
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 2
  :sort-value 6033
  :the-kind '<food>
  :on-eat #'%dummy-eat-fun
  :game-values (make-game-values :food-value 1500)) 

(define-object-kind "slime-mold" "& slime mold~"
  :numeric-id 24
  :text-attr #\g
  :text-char #\,
  :x-attr (tile-file 5) :x-char (tile-number 5)
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 5
  :cost 2
  :sort-value 6036
  :the-kind '<food>
  :on-eat #'%dummy-eat-fun
  :game-values (make-game-values :food-value 3000)) 

(define-object-kind "elvish-bread" "& piece~ of elvish waybread"
  :numeric-id 25
  :text-attr #\B
  :text-char #\,
  :x-attr (tile-file 5) :x-char (tile-number 6)
  :depth 5
  :rarity 0
  :chance #(1 1 1 0)
  :locale #(5 10 20 0)
  :weight 3
  :cost 10
  :sort-value 6037
  :the-kind '<food>
  :on-eat #'%dummy-eat-fun
  :game-values (make-game-values :food-value 7500)) 

(define-object-kind "pint-ale" "& pint~ of fine ale"
  :numeric-id 26
  :x-attr (tile-file 5)
  :x-char (tile-number 0)
  :text-attr #\y
  :text-char #\,
  :depth 0
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 5
  :cost 1
  :sort-value 6038
  :the-kind '<food>
  :on-eat #'%dummy-eat-fun
  :game-values (make-game-values :food-value 500)) 

(define-object-kind "pint-wine" "& pint~ of fine wine"
  :numeric-id 27
  :x-attr (tile-file 5)
  :x-char (tile-number 1)
  :text-attr #\r
  :text-char #\,
  :depth 0
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 10
  :cost 2
  :sort-value 6039
  :the-kind '<food>
  :on-eat #'%dummy-eat-fun
  :game-values (make-game-values :food-value 1000))

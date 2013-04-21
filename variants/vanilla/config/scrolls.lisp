;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/scrolls.lisp - scrolls for vanilla variant
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(define-object-kind "scroll-enchant-wpn-hit" "enchant weapon to-hit"
  :numeric-id 173
  :text-attr #\d
  :text-char #\?
  :power-lvl 15
  :locations '((15 . 1))
  :weight 5
  :cost 125
  :sort-value 5017
  :on-read (object-effect (dungeon player item)
	     (let ((retval (enchant-item! dungeon player :type '<weapon> :bonus 1 :restrict '<to-hit>)))
	       (possible-identify! player item)
	       retval))
  :the-kind '<scroll>) 

(define-object-kind "scroll-enchant-wpn-dmg" "enchant weapon to-dam"
  :numeric-id 174
  :text-attr #\d
  :text-char #\?
  :power-lvl 15
  :locations '((15 . 1))
  :weight 5
  :cost 125
  :sort-value 5018
  :on-read (object-effect (dungeon player item)
	     (let ((retval (enchant-item! dungeon player :type '<weapon> :bonus 1 :restrict '<to-dmg>)))
	       (possible-identify! player item)
	       retval))
  :the-kind '<scroll>) 

(define-object-kind "scroll-enchant-armour" "enchant armour"
  :numeric-id 175
  :text-attr #\d
  :text-char #\?
  :power-lvl 15
  :locations '((15 . 1))
  :weight 5
  :cost 125
  :sort-value 5016
  :on-read (object-effect (dungeon player item)
	     (let ((retval (enchant-item! dungeon player :type '<armour> :bonus 1)))
	       (possible-identify! player item)
	       retval))
  :the-kind '<scroll>) 

(define-object-kind "scroll-identify" "identify"
  :numeric-id 176
  :text-attr #\d
  :text-char #\?
  :power-lvl 1
  :locations '((1 . 1) (5 . 1) (10 . 1) (30 . 1))
  :weight 5
  :cost 50
  :sort-value 5012
  :on-read (object-effect (dungeon player item)
	     (interactive-identify-object! dungeon player :type '<normal>)
	     (possible-identify! player item)
	     :used)

  :the-kind '<scroll>) 

(define-object-kind "scroll-*identify*" "*identify*"
  :numeric-id 177
  :text-attr #\d
  :text-char #\?
  :power-lvl 30
  :locations '((30 . 1))
  :weight 5
  :cost 1000
  :sort-value 5013
  :on-read (object-effect (dungeon player item)
	    (interactive-identify-object! dungeon player :type '<powerful>)
	    (possible-identify! player item)
	    :used)
  
  :the-kind '<scroll>) 

(define-object-kind "scroll-remove-curse" "remove curse"
  :numeric-id 180
  :text-attr #\d
  :text-char #\?
  :power-lvl 10
  :locations '((10 . 1))
  :weight 5
  :cost 100
  :sort-value 5014
  :the-kind '<scroll>
  :on-read (object-effect (dungeon player item)
	     (when (plusp (remove-curse! player :light))
	       (print-message! "You feel as if someone is watching over you.")
	       (possible-identify! player item))
	    :used))

(define-object-kind "scroll-illumination" "illumination"
  :numeric-id 181
  :text-attr #\d
  :text-char #\?
  :power-lvl 0
  :locations '((0 . 1) (3 . 1) (10 . 1))
  :weight 5
  :cost 15
  :sort-value 5024
  :on-read (object-effect (dungeon player item)
	     ;;(warn "illumination.")
	     (when (light-area! dungeon player (location-x player)
				(location-y player) (roll-dice 2 8)
				2 :type '<light>) ;; 2d8 dmg, radius 2
	       (possible-identify! player item))
	     :used)
  :the-kind '<scroll>) 

(define-object-kind "scroll-summon-monster" "summon monster"
  :numeric-id 184
  :text-attr #\d
  :text-char #\?
  :power-lvl 1
  :locations '((1 . 1))
  :weight 5
  :cost 0
  :sort-value 5004
  :on-read (object-effect (dungeon player item)
	     ;; (warn "summon")
	     (dotimes (i (randint 3))
	       (when (summon-monster dungeon
				     (location-x player) (location-y player)
				     (dungeon.depth dungeon) :type :any)
		 (possible-identify! player item))))
  :the-kind '<scroll>) 

(define-object-kind "scroll-phase-door" "phase door"
  :numeric-id 185
  :text-attr #\d
  :text-char #\?
  :power-lvl 1
  :locations '((1 . 1))
  :weight 5
  :cost 15
  :sort-value 5008
  :on-read (object-effect (dungeon player item)
	     (teleport-creature! dungeon player player 10)
	     (possible-identify! player item)
	     :used)
  :the-kind '<scroll>) 

(define-object-kind "scroll-teleport" "teleportation"
  :numeric-id 186
  :text-attr #\d
  :text-char #\?
  :power-lvl 10
  :locations '((10 . 1))
  :weight 5
  :cost 40
  :sort-value 5009
  :on-read (object-effect (dungeon player item)
	     ;;  (warn "phase door.")
	     (teleport-creature! dungeon player player 100)
	     (possible-identify! player item)
	     :used)
  :the-kind '<scroll>) 

(define-object-kind "scroll-teleport-lvl" "teleport level"
  :numeric-id 187
  :text-attr #\d
  :text-char #\?
  :power-lvl 20
  :locations '((20 . 1))
  :weight 5
  :cost 50
  :sort-value 5010
  :on-read (object-effect (dungeon player item)
	     (let* ((cur-depth (player.depth player))
		    (dir (cond ((= cur-depth 0)
				:down)
			       (t
				(if (< (random 100) 50)
				    :up
				    :down)))))
	       (if (eq dir :up)
		   (print-message! "You rise up through the ceiling.")
		   (print-message! "You sink through the floor."))

	       (move-creature-to-depth! dungeon player :direction dir :amount 1 :type :teleport)

	       (possible-identify! player item)
	       :used))

  :the-kind '<scroll>) 

(define-object-kind "scroll-monster-confusion" "monster confusion"
  :numeric-id 188
  :text-attr #\d
  :text-char #\?
  :power-lvl 5
  :locations '((5 . 1))
  :weight 5
  :cost 30
  :sort-value 5036
  :the-kind '<scroll>) 

(define-object-kind "scroll-mapping" "magic mapping"
  :numeric-id 189
  :text-attr #\d
  :text-char #\?
  :power-lvl 5
  :locations '((5 . 1))
  :weight 5
  :cost 40
  :sort-value 5025
  :the-kind '<scroll>) 

(define-object-kind "scroll-rune-prot" "rune of protection"
  :numeric-id 190
  :text-attr #\d
  :text-char #\?
  :power-lvl 60
  :locations '((60 . 2) (90 . 4))
  :weight 5
  :cost 500
  :sort-value 5038
  :the-kind '<scroll>) 

(define-object-kind "scroll-*remove-curse*" "*remove curse*"
  :numeric-id 191
  :text-attr #\d
  :text-char #\?
  :power-lvl 50
  :locations '((50 . 2))
  :weight 5
  :cost 8000
  :sort-value 5015
  :the-kind '<scroll>
  :on-read (object-effect (dungeon player item)
	     (remove-curse! player :heavy)
	     (possible-identify! player item)
	     :used))

(define-object-kind "scroll-det-gold" "treasure detection"
  :numeric-id 192
  :text-attr #\d
  :text-char #\?
  :power-lvl 0
  :locations '((0 . 1))
  :weight 5
  :cost 15
  :sort-value 5026
  :on-read (object-effect (dungeon player item)
	    (when (detect-gold! dungeon player item)
	      (possible-identify! player item))
	    :used)
  :the-kind '<scroll>) 

(define-object-kind "scroll-det-item" "object detection"
  :numeric-id 193
  :text-attr #\d
  :text-char #\?
  :power-lvl 0
  :locations '((0 . 1))
  :weight 5
  :cost 15
  :sort-value 5027
  :on-read (object-effect (dungeon player item)
	    (when (detect-normal-objects! dungeon player item)
	      (possible-identify! player item))
	    :used)
  :the-kind '<scroll>) 

(define-object-kind "scroll-det-trap" "trap detection"
  :numeric-id 194
  :text-attr #\d
  :text-char #\?
  :power-lvl 5
  :locations '((5 . 1) (10 . 1))
  :weight 5
  :cost 35
  :sort-value 5028
  :on-read (object-effect (dungeon player item)
	    (when (detect-traps! dungeon player item)
	      (possible-identify! player item))
	    :used)
  :the-kind '<scroll>) 

(define-object-kind "scroll-det-door" "door/stair location"
  :numeric-id 197
  :text-attr #\d
  :text-char #\?
  :power-lvl 5
  :locations '((5 . 1) (10 . 1) (15 . 1))
  :weight 5
  :cost 35
  :sort-value 5029
  :on-read (object-effect (dungeon player item)
	     (detect-doors! dungeon player item)
	     (detect-stairs! dungeon player item)
	     (possible-identify! player item)
	     :used)
  :the-kind '<scroll>) 

(define-object-kind "scroll-acquirement" "acquirement"
  :numeric-id 198
  :text-attr #\d
  :text-char #\?
  :power-lvl 20
  :locations '((20 . 8))
  :weight 5
  :cost 100000
  :sort-value 5046
  :the-kind '<scroll>) 

(define-object-kind "scroll-*acquirement*" "*acquirement*"
  :numeric-id 199
  :text-attr #\d
  :text-char #\?
  :power-lvl 60
  :locations '((60 . 16))
  :weight 5
  :cost 200000
  :sort-value 5047
  :the-kind '<scroll>) 

(define-object-kind "sroll-mass-xenocide" "mass xenocide"
  :numeric-id 200
  :text-attr #\d
  :text-char #\?
  :power-lvl 50
  :locations '((50 . 4))
  :weight 5
  :cost 1000
  :sort-value 5045
  :the-kind '<scroll>) 

(define-object-kind "scroll-det-inv" "detect invisible"
  :numeric-id 201
  :text-attr #\d
  :text-char #\?
  :power-lvl 1
  :locations '((1 . 1))
  :weight 5
  :cost 15
  :sort-value 5030
  :on-read (object-effect (dungeon player item)
	    (when (detect-invisible! dungeon player item +default-detect-radius+)
	      (possible-identify! player item))
	    :used)

  :the-kind '<scroll>) 

(define-object-kind "scroll-aggr-monster" "aggravate monster"
  :numeric-id 202
  :text-attr #\d
  :text-char #\?
  :power-lvl 5
  :locations '((5 . 1))
  :weight 5
  :cost 0
  :sort-value 5001
  :the-kind '<scroll>) 

(define-object-kind "scroll-create-trap" "trap creation"
  :numeric-id 203
  :text-attr #\d
  :text-char #\?
  :power-lvl 10
  :locations '((10 . 1))
  :weight 5
  :cost 0
  :sort-value 5007
  :the-kind '<scroll>) 

(define-object-kind "scroll-destroy-door" "trap/door destruction"
  :numeric-id 204
  :text-attr #\d
  :text-char #\?
  :power-lvl 10
  :locations '((10 . 1))
  :weight 5
  :cost 50
  :sort-value 5039
  :the-kind '<scroll>) 

(define-object-kind "scroll-recharging" "recharging"
  :numeric-id 206
  :text-attr #\d
  :text-char #\?
  :power-lvl 40
  :locations '((40 . 1))
  :weight 5
  :cost 200
  :sort-value 5022
  :the-kind '<scroll>) 

(define-object-kind "scroll-xenocide" "xenocide"
  :numeric-id 207
  :text-attr #\d
  :text-char #\?
  :power-lvl 40
  :locations '((40 . 4))
  :weight 5
  :cost 750
  :sort-value 5044
  :the-kind '<scroll>) 

(define-object-kind "scroll-darkness" "darkness"
  :numeric-id 208
  :text-attr #\d
  :text-char #\?
  :power-lvl 1
  :locations '((1 . 1))
  :weight 5
  :cost 0
  :sort-value 5000
  :on-read (object-effect (dungeon player item)
	     (when (light-area! dungeon player (location-x player)
				(location-y player) (roll-dice 2 8)
				2 :type '<darkness>) ;; 2d8 dmg, radius 2
	       (possible-identify! player item))
	     :used)
  :the-kind '<scroll>) 

(define-object-kind "scroll-protect-from-evil" "protection from evil"
  :numeric-id 209
  :text-attr #\d
  :text-char #\?
  :power-lvl 30
  :locations '((30 . 1))
  :weight 5
  :cost 50
  :sort-value 5037
  :on-read (object-effect (dungeon player item)
	     (modify-creature-state! player '<prot-from-evil>
				     :add (+ (* 3 (player.power-lvl player)) (random 25)))
	     (possible-identify! player item)
	     :used)
  :the-kind '<scroll>) 

(define-object-kind "scroll-satisfy-hunger" "satisfy hunger"
  :numeric-id 210
  :text-attr #\d
  :text-char #\?
  :power-lvl 5
  :locations '((5 . 1))
  :weight 5
  :cost 10
  :sort-value 5032
  :on-read (object-effect (dungeon player item)
	     (let ((curamount (player.satiation player)))
	       (when (< curamount +food-max+)
		 (possible-identify! player item)
		 (modify-satiation! player (- +food-max+ curamount 1))))
	    :used)
  :the-kind '<scroll>) 

(define-object-kind "scroll-dispel-undead" "dispel undead"
  :numeric-id 211
  :text-attr #\d
  :text-char #\?
  :power-lvl 40
  :locations '((40 . 1))
  :weight 5
  :cost 200
  :sort-value 5042
  :the-kind '<scroll>) 

(define-object-kind "scroll-*enchant-weapon*" "*enchant weapon*"
  :numeric-id 212
  :text-attr #\d
  :text-char #\?
  :power-lvl 50
  :locations '((50 . 1))
  :weight 5
  :cost 500
  :sort-value 5021
  :on-read (object-effect (dungeon player item)
	     (let ((retval (enchant-item! dungeon player :type '<weapon> :bonus (+ (randint 3) (randint 3)))))
	       (possible-identify! player item)
	       retval))
  :the-kind '<scroll>) 

(define-object-kind "scroll-curse-weapon" "curse weapon"
  :numeric-id 213
  :text-attr #\d
  :text-char #\?
  :power-lvl 50
  :locations '((50 . 1))
  :weight 5
  :cost 0
  :sort-value 5003
  :the-kind '<scroll>) 

(define-object-kind "scroll-*enchant-armour*" "*enchant armour*"
  :numeric-id 214
  :text-attr #\d
  :text-char #\?
  :power-lvl 50
  :locations '((50 . 1) (50 . 1))
  :weight 5
  :cost 500
  :sort-value 5020
  :on-read (object-effect (dungeon player item)
	     (let ((retval (enchant-item! dungeon player :type '<armour> :bonus (+ 2 (randint 3)))))
	       (possible-identify! player item)
	       retval))
  :the-kind '<scroll>) 

(define-object-kind "scroll-curse-armour" "curse armour"
  :numeric-id 215
  :text-attr #\d
  :text-char #\?
  :power-lvl 50
  :locations '((50 . 1))
  :weight 5
  :cost 0
  :sort-value 5002
  :the-kind '<scroll>) 

(define-object-kind "scroll-summon-undead" "summon undead"
  :numeric-id 216
  :text-attr #\d
  :text-char #\?
  :power-lvl 15
  :locations '((15 . 1))
  :weight 5
  :cost 0
  :sort-value 5005
  :on-read (object-effect (dungeon player item)
	     (dotimes (i (randint 3))
	       (when (summon-monster dungeon
				     (location-x player) (location-y player)
				     (dungeon.depth dungeon) :type :undead)
		 (possible-identify! player item)))
	     :used)
  
  :the-kind '<scroll>) 

(define-object-kind "scroll-blessing" "blessing"
  :numeric-id 217
  :text-attr #\d
  :text-char #\?
  :power-lvl 1
  :locations '((1 . 1))
  :weight 5
  :cost 15
  :sort-value 5033
  :on-read (object-effect (dungeon player item)
	        (modify-creature-state! player '<blessed> :add (+ 6 (random 12)))
		(possible-identify! player item)
		:used)
  :the-kind '<scroll>) 

(define-object-kind "scroll-holy-chant" "holy chant"
  :numeric-id 218
  :text-attr #\d
  :text-char #\?
  :power-lvl 10
  :locations '((10 . 1))
  :weight 5
  :cost 40
  :sort-value 5034
  :on-read (object-effect (dungeon player item)
	     (modify-creature-state! player '<blessed> :add (+ 12 (random 24)))
	     (possible-identify! player item)
	     :used)
  :the-kind '<scroll>) 

(define-object-kind "scroll-holy-prayer" "holy prayer"
  :numeric-id 219
  :text-attr #\d
  :text-char #\?
  :power-lvl 25
  :locations '((25 . 1))
  :weight 5
  :cost 80
  :sort-value 5035
  :on-read (object-effect (dungeon player item)
	     (modify-creature-state! player '<blessed> :add (+ 24 (random 48)))
	     (possible-identify! player item)
	     :used)
  :the-kind '<scroll>) 

(define-object-kind "scroll-wor" "word of recall"
  :numeric-id 220
  :text-attr #\d
  :text-char #\?
  :power-lvl 5
  :locations '((5 . 1))
  :weight 5
  :cost 150
  :sort-value 5011
  :on-read (object-effect (dungeon player item)
	     (when (toggle-word-of-recall! player)
	       (possible-identify! player item))
	     :used)
  :the-kind '<scroll>) 

(define-object-kind "scroll-*destruction*" "*destruction*"
  :numeric-id 221
  :text-attr #\d
  :text-char #\?
  :power-lvl 40
  :locations '((40 . 1))
  :weight 5
  :cost 250
  :sort-value 5041
  :the-kind '<scroll>) 

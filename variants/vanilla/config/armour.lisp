;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/armour.lisp - armour for vanilla variant
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

;;; == footwear

(define-object-kind "soft-leather-boots" "& pair~ of soft leather boots"
  :numeric-id 91
  :x-attr (tile-file 3)
  :x-char (tile-number 20)
  :text-attr #\U
  :text-char #\]
  :depth 3
  :rarity 0
  :locations '((3 . 1))
  :weight 20
  :cost 7
  :sort-value 3302
  :the-kind '<boots>
  :game-values (make-game-values :base-ac 2 :base-dice 1 :num-dice 1)) 

(define-object-kind "hard-leather-boots" "& pair~ of hard leather boots"
  :numeric-id 92
  :x-attr (tile-file 3)
  :x-char (tile-number 21)
  :text-attr #\U
  :text-char #\]
  :depth 5
  :rarity 0
  :locations '((5 . 1))
  :weight 40
  :cost 12
  :sort-value 3303
  :the-kind '<boots>
  :game-values (make-game-values :base-ac 3 :base-dice 1 :num-dice 1)) 

(define-object-kind "metal-boots" "& pair~ of metal shod boots"
  :numeric-id 93
  :x-attr (tile-file 3)
  :x-char (tile-number 22)
  :text-attr #\s
  :text-char #\]
  :depth 20
  :rarity 0
  :locations '((20 . 1))
  :weight 80
  :cost 50
  :sort-value 3306
  :the-kind '<boots>
  :game-values (make-game-values :base-ac 6 :base-dice 1 :num-dice 1)) 

;;; == headwear

(define-object-kind "hard-leather-cap" "& hard leather cap~"
  :numeric-id 94
  :x-attr (tile-file 3)
  :x-char (tile-number 70)
  :text-attr #\u
  :text-char #\]
  :depth 3
  :rarity 0
  :locations '((3 . 1))
  :weight 15
  :cost 12
  :sort-value 3502
  :the-kind '<helmet>
  :game-values (make-game-values :base-ac 2)) 

(define-object-kind "metal-cap" "& metal cap~"
  :numeric-id 95
  :x-attr (tile-file 3)
  :x-char (tile-number 11)
  :text-attr #\s
  :text-char #\]
  :depth 10
  :rarity 0
  :locations '((10 . 1))
  :weight 20
  :cost 30
  :sort-value 3503
  :the-kind '<helmet>
  :game-values (make-game-values :base-ac 3 :base-dice 1 :num-dice 1)) 

(define-object-kind "iron-helm" "& iron helm~"
  :numeric-id 96
  :x-attr (tile-file 3)
  :x-char (tile-number 12)
  :text-attr #\s
  :text-char #\]
  :depth 20
  :rarity 0
  :locations '((20 . 1))
  :weight 75
  :cost 75
  :sort-value 3505
  :the-kind '<helmet>
  :game-values (make-game-values :base-ac 5 :base-dice 3 :num-dice 1)) 

(define-object-kind "steel-helm" "& steel helm~"
  :numeric-id 97
  :x-attr (tile-file 3)
  :x-char (tile-number 13)
  :text-attr #\W
  :text-char #\]
  :depth 40
  :rarity 0
  :locations '((40 . 1))
  :weight 60
  :cost 200
  :sort-value 3506
  :the-kind '<helmet>
  :game-values (make-game-values :base-ac 6 :base-dice 3 :num-dice 1)) 

(define-object-kind "iron-crown" "& iron crown~"
  :numeric-id 98
  :x-attr (tile-file 3)
  :x-char (tile-number 15)
  :text-attr #\s
  :text-char #\]
  :depth 45
  :rarity 0
  :locations '((45 . 1))
  :weight 20
  :cost 500
  :sort-value 3610
  :the-kind '<crown>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "golden-crown" "& golden crown~"
  :numeric-id 99
  :x-attr (tile-file 3)
  :x-char (tile-number 16)
  :text-attr #\y
  :text-char #\]
  :depth 45
  :rarity 0
  :locations '((45 . 1))
  :weight 30
  :cost 1000
  :sort-value 3611
  :the-kind '<crown>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<acid>))) 

(define-object-kind "jewel-crown" "& jewel encrusted crown~"
  :numeric-id 100
  :x-attr (tile-file 3)
  :x-char (tile-number 17)
  :text-attr #\v
  :text-char #\]
  :depth 50
  :rarity 0
  :locations '((50 . 1))
  :weight 40
  :cost 2000
  :sort-value 3612
  :the-kind '<crown>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<acid>))) 

(define-object-kind "massive-iron-crown" "& massive iron crown~"
  :numeric-id 499
  :text-attr #\D
  :text-char #\]
  :depth 44
  :rarity 0
  :weight 20
  :cost 1000
  :flags '(<instant-artifact>)
  :sort-value 3650
  :the-kind '<crown>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

;;; == bodywear

(define-object-kind "robe" "& robe~"
  :numeric-id 101
  :x-attr (tile-file 3)
  :x-char (tile-number 34)
  :text-attr #\b
  :text-char #\(
  :depth 1
  :rarity 0
  :locations '((1 . 1) (50 . 1))
  :weight 20
  :cost 4
  :sort-value 3902
  :the-kind '<soft-body-armour>
  :game-values (make-game-values :base-ac 2)) 

(define-object-kind "filthy-rag" "& filthy rag~"
  :numeric-id 102
  :x-attr (tile-file 3)
  :x-char (tile-number 33)
  :text-attr #\D
  :text-char #\(
  :depth 0
  :rarity 0
  :locations '((0 . 1))
  :weight 20
  :cost 1
  :sort-value 3901
  :the-kind '<soft-body-armour>
  :game-values (make-game-values :base-ac 1 :ac-modifier -1)) 

(define-object-kind "soft-leather-armour" "soft leather armour~"
  :numeric-id 103
  :x-attr (tile-file 3)
  :x-char (tile-number 38)
  :text-attr #\U
  :text-char #\(
  :depth 3
  :rarity 0
  :locations '((3 . 1))
  :weight 80
  :cost 18
  :sort-value 3904
  :the-kind '<soft-body-armour>
  :game-values (make-game-values :base-ac 4)) 

(define-object-kind "soft-studded-leather" "soft studded leather~"
  :numeric-id 104
  :x-attr (tile-file 3)
  :x-char (tile-number 39)
  :text-attr #\U
  :text-char #\(
  :depth 3
  :rarity 0
  :locations '((3 . 1))
  :weight 90
  :cost 35
  :sort-value 3905
  :the-kind '<soft-body-armour>
  :game-values (make-game-values :base-ac 5 :base-dice 1 :num-dice 1)) 

(define-object-kind "hard-leather-armour" "hard leather armour~"
  :numeric-id 105
  :x-attr (tile-file 3)
  :x-char (tile-number 40)
  :text-attr #\U
  :text-char #\(
  :depth 5
  :rarity 0
  :locations '((5 . 1))
  :weight 100
  :cost 150
  :sort-value 3906
  :the-kind '<soft-body-armour>
  :game-values (make-game-values :base-ac 6 :base-dice 1 :num-dice 1 :tohit-modifier -1)) 

(define-object-kind "hard-studded-leather" "hard studded leather~"
  :numeric-id 106
  :x-attr (tile-file 3)
  :x-char (tile-number 41)
  :text-attr #\U
  :text-char #\(
  :depth 10
  :rarity 0
  :locations '((10 . 1))
  :weight 110
  :cost 200
  :sort-value 3907
  :the-kind '<soft-body-armour>
  :game-values (make-game-values :base-ac 7 :base-dice 2 :num-dice 1 :tohit-modifier -1)) 

(define-object-kind "leather-scale-mail" "leather scale mail~"
  :numeric-id 107
  :x-attr (tile-file 3)
  :x-char (tile-number 42)
  :text-attr #\U
  :text-char #\(
  :depth 15
  :rarity 0
  :locations '((15 . 1))
  :weight 140
  :cost 450
  :sort-value 3911
  :the-kind '<soft-body-armour>
  :game-values (make-game-values :base-ac 11 :base-dice 1 :num-dice 1 :tohit-modifier -1)) 

(define-object-kind "metal-scale-mail" "metal scale mail~"
  :numeric-id 108
  :x-attr (tile-file 3)
  :x-char (tile-number 43)
  :text-attr #\s
  :text-char #\[
  :depth 25
  :rarity 0
  :locations '((25 . 1))
  :weight 250
  :cost 550
  :sort-value 4003
  :the-kind '<hard-body-armour>
  :game-values (make-game-values :base-ac 13 :base-dice 4 :num-dice 1 :tohit-modifier -2)) 

(define-object-kind "chain-mail" "chain mail~"
  :numeric-id 109
  :x-attr (tile-file 3)
  :x-char (tile-number 45)
  :text-attr #\s
  :text-char #\[
  :depth 25
  :rarity 0
  :locations '((25 . 1))
  :weight 220
  :cost 750
  :sort-value 4004
  :the-kind '<hard-body-armour>
  :game-values (make-game-values :base-ac 14 :base-dice 4 :num-dice 1 :tohit-modifier -2)) 

(define-object-kind "rusty-chain-mail" "rusty chain mail~"
  :numeric-id 110
  :x-attr (tile-file 3)
  :x-char (tile-number 44)
  :text-attr #\r
  :text-char #\[
  :depth 25
  :rarity 0
  :locations '((25 . 1))
  :weight 200
  :cost 550
  :sort-value 4001
  :the-kind '<hard-body-armour>
  :game-values (make-game-values :base-ac 14 :ac-modifier -8 :base-dice 4 :num-dice 1 :tohit-modifier -5)) 

(define-object-kind "augm-chain-mail" "augmented chain mail~"
  :numeric-id 111
  :x-attr (tile-file 3)
  :x-char (tile-number 47)
  :text-attr #\s
  :text-char #\[
  :depth 30
  :rarity 0
  :locations '((30 . 1))
  :weight 270
  :cost 900
  :sort-value 4006
  :the-kind '<hard-body-armour>
  :game-values (make-game-values :base-ac 16 :base-dice 4 :num-dice 1 :tohit-modifier -2)) 

(define-object-kind "bar-chain-mail" "bar chain mail~"
  :numeric-id 112
  :x-attr (tile-file 3)
  :x-char (tile-number 48)
  :text-attr #\s
  :text-char #\[
  :depth 35
  :rarity 0
  :locations '((35 . 1))
  :weight 280
  :cost 950
  :sort-value 4008
  :the-kind '<hard-body-armour>
  :game-values (make-game-values :base-ac 18 :base-dice 4 :num-dice 1 :tohit-modifier -2)) 

(define-object-kind "metal-brigandine" "metal brigandine armour~"
  :numeric-id 113
  :x-attr (tile-file 3)
  :x-char (tile-number 49)
  :text-attr #\s
  :text-char #\[
  :depth 35
  :rarity 0
  :locations '((35 . 1))
  :weight 290
  :cost 1100
  :sort-value 4009
  :the-kind '<hard-body-armour>
  :game-values (make-game-values :base-ac 19 :base-dice 4 :num-dice 1 :tohit-modifier -3)) 

(define-object-kind "partial-plate" "partial plate armour~"
  :numeric-id 114
  :x-attr (tile-file 3)
  :x-char (tile-number 50)
  :text-attr #\W
  :text-char #\[
  :depth 45
  :rarity 0
  :locations '((45 . 1))
  :weight 260
  :cost 1200
  :sort-value 4012
  :the-kind '<hard-body-armour>
  :game-values (make-game-values :base-ac 22 :base-dice 6 :num-dice 1 :tohit-modifier -3)) 

(define-object-kind "metal-lamellar" "metal lamellar armour~"
  :numeric-id 115
  :x-attr (tile-file 3)
  :x-char (tile-number 51)
  :text-attr #\W
  :text-char #\[
  :depth 45
  :rarity 0
  :locations '((45 . 1))
  :weight 340
  :cost 1250
  :sort-value 4013
  :the-kind '<hard-body-armour>
  :game-values (make-game-values :base-ac 23 :base-dice 6 :num-dice 1 :tohit-modifier -3)) 

(define-object-kind "full-plate" "full plate armour~"
  :numeric-id 116
  :x-attr (tile-file 3)
  :x-char (tile-number 52)
  :text-attr #\W
  :text-char #\[
  :depth 45
  :rarity 0
  :locations '((45 . 1))
  :weight 380
  :cost 1350
  :sort-value 4015
  :the-kind '<hard-body-armour>
  :game-values (make-game-values :base-ac 25 :base-dice 4 :num-dice 2 :tohit-modifier -3)) 

(define-object-kind "ribbed-plate" "ribbed plate armour~"
  :numeric-id 117
  :x-attr (tile-file 3)
  :x-char (tile-number 53)
  :text-attr #\W
  :text-char #\[
  :depth 50
  :rarity 0
  :locations '((50 . 1))
  :weight 380
  :cost 1500
  :sort-value 4018
  :the-kind '<hard-body-armour>
  :game-values (make-game-values :base-ac 28 :base-dice 4 :num-dice 2 :tohit-modifier -3)) 

(define-object-kind "adamantite-plate" "adamantite plate mail~"
  :numeric-id 118
  :x-attr (tile-file 3)
  :x-char (tile-number 56)
  :text-attr #\G
  :text-char #\[
  :depth 75
  :rarity 0
  :locations '((75 . 8))
  :weight 420
  :cost 20000
  :sort-value 4030
  :the-kind '<hard-body-armour>
  :game-values (make-game-values :base-ac 40 :base-dice 4 :num-dice 2 :tohit-modifier -4 :ignores '(<acid>))) 

(define-object-kind "mithril-plate" "mithril plate mail~"
  :numeric-id 119
  :x-attr (tile-file 3)
  :x-char (tile-number 54)
  :text-attr #\B
  :text-char #\[
  :depth 60
  :rarity 0
  :locations '((60 . 4))
  :weight 300
  :cost 15000
  :sort-value 4025
  :the-kind '<hard-body-armour>
  :game-values (make-game-values :base-ac 35 :base-dice 4 :num-dice 2 :tohit-modifier -3 :ignores '(<acid>))) 

(define-object-kind "mithril-chain-mail" "mithril chain mail~"
  :numeric-id 120
  :x-attr (tile-file 3)
  :x-char (tile-number 55)
  :text-attr #\B
  :text-char #\[
  :depth 55
  :rarity 0
  :locations '((55 . 4))
  :weight 150
  :cost 7000
  :sort-value 4020
  :the-kind '<hard-body-armour>
  :game-values (make-game-values :base-ac 28 :base-dice 4 :num-dice 1 :tohit-modifier -1 :ignores '(<acid>))) 

(define-object-kind "double-chain-mail" "double chain mail~"
  :numeric-id 121
  :x-attr (tile-file 3)
  :x-char (tile-number 46)
  :text-attr #\s
  :text-char #\[
  :depth 30
  :rarity 0
  :locations '((30 . 1))
  :weight 250
  :cost 850
  :sort-value 4007
  :the-kind '<hard-body-armour>
  :game-values (make-game-values :base-ac 16 :base-dice 4 :num-dice 1 :tohit-modifier -2)) 

;;; == handwear

(define-object-kind "cloak" "& cloak~"
  :numeric-id 123
  :x-attr (tile-file 3)
  :x-char (tile-number 0)
  :text-attr #\g
  :text-char #\(
  :depth 1
  :rarity 0
  :locations '((1 . 1) (20 . 1))
  :weight 10
  :cost 3
  :sort-value 3801
  :the-kind '<cloak>
  :game-values (make-game-values :base-ac 1)) 

(define-object-kind "shadow-cloak" "& shadow cloak~"
  :numeric-id 124
  :x-attr (tile-file 3)
  :x-char (tile-number 1)
  :text-attr #\D
  :text-char #\(
  :depth 60
  :rarity 0
  :locations '((60 . 4))
  :weight 5
  :cost 4000
  :sort-value 3806
  :the-kind '<cloak>
  :game-values (make-game-values :base-ac 6 :ac-modifier 4)) 

(define-object-kind "leather-gloves" "& set~ of leather gloves"
  :numeric-id 125
  :x-attr (tile-file 3)
  :x-char (tile-number 23)
  :text-attr #\U
  :text-char #\]
  :depth 1
  :rarity 0
  :locations '((1 . 1))
  :weight 5
  :cost 3
  :sort-value 3401
  :the-kind '<gloves>
  :game-values (make-game-values :base-ac 1)) 

(define-object-kind "gauntlets" "& set~ of gauntlets"
  :numeric-id 126
  :x-attr (tile-file 3)
  :x-char (tile-number 26)
  :text-attr #\U
  :text-char #\]
  :depth 10
  :rarity 0
  :locations '((10 . 1))
  :weight 25
  :cost 35
  :sort-value 3402
  :the-kind '<gloves>
  :game-values (make-game-values :base-ac 2 :base-dice 1 :num-dice 1)) 

(define-object-kind "cesti" "& set~ of cesti"
  :numeric-id 127
  :x-attr (tile-file 3)
  :x-char (tile-number 27)
  :text-attr #\W
  :text-char #\]
  :depth 50
  :rarity 0
  :locations '((50 . 1))
  :weight 40
  :cost 100
  :sort-value 3405
  :the-kind '<gloves>
  :game-values (make-game-values :base-ac 5 :base-dice 1 :num-dice 1)) 

;;; == shieldwear

(define-object-kind "small-leather-shield" "& small leather shield~"
  :numeric-id 128
  :x-attr (tile-file 3)
  :x-char (tile-number 90)
  :text-attr #\U
  :text-char #\)
  :depth 3
  :rarity 0
  :locations '((3 . 1))
  :weight 50
  :cost 30
  :sort-value 3702
  :the-kind '<shield>
  :game-values (make-game-values :base-ac 2 :base-dice 1 :num-dice 1)) 

(define-object-kind "large-leather-shield" "& large leather shield~"
  :numeric-id 129
  :x-attr (tile-file 3)
  :x-char (tile-number 29)
  :text-attr #\U
  :text-char #\)
  :depth 15
  :rarity 0
  :locations '((15 . 1))
  :weight 100
  :cost 120
  :sort-value 3704
  :the-kind '<shield>
  :game-values (make-game-values :base-ac 4 :base-dice 2 :num-dice 1)) 

(define-object-kind "small-metal-shield" "& small metal shield~"
  :numeric-id 130
  :x-attr (tile-file 3)
  :x-char (tile-number 93)
  :text-attr #\s
  :text-char #\)
  :depth 10
  :rarity 0
  :locations '((10 . 1))
  :weight 65
  :cost 50
  :sort-value 3703
  :the-kind '<shield>
  :game-values (make-game-values :base-ac 3 :base-dice 2 :num-dice 1)) 

(define-object-kind "large-metal-shield" "& large metal shield~"
  :numeric-id 131
  :x-attr (tile-file 3)
  :x-char (tile-number 89)
  :text-attr #\s
  :text-char #\)
  :depth 30
  :rarity 0
  :locations '((30 . 1))
  :weight 120
  :cost 200
  :sort-value 3705
  :the-kind '<shield>
  :game-values (make-game-values :base-ac 5 :base-dice 3 :num-dice 1)) 

(define-object-kind "deflection-shield" "& shield~ of deflection"
  :numeric-id 122
  :x-attr (tile-file 3)
  :x-char (tile-number 32)
  :text-attr #\B
  :text-char #\[
  :depth 70
  :rarity 0
  :locations '((70 . 8))
  :weight 100
  :cost 10000
  :sort-value 3710
  :the-kind '<shield>
  :game-values (make-game-values :base-ac 10 :ac-modifier 10 :base-dice 1 :num-dice 1 :ignores '(<acid>))) 

;;; === dragonwear

(define-object-kind "dsm-black" "black dragon scale mail~"
  :numeric-id 400
  :x-attr (tile-file 3)
  :x-char (tile-number 59)
  :text-attr #\s
  :text-char #\[
  :depth 60
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(60 0 0 0)
  :weight 200
  :cost 30000
  :flags '(<activation>)
  :sort-value 4101
  :the-kind '<dsm-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<acid>))) 

(define-object-kind "dsm-blue" "blue dragon scale mail~"
  :numeric-id 401
  :x-attr (tile-file 3)
  :x-char (tile-number 57)
  :text-attr #\b
  :text-char #\[
  :depth 40
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(40 0 0 0)
  :weight 200
  :cost 35000
  :flags '(<activation>)
  :sort-value 4102
  :the-kind '<dsm-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<electricity>))) 

(define-object-kind "dsm-white" "white dragon scale mail~"
  :numeric-id 402
  :x-attr (tile-file 3)
  :x-char (tile-number 58)
  :text-attr #\w
  :text-char #\[
  :depth 50
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(50 0 0 0)
  :weight 200
  :cost 40000
  :flags '(<activation>)
  :sort-value 4103
  :the-kind '<dsm-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<cold>))) 

(define-object-kind "dsm-red" "red dragon scale mail~"
  :numeric-id 403
  :x-attr (tile-file 3)
  :x-char (tile-number 60)
  :text-attr #\r
  :text-char #\[
  :depth 80
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(80 0 0 0)
  :weight 200
  :cost 100000
  :flags '(<activation>)
  :sort-value 4104
  :the-kind '<dsm-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<fire>))) 

(define-object-kind "dsm-green" "green dragon scale mail~"
  :numeric-id 404
  :x-attr (tile-file 3)
  :x-char (tile-number 61)
  :text-attr #\g
  :text-char #\[
  :depth 70
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(70 0 0 0)
  :weight 200
  :cost 80000
  :flags '(<activation>)
  :sort-value 4105
  :the-kind '<dsm-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<poison>))) 

(define-object-kind "dsm-mh" "multi-hued dragon scale mail~"
  :numeric-id 405
  :x-attr (tile-file 3)
  :x-char (tile-number 68)
  :text-attr #\v
  :text-char #\[
  :depth 100
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(100 0 0 0)
  :weight 200
  :cost 150000
  :flags '(<activation>)
  :sort-value 4106
  :the-kind '<dsm-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists
                              '(<poison> <cold> <fire> <electricity> <acid>))) 

(define-object-kind "dsm-shining" "shining dragon scale mail~"
  :numeric-id 406
  :x-attr (tile-file 3)
  :x-char (tile-number 64)
  :text-attr #\o
  :text-char #\[
  :depth 65
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(65 0 0 0)
  :weight 200
  :cost 60000
  :flags '(<activation>)
  :sort-value 4110
  :the-kind '<dsm-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<darkness> <light>))) 

(define-object-kind "dsm-law" "law dragon scale mail~"
  :numeric-id 407
  :x-attr (tile-file 3)
  :x-char (tile-number 66)
  :text-attr #\B
  :text-char #\[
  :depth 80
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(80 0 0 0)
  :weight 200
  :cost 80000
  :flags '(<activation>)
  :sort-value 4112
  :the-kind '<dsm-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<shards> <sound>))) 

(define-object-kind "dsm-bronze" "bronze dragon scale mail~"
  :numeric-id 408
  :x-attr (tile-file 3)
  :x-char (tile-number 62)
  :text-attr #\U
  :text-char #\[
  :depth 55
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(55 0 0 0)
  :weight 200
  :cost 30000
  :flags '(<activation>)
  :sort-value 4114
  :the-kind '<dsm-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<confusion>))) 

(define-object-kind "dsm-gold" "gold dragon scale mail~"
  :numeric-id 409
  :x-attr (tile-file 3)
  :x-char (tile-number 63)
  :text-attr #\y
  :text-char #\[
  :depth 65
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(65 0 0 0)
  :weight 200
  :cost 40000
  :flags '(<activation>)
  :sort-value 4116
  :the-kind '<dsm-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<sound>))) 

(define-object-kind "dsm-chaos" "chaos dragon scale mail~"
  :numeric-id 410
  :x-attr (tile-file 3)
  :x-char (tile-number 65)
  :text-attr #\v
  :text-char #\[
  :depth 75
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(75 0 0 0)
  :weight 200
  :cost 70000
  :flags '(<activation>)
  :sort-value 4118
  :the-kind '<dsm-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<disenchant> <chaos> <confusion>))) 

(define-object-kind "dsm-balance" "balance dragon scale mail~"
  :numeric-id 411
  :x-attr (tile-file 3)
  :x-char (tile-number 67)
  :text-attr #\v
  :text-char #\[
  :depth 90
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(90 0 0 0)
  :weight 200
  :cost 100000
  :flags '(<activation>)
  :sort-value 4120
  :the-kind '<dsm-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists
                              '(<disenchant> <chaos> <shards> <sound> <confusion>))) 

(define-object-kind "dsm-power" "power dragon scale mail~"
  :numeric-id 412
  :x-attr (tile-file 3)
  :x-char (tile-number 69)
  :text-attr #\v
  :text-char #\[
  :depth 110
  :rarity 0
  :chance #(64 0 0 0)
  :locale #(110 0 0 0)
  :weight 200
  :cost 300000
  :flags '(<activation>)
  :sort-value 4130
  :the-kind '<dsm-armour>
  :game-values (make-game-values :base-ac 40 :ac-modifier 15 :base-dice 4 :num-dice 2 :tohit-modifier -3 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists
                              '(<chaos> <nether> <nexus> <darkness> <light> <confusion> <poison> <electricity> <cold>
                                <fire> <acid>))) 


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

(define-object-kind "soft-leather-boots" "& pair~ of soft leather boots"
  :numeric-id 91
  :x-attr #\U
  :x-char #\]
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 20
  :cost 7
  :obj-type '(<soft-leather> <boots>)
  :sort-value 3302
  :the-kind '<boots>
  :game-values (make-game-values :base-ac 2 :base-dice 1 :num-dice 1)) 

(define-object-kind "hard-leather-boots" "& pair~ of hard leather boots"
  :numeric-id 92
  :x-attr #\U
  :x-char #\]
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 40
  :cost 12
  :obj-type '(<hard-leather> <boots>)
  :sort-value 3303
  :the-kind '<boots>
  :game-values (make-game-values :base-ac 3 :base-dice 1 :num-dice 1)) 

(define-object-kind "metal-boots" "& pair~ of metal shod boots"
  :numeric-id 93
  :x-attr #\s
  :x-char #\]
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 80
  :cost 50
  :obj-type '(<metal-shod> <boots>)
  :sort-value 3306
  :the-kind '<boots>
  :game-values (make-game-values :base-ac 6 :base-dice 1 :num-dice 1)) 

(define-object-kind "hard-leather-cap" "& hard leather cap~"
  :numeric-id 94
  :x-attr #\u
  :x-char #\]
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 15
  :cost 12
  :obj-type '(<hard-leather> <headgear> <helmet>)
  :sort-value 3502
  :the-kind '<headgear>
  :game-values (make-game-values :base-ac 2)) 

(define-object-kind "metal-cap" "& metal cap~"
  :numeric-id 95
  :x-attr #\s
  :x-char #\]
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 20
  :cost 30
  :obj-type '(<metal-cap> <headgear> <helmet>)
  :sort-value 3503
  :the-kind '<headgear>
  :game-values (make-game-values :base-ac 3 :base-dice 1 :num-dice 1)) 

(define-object-kind "iron-helm" "& iron helm~"
  :numeric-id 96
  :x-attr #\s
  :x-char #\]
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 75
  :cost 75
  :obj-type '(<iron-helm> <headgear> <helmet>)
  :sort-value 3505
  :the-kind '<headgear>
  :game-values (make-game-values :base-ac 5 :base-dice 3 :num-dice 1)) 

(define-object-kind "steel-helm" "& steel helm~"
  :numeric-id 97
  :x-attr #\W
  :x-char #\]
  :depth 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 60
  :cost 200
  :obj-type '(<steel-helm> <headgear> <helmet>)
  :sort-value 3506
  :the-kind '<headgear>
  :game-values (make-game-values :base-ac 6 :base-dice 3 :num-dice 1)) 

(define-object-kind "iron-crown" "& iron crown~"
  :numeric-id 98
  :x-attr #\s
  :x-char #\]
  :depth 45
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(45 0 0 0)
  :weight 20
  :cost 500
  :obj-type '(<iron> <headgear> <crown>)
  :sort-value 3610
  :the-kind '<headgear>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "golden-crown" "& golden crown~"
  :numeric-id 99
  :x-attr #\y
  :x-char #\]
  :depth 45
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(45 0 0 0)
  :weight 30
  :cost 1000
  :obj-type '(<golden> <headgear> <crown>)
  :sort-value 3611
  :the-kind '<headgear>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<acid>))) 

(define-object-kind "jewel-crown" "& jewel encrusted crown~"
  :numeric-id 100
  :x-attr #\v
  :x-char #\]
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 40
  :cost 2000
  :obj-type '(<jeweled> <headgear> <crown>)
  :sort-value 3612
  :the-kind '<headgear>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<acid>))) 

(define-object-kind "robe" "& robe~"
  :numeric-id 101
  :x-attr #\b
  :x-char #\(
  :depth 1
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(1 50 0 0)
  :weight 20
  :cost 4
  :obj-type '(<robe> <body-armour> <soft>)
  :sort-value 3902
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 2)) 

(define-object-kind "filthy-rag" "& filthy rag~"
  :numeric-id 102
  :x-attr #\D
  :x-char #\(
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 20
  :cost 1
  :obj-type '(<filthy-rag> <body-armour> <soft>)
  :sort-value 3901
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 1 :ac-modifier -1)) 

(define-object-kind "soft-leather-armour" "soft leather armour~"
  :numeric-id 103
  :x-attr #\U
  :x-char #\(
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 80
  :cost 18
  :obj-type '(<soft-leather> <body-armour> <soft>)
  :sort-value 3904
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 4)) 

(define-object-kind "soft-studded-leather" "soft studded leather~"
  :numeric-id 104
  :x-attr #\U
  :x-char #\(
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 90
  :cost 35
  :obj-type '(<soft-studded> <body-armour> <soft>)
  :sort-value 3905
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 5 :base-dice 1 :num-dice 1)) 

(define-object-kind "hard-leather-armour" "hard leather armour~"
  :numeric-id 105
  :x-attr #\U
  :x-char #\(
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 100
  :cost 150
  :obj-type '(<hard-leather> <body-armour> <soft>)
  :sort-value 3906
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 6 :base-dice 1 :num-dice 1 :tohit-modifier -1)) 

(define-object-kind "hard-studded-leather" "hard studded leather~"
  :numeric-id 106
  :x-attr #\U
  :x-char #\(
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 110
  :cost 200
  :obj-type '(<hard-studded> <body-armour> <soft>)
  :sort-value 3907
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 7 :base-dice 2 :num-dice 1 :tohit-modifier -1)) 

(define-object-kind "leather-scale-mail" "leather scale mail~"
  :numeric-id 107
  :x-attr #\U
  :x-char #\(
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 140
  :cost 450
  :obj-type '(<leather-scale> <body-armour> <soft>)
  :sort-value 3911
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 11 :base-dice 1 :num-dice 1 :tohit-modifier -1)) 

(define-object-kind "metal-scale-mail" "metal scale mail~"
  :numeric-id 108
  :x-attr #\s
  :x-char #\[
  :depth 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 250
  :cost 550
  :obj-type '(<metal-scale> <body-armour> <hard>)
  :sort-value 4003
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 13 :base-dice 4 :num-dice 1 :tohit-modifier -2)) 

(define-object-kind "chain-mail" "chain mail~"
  :numeric-id 109
  :x-attr #\s
  :x-char #\[
  :depth 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 220
  :cost 750
  :obj-type '(<chain> <body-armour> <hard>)
  :sort-value 4004
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 14 :base-dice 4 :num-dice 1 :tohit-modifier -2)) 

(define-object-kind "rusty-chain-mail" "rusty chain mail~"
  :numeric-id 110
  :x-attr #\r
  :x-char #\[
  :depth 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 200
  :cost 550
  :obj-type '(<rusty-chain> <body-armour> <hard>)
  :sort-value 4001
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 14 :ac-modifier -8 :base-dice 4 :num-dice 1 :tohit-modifier -5)) 

(define-object-kind "augm-chain-mail" "augmented chain mail~"
  :numeric-id 111
  :x-attr #\s
  :x-char #\[
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 270
  :cost 900
  :obj-type '(<augmented-chain> <body-armour> <hard>)
  :sort-value 4006
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 16 :base-dice 4 :num-dice 1 :tohit-modifier -2)) 

(define-object-kind "bar-chain-mail" "bar chain mail~"
  :numeric-id 112
  :x-attr #\s
  :x-char #\[
  :depth 35
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(35 0 0 0)
  :weight 280
  :cost 950
  :obj-type '(<bar-chain> <body-armour> <hard>)
  :sort-value 4008
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 18 :base-dice 4 :num-dice 1 :tohit-modifier -2)) 

(define-object-kind "metal-brigandine" "metal brigandine armour~"
  :numeric-id 113
  :x-attr #\s
  :x-char #\[
  :depth 35
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(35 0 0 0)
  :weight 290
  :cost 1100
  :obj-type '(<metal-brigandine> <body-armour> <hard>)
  :sort-value 4009
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 19 :base-dice 4 :num-dice 1 :tohit-modifier -3)) 

(define-object-kind "partial-plate" "partial plate armour~"
  :numeric-id 114
  :x-attr #\W
  :x-char #\[
  :depth 45
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(45 0 0 0)
  :weight 260
  :cost 1200
  :obj-type '(<partial-plate> <body-armour> <hard>)
  :sort-value 4012
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 22 :base-dice 6 :num-dice 1 :tohit-modifier -3)) 

(define-object-kind "metal-lamellar" "metal lamellar armour~"
  :numeric-id 115
  :x-attr #\W
  :x-char #\[
  :depth 45
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(45 0 0 0)
  :weight 340
  :cost 1250
  :obj-type '(<metal-lamellar> <body-armour> <hard>)
  :sort-value 4013
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 23 :base-dice 6 :num-dice 1 :tohit-modifier -3)) 

(define-object-kind "full-plate" "full plate armour~"
  :numeric-id 116
  :x-attr #\W
  :x-char #\[
  :depth 45
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(45 0 0 0)
  :weight 380
  :cost 1350
  :obj-type '(<full-plate> <body-armour> <hard>)
  :sort-value 4015
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 25 :base-dice 4 :num-dice 2 :tohit-modifier -3)) 

(define-object-kind "ribbed-plate" "ribbed plate armour~"
  :numeric-id 117
  :x-attr #\W
  :x-char #\[
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 380
  :cost 1500
  :obj-type '(<ribbed-plate> <body-armour> <hard>)
  :sort-value 4018
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 28 :base-dice 4 :num-dice 2 :tohit-modifier -3)) 

(define-object-kind "adamantite-plater" "adamantite plate mail~"
  :numeric-id 118
  :x-attr #\G
  :x-char #\[
  :depth 75
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(75 0 0 0)
  :weight 420
  :cost 20000
  :obj-type '(<adamantite-plate> <body-armour> <hard>)
  :sort-value 4030
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 40 :base-dice 4 :num-dice 2 :tohit-modifier -4 :ignores '(<acid>))) 

(define-object-kind "mithril-plate" "mithril plate mail~"
  :numeric-id 119
  :x-attr #\B
  :x-char #\[
  :depth 60
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(60 0 0 0)
  :weight 300
  :cost 15000
  :obj-type '(<mithril-plate> <body-armour> <hard>)
  :sort-value 4025
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 35 :base-dice 4 :num-dice 2 :tohit-modifier -3 :ignores '(<acid>))) 

(define-object-kind "mithril-chain-mail" "mithril chain mail~"
  :numeric-id 120
  :x-attr #\B
  :x-char #\[
  :depth 55
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(55 0 0 0)
  :weight 150
  :cost 7000
  :obj-type '(<mithril-chain> <body-armour> <hard>)
  :sort-value 4020
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 28 :base-dice 4 :num-dice 1 :tohit-modifier -1 :ignores '(<acid>))) 

(define-object-kind "double-chain-mail" "double chain mail~"
  :numeric-id 121
  :x-attr #\s
  :x-char #\[
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 250
  :cost 850
  :obj-type '(<double-chain> <body-armour> <hard>)
  :sort-value 4007
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 16 :base-dice 4 :num-dice 1 :tohit-modifier -2)) 

(define-object-kind "deflection-shield" "& shield~ of deflection"
  :numeric-id 122
  :x-attr #\B
  :x-char #\[
  :depth 70
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(70 0 0 0)
  :weight 100
  :cost 10000
  :obj-type '(<shield> <large> <metal> <deflection>)
  :sort-value 3710
  :the-kind '<shield>
  :game-values (make-game-values :base-ac 10 :ac-modifier 10 :base-dice 1 :num-dice 1 :ignores '(<acid>))) 

(define-object-kind "cloak" "& cloak~"
  :numeric-id 123
  :x-attr #\g
  :x-char #\(
  :depth 1
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(1 20 0 0)
  :weight 10
  :cost 3
  :obj-type '(<cloth> <cloak>)
  :sort-value 3801
  :the-kind '<cloak>
  :game-values (make-game-values :base-ac 1)) 

(define-object-kind "shadow-cloak" "& shadow cloak~"
  :numeric-id 124
  :x-attr #\D
  :x-char #\(
  :depth 60
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(60 0 0 0)
  :weight 5
  :cost 4000
  :obj-type '(<shadow> <cloak>)
  :sort-value 3806
  :the-kind '<cloak>
  :game-values (make-game-values :base-ac 6 :ac-modifier 4)) 

(define-object-kind "leather-gloves" "& set~ of leather gloves"
  :numeric-id 125
  :x-attr #\U
  :x-char #\]
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 5
  :cost 3
  :obj-type '(<leather> <gloves>)
  :sort-value 3401
  :the-kind '<gloves>
  :game-values (make-game-values :base-ac 1)) 

(define-object-kind "gauntlets" "& set~ of gauntlets"
  :numeric-id 126
  :x-attr #\U
  :x-char #\]
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 25
  :cost 35
  :obj-type '(<gauntlets> <gloves>)
  :sort-value 3402
  :the-kind '<gloves>
  :game-values (make-game-values :base-ac 2 :base-dice 1 :num-dice 1)) 

(define-object-kind "cesti" "& set~ of cesti"
  :numeric-id 127
  :x-attr #\W
  :x-char #\]
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 40
  :cost 100
  :obj-type '(<cesti> <gloves>)
  :sort-value 3405
  :the-kind '<gloves>
  :game-values (make-game-values :base-ac 5 :base-dice 1 :num-dice 1)) 

(define-object-kind "small-leather-shield" "& small leather shield~"
  :numeric-id 128
  :x-attr #\U
  :x-char #\)
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 50
  :cost 30
  :obj-type '(<shield> <small> <leather>)
  :sort-value 3702
  :the-kind '<shield>
  :game-values (make-game-values :base-ac 2 :base-dice 1 :num-dice 1)) 

(define-object-kind "large-leather-shield" "& large leather shield~"
  :numeric-id 129
  :x-attr #\U
  :x-char #\)
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 100
  :cost 120
  :obj-type '(<shield> <large> <leather>)
  :sort-value 3704
  :the-kind '<shield>
  :game-values (make-game-values :base-ac 4 :base-dice 2 :num-dice 1)) 

(define-object-kind "small-metal-shield" "& small metal shield~"
  :numeric-id 130
  :x-attr #\s
  :x-char #\)
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 65
  :cost 50
  :obj-type '(<shield> <small> <metal>)
  :sort-value 3703
  :the-kind '<shield>
  :game-values (make-game-values :base-ac 3 :base-dice 2 :num-dice 1)) 

(define-object-kind "large-metal-shield" "& large metal shield~"
  :numeric-id 131
  :x-attr #\s
  :x-char #\)
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 120
  :cost 200
  :obj-type '(<shield> <large> <metal>)
  :sort-value 3705
  :the-kind '<shield>
  :game-values (make-game-values :base-ac 5 :base-dice 3 :num-dice 1)) 


(define-object-kind "object-400" "black dragon scale mail~"
  :numeric-id 400
  :x-attr #\s
  :x-char #\[
  :depth 60
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(60 0 0 0)
  :weight 200
  :cost 30000
  :obj-type '(<black> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4101
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<acid>))) 

(define-object-kind "object-401" "blue dragon scale mail~"
  :numeric-id 401
  :x-attr #\b
  :x-char #\[
  :depth 40
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(40 0 0 0)
  :weight 200
  :cost 35000
  :obj-type '(<blue> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4102
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<electricity>))) 

(define-object-kind "object-402" "white dragon scale mail~"
  :numeric-id 402
  :x-attr #\w
  :x-char #\[
  :depth 50
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(50 0 0 0)
  :weight 200
  :cost 40000
  :obj-type '(<white> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4103
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<cold>))) 

(define-object-kind "object-403" "red dragon scale mail~"
  :numeric-id 403
  :x-attr #\r
  :x-char #\[
  :depth 80
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(80 0 0 0)
  :weight 200
  :cost 100000
  :obj-type '(<red> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4104
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<fire>))) 

(define-object-kind "object-404" "green dragon scale mail~"
  :numeric-id 404
  :x-attr #\g
  :x-char #\[
  :depth 70
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(70 0 0 0)
  :weight 200
  :cost 80000
  :obj-type '(<green> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4105
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<poison>))) 

(define-object-kind "object-405" "multi-hued dragon scale mail~"
  :numeric-id 405
  :x-attr #\v
  :x-char #\[
  :depth 100
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(100 0 0 0)
  :weight 200
  :cost 150000
  :obj-type '(<multihued> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4106
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists
                              '(<poison> <cold> <fire> <electricity> <acid>))) 

(define-object-kind "object-406" "shining dragon scale mail~"
  :numeric-id 406
  :x-attr #\o
  :x-char #\[
  :depth 65
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(65 0 0 0)
  :weight 200
  :cost 60000
  :obj-type '(<shining> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4110
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<darkness> <light>))) 

(define-object-kind "object-407" "law dragon scale mail~"
  :numeric-id 407
  :x-attr #\B
  :x-char #\[
  :depth 80
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(80 0 0 0)
  :weight 200
  :cost 80000
  :obj-type '(<law> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4112
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<shards> <sound>))) 

(define-object-kind "object-408" "bronze dragon scale mail~"
  :numeric-id 408
  :x-attr #\U
  :x-char #\[
  :depth 55
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(55 0 0 0)
  :weight 200
  :cost 30000
  :obj-type '(<bronze> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4114
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<confusion>))) 

(define-object-kind "object-409" "gold dragon scale mail~"
  :numeric-id 409
  :x-attr #\y
  :x-char #\[
  :depth 65
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(65 0 0 0)
  :weight 200
  :cost 40000
  :obj-type '(<gold> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4116
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<sound>))) 

(define-object-kind "object-410" "chaos dragon scale mail~"
  :numeric-id 410
  :x-attr #\v
  :x-char #\[
  :depth 75
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(75 0 0 0)
  :weight 200
  :cost 70000
  :obj-type '(<chaos> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4118
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<disenchant> <chaos> <confusion>))) 

(define-object-kind "object-411" "balance dragon scale mail~"
  :numeric-id 411
  :x-attr #\v
  :x-char #\[
  :depth 90
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(90 0 0 0)
  :weight 200
  :cost 100000
  :obj-type '(<balance> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4120
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 30 :ac-modifier 10 :base-dice 4 :num-dice 2 :tohit-modifier -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists
                              '(<disenchant> <chaos> <shards> <sound> <confusion>))) 

(define-object-kind "power-dsm" "power dragon scale mail~"
  :numeric-id 412
  :x-attr #\v
  :x-char #\[
  :depth 110
  :rarity 0
  :chance #(64 0 0 0)
  :locale #(110 0 0 0)
  :weight 200
  :cost 300000
  :obj-type '(<power> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4130
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 40 :ac-modifier 15 :base-dice 4 :num-dice 2 :tohit-modifier -3 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists
                              '(<chaos> <nether> <nexus> <darkness> <light> <confusion> <poison> <electricity> <cold>
                                <fire> <acid>))) 

(define-object-kind "massive-iron-crown" "& massive iron crown~"
  :numeric-id 499
  :x-attr #\D
  :x-char #\]
  :depth 44
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 20
  :cost 1000
  :obj-type '(<morgoth> <headgear> <crown>)
  :flags '(<instant-artifact>)
  :sort-value 3650
  :the-kind '<headgear>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 


;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/config/armour.lisp - armour for vanilla variant
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.contraband)

#||
;;; === Overall armour info and rating

A piece of armour is made of an armour-type and an armour-type has
a rating.

cloth
heavy cloth

leather
fur

studded leather

ring
chain
elven chain

lamellar http://www.regia.org/lamellar.htm
bronze
iron
steel
silver

dragonscale
dragonbone

maybe add:
ebony
mithril
adamant

---

chest/cuirass/body armour is 50%
headgear is 15%
left glove is 5%
right glove is 5%
legs are 15%
boots are 10%

||#


(defconstant +armour-cloth+ 2)
(defconstant +armour-heavy-cloth+ 3)

(defconstant +armour-leather+ 6)
(defconstant +armour-fur+ 6)

(defconstant +armour-studded-leather+ 9)

(defconstant +armour-ring-mailed+ 12)
(defconstant +armour-chain-mail+ 16)
(defconstant +armour-elven-chain+ 20)

(defconstant +armour-lamellar+ 20)
(defconstant +armour-bronze+ 17)
(defconstant +armour-iron+ 19)
(defconstant +armour-steel+ 24)
(defconstant +armour-silver+ 25)

(defconstant +armour-dragonscale+ 32)
(defconstant +armour-dragonbone+ 40)


;;; Weights and costs have not been checked well
;;; sorting values must be fixed later

;;; == footwear

(define-object-kind "sandals" "& pair~ of sandals"
  :text-attr #\w
  :text-char #\]
  :weight 10
  :cost 4
  :sort-value 3301
  :the-kind '<boots>
  :game-values (make-game-values :base-ac +armour-cloth+ :base-dice 1 :num-dice 1)) 

(define-object-kind "cloth-shoes" "& pair~ of cloth shoes"
  :text-attr #\w
  :text-char #\]
  :weight 10
  :cost 4
  :sort-value 3301
  :the-kind '<boots>
  :game-values (make-game-values :base-ac +armour-cloth+ :base-dice 1 :num-dice 1)) 

(define-object-kind "leather-boots" "& pair~ of leather boots"
  :x-attr (tile-file 3)
  :x-char (tile-number 20)
  :text-attr #\U
  :text-char #\]
  :weight 20
  :cost 7
  :sort-value 3302
  :the-kind '<boots>
  :game-values (make-game-values :base-ac +armour-leather+ :base-dice 1 :num-dice 1)) 

(define-object-kind "fur-boots" "& pair~ of fur boots"
  :text-attr #\s
  :text-char #\]
  :weight 40
  :cost 12
  :sort-value 3303
  :the-kind '<boots>
  :game-values (make-game-values :base-ac +armour-fur+ :base-dice 1 :num-dice 1)) 

(define-object-kind "chain-boots" "& pair~ of chain boots"
  :numeric-id 93
  :x-attr (tile-file 3)
  :x-char (tile-number 22)
  :text-attr #\s
  :text-char #\]
  :weight 80
  :cost 50
  :sort-value 3306
  :the-kind '<boots>
  :game-values (make-game-values :base-ac +armour-chain-mail+ :base-dice 1 :num-dice 1))

(define-object-kind "bronze-boots" "& pair~ of bronze-plated boots"
  :numeric-id 93
  :x-attr (tile-file 3)
  :x-char (tile-number 22)
  :text-attr #\s
  :text-char #\]
  :weight 80
  :cost 50
  :sort-value 3306
  :the-kind '<boots>
  :game-values (make-game-values :base-ac +armour-steel+ :base-dice 1 :num-dice 1)) 

(define-object-kind "steel-boots" "& pair~ of steel-plated boots"
  :numeric-id 93
  :x-attr (tile-file 3)
  :x-char (tile-number 22)
  :text-attr #\s
  :text-char #\]
  :weight 80
  :cost 50
  :sort-value 3306
  :the-kind '<boots>
  :game-values (make-game-values :base-ac +armour-steel+ :base-dice 1 :num-dice 1)) 

;;; == headwear

(define-object-kind "hat" "& hat~"
  :numeric-id 94
  :x-attr (tile-file 3)
  :x-char (tile-number 70)
  :text-attr #\u
  :text-char #\]
  :weight 15
  :cost 12
  :sort-value 3502
  :the-kind '<headgear>
  :game-values (make-game-values :base-ac +armour-cloth+)) 

(define-object-kind "leather-cap" "& leather cap~"
  :numeric-id 94
  :x-attr (tile-file 3)
  :x-char (tile-number 70)
  :text-attr #\u
  :text-char #\]
  :weight 15
  :cost 12
  :sort-value 3502
  :the-kind '<headgear>
  :game-values (make-game-values :base-ac +armour-leather+)) 

(define-object-kind "fur-cap" "& fur cap~"
  :numeric-id 94
  :text-attr #\u
  :text-char #\]
  :weight 15
  :cost 12
  :sort-value 3502
  :the-kind '<headgear>
  :game-values (make-game-values :base-ac +armour-fur+)) 

(define-object-kind "chain-coif" "& chain-coif~"
  :numeric-id 95
  :x-attr (tile-file 3)
  :x-char (tile-number 11)
  :text-attr #\s
  :text-char #\]
  :weight 20
  :cost 30
  :sort-value 3503
  :the-kind '<headgear>
  :game-values (make-game-values :base-ac +armour-chain-mail+ :base-dice 1 :num-dice 1)) 

(define-object-kind "iron-helm" "& iron helm~"
  :numeric-id 96
  :x-attr (tile-file 3)
  :x-char (tile-number 12)
  :text-attr #\s
  :text-char #\]
  :weight 75
  :cost 75
  :sort-value 3505
  :the-kind '<headgear>
  :game-values (make-game-values :base-ac 5 :base-dice 3 :num-dice 1)) 

(define-object-kind "steel-helm" "& steel helm~"
  :numeric-id 97
  :x-attr (tile-file 3)
  :x-char (tile-number 13)
  :text-attr #\W
  :text-char #\]
  :weight 60
  :cost 200
  :sort-value 3506
  :the-kind '<headgear>
  :game-values (make-game-values :base-ac 6 :base-dice 3 :num-dice 1)) 


;;; == bodywear

(define-object-kind "robe" "& robe~"
  :numeric-id 101
  :x-attr (tile-file 3)
  :x-char (tile-number 34)
  :text-attr #\b
  :text-char #\(
  :weight 20
  :cost 4
  :sort-value 3902
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac +armour-cloth+)) 


(define-object-kind "leather-armour" "leather armour~"
  :numeric-id 103
  :x-attr (tile-file 3)
  :x-char (tile-number 38)
  :text-attr #\U
  :text-char #\(
  :weight 80
  :cost 18
  :sort-value 3904
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac +armour-leather+)) 

(define-object-kind "studded-leather" "studded leather~"
  :numeric-id 104
  :x-attr (tile-file 3)
  :x-char (tile-number 39)
  :text-attr #\U
  :text-char #\(
  :weight 90
  :cost 35
  :sort-value 3905
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 5 :base-dice 1 :num-dice 1)) 

(define-object-kind "chain-mail" "chain mail~"
  :numeric-id 109
  :x-attr (tile-file 3)
  :x-char (tile-number 45)
  :text-attr #\s
  :text-char #\[
  :weight 220
  :cost 750
  :sort-value 4004
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac +armour-chain-mail+ :base-dice 4 :num-dice 1 :tohit-modifier -2)) 

(define-object-kind "metal-lamellar" "metal lamellar armour~"
  :numeric-id 115
  :x-attr (tile-file 3)
  :x-char (tile-number 51)
  :text-attr #\W
  :text-char #\[
  :weight 340
  :cost 1250
  :sort-value 4013
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac +armour-lamellar+ :base-dice 6 :num-dice 1 :tohit-modifier -3)) 

(define-object-kind "steel-plate" "steel plate armour~"
  :numeric-id 116
  :x-attr (tile-file 3)
  :x-char (tile-number 52)
  :text-attr #\W
  :text-char #\[
  :weight 380
  :cost 1350
  :sort-value 4015
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac +armour-steel+ :base-dice 4 :num-dice 2 :tohit-modifier -3)) 


;;; == cloaks

(define-object-kind "cloak" "& cloak~"
  :numeric-id 123
  :x-attr (tile-file 3)
  :x-char (tile-number 0)
  :text-attr #\g
  :text-char #\(
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
  :weight 5
  :cost 4000
  :sort-value 3806
  :the-kind '<cloak>
  :game-values (make-game-values :base-ac 6 :ac-modifier 4)) 

;;; == handwear

(define-object-kind "leather-gloves" "& set~ of leather gloves"
  :numeric-id 125
  :x-attr (tile-file 3)
  :x-char (tile-number 23)
  :text-attr #\U
  :text-char #\]
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
  :weight 40
  :cost 100
  :sort-value 3405
  :the-kind '<gloves>
  :game-values (make-game-values :base-ac 5 :base-dice 1 :num-dice 1)) 

;;; == shields

(define-object-kind "small-leather-shield" "& small leather shield~"
  :numeric-id 128
  :x-attr (tile-file 3)
  :x-char (tile-number 90)
  :text-attr #\U
  :text-char #\)
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
  :weight 120
  :cost 200
  :sort-value 3705
  :the-kind '<shield>
  :game-values (make-game-values :base-ac 5 :base-dice 3 :num-dice 1)) 

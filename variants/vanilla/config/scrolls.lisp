
(in-package :org.langband.engine)

(define-object-kind "scroll-enchant-wpn-hit" "enchant weapon to-hit"
  :numeric-id 173
  :x-attr #\d
  :x-char #\?
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 5
  :cost 125
  :obj-type '(<scroll> <enchant> <weapon> <to-hit>)
  :sort-value 5017
  :the-kind '<scroll>) 

(define-object-kind "scroll-enchant-wpn-dmg" "enchant weapon to-dam"
  :numeric-id 174
  :x-attr #\d
  :x-char #\?
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 5
  :cost 125
  :obj-type '(<scroll> <enchant> <weapon> <to-dmg>)
  :sort-value 5018
  :the-kind '<scroll>) 

(define-object-kind "scroll-enchant-armour" "enchant armour"
  :numeric-id 175
  :x-attr #\d
  :x-char #\?
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 5
  :cost 125
  :obj-type '(<scroll> <enchant> <armour> <normal>)
  :sort-value 5016
  :the-kind '<scroll>) 

(define-object-kind "scroll-identify" "identify"
  :numeric-id 176
  :x-attr #\d
  :x-char #\?
  :depth 1
  :rarity 0
  :chance #(1 1 1 1)
  :locale #(1 5 10 30)
  :weight 5
  :cost 50
  :obj-type '(<scroll> <identify> <normal>)
  :sort-value 5012
  :the-kind '<scroll>) 

(define-object-kind "scroll-*identify*" "*identify*"
  :numeric-id 177
  :x-attr #\d
  :x-char #\?
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 5
  :cost 1000
  :obj-type '(<scroll> <identify> <powerful>)
  :sort-value 5013
  :the-kind '<scroll>) 

(define-object-kind "scroll-remove-curse" "remove curse"
  :numeric-id 180
  :x-attr #\d
  :x-char #\?
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 5
  :cost 100
  :obj-type '(<scroll> <remove-curse> <normal>)
  :sort-value 5014
  :the-kind '<scroll>) 

(define-object-kind "scroll-light" "light"
  :numeric-id 181
  :x-attr #\d
  :x-char #\?
  :depth 0
  :rarity 0
  :chance #(1 1 1 0)
  :locale #(0 3 10 0)
  :weight 5
  :cost 15
  :obj-type '(<illuminate> <scroll>)
  :sort-value 5024
  :the-kind '<scroll>) 

(define-object-kind "scroll-summon-monster" "summon monster"
  :numeric-id 184
  :x-attr #\d
  :x-char #\?
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 5
  :cost 0
  :obj-type '(<scroll> <summon> <monster>)
  :sort-value 5004
  :the-kind '<scroll>) 

(define-object-kind "scroll-phase-door" "phase door"
  :numeric-id 185
  :x-attr #\d
  :x-char #\?
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 5
  :cost 15
  :obj-type '(<phase-door> <scroll>)
  :sort-value 5008
  :the-kind '<scroll>) 

(define-object-kind "scroll-teleport" "teleportation"
  :numeric-id 186
  :x-attr #\d
  :x-char #\?
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 5
  :cost 40
  :obj-type '(<teleportation> <scroll>)
  :sort-value 5009
  :the-kind '<scroll>) 

(define-object-kind "scroll-teleport-lvl" "teleport level"
  :numeric-id 187
  :x-attr #\d
  :x-char #\?
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 5
  :cost 50
  :obj-type '(<teleport-level> <scroll>)
  :sort-value 5010
  :the-kind '<scroll>) 

(define-object-kind "scroll-monster-confusion" "monster confusion"
  :numeric-id 188
  :x-attr #\d
  :x-char #\?
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 5
  :cost 30
  :obj-type '(<confuse-monster> <scroll>)
  :sort-value 5036
  :the-kind '<scroll>) 

(define-object-kind "scroll-mapping" "magic mapping"
  :numeric-id 189
  :x-attr #\d
  :x-char #\?
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 5
  :cost 40
  :obj-type '(<mapping> <scroll>)
  :sort-value 5025
  :the-kind '<scroll>) 

(define-object-kind "scroll-rune-prot" "rune of protection"
  :numeric-id 190
  :x-attr #\d
  :x-char #\?
  :depth 60
  :rarity 0
  :chance #(2 4 0 0)
  :locale #(60 90 0 0)
  :weight 5
  :cost 500
  :obj-type '(<scroll> <protection> <rune>)
  :sort-value 5038
  :the-kind '<scroll>) 

(define-object-kind "scroll-*remove-curse*" "*remove curse*"
  :numeric-id 191
  :x-attr #\d
  :x-char #\?
  :depth 50
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(50 0 0 0)
  :weight 5
  :cost 8000
  :obj-type '(<scroll> <remove-curse> <powerful>)
  :sort-value 5015
  :the-kind '<scroll>) 

(define-object-kind "scroll-det-gold" "treasure detection"
  :numeric-id 192
  :x-attr #\d
  :x-char #\?
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 5
  :cost 15
  :obj-type '(<scroll> <detect> <money>)
  :sort-value 5026
  :the-kind '<scroll>) 

(define-object-kind "scroll-det-item" "object detection"
  :numeric-id 193
  :x-attr #\d
  :x-char #\?
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 5
  :cost 15
  :obj-type '(<scroll> <detect> <item>)
  :sort-value 5027
  :the-kind '<scroll>) 

(define-object-kind "scroll-det-trap" "trap detection"
  :numeric-id 194
  :x-attr #\d
  :x-char #\?
  :depth 5
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(5 10 0 0)
  :weight 5
  :cost 35
  :obj-type '(<scroll> <detect> <trap>)
  :sort-value 5028
  :the-kind '<scroll>) 

(define-object-kind "scroll-det-door" "door/stair location"
  :numeric-id 197
  :x-attr #\d
  :x-char #\?
  :depth 5
  :rarity 0
  :chance #(1 1 1 0)
  :locale #(5 10 15 0)
  :weight 5
  :cost 35
  :obj-type '(<scroll> <detect> <door>)
  :sort-value 5029
  :the-kind '<scroll>) 

(define-object-kind "scroll-acquirement" "acquirement"
  :numeric-id 198
  :x-attr #\d
  :x-char #\?
  :depth 20
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(20 0 0 0)
  :weight 5
  :cost 100000
  :obj-type '(<scroll> <acquirement> <normal>)
  :sort-value 5046
  :the-kind '<scroll>) 

(define-object-kind "scroll-*acquirement*" "*acquirement*"
  :numeric-id 199
  :x-attr #\d
  :x-char #\?
  :depth 60
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(60 0 0 0)
  :weight 5
  :cost 200000
  :obj-type '(<scroll> <acquirement> <powerful>)
  :sort-value 5047
  :the-kind '<scroll>) 

(define-object-kind "sroll-mass-genocide" "mass genocide"
  :numeric-id 200
  :x-attr #\d
  :x-char #\?
  :depth 50
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(50 0 0 0)
  :weight 5
  :cost 1000
  :obj-type '(<scroll> <genocide> <mass>)
  :sort-value 5045
  :the-kind '<scroll>) 

(define-object-kind "scroll-det-inv" "detect invisible"
  :numeric-id 201
  :x-attr #\d
  :x-char #\?
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 5
  :cost 15
  :obj-type '(<scroll> <detect> <invisible>)
  :sort-value 5030
  :the-kind '<scroll>) 

(define-object-kind "scroll-aggr-monster" "aggravate monster"
  :numeric-id 202
  :x-attr #\d
  :x-char #\?
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 5
  :cost 0
  :obj-type '(<aggravate> <scroll>)
  :sort-value 5001
  :the-kind '<scroll>) 

(define-object-kind "scroll-create-trap" "trap creation"
  :numeric-id 203
  :x-attr #\d
  :x-char #\?
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 5
  :cost 0
  :obj-type '(<create-trap> <scroll>)
  :sort-value 5007
  :the-kind '<scroll>) 

(define-object-kind "scroll-destroy-door" "trap/door destruction"
  :numeric-id 204
  :x-attr #\d
  :x-char #\?
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 5
  :cost 50
  :obj-type '(<trap/door-destruction> <scroll>)
  :sort-value 5039
  :the-kind '<scroll>) 

(define-object-kind "scroll-recharging" "recharging"
  :numeric-id 206
  :x-attr #\d
  :x-char #\?
  :depth 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 5
  :cost 200
  :obj-type '(<recharge> <scroll>)
  :sort-value 5022
  :the-kind '<scroll>) 

(define-object-kind "scroll-genocide" "genocide"
  :numeric-id 207
  :x-attr #\d
  :x-char #\?
  :depth 40
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(40 0 0 0)
  :weight 5
  :cost 750
  :obj-type '(<genocide> <scroll>)
  :sort-value 5044
  :the-kind '<scroll>) 

(define-object-kind "scroll-darkness" "darkness"
  :numeric-id 208
  :x-attr #\d
  :x-char #\?
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 5
  :cost 0
  :obj-type '(<darkness> <scroll>)
  :sort-value 5000
  :the-kind '<scroll>) 

(define-object-kind "scroll-protect-from-evil" "protection from evil"
  :numeric-id 209
  :x-attr #\d
  :x-char #\?
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 5
  :cost 50
  :obj-type '(<scroll> <protection> <evil>)
  :sort-value 5037
  :the-kind '<scroll>) 

(define-object-kind "scroll-satisfy-hunger" "satisfy hunger"
  :numeric-id 210
  :x-attr #\d
  :x-char #\?
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 5
  :cost 10
  :obj-type '(<satisfy-hunger> <scroll>)
  :sort-value 5032
  :the-kind '<scroll>) 

(define-object-kind "scroll-dispel-undead" "dispel undead"
  :numeric-id 211
  :x-attr #\d
  :x-char #\?
  :depth 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 5
  :cost 200
  :obj-type '(<scroll> <dispel> <undead>)
  :sort-value 5042
  :the-kind '<scroll>) 

(define-object-kind "object-212" "*enchant weapon*"
  :numeric-id 212
  :x-attr #\d
  :x-char #\?
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 5
  :cost 500
  :obj-type '(<scroll> <enchant> <weapon> <powerful>)
  :sort-value 5021
  :the-kind '<scroll>) 

(define-object-kind "scroll-curse-weapon" "curse weapon"
  :numeric-id 213
  :x-attr #\d
  :x-char #\?
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 5
  :cost 0
  :obj-type '(<curse-weapon> <scroll>)
  :sort-value 5003
  :the-kind '<scroll>) 

(define-object-kind "object-214" "*enchant armour*"
  :numeric-id 214
  :x-attr #\d
  :x-char #\?
  :depth 50
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(50 50 0 0)
  :weight 5
  :cost 500
  :obj-type '(<scroll> <enchant> <armour> <powerful>)
  :sort-value 5020
  :the-kind '<scroll>) 

(define-object-kind "scroll-curse-armour" "curse armour"
  :numeric-id 215
  :x-attr #\d
  :x-char #\?
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 5
  :cost 0
  :obj-type '(<curse-armour> <scroll>)
  :sort-value 5002
  :the-kind '<scroll>) 

(define-object-kind "scroll-summon-undead" "summon undead"
  :numeric-id 216
  :x-attr #\d
  :x-char #\?
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 5
  :cost 0
  :obj-type '(<scroll> <summon> <undead>)
  :sort-value 5005
  :the-kind '<scroll>) 

(define-object-kind "scroll-blessing" "blessing"
  :numeric-id 217
  :x-attr #\d
  :x-char #\?
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 5
  :cost 15
  :obj-type '(<scroll> <blessing> <light>)
  :sort-value 5033
  :the-kind '<scroll>) 

(define-object-kind "scroll-holy-chant" "holy chant"
  :numeric-id 218
  :x-attr #\d
  :x-char #\?
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 5
  :cost 40
  :obj-type '(<scroll> <blessing> <chant>)
  :sort-value 5034
  :the-kind '<scroll>) 

(define-object-kind "scroll-holy-prayer" "holy prayer"
  :numeric-id 219
  :x-attr #\d
  :x-char #\?
  :depth 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 5
  :cost 80
  :obj-type '(<scroll> <blessing> <prayer>)
  :sort-value 5035
  :the-kind '<scroll>) 

(define-object-kind "scroll-wor" "word of recall"
  :numeric-id 220
  :x-attr #\d
  :x-char #\?
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 5
  :cost 150
  :obj-type '(<word-of-recall> <scroll>)
  :sort-value 5011
  :the-kind '<scroll>) 

(define-object-kind "object-221" "*destruction*"
  :numeric-id 221
  :x-attr #\d
  :x-char #\?
  :depth 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 5
  :cost 250
  :obj-type '(<scroll> <destruction> <powerful>)
  :sort-value 5041
  :the-kind '<scroll>) 


(in-package :langband)

(define-object-kind "slime-mold-juice" "slime mold juice"
  :numeric-id 222
  :x-attr #\d
  :x-char #\!
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 4
  :cost 2
  :obj-type '(<slime-mold> <potion>)
  :sort-value 5502
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 400)) 

(define-object-kind "apple-juice" "apple juice"
  :numeric-id 223
  :x-attr #\d
  :x-char #\!
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 4
  :cost 1
  :obj-type '(<apple-juice> <potion>)
  :sort-value 5501
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 250)) 

(define-object-kind "water" "water"
  :numeric-id 224
  :x-attr #\d
  :x-char #\!
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 4
  :cost 1
  :obj-type '(<water> <potion>)
  :sort-value 5500
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 200)) 

(define-object-kind "potion-str" "strength"
  :numeric-id 225
  :x-attr #\d
  :x-char #\!
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 4
  :cost 8000
  :obj-type '(<potion> <increase> <str>)
  :sort-value 5548
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-reduce-str" "weakness"
  :numeric-id 226
  :x-attr #\d
  :x-char #\!
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<potion> <reduce> <str>)
  :sort-value 5516
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-restore-str" "restore strength"
  :numeric-id 227
  :x-attr #\d
  :x-char #\!
  :depth 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 4
  :cost 300
  :obj-type '(<potion> <restore> <str>)
  :sort-value 5542
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-int" "intelligence"
  :numeric-id 228
  :x-attr #\d
  :x-char #\!
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 4
  :cost 8000
  :obj-type '(<potion> <increase> <int>)
  :sort-value 5549
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-reduce-int" "stupidity"
  :numeric-id 229
  :x-attr #\d
  :x-char #\!
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<potion> <reduce> <int>)
  :sort-value 5517
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-restore-int" "restore intelligence"
  :numeric-id 230
  :x-attr #\d
  :x-char #\!
  :depth 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 4
  :cost 300
  :obj-type '(<potion> <restore> <int>)
  :sort-value 5543
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-wis" "wisdom"
  :numeric-id 231
  :x-attr #\d
  :x-char #\!
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 4
  :cost 8000
  :obj-type '(<potion> <increase> <wis>)
  :sort-value 5550
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-reduce-wis" "naivety"
  :numeric-id 232
  :x-attr #\d
  :x-char #\!
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<potion> <reduce> <wis>)
  :sort-value 5518
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-restore-wis" "restore wisdom"
  :numeric-id 233
  :x-attr #\d
  :x-char #\!
  :depth 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 4
  :cost 300
  :obj-type '(<potion> <restore> <wis>)
  :sort-value 5544
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-chr" "charisma"
  :numeric-id 234
  :x-attr #\d
  :x-char #\!
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 4
  :cost 1000
  :obj-type '(<potion> <increase> <chr>)
  :sort-value 5553
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-reduce-chr" "ugliness"
  :numeric-id 235
  :x-attr #\d
  :x-char #\!
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<potion> <reduce> <chr>)
  :sort-value 5521
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-restore-chr" "restore charisma"
  :numeric-id 236
  :x-attr #\d
  :x-char #\!
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 4
  :cost 300
  :obj-type '(<potion> <restore> <chr>)
  :sort-value 5547
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-cure-light" "cure light wounds"
  :numeric-id 237
  :x-attr #\d
  :x-char #\!
  :depth 0
  :rarity 0
  :chance #(1 1 1 0)
  :locale #(0 1 3 0)
  :weight 4
  :cost 15
  :obj-type '(<potion> <cure> <light>)
  :sort-value 5534
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 50)) 

(define-object-kind "potion-reduce-dex" "clumsiness"
  :numeric-id 238
  :x-attr #\d
  :x-char #\!
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<potion> <reduce> <dex>)
  :sort-value 5519
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-reduce-con" "sickliness"
  :numeric-id 239
  :x-attr #\d
  :x-char #\!
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<potion> <reduce> <con>)
  :sort-value 5520
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-cure-serious" "cure serious wounds"
  :numeric-id 240
  :x-attr #\d
  :x-char #\!
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 4
  :cost 40
  :obj-type '(<potion> <cure> <serious>)
  :sort-value 5535
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 100)) 

(define-object-kind "potion-cure-critical" "cure critical wounds"
  :numeric-id 241
  :x-attr #\d
  :x-char #\!
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 4
  :cost 100
  :obj-type '(<potion> <cure> <critical>)
  :sort-value 5536
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 100)) 

(define-object-kind "potion-healing" "healing"
  :numeric-id 242
  :x-attr #\d
  :x-char #\!
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 4
  :cost 300
  :obj-type '(<potion> <cure> <healing> <normal>)
  :sort-value 5537
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 200)) 

(define-object-kind "potion-con" "constitution"
  :numeric-id 243
  :x-attr #\d
  :x-char #\!
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 4
  :cost 8000
  :obj-type '(<potion> <increase> <con>)
  :sort-value 5552
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-xp" "experience"
  :numeric-id 244
  :x-attr #\d
  :x-char #\!
  :depth 65
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(65 0 0 0)
  :weight 4
  :cost 25000
  :obj-type '(<xp> <potion>)
  :sort-value 5559
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-sleep" "sleep"
  :numeric-id 245
  :x-attr #\d
  :x-char #\!
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<sleep> <potion>)
  :sort-value 5511
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 100)) 

(define-object-kind "potion-blindness" "blindness"
  :numeric-id 246
  :x-attr #\d
  :x-char #\!
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<blindness> <potion>)
  :sort-value 5507
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-confusion" "confusion"
  :numeric-id 247
  :x-attr #\d
  :x-char #\!
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<confusion> <potion>)
  :sort-value 5509
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 50)) 

(define-object-kind "potion-poison" "poison"
  :numeric-id 248
  :x-attr #\d
  :x-char #\!
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<poison> <potion>)
  :sort-value 5506
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-speed" "speed"
  :numeric-id 249
  :x-attr #\d
  :x-char #\!
  :depth 1
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(1 40 0 0)
  :weight 4
  :cost 75
  :obj-type '(<speed> <potion>)
  :sort-value 5529
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-slowness" "slowness"
  :numeric-id 250
  :x-attr #\d
  :x-char #\!
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<slowness> <potion>)
  :sort-value 5504
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :food-value 50)) 

(define-object-kind "potion-dex" "dexterity"
  :numeric-id 251
  :x-attr #\d
  :x-char #\!
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 4
  :cost 8000
  :obj-type '(<potion> <increase> <dex>)
  :sort-value 5551
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-restore-dex" "restore dexterity"
  :numeric-id 252
  :x-attr #\d
  :x-char #\!
  :depth 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 4
  :cost 300
  :obj-type '(<potion> <restore> <dex>)
  :sort-value 5545
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-restore-con" "restore constitution"
  :numeric-id 253
  :x-attr #\d
  :x-char #\!
  :depth 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 4
  :cost 300
  :obj-type '(<potion> <restore> <con>)
  :sort-value 5546
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-lose-memory" "lose memories"
  :numeric-id 254
  :x-attr #\d
  :x-char #\!
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<amnesia> <potion>)
  :sort-value 5513
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-255" "salt water"
  :numeric-id 255
  :x-attr #\d
  :x-char #\!
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<salt-water> <potion>)
  :sort-value 5505
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-self-know" "enlightenment"
  :numeric-id 256
  :x-attr #\d
  :x-char #\!
  :depth 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 4
  :cost 800
  :obj-type '(<potion> <enlightenment> <normal>)
  :sort-value 5556
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-heroism" "heroism"
  :numeric-id 257
  :x-attr #\d
  :x-char #\!
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 4
  :cost 35
  :obj-type '(<heroism> <potion>)
  :sort-value 5532
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-berserk" "berserk strength"
  :numeric-id 258
  :x-attr #\d
  :x-char #\!
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 4
  :cost 100
  :obj-type '(<berserk-strength> <potion>)
  :sort-value 5533
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-boldness" "boldness"
  :numeric-id 259
  :x-attr #\d
  :x-char #\!
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 4
  :cost 10
  :obj-type '(<boldness> <potion>)
  :sort-value 5528
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-restore-xp" "restore life levels"
  :numeric-id 260
  :x-attr #\d
  :x-char #\!
  :depth 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 4
  :cost 400
  :obj-type '(<potion> <restore> <xp>)
  :sort-value 5541
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-resist-heat" "resist heat"
  :numeric-id 261
  :x-attr #\d
  :x-char #\!
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 4
  :cost 30
  :obj-type '(<potion> <resist> <fire>)
  :sort-value 5530
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-resist-cold" "resist cold"
  :numeric-id 262
  :x-attr #\d
  :x-char #\!
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 4
  :cost 30
  :obj-type '(<potion> <resist> <cold>)
  :sort-value 5531
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-det-inv" "detect invisible"
  :numeric-id 263
  :x-attr #\d
  :x-char #\!
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 4
  :cost 50
  :obj-type '(<potion> <detect> <invisible>)
  :sort-value 5525
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-slow-poison" "slow poison"
  :numeric-id 264
  :x-attr #\d
  :x-char #\!
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 4
  :cost 25
  :obj-type '(<slow-poison> <potion>)
  :sort-value 5526
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-cure-poison" "neutralize poison"
  :numeric-id 265
  :x-attr #\d
  :x-char #\!
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 4
  :cost 75
  :obj-type '(<potion> <cure> <poison>)
  :sort-value 5527
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-restore-mana" "restore mana"
  :numeric-id 266
  :x-attr #\d
  :x-char #\!
  :depth 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 4
  :cost 350
  :obj-type '(<potion> <restore> <mana>)
  :sort-value 5540
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-infravision" "infravision"
  :numeric-id 267
  :x-attr #\d
  :x-char #\!
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 4
  :cost 20
  :obj-type '(<infravision> <potion>)
  :sort-value 5524
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-death" "death"
  :numeric-id 415
  :x-attr #\d
  :x-char #\!
  :depth 55
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(55 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<death> <potion>)
  :sort-value 5523
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-416" "ruination"
  :numeric-id 416
  :x-attr #\d
  :x-char #\!
  :depth 40
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(40 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<ruination> <potion>)
  :sort-value 5515
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-417" "detonations"
  :numeric-id 417
  :x-attr #\d
  :x-char #\!
  :depth 60
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(60 0 0 0)
  :weight 4
  :cost 10000
  :obj-type '(<detonations> <potion>)
  :sort-value 5522
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 25 :num-dice 25)) 

(define-object-kind "object-418" "augmentation"
  :numeric-id 418
  :x-attr #\d
  :x-char #\!
  :depth 40
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(40 0 0 0)
  :weight 4
  :cost 60000
  :obj-type '(<augmentation> <potion>)
  :sort-value 5555
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-419" "*healing*"
  :numeric-id 419
  :x-attr #\d
  :x-char #\!
  :depth 40
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(40 0 0 0)
  :weight 4
  :cost 1500
  :obj-type '(<potion> <cure> <healing> <powerful>)
  :sort-value 5538
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-life" "life"
  :numeric-id 420
  :x-attr #\d
  :x-char #\!
  :depth 60
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(60 0 0 0)
  :weight 4
  :cost 5000
  :obj-type '(<life> <potion>)
  :sort-value 5539
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-421" "self knowledge"
  :numeric-id 421
  :x-attr #\d
  :x-char #\!
  :depth 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 4
  :cost 2000
  :obj-type '(<self-knowledge> <potion>)
  :sort-value 5558
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "potion-*enlightenment*" "*enlightenment*"
  :numeric-id 422
  :x-attr #\d
  :x-char #\!
  :depth 70
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(70 0 0 0)
  :weight 4
  :cost 80000
  :obj-type '(<potion> <enlightenment> <powerful>)
  :sort-value 5557
  :the-kind '<potion>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

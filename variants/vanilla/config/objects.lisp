;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/objects.lisp - objects for vanilla variant
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(define-object-kind "pile" "<pile>"
  :numeric-id 0
  :text-attr #\w
  :text-char #\&
  :x-attr (tile-file 10)
  :x-char (tile-number 54)
  :depth 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)) 

(define-object-kind "shovel" "& shovel~"
  :numeric-id 84
  :x-attr (tile-file 10)
  :x-char (tile-number 30)
  :text-attr #\s
  :text-char #\\
  :depth 1
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(5 0 0 0)
  :weight 60
  :cost 10
  :flags '(<show-modififers>)
  :sort-value 2901
  :the-kind '<digger>
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "gnomish-shovel" "& gnomish shovel~"
  :numeric-id 85
  :x-attr (tile-file 10)
  :x-char (tile-number 31)
  :text-attr #\G
  :text-char #\\
  :depth 20
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(20 0 0 0)
  :weight 60
  :cost 100
  :flags '(<show-modififers>)
  :sort-value 2902
  :the-kind '<digger>
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "dwarven-shovel" "& dwarven shovel~"
  :numeric-id 86
  :x-attr (tile-file 10)
  :x-char (tile-number 32)
  :text-attr #\B
  :text-char #\\
  :depth 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 120
  :cost 200
  :flags '(<show-modififers>)
  :sort-value 2903
  :the-kind '<digger>
  :game-values (make-game-values :base-dice 3 :num-dice 1)) 

(define-object-kind "pick" "& pick~"
  :numeric-id 87
  :x-attr (tile-file 10)
  :x-char (tile-number 27)
  :text-attr #\s
  :text-char #\\
  :depth 5
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(10 0 0 0)
  :weight 150
  :cost 50
  :flags '(<show-modififers>)
  :sort-value 2904
  :the-kind '<digger>
  :game-values (make-game-values :base-dice 3 :num-dice 1)) 

(define-object-kind "orcish-pick" "& orcish pick~"
  :numeric-id 88
  :x-attr (tile-file 10)
  :x-char (tile-number 28)
  :text-attr #\g
  :text-char #\\
  :depth 30
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(30 0 0 0)
  :weight 150
  :cost 300
  :flags '(<show-modififers>)
  :sort-value 2905
  :the-kind '<digger>
  :game-values (make-game-values :base-dice 3 :num-dice 1)) 

(define-object-kind "dwarven-pick" "& dwarven pick~"
  :numeric-id 89
  :x-attr (tile-file 10)
  :x-char (tile-number 29)
  :text-attr #\b
  :text-char #\\
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 200
  :cost 600
  :flags '(<show-modififers>)
  :sort-value 2906
  :the-kind '<digger>
  :game-values (make-game-values :base-dice 4 :num-dice 1)) 


(define-object-kind "small-wooden-chest" "& small wooden chest~"
  :numeric-id 338
  :x-attr (tile-file 10)
  :x-char (tile-number 0)
  :text-attr #\s
  :text-char #\~
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 250
  :cost 20
  :sort-value 2401
  :the-kind '<chest>
  :game-values (make-game-values :base-dice 3 :num-dice 2)) 

(define-object-kind "large-wooden-chest" "& large wooden chest~"
  :numeric-id 339
  :x-attr (tile-file 10)
  :x-char (tile-number 1)
  :text-attr #\s
  :text-char #\~
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 500
  :cost 60
  :sort-value 2405
  :the-kind '<chest>
  :game-values (make-game-values :base-dice 5 :num-dice 2)) 

(define-object-kind "small-iron-chest" "& small iron chest~"
  :numeric-id 340
  :x-attr (tile-file 10)
  :x-char (tile-number 2)
  :text-attr #\s
  :text-char #\~
  :depth 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 300
  :cost 100
  :sort-value 2402
  :the-kind '<chest>
  :game-values (make-game-values :base-dice 4 :num-dice 2)) 

(define-object-kind "large-iron-chest" "& large iron chest~"
  :numeric-id 341
  :x-attr (tile-file 10)
  :x-char (tile-number 3)
  :text-attr #\s
  :text-char #\~
  :depth 35
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(35 0 0 0)
  :weight 1000
  :cost 150
  :sort-value 2406
  :the-kind '<chest>
  :game-values (make-game-values :base-dice 6 :num-dice 2)) 

(define-object-kind "small-steel-chest" "& small steel chest~"
  :numeric-id 342
  :x-attr (tile-file 10)
  :x-char (tile-number 4)
  :text-attr #\s
  :text-char #\~
  :depth 45
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(45 0 0 0)
  :weight 500
  :cost 200
  :sort-value 2403
  :the-kind '<chest>
  :game-values (make-game-values :base-dice 4 :num-dice 2)) 

(define-object-kind "large-steel-chest" "& large steel chest~"
  :numeric-id 343
  :x-attr (tile-file 10)
  :x-char (tile-number 5)
  :text-attr #\s
  :text-char #\~
  :depth 55
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(55 0 0 0)
  :weight 1000
  :cost 250
  :sort-value 2407
  :the-kind '<chest>
  :game-values (make-game-values :base-dice 6 :num-dice 2)) 

(define-object-kind "ruined-chest" "& ruined chest~"
  :numeric-id 344
  :x-attr (tile-file 10)
  :x-char (tile-number 6)
  :text-attr #\s
  :text-char #\~
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(75 0 0 0)
  :weight 250
  :cost 0
  :sort-value 2400
  :the-kind '<chest>)

#||
(define-object-kind "iron-spike" "& iron spike~"
  :numeric-id 345
  :text-attr #\W
  :text-char #\~
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 10
  :cost 1
  :sort-value 2300
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 
||#

(define-object-kind "torch" "& wooden torch~"
  :numeric-id 346
  :x-attr (tile-file 10)
  :x-char (tile-number 26)
  :text-attr #\u
  :text-char #\~
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 30
  :cost 2
  :flags '(<easy-know>)
  :sort-value 4200
  :the-kind '<light-source>
  :max-fuel 5000
  :status-descs '("fresh" "almost fresh" "half-burnt" "well-burnt" "almost out" "burnt out")
  :game-values (make-game-values :base-dice 1 :num-dice 1 :charges 4000 :light-radius 1)) 

(define-object-kind "lantern" "& brass lantern~"
  :numeric-id 347
  :x-attr (tile-file 10)
  :x-char (tile-number 25)
  :text-attr #\U
  :text-char #\~
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 50
  :cost 35
  :flags '(<easy-know>)
  :sort-value 4201
  :the-kind '<light-source>
  :max-fuel 15000
  :status-descs '("full" "almost full" "half-full" "little left" "almost empty" "empty")
  :game-values (make-game-values :base-dice 1 :num-dice 1 :charges 7500 :light-radius 2 :ignores '(<fire>))) 

(define-object-kind "oil-flask" "& flask~ of oil"
  :numeric-id 348
  :x-attr (tile-file 10)
  :x-char (tile-number 23)
  :text-attr #\y
  :text-char #\!
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 10
  :cost 3
  :sort-value 5700
  :game-values (make-game-values :base-dice 6 :num-dice 2 :charges 7500)) 

(define-object-kind "empty-bottle" "& empty bottle~"
  :numeric-id 349
  :x-attr (tile-file 10)
  :x-char (tile-number 22)
  :text-attr #\w
  :text-char #\!
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 0
  :sort-value 2101
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "pottery-shards" "& shard~ of pottery"
  :numeric-id 389
  :x-attr (tile-file 10)
  :x-char (tile-number 34)
  :text-attr #\r
  :text-char #\~
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 5
  :cost 0
  :the-kind '<junk>
  :sort-value 2203
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "broken-stick" "& broken stick~"
  :numeric-id 390
  :x-attr (tile-file 10)
  :x-char (tile-number 42)
  :text-attr #\r
  :text-char #\~
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 3
  :cost 0
  :the-kind '<junk>
  :sort-value 2206
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "broken-skull" "& broken skull~"
  :numeric-id 391
  :x-attr (tile-file 10)
  :x-char (tile-number 43)
  :text-attr #\w
  :text-char #\~
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 1
  :cost 0
  :the-kind '<skeleton>
  :sort-value 2001
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "broken-bone" "& broken bone~"
  :numeric-id 392
  :x-attr (tile-file 10)
  :x-char (tile-number 44)
  :text-attr #\w
  :text-char #\~
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 0
  :the-kind '<skeleton>
  :sort-value 2002
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "canine-skeleton" "& canine skeleton~"
  :numeric-id 393
  :x-attr (tile-file 10)
  :x-char (tile-number 49)
  :text-attr #\w
  :text-char #\~
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 10
  :cost 0
  :the-kind '<skeleton>
  :sort-value 2004
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rodent-skeleton" "& rodent skeleton~"
  :numeric-id 394
  :x-attr (tile-file 10)
  :x-char (tile-number 50)
  :text-attr #\w
  :text-char #\~
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 10
  :cost 0
  :the-kind '<skeleton>
  :sort-value 2003
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "human-skeleton" "& human skeleton~"
  :numeric-id 395
  :x-attr (tile-file 10)
  :x-char (tile-number 45)
  :text-attr #\w
  :text-char #\~
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 60
  :cost 0
  :the-kind '<skeleton>
  :sort-value 2008
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "dwarf-skeleton" "& dwarf skeleton~"
  :numeric-id 396
  :x-attr (tile-file 10)
  :x-char (tile-number 47)
  :text-attr #\w
  :text-char #\~
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 50
  :cost 0
  :the-kind '<skeleton>
  :sort-value 2007
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "elf-skeleton" "& elf skeleton~"
  :numeric-id 397
  :x-attr (tile-file 10)
  :x-char (tile-number 46)
  :text-attr #\w
  :text-char #\~
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 40
  :cost 0
  :the-kind '<skeleton>
  :sort-value 2006
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "gnome-skeleton" "& gnome skeleton~"
  :numeric-id 398
  :x-attr (tile-file 10)
  :x-char (tile-number 48)
  :text-attr #\w
  :text-char #\~
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 30
  :cost 0
  :the-kind '<skeleton>
  :sort-value 2005
  :game-values (make-game-values :base-dice 2 :num-dice 1)) 

;;; artifact items
#||
(define-object-kind "the-phial" "& phial~"
  :numeric-id 500
  :text-attr #\y
  :text-char #\~
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 10
  :cost 10000
  :flags '(<instant-artifact>)
  :sort-value 4204
  :the-kind '<light-source>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :light-radius 3)) 

(define-object-kind "the-star" "& star~"
  :numeric-id 501
  :text-attr #\y
  :text-char #\~
  :depth 30
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 5
  :cost 25000
  :flags '(<instant-artifact>)
  :sort-value 4205
  :the-kind '<light-source>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :light-radius 3)) 

(define-object-kind "the-arkenstone" "& arkenstone~"
  :numeric-id 502
  :text-attr #\y
  :text-char #\~
  :depth 60
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 5
  :cost 60000
  :flags '(<instant-artifact>)
  :sort-value 4206
  :the-kind '<light-source>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :light-radius 3)) 

(define-object-kind "amulet-carlammas" "& amulet~"
  :numeric-id 503
  :text-attr #\d
  :text-char #\"
  :depth 50
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 3
  :cost 60000
  :flags '(<instant-artifact>)
  :sort-value 4310
  :the-kind '<amulet>)

(define-object-kind "amulet-ingwe" "& amulet~"
  :numeric-id 504
  :text-attr #\d
  :text-char #\"
  :depth 60
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 3
  :cost 90000
  :flags '(<instant-artifact>)
  :sort-value 4311
  :the-kind '<amulet>)

(define-object-kind "dwarven-necklace" "& necklace~"
  :numeric-id 505
  :text-attr #\d
  :text-char #\"
  :depth 70
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 3
  :cost 75000
  :flags '(<instant-artifact>)
  :sort-value 4312
  :the-kind '<neckwear>)

(define-object-kind "object-506" "& ring~"
  :numeric-id 506
  :text-attr #\d
  :text-char #\=
  :depth 50
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 65000
  :flags '(<instant-artifact>)
  :sort-value 4432
  :the-kind '<ring>)

(define-object-kind "object-507" "& ring~"
  :numeric-id 507
  :text-attr #\d
  :text-char #\=
  :depth 90
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 150000
  :flags '(<instant-artifact>)
  :sort-value 4433
  :the-kind '<ring>)

(define-object-kind "object-508" "& ring~"
  :numeric-id 508
  :text-attr #\d
  :text-char #\=
  :depth 80
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 100000
  :flags '(<instant-artifact>)
  :sort-value 4434
  :the-kind '<ring>)

(define-object-kind "object-509" "& ring~"
  :numeric-id 509
  :text-attr #\d
  :text-char #\=
  :depth 90
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 200000
  :flags '(<instant-artifact>)
  :sort-value 4435
  :the-kind '<ring>)

(define-object-kind "object-510" "& ring~"
  :numeric-id 510
  :text-attr #\d
  :text-char #\=
  :depth 100
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 300000
  :flags '(<instant-artifact>)
  :sort-value 4436
  :the-kind '<ring>)

(define-object-kind "one-ring" "& ring~"
  :numeric-id 511
  :text-attr #\y
  :text-char #\=
  :depth 110
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 5000000
  :flags '(<instant-artifact>)
  :sort-value 4437
  :the-kind '<ring>)
||#

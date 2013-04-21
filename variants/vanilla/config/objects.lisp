
(in-package :langband)

(define-object-kind "pile" "<pile>"
  :numeric-id 0
  :x-attr #\w
  :x-char #\&
  :depth 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)) 

(define-object-kind "mushroom-blindness" "blindness"
  :numeric-id 1
  :x-attr #\d
  :x-char #\,
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 1
  :cost 0
  :obj-type '(<blindness> <mushroom> <food>)
  :sort-value 6001
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "mushroom-paranoia" "paranoia"
  :numeric-id 2
  :x-attr #\d
  :x-char #\,
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 1
  :cost 0
  :obj-type '(<paranoia> <mushroom> <food>)
  :sort-value 6002
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "mushroom-confusion" "confusion"
  :numeric-id 3
  :x-attr #\d
  :x-char #\,
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 1
  :cost 0
  :obj-type '(<confusion> <mushroom> <food>)
  :sort-value 6003
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "mushroom-hallucination" "hallucination"
  :numeric-id 4
  :x-attr #\d
  :x-char #\,
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 1
  :cost 0
  :obj-type '(<hallucination> <mushroom> <food>)
  :sort-value 6004
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "mushroom-cure-poison" "cure poison"
  :numeric-id 5
  :x-attr #\d
  :x-char #\,
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 1
  :cost 60
  :obj-type '(<mushroom> <food> <cure> <poison>)
  :sort-value 6012
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-6" "cure blindness"
  :numeric-id 6
  :x-attr #\d
  :x-char #\,
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 1
  :cost 50
  :obj-type '(<mushroom> <food> <cure> <blindness>)
  :sort-value 6013
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-7" "cure paranoia"
  :numeric-id 7
  :x-attr #\d
  :x-char #\,
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 1
  :cost 25
  :obj-type '(<mushroom> <food> <cure> <paranoia>)
  :sort-value 6014
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "mushroom-cure-confusion" "cure confusion"
  :numeric-id 8
  :x-attr #\d
  :x-char #\,
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 1
  :cost 50
  :obj-type '(<mushroom> <food> <cure> <confusion>)
  :sort-value 6015
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "mushroom-reduce-str" "weakness"
  :numeric-id 9
  :x-attr #\d
  :x-char #\,
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 1
  :cost 0
  :obj-type '(<mushroom> <food> <reduce> <str>)
  :sort-value 6006
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-10" "unhealth"
  :numeric-id 10
  :x-attr #\d
  :x-char #\,
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 1
  :cost 50
  :obj-type '(<mushroom> <food> <reduce> <con> <powerful>)
  :sort-value 6010
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :base-dice 10 :num-dice 10 :food-val 500)) 

(define-object-kind "object-11" "restore constitution"
  :numeric-id 11
  :x-attr #\d
  :x-char #\,
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 1
  :cost 350
  :obj-type '(<mushroom> <food> <restore> <con>)
  :sort-value 6018
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-12" "restoring"
  :numeric-id 12
  :x-attr #\d
  :x-char #\,
  :depth 20
  :rarity 0
  :chance #(8 4 1 0)
  :locale #(20 30 40 0)
  :weight 1
  :cost 1000
  :obj-type '(<restoring> <mushroom> <food>)
  :sort-value 6019
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-13" "stupidity"
  :numeric-id 13
  :x-attr #\d
  :x-char #\,
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 1
  :cost 0
  :obj-type '(<mushroom> <food> <reduce> <int>)
  :sort-value 6008
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-14" "naivety"
  :numeric-id 14
  :x-attr #\d
  :x-char #\,
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 1
  :cost 0
  :obj-type '(<mushroom> <food> <reduce> <wis>)
  :sort-value 6009
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-15" "poison"
  :numeric-id 15
  :x-attr #\d
  :x-char #\,
  :depth 5
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(5 5 0 0)
  :weight 1
  :cost 0
  :obj-type '(<poison> <mushroom> <food>)
  :sort-value 6000
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-16" "sickness"
  :numeric-id 16
  :x-attr #\d
  :x-char #\,
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 1
  :cost 0
  :obj-type '(<mushroom> <food> <reduce> <con>)
  :sort-value 6007
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-17" "paralysis"
  :numeric-id 17
  :x-attr #\d
  :x-char #\,
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 1
  :cost 0
  :obj-type '(<paralysis> <mushroom> <food>)
  :sort-value 6005
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-18" "restore strength"
  :numeric-id 18
  :x-attr #\d
  :x-char #\,
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 1
  :cost 350
  :obj-type '(<mushroom> <food> <restore> <str>)
  :sort-value 6017
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-19" "disease"
  :numeric-id 19
  :x-attr #\d
  :x-char #\,
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 1
  :cost 50
  :obj-type '(<mushroom> <food> <reduce> <str> <powerful>)
  :sort-value 6011
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :base-dice 10 :num-dice 10 :food-val 500)) 

(define-object-kind "object-20" "cure serious wounds"
  :numeric-id 20
  :x-attr #\d
  :x-char #\,
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 2
  :cost 75
  :obj-type '(<mushroom> <food> <cure> <serious>)
  :sort-value 6016
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "food-ration" "& ration~ of food"
  :numeric-id 21
  :x-attr #\U
  :x-char #\,
  :depth 0
  :rarity 0
  :chance #(1 1 1 0)
  :locale #(0 5 10 0)
  :weight 10
  :cost 3
  :obj-type '(<ration> <food>)
  :sort-value 6035
  :the-kind '<food>
  :game-values (make-instance 'game-values :food-val 5000)) 

(define-object-kind "biscuit" "& hard biscuit~"
  :numeric-id 22
  :x-attr #\U
  :x-char #\,
  :depth 0
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 1
  :obj-type '(<biscuit> <food>)
  :sort-value 6032
  :the-kind '<food>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "beef-jerky" "& strip~ of beef jerky"
  :numeric-id 23
  :x-attr #\u
  :x-char #\,
  :depth 0
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 2
  :obj-type '(<jerky> <food>)
  :sort-value 6033
  :the-kind '<food>
  :game-values (make-instance 'game-values :food-val 1500)) 

(define-object-kind "slime-mold" "& slime mold~"
  :numeric-id 24
  :x-attr #\g
  :x-char #\,
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 5
  :cost 2
  :obj-type '(<slime-mold> <food>)
  :sort-value 6036
  :the-kind '<food>
  :game-values (make-instance 'game-values :food-val 3000)) 

(define-object-kind "object-25" "& piece~ of elvish waybread"
  :numeric-id 25
  :x-attr #\B
  :x-char #\,
  :depth 5
  :rarity 0
  :chance #(1 1 1 0)
  :locale #(5 10 20 0)
  :weight 3
  :cost 10
  :obj-type '(<waybread> <food>)
  :sort-value 6037
  :the-kind '<food>
  :game-values (make-instance 'game-values :food-val 7500)) 

(define-object-kind "pint-ale" "& pint~ of fine ale"
  :numeric-id 26
  :x-attr #\y
  :x-char #\,
  :depth 0
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 5
  :cost 1
  :obj-type '(<ale> <food>)
  :sort-value 6038
  :the-kind '<food>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "pint-wine" "& pint~ of fine wine"
  :numeric-id 27
  :x-attr #\r
  :x-char #\,
  :depth 0
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 10
  :cost 2
  :obj-type '(<wine> <food>)
  :sort-value 6039
  :the-kind '<food>
  :game-values (make-instance 'game-values :food-val 1000)) 

(define-object-kind "shovel" "& shovel~"
  :numeric-id 84
  :x-attr #\s
  :x-char #\\
  :depth 1
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(5 0 0 0)
  :weight 60
  :cost 10
  :obj-type '(<shovel> <digging>)
  :flags '(<show-modififers>)
  :sort-value 2901
  :the-kind '<digger>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "gnomish-shovel" "& gnomish shovel~"
  :numeric-id 85
  :x-attr #\G
  :x-char #\\
  :depth 20
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(20 0 0 0)
  :weight 60
  :cost 100
  :obj-type '(<digging> <shovel> <gnome>)
  :flags '(<show-modififers>)
  :sort-value 2902
  :the-kind '<digger>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "dwarven-shovel" "& dwarven shovel~"
  :numeric-id 86
  :x-attr #\B
  :x-char #\\
  :depth 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 120
  :cost 200
  :obj-type '(<digging> <shovel> <dwarf>)
  :flags '(<show-modififers>)
  :sort-value 2903
  :the-kind '<digger>
  :game-values (make-instance 'game-values :base-dice 3 :num-dice 1)) 

(define-object-kind "pick" "& pick~"
  :numeric-id 87
  :x-attr #\s
  :x-char #\\
  :depth 5
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(10 0 0 0)
  :weight 150
  :cost 50
  :obj-type '(<pick> <digging>)
  :flags '(<show-modififers>)
  :sort-value 2904
  :the-kind '<digger>
  :game-values (make-instance 'game-values :base-dice 3 :num-dice 1)) 

(define-object-kind "orcish-pick" "& orcish pick~"
  :numeric-id 88
  :x-attr #\g
  :x-char #\\
  :depth 30
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(30 0 0 0)
  :weight 150
  :cost 300
  :obj-type '(<digging> <pick> <orc>)
  :flags '(<show-modififers>)
  :sort-value 2905
  :the-kind '<digger>
  :game-values (make-instance 'game-values :base-dice 3 :num-dice 1)) 

(define-object-kind "dwarven-pick" "& dwarven pick~"
  :numeric-id 89
  :x-attr #\b
  :x-char #\\
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 200
  :cost 600
  :obj-type '(<digging> <pick> <dwarf>)
  :flags '(<show-modififers>)
  :sort-value 2906
  :the-kind '<digger>
  :game-values (make-instance 'game-values :base-dice 4 :num-dice 1)) 

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
  :game-values (make-instance 'game-values :stat-bonuses '(<str>))) 

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
  :game-values (make-instance 'game-values :stat-bonuses '(<dex>))) 

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
  :game-values (make-instance 'game-values :stat-bonuses '(<con>))) 

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
  :game-values (make-instance 'game-values :stat-bonuses '(<int>))) 

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
  :game-values (make-instance 'game-values :abilities '(<speed>))) 

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
  :game-values (make-instance 'game-values :skill-bonuses '(<search>))) 

(define-object-kind "object-138" "teleportation"
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
  :game-values (make-instance 'game-values :abilities '(<teleport>))) 

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
  :game-values (make-instance 'game-values :abilities '(<slow-digestion>))) 

(define-object-kind "object-140" "resist fire"
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
  :game-values (make-instance 'game-values :ignores '(<fire>) :resists '(<fire>))) 

(define-object-kind "object-141" "resist cold"
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
  :game-values (make-instance 'game-values :ignores '(<cold>) :resists '(<cold>))) 

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
  :game-values (make-instance 'game-values :abilities '(<feather-fall>))) 

(define-object-kind "object-143" "poison resistance"
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
  :game-values (make-instance 'game-values :resists '(<poison>))) 

(define-object-kind "object-144" "free action"
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
  :game-values (make-instance 'game-values :abilities '(<free-action>))) 

(define-object-kind "object-145" "weakness"
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
  :game-values (make-instance 'game-values :stat-bonuses '(<str>))) 

(define-object-kind "object-146" "flames"
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
  :game-values (make-instance 'game-values :ac-bonus 15 :ignores '(<fire>) :resists '(<fire>))) 

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
  :game-values (make-instance 'game-values :ac-bonus 15 :ignores '(<acid>) :resists '(<acid>))) 

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
  :game-values (make-instance 'game-values :ac-bonus 15 :ignores '(<cold>) :resists '(<cold>))) 

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
  :game-values (make-instance 'game-values :stat-bonuses '(<chr> <wis>) :abilities '(<teleport>))) 

(define-object-kind "object-150" "stupidity"
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
  :game-values (make-instance 'game-values :stat-bonuses '(<int>))) 

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
  :game-values (make-instance 'game-values :abilities '(<aggravate>))) 

(define-object-kind "object-155" "see invisible"
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
  :game-values (make-instance 'game-values :abilities '(<see-invisible>))) 

(define-object-kind "object-156" "sustain strength"
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
  :game-values (make-instance 'game-values :sustains '(<str>))) 

(define-object-kind "object-157" "sustain intelligence"
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
  :game-values (make-instance 'game-values :sustains '(<int>))) 

(define-object-kind "object-158" "sustain wisdom"
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
  :game-values (make-instance 'game-values :sustains '(<wis>))) 

(define-object-kind "object-159" "sustain constitution"
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
  :game-values (make-instance 'game-values :sustains '(<con>))) 

(define-object-kind "object-160" "sustain dexterity"
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
  :game-values (make-instance 'game-values :sustains '(<dex>))) 

(define-object-kind "object-161" "sustain charisma"
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
  :game-values (make-instance 'game-values :sustains '(<chr>))) 

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
  :game-values (make-instance 'game-values)) 

;;; amulets

(define-object-kind "amulet-wis" "wisdom"
  :numeric-id 163
  :x-attr #\d
  :x-char #\"
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 3
  :cost 500
  :obj-type '(<neckwear> <amulet> <wis>)
  :flags '(<hide-type>)
  :sort-value 4306
  :the-kind '<amulet>
  :game-values (make-instance 'game-values :stat-bonuses '(<wis>))) 

(define-object-kind "amulet-chr" "charisma"
  :numeric-id 164
  :x-attr #\d
  :x-char #\"
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 3
  :cost 500
  :obj-type '(<neckwear> <amulet> <chr>)
  :flags '(<hide-type>)
  :sort-value 4307
  :the-kind '<amulet>
  :game-values (make-instance 'game-values :stat-bonuses '(<chr>))) 

(define-object-kind "amulet-searching" "searching"
  :numeric-id 165
  :x-attr #\d
  :x-char #\"
  :depth 30
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(30 0 0 0)
  :weight 3
  :cost 600
  :obj-type '(<neckwear> <amulet> <searching>)
  :flags '(<hide-type>)
  :sort-value 4305
  :the-kind '<amulet>
  :game-values (make-instance 'game-values :skill-bonuses '(<search>))) 

(define-object-kind "amulet-teleport" "teleportation"
  :numeric-id 166
  :x-attr #\d
  :x-char #\"
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 3
  :cost 0
  :obj-type '(<neckwear> <amulet> <teleport>)
  :flags '(<easy-know> <curse>)
  :sort-value 4301
  :the-kind '<amulet>
  :game-values (make-instance 'game-values :abilities '(<teleport>))) 

(define-object-kind "amulet-slow-digest" "slow digestion"
  :numeric-id 167
  :x-attr #\d
  :x-char #\"
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 3
  :cost 200
  :obj-type '(<neckwear> <amulet> <slow-digestion>)
  :flags '(<easy-know>)
  :sort-value 4303
  :the-kind '<amulet>
  :game-values (make-instance 'game-values :abilities '(<slow-digestion>))) 

(define-object-kind "amulet-resist-acid" "resist acid"
  :numeric-id 168
  :x-attr #\d
  :x-char #\"
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 3
  :cost 300
  :obj-type '(<neckwear> <amulet> <resist> <acid>)
  :flags '(<easy-know>)
  :sort-value 4304
  :the-kind '<amulet>
  :game-values (make-instance 'game-values :ignores '(<acid>) :resists '(<acid>))) 

(define-object-kind "amulet-adornment" "adornment"
  :numeric-id 169
  :x-attr #\d
  :x-char #\"
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 3
  :cost 20
  :obj-type '(<neckwear> <amulet> <adornment>)
  :flags '(<easy-know>)
  :sort-value 4302
  :the-kind '<amulet>
  :game-values (make-instance 'game-values)) 

(define-object-kind "amulet-magi" "the magi"
  :numeric-id 171
  :x-attr #\d
  :x-char #\"
  :depth 50
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(50 0 0 0)
  :weight 3
  :cost 30000
  :obj-type '(<neckwear> <amulet> <magi>)
  :sort-value 4308
  :the-kind '<amulet>
  :game-values (make-instance 'game-values :ac-bonus 3 :skill-bonuses '(<search>) :ignores
                              '(<cold> <fire> <electricity> <acid>) :abilities '(<see-invisible> <free-action>))) 

(define-object-kind "amulet-doom" "doom"
  :numeric-id 172
  :x-attr #\d
  :x-char #\"
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 3
  :cost 0
  :obj-type '(<neckwear> <amulet> <doom>)
  :flags '(<hide-type> <curse>)
  :sort-value 4300
  :the-kind '<amulet>
  :game-values (make-instance 'game-values :stat-bonuses '(<chr> <con> <dex> <wis> <int> <str>))) 


(define-object-kind "small-wooden-chest" "& small wooden chest~"
  :numeric-id 338
  :x-attr #\s
  :x-char #\~
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 250
  :cost 20
  :obj-type '(<chest>)
  :sort-value 2401
  :the-kind '<chest>
  :game-values (make-instance 'game-values :base-dice 3 :num-dice 2)) 

(define-object-kind "large-wooden-chest" "& large wooden chest~"
  :numeric-id 339
  :x-attr #\s
  :x-char #\~
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 500
  :cost 60
  :obj-type '(<chest>)
  :sort-value 2405
  :the-kind '<chest>
  :game-values (make-instance 'game-values :base-dice 5 :num-dice 2)) 

(define-object-kind "small-iron-chest" "& small iron chest~"
  :numeric-id 340
  :x-attr #\s
  :x-char #\~
  :depth 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 300
  :cost 100
  :obj-type '(<chest>)
  :sort-value 2402
  :the-kind '<chest>
  :game-values (make-instance 'game-values :base-dice 4 :num-dice 2)) 

(define-object-kind "large-iron-chest" "& large iron chest~"
  :numeric-id 341
  :x-attr #\s
  :x-char #\~
  :depth 35
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(35 0 0 0)
  :weight 1000
  :cost 150
  :obj-type '(<chest>)
  :sort-value 2406
  :the-kind '<chest>
  :game-values (make-instance 'game-values :base-dice 6 :num-dice 2)) 

(define-object-kind "small-steel-chest" "& small steel chest~"
  :numeric-id 342
  :x-attr #\s
  :x-char #\~
  :depth 45
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(45 0 0 0)
  :weight 500
  :cost 200
  :obj-type '(<chest>)
  :sort-value 2403
  :the-kind '<chest>
  :game-values (make-instance 'game-values :base-dice 4 :num-dice 2)) 

(define-object-kind "large-steel-chest" "& large steel chest~"
  :numeric-id 343
  :x-attr #\s
  :x-char #\~
  :depth 55
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(55 0 0 0)
  :weight 1000
  :cost 250
  :obj-type '(<chest>)
  :sort-value 2407
  :the-kind '<chest>
  :game-values (make-instance 'game-values :base-dice 6 :num-dice 2)) 

(define-object-kind "ruined-chest" "& ruined chest~"
  :numeric-id 344
  :x-attr #\s
  :x-char #\~
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(75 0 0 0)
  :weight 250
  :cost 0
  :obj-type '(<chest>)
  :sort-value 2400
  :the-kind '<chest>) 

(define-object-kind "iron-spike" "& iron spike~"
  :numeric-id 345
  :x-attr #\W
  :x-char #\~
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 10
  :cost 1
  :obj-type '(<spike>)
  :sort-value 2300
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "torch" "& wooden torch~"
  :numeric-id 346
  :x-attr #\u
  :x-char #\~
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 30
  :cost 2
  :obj-type '(<torch> <light-source>)
  :flags '(<easy-know>)
  :sort-value 4200
  :the-kind '<light-source>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :charges 4000 :light-radius 1)) 

(define-object-kind "lantern" "& brass lantern~"
  :numeric-id 347
  :x-attr #\U
  :x-char #\~
  :depth 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 50
  :cost 35
  :obj-type '(<lantern> <light-source>)
  :flags '(<easy-know>)
  :sort-value 4201
  :the-kind '<light-source>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :charges 7500 :light-radius 2 :ignores '(<fire>))) 

(define-object-kind "oil-flask" "& flask~ of oil"
  :numeric-id 348
  :x-attr #\y
  :x-char #\!
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 10
  :cost 3
  :obj-type '(<oil> <flask>)
  :sort-value 5700
  :game-values (make-instance 'game-values :base-dice 6 :num-dice 2 :charges 7500)) 

(define-object-kind "empty-bottle" "& empty bottle~"
  :numeric-id 349
  :x-attr #\w
  :x-char #\!
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 0
  :obj-type '(<bottle>)
  :sort-value 2101
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "pottery-shards" "& shard~ of pottery"
  :numeric-id 389
  :x-attr #\r
  :x-char #\~
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 5
  :cost 0
  :obj-type '(<junk>)
  :sort-value 2203
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "broken-stick" "& broken stick~"
  :numeric-id 390
  :x-attr #\r
  :x-char #\~
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 3
  :cost 0
  :obj-type '(<junk>)
  :sort-value 2206
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "broken-skull" "& broken skull~"
  :numeric-id 391
  :x-attr #\w
  :x-char #\~
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 1
  :cost 0
  :obj-type '(<skeleton>)
  :sort-value 2001
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "broken-bone" "& broken bone~"
  :numeric-id 392
  :x-attr #\w
  :x-char #\~
  :depth 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 0
  :obj-type '(<skeleton>)
  :sort-value 2002
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "canine-skeleton" "& canine skeleton~"
  :numeric-id 393
  :x-attr #\w
  :x-char #\~
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 10
  :cost 0
  :obj-type '(<skeleton>)
  :sort-value 2004
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "rodent-skeleton" "& rodent skeleton~"
  :numeric-id 394
  :x-attr #\w
  :x-char #\~
  :depth 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 10
  :cost 0
  :obj-type '(<skeleton>)
  :sort-value 2003
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "human-skeleton" "& human skeleton~"
  :numeric-id 395
  :x-attr #\w
  :x-char #\~
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 60
  :cost 0
  :obj-type '(<skeleton>)
  :sort-value 2008
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "dwarf-skeleton" "& dwarf skeleton~"
  :numeric-id 396
  :x-attr #\w
  :x-char #\~
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 50
  :cost 0
  :obj-type '(<skeleton>)
  :sort-value 2007
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "elf-skeleton" "& elf skeleton~"
  :numeric-id 397
  :x-attr #\w
  :x-char #\~
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 40
  :cost 0
  :obj-type '(<skeleton>)
  :sort-value 2006
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-398" "& gnome skeleton~"
  :numeric-id 398
  :x-attr #\w
  :x-char #\~
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 30
  :cost 0
  :obj-type '(<skeleton>)
  :sort-value 2005
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 


(define-object-kind "object-480" "copper"
  :numeric-id 480
  :x-attr #\u
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 3
  :obj-type '(<money>)
  :sort-value 7101
  :the-kind '<money>) 

(define-object-kind "object-481" "copper"
  :numeric-id 481
  :x-attr #\u
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 4
  :obj-type '(<money>)
  :sort-value 7102
  :the-kind '<money>) 

(define-object-kind "object-482" "copper"
  :numeric-id 482
  :x-attr #\u
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 5
  :obj-type '(<money>)
  :sort-value 7103
  :the-kind '<money>) 

(define-object-kind "object-483" "silver"
  :numeric-id 483
  :x-attr #\s
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 6
  :obj-type '(<money>)
  :sort-value 7104
  :the-kind '<money>) 

(define-object-kind "object-484" "silver"
  :numeric-id 484
  :x-attr #\s
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 7
  :obj-type '(<money>)
  :sort-value 7105
  :the-kind '<money>) 

(define-object-kind "object-485" "silver"
  :numeric-id 485
  :x-attr #\s
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 8
  :obj-type '(<money>)
  :sort-value 7106
  :the-kind '<money>) 

(define-object-kind "object-486" "garnets"
  :numeric-id 486
  :x-attr #\r
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 9
  :obj-type '(<money>)
  :sort-value 7107
  :the-kind '<money>) 

(define-object-kind "object-487" "garnets"
  :numeric-id 487
  :x-attr #\r
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 10
  :obj-type '(<money>)
  :sort-value 7108
  :the-kind '<money>) 

(define-object-kind "object-488" "gold"
  :numeric-id 488
  :x-attr #\y
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 12
  :obj-type '(<money>)
  :sort-value 7109
  :the-kind '<money>) 

(define-object-kind "object-489" "gold"
  :numeric-id 489
  :x-attr #\y
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 14
  :obj-type '(<money>)
  :sort-value 7110
  :the-kind '<money>) 

(define-object-kind "object-490" "gold"
  :numeric-id 490
  :x-attr #\y
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 16
  :obj-type '(<money>)
  :sort-value 7111
  :the-kind '<money>) 

(define-object-kind "object-491" "opals"
  :numeric-id 491
  :x-attr #\W
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 18
  :obj-type '(<money>)
  :sort-value 7112
  :the-kind '<money>) 

(define-object-kind "object-492" "sapphires"
  :numeric-id 492
  :x-attr #\b
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 20
  :obj-type '(<money>)
  :sort-value 7113
  :the-kind '<money>) 

(define-object-kind "object-493" "rubies"
  :numeric-id 493
  :x-attr #\r
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 24
  :obj-type '(<money>)
  :sort-value 7114
  :the-kind '<money>) 

(define-object-kind "object-494" "diamonds"
  :numeric-id 494
  :x-attr #\w
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 28
  :obj-type '(<money>)
  :sort-value 7115
  :the-kind '<money>) 

(define-object-kind "object-495" "emeralds"
  :numeric-id 495
  :x-attr #\g
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 32
  :obj-type '(<money>)
  :sort-value 7116
  :the-kind '<money>) 

(define-object-kind "object-496" "mithril"
  :numeric-id 496
  :x-attr #\B
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 40
  :obj-type '(<money>)
  :sort-value 7117
  :the-kind '<money>) 

(define-object-kind "object-497" "adamantite"
  :numeric-id 497
  :x-attr #\G
  :x-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 80
  :obj-type '(<money>)
  :sort-value 7118
  :the-kind '<money>) 

;;; artifact items

(define-object-kind "object-500" "& phial~"
  :numeric-id 500
  :x-attr #\y
  :x-char #\~
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 10
  :cost 10000
  :obj-type '(<phial> <light-source>)
  :flags '(<instant-artifact>)
  :sort-value 4204
  :the-kind '<light-source>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :light-radius 3)) 

(define-object-kind "object-501" "& star~"
  :numeric-id 501
  :x-attr #\y
  :x-char #\~
  :depth 30
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 5
  :cost 25000
  :obj-type '(<star> <light-source>)
  :flags '(<instant-artifact>)
  :sort-value 4205
  :the-kind '<light-source>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :light-radius 3)) 

(define-object-kind "object-502" "& arkenstone~"
  :numeric-id 502
  :x-attr #\y
  :x-char #\~
  :depth 60
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 5
  :cost 60000
  :obj-type '(<arkenstone> <light-source>)
  :flags '(<instant-artifact>)
  :sort-value 4206
  :the-kind '<light-source>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :light-radius 3)) 

(define-object-kind "object-503" "& amulet~"
  :numeric-id 503
  :x-attr #\d
  :x-char #\"
  :depth 50
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 3
  :cost 60000
  :obj-type '(<neckwear> <amulet> <carlammas>)
  :flags '(<instant-artifact>)
  :sort-value 4310
  :the-kind '<amulet>
  :game-values (make-instance 'game-values)) 

(define-object-kind "object-504" "& amulet~"
  :numeric-id 504
  :x-attr #\d
  :x-char #\"
  :depth 60
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 3
  :cost 90000
  :obj-type '(<neckwear> <amulet> <ingwe>)
  :flags '(<instant-artifact>)
  :sort-value 4311
  :the-kind '<amulet>
  :game-values (make-instance 'game-values)) 

(define-object-kind "object-505" "& necklace~"
  :numeric-id 505
  :x-attr #\d
  :x-char #\"
  :depth 70
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 3
  :cost 75000
  :obj-type '(<neckwear> <necklace> <dwarves>)
  :flags '(<instant-artifact>)
  :sort-value 4312
  :the-kind '<neckwear>
  :game-values (make-instance 'game-values)) 

(define-object-kind "object-506" "& ring~"
  :numeric-id 506
  :x-attr #\d
  :x-char #\=
  :depth 50
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 65000
  :obj-type '(<ring>)
  :flags '(<instant-artifact>)
  :sort-value 4432
  :the-kind '<ring>
  :game-values (make-instance 'game-values)) 

(define-object-kind "object-507" "& ring~"
  :numeric-id 507
  :x-attr #\d
  :x-char #\=
  :depth 90
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 150000
  :obj-type '(<ring>)
  :flags '(<instant-artifact>)
  :sort-value 4433
  :the-kind '<ring>
  :game-values (make-instance 'game-values)) 

(define-object-kind "object-508" "& ring~"
  :numeric-id 508
  :x-attr #\d
  :x-char #\=
  :depth 80
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 100000
  :obj-type '(<ring>)
  :flags '(<instant-artifact>)
  :sort-value 4434
  :the-kind '<ring>
  :game-values (make-instance 'game-values)) 

(define-object-kind "object-509" "& ring~"
  :numeric-id 509
  :x-attr #\d
  :x-char #\=
  :depth 90
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 200000
  :obj-type '(<ring>)
  :flags '(<instant-artifact>)
  :sort-value 4435
  :the-kind '<ring>
  :game-values (make-instance 'game-values)) 

(define-object-kind "object-510" "& ring~"
  :numeric-id 510
  :x-attr #\d
  :x-char #\=
  :depth 100
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 300000
  :obj-type '(<ring>)
  :flags '(<instant-artifact>)
  :sort-value 4436
  :the-kind '<ring>
  :game-values (make-instance 'game-values)) 

(define-object-kind "object-511" "& ring~"
  :numeric-id 511
  :x-attr #\y
  :x-char #\=
  :depth 110
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 5000000
  :obj-type '(<ring>)
  :flags '(<instant-artifact>)
  :sort-value 4437
  :the-kind '<ring>
  :game-values (make-instance 'game-values)) 


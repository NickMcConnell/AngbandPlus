
(in-package :langband)

(define-object-kind "object-0" "<pile>"
  :numeric-id 0
  :x-attr #\w
  :x-char #\&
  :level 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)) 

(define-object-kind "object-1" "blindness"
  :numeric-id 1
  :x-attr #\d
  :x-char #\,
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 1
  :cost 0
  :obj-type '(<blindness> <mushroom> <food>)
  :sort-value 6001
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-2" "paranoia"
  :numeric-id 2
  :x-attr #\d
  :x-char #\,
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 1
  :cost 0
  :obj-type '(<paranoia> <mushroom> <food>)
  :sort-value 6002
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-3" "confusion"
  :numeric-id 3
  :x-attr #\d
  :x-char #\,
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 1
  :cost 0
  :obj-type '(<confusion> <mushroom> <food>)
  :sort-value 6003
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-4" "hallucination"
  :numeric-id 4
  :x-attr #\d
  :x-char #\,
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 1
  :cost 0
  :obj-type '(<hallucination> <mushroom> <food>)
  :sort-value 6004
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-5" "cure poison"
  :numeric-id 5
  :x-attr #\d
  :x-char #\,
  :level 10
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
  :level 10
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
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 1
  :cost 25
  :obj-type '(<mushroom> <food> <cure> <paranoia>)
  :sort-value 6014
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-8" "cure confusion"
  :numeric-id 8
  :x-attr #\d
  :x-char #\,
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 1
  :cost 50
  :obj-type '(<mushroom> <food> <cure> <confusion>)
  :sort-value 6015
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-9" "weakness"
  :numeric-id 9
  :x-attr #\d
  :x-char #\,
  :level 10
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
  :level 15
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
  :level 20
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
  :level 20
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
  :level 15
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
  :level 15
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
  :level 5
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
  :level 10
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
  :level 20
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
  :level 20
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
  :level 20
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
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 2
  :cost 75
  :obj-type '(<mushroom> <food> <cure> <serious>)
  :sort-value 6016
  :the-kind '<mushroom>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-21" "& ration~ of food"
  :numeric-id 21
  :x-attr #\U
  :x-char #\,
  :level 0
  :rarity 0
  :chance #(1 1 1 0)
  :locale #(0 5 10 0)
  :weight 10
  :cost 3
  :obj-type '(<ration> <food>)
  :sort-value 6035
  :the-kind '<food>
  :game-values (make-instance 'game-values :food-val 5000)) 

(define-object-kind "object-22" "& hard biscuit~"
  :numeric-id 22
  :x-attr #\U
  :x-char #\,
  :level 0
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 1
  :obj-type '(<biscuit> <food>)
  :sort-value 6032
  :the-kind '<food>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-23" "& strip~ of beef jerky"
  :numeric-id 23
  :x-attr #\u
  :x-char #\,
  :level 0
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 2
  :obj-type '(<jerky> <food>)
  :sort-value 6033
  :the-kind '<food>
  :game-values (make-instance 'game-values :food-val 1500)) 

(define-object-kind "object-24" "& slime mold~"
  :numeric-id 24
  :x-attr #\g
  :x-char #\,
  :level 1
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
  :level 5
  :rarity 0
  :chance #(1 1 1 0)
  :locale #(5 10 20 0)
  :weight 3
  :cost 10
  :obj-type '(<waybread> <food>)
  :sort-value 6037
  :the-kind '<food>
  :game-values (make-instance 'game-values :food-val 7500)) 

(define-object-kind "object-26" "& pint~ of fine ale"
  :numeric-id 26
  :x-attr #\y
  :x-char #\,
  :level 0
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 5
  :cost 1
  :obj-type '(<ale> <food>)
  :sort-value 6038
  :the-kind '<food>
  :game-values (make-instance 'game-values :food-val 500)) 

(define-object-kind "object-27" "& pint~ of fine wine"
  :numeric-id 27
  :x-attr #\r
  :x-char #\,
  :level 0
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 10
  :cost 2
  :obj-type '(<wine> <food>)
  :sort-value 6039
  :the-kind '<food>
  :game-values (make-instance 'game-values :food-val 1000)) 

(define-object-kind "object-30" "& broken dagger~"
  :numeric-id 30
  :x-attr #\D
  :x-char #\|
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 5
  :cost 1
  :obj-type '(<broken-dagger> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3201
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :tohit-bonus -2 :dmg-bonus -4)) 

(define-object-kind "object-31" "& bastard sword~"
  :numeric-id 31
  :x-attr #\W
  :x-char #\|
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 140
  :cost 350
  :obj-type '(<bastard> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3221
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 4 :num-dice 3)) 

(define-object-kind "object-32" "& scimitar~"
  :numeric-id 32
  :x-attr #\W
  :x-char #\|
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 130
  :cost 250
  :obj-type '(<scimitar> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3218
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 5 :num-dice 2)) 

(define-object-kind "object-33" "& tulwar~"
  :numeric-id 33
  :x-attr #\W
  :x-char #\|
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 100
  :cost 200
  :obj-type '(<tulwar> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3215
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 4 :num-dice 2)) 

(define-object-kind "object-34" "& broad sword~"
  :numeric-id 34
  :x-attr #\W
  :x-char #\|
  :level 10
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(10 15 0 0)
  :weight 150
  :cost 255
  :obj-type '(<broad-sword> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3216
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 5 :num-dice 2)) 

(define-object-kind "object-35" "& short sword~"
  :numeric-id 35
  :x-attr #\W
  :x-char #\|
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 80
  :cost 90
  :obj-type '(<short-sword> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3210
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 7 :num-dice 1)) 

(define-object-kind "object-36" "& blade~ of chaos"
  :numeric-id 36
  :x-attr #\v
  :x-char #\|
  :level 70
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(70 0 0 0)
  :weight 180
  :cost 4000
  :obj-type '(<blade-of-chaos> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3230
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 5 :num-dice 6 :resists '(<chaos> <confusion>))) 

(define-object-kind "object-37" "& two-handed sword~"
  :numeric-id 37
  :x-attr #\W
  :x-char #\|
  :level 30
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(30 40 0 0)
  :weight 200
  :cost 775
  :obj-type '(<two-handed-sword> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3225
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 6 :num-dice 3)) 

(define-object-kind "object-38" "& dirk~"
  :numeric-id 38
  :x-attr #\W
  :x-char #\|
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 30
  :cost 25
  :obj-type '(<main-gauche> <dirk> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3205
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 5 :num-dice 1)) 

(define-object-kind "object-39" "& cutlass~"
  :numeric-id 39
  :x-attr #\W
  :x-char #\|
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 110
  :cost 85
  :obj-type '(<cutlass> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3212
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 7 :num-dice 1)) 

(define-object-kind "object-40" "& executioner's sword~"
  :numeric-id 40
  :x-attr #\r
  :x-char #\|
  :level 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 260
  :cost 850
  :obj-type '(<exec-sword> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3228
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 5 :num-dice 4)) 

(define-object-kind "object-41" "& katana~"
  :numeric-id 41
  :x-attr #\W
  :x-char #\|
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 120
  :cost 400
  :obj-type '(<katana> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3220
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 4 :num-dice 3)) 

(define-object-kind "object-42" "& long sword~"
  :numeric-id 42
  :x-attr #\W
  :x-char #\|
  :level 10
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(10 20 0 0)
  :weight 130
  :cost 300
  :obj-type '(<long-sword> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3217
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 5 :num-dice 2)) 

(define-object-kind "object-43" "& dagger~"
  :numeric-id 43
  :x-attr #\W
  :x-char #\|
  :level 0
  :rarity 0
  :chance #(1 1 1 1)
  :locale #(0 5 10 20)
  :weight 12
  :cost 10
  :obj-type '(<dagger> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3204
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 4 :num-dice 1)) 

(define-object-kind "object-44" "& rapier~"
  :numeric-id 44
  :x-attr #\W
  :x-char #\|
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 40
  :cost 42
  :obj-type '(<rapier> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3207
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 6 :num-dice 1)) 

(define-object-kind "object-45" "& sabre~"
  :numeric-id 45
  :x-attr #\W
  :x-char #\|
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 50
  :cost 50
  :obj-type '(<sabre> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3211
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 7 :num-dice 1)) 

(define-object-kind "object-46" "& small sword~"
  :numeric-id 46
  :x-attr #\W
  :x-char #\|
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 75
  :cost 48
  :obj-type '(<small-sword> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3208
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 6 :num-dice 1)) 

(define-object-kind "object-47" "& broken sword~"
  :numeric-id 47
  :x-attr #\D
  :x-char #\|
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 30
  :cost 2
  :obj-type '(<broken-sword> <weapon> <sword>)
  :flags '(<show-modififers>)
  :sort-value 3202
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1 :tohit-bonus -2 :dmg-bonus -4)) 

(define-object-kind "object-48" "& ball-and-chain~"
  :numeric-id 48
  :x-attr #\D
  :x-char #\\
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 150
  :cost 200
  :obj-type '(<ball-and-chain> <weapon> <hafted>)
  :flags '(<show-modififers>)
  :sort-value 3006
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 4 :num-dice 2)) 

(define-object-kind "object-49" "& whip~"
  :numeric-id 49
  :x-attr #\D
  :x-char #\\
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 30
  :cost 30
  :obj-type '(<whip> <weapon> <hafted>)
  :flags '(<show-modififers>)
  :sort-value 3002
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 6 :num-dice 1)) 

(define-object-kind "object-50" "& flail~"
  :numeric-id 50
  :x-attr #\D
  :x-char #\\
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 150
  :cost 353
  :obj-type '(<flail> <weapon> <hafted>)
  :flags '(<show-modififers>)
  :sort-value 3013
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 6 :num-dice 2)) 

(define-object-kind "object-51" "& two-handed flail~"
  :numeric-id 51
  :x-attr #\y
  :x-char #\\
  :level 45
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(45 0 0 0)
  :weight 280
  :cost 590
  :obj-type '(<two-handed-flail> <weapon> <hafted>)
  :flags '(<show-modififers>)
  :sort-value 3018
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 6 :num-dice 3)) 

(define-object-kind "object-52" "& morning star~"
  :numeric-id 52
  :x-attr #\D
  :x-char #\\
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 150
  :cost 396
  :obj-type '(<morning-star> <weapon> <hafted>)
  :flags '(<show-modififers>)
  :sort-value 3012
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 6 :num-dice 2)) 

(define-object-kind "object-53" "& mace~"
  :numeric-id 53
  :x-attr #\D
  :x-char #\\
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 120
  :cost 130
  :obj-type '(<mace> <weapon> <hafted>)
  :flags '(<show-modififers>)
  :sort-value 3005
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 4 :num-dice 2)) 

(define-object-kind "object-54" "& quarterstaff~"
  :numeric-id 54
  :x-attr #\U
  :x-char #\\
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 150
  :cost 200
  :obj-type '(<quarterstaff> <weapon> <hafted>)
  :flags '(<show-modififers>)
  :sort-value 3003
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 9 :num-dice 1)) 

(define-object-kind "object-55" "& war hammer~"
  :numeric-id 55
  :x-attr #\D
  :x-char #\\
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 120
  :cost 225
  :obj-type '(<war-hammer> <weapon> <hafted>)
  :flags '(<show-modififers>)
  :sort-value 3008
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 3 :num-dice 3)) 

(define-object-kind "object-56" "& lead-filled mace~"
  :numeric-id 56
  :x-attr #\D
  :x-char #\\
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 180
  :cost 502
  :obj-type '(<lead-filled-mace> <weapon> <hafted>)
  :flags '(<show-modififers>)
  :sort-value 3015
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 4 :num-dice 3)) 

(define-object-kind "object-57" "& mace~ of disruption"
  :numeric-id 57
  :x-attr #\v
  :x-char #\\
  :level 80
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(80 0 0 0)
  :weight 400
  :cost 4300
  :obj-type '(<mace-of-disruption> <weapon> <hafted>)
  :flags '(<show-modififers>)
  :sort-value 3020
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 8 :num-dice 5 :slays '(<undead>))) 

(define-object-kind "object-58" "& lucerne hammer~"
  :numeric-id 58
  :x-attr #\B
  :x-char #\\
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 120
  :cost 376
  :obj-type '(<lucern-hammer> <weapon> <hafted>)
  :flags '(<show-modififers>)
  :sort-value 3010
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 5 :num-dice 2)) 

(define-object-kind "object-59" "& beaked axe~"
  :numeric-id 59
  :x-attr #\s
  :x-char #\/
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 180
  :cost 408
  :obj-type '(<beaked-axe> <weapon> <polearm>)
  :flags '(<show-modififers>)
  :sort-value 3110
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 6 :num-dice 2)) 

(define-object-kind "object-60" "& glaive~"
  :numeric-id 60
  :x-attr #\s
  :x-char #\/
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 190
  :cost 363
  :obj-type '(<glaive> <weapon> <polearm>)
  :flags '(<show-modififers>)
  :sort-value 3113
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 6 :num-dice 2)) 

(define-object-kind "object-61" "& halberd~"
  :numeric-id 61
  :x-attr #\s
  :x-char #\/
  :level 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 190
  :cost 430
  :obj-type '(<halberd> <weapon> <polearm>)
  :flags '(<show-modififers>)
  :sort-value 3115
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 5 :num-dice 3)) 

(define-object-kind "object-62" "& awl-pike~"
  :numeric-id 62
  :x-attr #\s
  :x-char #\/
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 160
  :cost 340
  :obj-type '(<awl-pike> <weapon> <polearm>)
  :flags '(<show-modififers>)
  :sort-value 3104
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 8 :num-dice 1)) 

(define-object-kind "object-63" "& pike~"
  :numeric-id 63
  :x-attr #\s
  :x-char #\/
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 160
  :cost 358
  :obj-type '(<pike> <weapon> <polearm>)
  :flags '(<show-modififers>)
  :sort-value 3108
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 5 :num-dice 2)) 

(define-object-kind "object-64" "& spear~"
  :numeric-id 64
  :x-attr #\s
  :x-char #\/
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 50
  :cost 36
  :obj-type '(<spear> <weapon> <polearm>)
  :flags '(<show-modififers>)
  :sort-value 3102
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 6 :num-dice 1)) 

(define-object-kind "object-65" "& trident~"
  :numeric-id 65
  :x-attr #\y
  :x-char #\/
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 70
  :cost 120
  :obj-type '(<trident> <weapon> <polearm>)
  :flags '(<show-modififers>)
  :sort-value 3105
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 8 :num-dice 1)) 

(define-object-kind "object-66" "& lance~"
  :numeric-id 66
  :x-attr #\s
  :x-char #\/
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 300
  :cost 230
  :obj-type '(<lance> <weapon> <polearm>)
  :flags '(<show-modififers>)
  :sort-value 3120
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 8 :num-dice 2)) 

(define-object-kind "object-67" "& great axe~"
  :numeric-id 67
  :x-attr #\s
  :x-char #\/
  :level 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 230
  :cost 500
  :obj-type '(<great-axe> <weapon> <polearm>)
  :flags '(<show-modififers>)
  :sort-value 3125
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 4 :num-dice 4)) 

(define-object-kind "object-68" "& battle axe~"
  :numeric-id 68
  :x-attr #\s
  :x-char #\/
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 170
  :cost 334
  :obj-type '(<battle-axe> <weapon> <polearm>)
  :flags '(<show-modififers>)
  :sort-value 3122
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 8 :num-dice 2)) 

(define-object-kind "object-69" "& lochaber axe~"
  :numeric-id 69
  :x-attr #\D
  :x-char #\/
  :level 45
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(45 0 0 0)
  :weight 250
  :cost 750
  :obj-type '(<lochaber-axe> <weapon> <polearm>)
  :flags '(<show-modififers>)
  :sort-value 3128
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 8 :num-dice 3)) 

(define-object-kind "object-70" "& broad axe~"
  :numeric-id 70
  :x-attr #\s
  :x-char #\/
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 160
  :cost 304
  :obj-type '(<broad-axe> <weapon> <polearm>)
  :flags '(<show-modififers>)
  :sort-value 3111
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 6 :num-dice 2)) 

(define-object-kind "object-71" "& scythe~"
  :numeric-id 71
  :x-attr #\s
  :x-char #\/
  :level 45
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(45 0 0 0)
  :weight 250
  :cost 800
  :obj-type '(<scythe> <weapon> <polearm>)
  :flags '(<show-modififers>)
  :sort-value 3117
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 3 :num-dice 5)) 

(define-object-kind "object-72" "& scythe~ of slicing"
  :numeric-id 72
  :x-attr #\r
  :x-char #\/
  :level 60
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(60 0 0 0)
  :weight 250
  :cost 3500
  :obj-type '(<scythe-slicing> <weapon> <polearm>)
  :flags '(<show-modififers>)
  :sort-value 3130
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 4 :num-dice 8)) 

(define-object-kind "object-73" "& short bow~"
  :numeric-id 73
  :x-attr #\U
  :x-char #\}
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 30
  :cost 50
  :obj-type '(<short> <bow>)
  :flags '(<show-modififers>)
  :sort-value 2812
  :the-kind '<bow>
  :game-values (make-instance 'game-values)) 

(define-object-kind "object-74" "& long bow~"
  :numeric-id 74
  :x-attr #\U
  :x-char #\}
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 40
  :cost 120
  :obj-type '(<long> <bow>)
  :flags '(<show-modififers>)
  :sort-value 2813
  :the-kind '<bow>
  :game-values (make-instance 'game-values)) 

(define-object-kind "object-75" "& light crossbow~"
  :numeric-id 75
  :x-attr #\s
  :x-char #\}
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 110
  :cost 140
  :obj-type '(<bow> <light> <xbow>)
  :flags '(<show-modififers>)
  :sort-value 2823
  :the-kind '<bow>
  :game-values (make-instance 'game-values)) 

(define-object-kind "object-76" "& heavy crossbow~"
  :numeric-id 76
  :x-attr #\s
  :x-char #\}
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 200
  :cost 300
  :obj-type '(<bow> <heavy> <xbow>)
  :flags '(<show-modififers>)
  :sort-value 2824
  :the-kind '<bow>
  :game-values (make-instance 'game-values)) 

(define-object-kind "object-77" "& sling~"
  :numeric-id 77
  :x-attr #\u
  :x-char #\}
  :level 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 5
  :cost 5
  :obj-type '(<sling> <bow>)
  :flags '(<show-modififers>)
  :sort-value 2802
  :the-kind '<bow>
  :game-values (make-instance 'game-values)) 

(define-object-kind "object-78" "& arrow~"
  :numeric-id 78
  :x-attr #\U
  :x-char #\{
  :level 3
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(3 15 0 0)
  :weight 2
  :cost 1
  :obj-type '(<normal> <ammo> <arrow>)
  :flags '(<show-modififers>)
  :sort-value 2601
  :the-kind '<ammo>
  :game-values (make-instance 'game-values :base-dice 4 :num-dice 1)) 

(define-object-kind "object-79" "& seeker arrow~"
  :numeric-id 79
  :x-attr #\G
  :x-char #\{
  :level 55
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(55 0 0 0)
  :weight 2
  :cost 20
  :obj-type '(<heavy> <ammo> <arrow>)
  :flags '(<show-modififers>)
  :sort-value 2602
  :the-kind '<ammo>
  :game-values (make-instance 'game-values :base-dice 4 :num-dice 4)) 

(define-object-kind "object-80" "& bolt~"
  :numeric-id 80
  :x-attr #\s
  :x-char #\{
  :level 3
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(3 25 0 0)
  :weight 3
  :cost 2
  :obj-type '(<normal> <ammo> <bolt>)
  :flags '(<show-modififers>)
  :sort-value 2701
  :the-kind '<ammo>
  :game-values (make-instance 'game-values :base-dice 5 :num-dice 1)) 

(define-object-kind "object-81" "& seeker bolt~"
  :numeric-id 81
  :x-attr #\B
  :x-char #\{
  :level 65
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(65 0 0 0)
  :weight 3
  :cost 25
  :obj-type '(<heavy> <ammo> <bolt>)
  :flags '(<show-modififers>)
  :sort-value 2702
  :the-kind '<ammo>
  :game-values (make-instance 'game-values :base-dice 5 :num-dice 4)) 

(define-object-kind "object-82" "& rounded pebble~"
  :numeric-id 82
  :x-attr #\s
  :x-char #\{
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 4
  :cost 1
  :obj-type '(<light> <ammo> <shot>)
  :flags '(<show-modififers>)
  :sort-value 2500
  :the-kind '<ammo>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-83" "& iron shot~"
  :numeric-id 83
  :x-attr #\s
  :x-char #\{
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 5
  :cost 2
  :obj-type '(<normal> <ammo> <shot>)
  :flags '(<show-modififers>)
  :sort-value 2501
  :the-kind '<ammo>
  :game-values (make-instance 'game-values :base-dice 3 :num-dice 1)) 

(define-object-kind "object-84" "& shovel~"
  :numeric-id 84
  :x-attr #\s
  :x-char #\\
  :level 1
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

(define-object-kind "object-85" "& gnomish shovel~"
  :numeric-id 85
  :x-attr #\G
  :x-char #\\
  :level 20
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

(define-object-kind "object-86" "& dwarven shovel~"
  :numeric-id 86
  :x-attr #\B
  :x-char #\\
  :level 40
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

(define-object-kind "object-87" "& pick~"
  :numeric-id 87
  :x-attr #\s
  :x-char #\\
  :level 5
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

(define-object-kind "object-88" "& orcish pick~"
  :numeric-id 88
  :x-attr #\g
  :x-char #\\
  :level 30
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

(define-object-kind "object-89" "& dwarven pick~"
  :numeric-id 89
  :x-attr #\b
  :x-char #\\
  :level 50
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

(define-object-kind "object-91" "& pair~ of soft leather boots"
  :numeric-id 91
  :x-attr #\U
  :x-char #\]
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 20
  :cost 7
  :obj-type '(<soft-leather <boots>)
  :sort-value 3302
  :the-kind '<boots>
  :game-values (make-instance 'game-values :base-ac 2 :base-dice 1 :num-dice 1)) 

(define-object-kind "object-92" "& pair~ of hard leather boots"
  :numeric-id 92
  :x-attr #\U
  :x-char #\]
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 40
  :cost 12
  :obj-type '(<hard-leather> <boots>)
  :sort-value 3303
  :the-kind '<boots>
  :game-values (make-instance 'game-values :base-ac 3 :base-dice 1 :num-dice 1)) 

(define-object-kind "object-93" "& pair~ of metal shod boots"
  :numeric-id 93
  :x-attr #\s
  :x-char #\]
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 80
  :cost 50
  :obj-type '(<metal-shod> <boots>)
  :sort-value 3306
  :the-kind '<boots>
  :game-values (make-instance 'game-values :base-ac 6 :base-dice 1 :num-dice 1)) 

(define-object-kind "object-94" "& hard leather cap~"
  :numeric-id 94
  :x-attr #\u
  :x-char #\]
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 15
  :cost 12
  :obj-type '(<hard-leather> <headgear> <helmet>)
  :sort-value 3502
  :the-kind '<headgear>
  :game-values (make-instance 'game-values :base-ac 2)) 

(define-object-kind "object-95" "& metal cap~"
  :numeric-id 95
  :x-attr #\s
  :x-char #\]
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 20
  :cost 30
  :obj-type '(<metal-cap> <headgear> <helmet>)
  :sort-value 3503
  :the-kind '<headgear>
  :game-values (make-instance 'game-values :base-ac 3 :base-dice 1 :num-dice 1)) 

(define-object-kind "object-96" "& iron helm~"
  :numeric-id 96
  :x-attr #\s
  :x-char #\]
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 75
  :cost 75
  :obj-type '(<iron-helm> <headgear> <helmet>)
  :sort-value 3505
  :the-kind '<headgear>
  :game-values (make-instance 'game-values :base-ac 5 :base-dice 3 :num-dice 1)) 

(define-object-kind "object-97" "& steel helm~"
  :numeric-id 97
  :x-attr #\W
  :x-char #\]
  :level 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 60
  :cost 200
  :obj-type '(<steel-helm> <headgear> <helmet>)
  :sort-value 3506
  :the-kind '<headgear>
  :game-values (make-instance 'game-values :base-ac 6 :base-dice 3 :num-dice 1)) 

(define-object-kind "object-98" "& iron crown~"
  :numeric-id 98
  :x-attr #\s
  :x-char #\]
  :level 45
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(45 0 0 0)
  :weight 20
  :cost 500
  :obj-type '(<iron> <headgear> <crown>)
  :sort-value 3610
  :the-kind '<headgear>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-99" "& golden crown~"
  :numeric-id 99
  :x-attr #\y
  :x-char #\]
  :level 45
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(45 0 0 0)
  :weight 30
  :cost 1000
  :obj-type '(<golden> <headgear> <crown>)
  :sort-value 3611
  :the-kind '<headgear>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<acid>))) 

(define-object-kind "object-100" "& jewel encrusted crown~"
  :numeric-id 100
  :x-attr #\v
  :x-char #\]
  :level 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 40
  :cost 2000
  :obj-type '(<jeweled> <headgear> <crown>)
  :sort-value 3612
  :the-kind '<headgear>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<acid>))) 

(define-object-kind "object-101" "& robe~"
  :numeric-id 101
  :x-attr #\b
  :x-char #\(
  :level 1
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(1 50 0 0)
  :weight 20
  :cost 4
  :obj-type '(<robe> <body-armour> <soft>)
  :sort-value 3902
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 2)) 

(define-object-kind "object-102" "& filthy rag~"
  :numeric-id 102
  :x-attr #\D
  :x-char #\(
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 20
  :cost 1
  :obj-type '(<filthy-rag> <body-armour> <soft>)
  :sort-value 3901
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 1 :ac-bonus -1)) 

(define-object-kind "object-103" "soft leather armour~"
  :numeric-id 103
  :x-attr #\U
  :x-char #\(
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 80
  :cost 18
  :obj-type '(<soft-leather> <body-armour> <soft>)
  :sort-value 3904
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 4)) 

(define-object-kind "object-104" "soft studded leather~"
  :numeric-id 104
  :x-attr #\U
  :x-char #\(
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 90
  :cost 35
  :obj-type '(<soft-studded> <body-armour> <soft>)
  :sort-value 3905
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 5 :base-dice 1 :num-dice 1)) 

(define-object-kind "object-105" "hard leather armour~"
  :numeric-id 105
  :x-attr #\U
  :x-char #\(
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 100
  :cost 150
  :obj-type '(<hard-leather> <body-armour> <soft>)
  :sort-value 3906
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 6 :base-dice 1 :num-dice 1 :tohit-bonus -1)) 

(define-object-kind "object-106" "hard studded leather~"
  :numeric-id 106
  :x-attr #\U
  :x-char #\(
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 110
  :cost 200
  :obj-type '(<hard-studded> <body-armour> <soft>)
  :sort-value 3907
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 7 :base-dice 2 :num-dice 1 :tohit-bonus -1)) 

(define-object-kind "object-107" "leather scale mail~"
  :numeric-id 107
  :x-attr #\U
  :x-char #\(
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 140
  :cost 450
  :obj-type '(<leather-scale> <body-armour> <soft>)
  :sort-value 3911
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 11 :base-dice 1 :num-dice 1 :tohit-bonus -1)) 

(define-object-kind "object-108" "metal scale mail~"
  :numeric-id 108
  :x-attr #\s
  :x-char #\[
  :level 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 250
  :cost 550
  :obj-type '(<metal-scale> <body-armour> <hard>)
  :sort-value 4003
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 13 :base-dice 4 :num-dice 1 :tohit-bonus -2)) 

(define-object-kind "object-109" "chain mail~"
  :numeric-id 109
  :x-attr #\s
  :x-char #\[
  :level 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 220
  :cost 750
  :obj-type '(<chain> <body-armour> <hard>)
  :sort-value 4004
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 14 :base-dice 4 :num-dice 1 :tohit-bonus -2)) 

(define-object-kind "object-110" "rusty chain mail~"
  :numeric-id 110
  :x-attr #\r
  :x-char #\[
  :level 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 200
  :cost 550
  :obj-type '(<rusty-chain> <body-armour> <hard>)
  :sort-value 4001
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 14 :ac-bonus -8 :base-dice 4 :num-dice 1 :tohit-bonus -5)) 

(define-object-kind "object-111" "augmented chain mail~"
  :numeric-id 111
  :x-attr #\s
  :x-char #\[
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 270
  :cost 900
  :obj-type '(<augmented-chain> <body-armour> <hard>)
  :sort-value 4006
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 16 :base-dice 4 :num-dice 1 :tohit-bonus -2)) 

(define-object-kind "object-112" "bar chain mail~"
  :numeric-id 112
  :x-attr #\s
  :x-char #\[
  :level 35
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(35 0 0 0)
  :weight 280
  :cost 950
  :obj-type '(<bar-chain> <body-armour> <hard>)
  :sort-value 4008
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 18 :base-dice 4 :num-dice 1 :tohit-bonus -2)) 

(define-object-kind "object-113" "metal brigandine armour~"
  :numeric-id 113
  :x-attr #\s
  :x-char #\[
  :level 35
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(35 0 0 0)
  :weight 290
  :cost 1100
  :obj-type '(<metal-brigandine> <body-armour> <hard>)
  :sort-value 4009
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 19 :base-dice 4 :num-dice 1 :tohit-bonus -3)) 

(define-object-kind "object-114" "partial plate armour~"
  :numeric-id 114
  :x-attr #\W
  :x-char #\[
  :level 45
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(45 0 0 0)
  :weight 260
  :cost 1200
  :obj-type '(<partial-plate> <body-armour> <hard>)
  :sort-value 4012
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 22 :base-dice 6 :num-dice 1 :tohit-bonus -3)) 

(define-object-kind "object-115" "metal lamellar armour~"
  :numeric-id 115
  :x-attr #\W
  :x-char #\[
  :level 45
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(45 0 0 0)
  :weight 340
  :cost 1250
  :obj-type '(<metal-lamellar> <body-armour> <hard>)
  :sort-value 4013
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 23 :base-dice 6 :num-dice 1 :tohit-bonus -3)) 

(define-object-kind "object-116" "full plate armour~"
  :numeric-id 116
  :x-attr #\W
  :x-char #\[
  :level 45
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(45 0 0 0)
  :weight 380
  :cost 1350
  :obj-type '(<full-plate> <body-armour> <hard>)
  :sort-value 4015
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 25 :base-dice 4 :num-dice 2 :tohit-bonus -3)) 

(define-object-kind "object-117" "ribbed plate armour~"
  :numeric-id 117
  :x-attr #\W
  :x-char #\[
  :level 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 380
  :cost 1500
  :obj-type '(<ribbed-plate> <body-armour> <hard>)
  :sort-value 4018
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 28 :base-dice 4 :num-dice 2 :tohit-bonus -3)) 

(define-object-kind "object-118" "adamantite plate mail~"
  :numeric-id 118
  :x-attr #\G
  :x-char #\[
  :level 75
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(75 0 0 0)
  :weight 420
  :cost 20000
  :obj-type '(<adamantite-plate> <body-armour> <hard>)
  :sort-value 4030
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 40 :base-dice 4 :num-dice 2 :tohit-bonus -4 :ignores '(<acid>))) 

(define-object-kind "object-119" "mithril plate mail~"
  :numeric-id 119
  :x-attr #\B
  :x-char #\[
  :level 60
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(60 0 0 0)
  :weight 300
  :cost 15000
  :obj-type '(<mithril-plate> <body-armour> <hard>)
  :sort-value 4025
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 35 :base-dice 4 :num-dice 2 :tohit-bonus -3 :ignores '(<acid>))) 

(define-object-kind "object-120" "mithril chain mail~"
  :numeric-id 120
  :x-attr #\B
  :x-char #\[
  :level 55
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(55 0 0 0)
  :weight 150
  :cost 7000
  :obj-type '(<mithril-chain> <body-armour> <hard>)
  :sort-value 4020
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 28 :base-dice 4 :num-dice 1 :tohit-bonus -1 :ignores '(<acid>))) 

(define-object-kind "object-121" "double chain mail~"
  :numeric-id 121
  :x-attr #\s
  :x-char #\[
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 250
  :cost 850
  :obj-type '(<double-chain> <body-armour> <hard>)
  :sort-value 4007
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 16 :base-dice 4 :num-dice 1 :tohit-bonus -2)) 

(define-object-kind "object-122" "& shield~ of deflection"
  :numeric-id 122
  :x-attr #\B
  :x-char #\[
  :level 70
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(70 0 0 0)
  :weight 100
  :cost 10000
  :obj-type '(<shield> <large> <metal> <deflection>)
  :sort-value 3710
  :the-kind '<shield>
  :game-values (make-instance 'game-values :base-ac 10 :ac-bonus 10 :base-dice 1 :num-dice 1 :ignores '(<acid>))) 

(define-object-kind "object-123" "& cloak~"
  :numeric-id 123
  :x-attr #\g
  :x-char #\(
  :level 1
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(1 20 0 0)
  :weight 10
  :cost 3
  :obj-type '(<cloth> <cloak>)
  :sort-value 3801
  :the-kind '<cloak>
  :game-values (make-instance 'game-values :base-ac 1)) 

(define-object-kind "object-124" "& shadow cloak~"
  :numeric-id 124
  :x-attr #\D
  :x-char #\(
  :level 60
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(60 0 0 0)
  :weight 5
  :cost 4000
  :obj-type '(<shadow> <cloak>)
  :sort-value 3806
  :the-kind '<cloak>
  :game-values (make-instance 'game-values :base-ac 6 :ac-bonus 4)) 

(define-object-kind "object-125" "& set~ of leather gloves"
  :numeric-id 125
  :x-attr #\U
  :x-char #\]
  :level 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 5
  :cost 3
  :obj-type '(<leather> <gloves>)
  :sort-value 3401
  :the-kind '<gloves>
  :game-values (make-instance 'game-values :base-ac 1)) 

(define-object-kind "object-126" "& set~ of gauntlets"
  :numeric-id 126
  :x-attr #\U
  :x-char #\]
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 25
  :cost 35
  :obj-type '(<gauntlets> <gloves>)
  :sort-value 3402
  :the-kind '<gloves>
  :game-values (make-instance 'game-values :base-ac 2 :base-dice 1 :num-dice 1)) 

(define-object-kind "object-127" "& set~ of cesti"
  :numeric-id 127
  :x-attr #\W
  :x-char #\]
  :level 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 40
  :cost 100
  :obj-type '(<cesti> <gloves>)
  :sort-value 3405
  :the-kind '<gloves>
  :game-values (make-instance 'game-values :base-ac 5 :base-dice 1 :num-dice 1)) 

(define-object-kind "object-128" "& small leather shield~"
  :numeric-id 128
  :x-attr #\U
  :x-char #\)
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 50
  :cost 30
  :obj-type '(<shield> <small> <leather>)
  :sort-value 3702
  :the-kind '<shield>
  :game-values (make-instance 'game-values :base-ac 2 :base-dice 1 :num-dice 1)) 

(define-object-kind "object-129" "& large leather shield~"
  :numeric-id 129
  :x-attr #\U
  :x-char #\)
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 100
  :cost 120
  :obj-type '(<shield> <large> <leather>)
  :sort-value 3704
  :the-kind '<shield>
  :game-values (make-instance 'game-values :base-ac 4 :base-dice 2 :num-dice 1)) 

(define-object-kind "object-130" "& small metal shield~"
  :numeric-id 130
  :x-attr #\s
  :x-char #\)
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 65
  :cost 50
  :obj-type '(<shield> <small> <metal>)
  :sort-value 3703
  :the-kind '<shield>
  :game-values (make-instance 'game-values :base-ac 3 :base-dice 2 :num-dice 1)) 

(define-object-kind "object-131" "& large metal shield~"
  :numeric-id 131
  :x-attr #\s
  :x-char #\)
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 120
  :cost 200
  :obj-type '(<shield> <large> <metal>)
  :sort-value 3705
  :the-kind '<shield>
  :game-values (make-instance 'game-values :base-ac 5 :base-dice 3 :num-dice 1)) 

(define-object-kind "object-132" "strength"
  :numeric-id 132
  :x-attr #\d
  :x-char #\=
  :level 30
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

(define-object-kind "object-133" "dexterity"
  :numeric-id 133
  :x-attr #\d
  :x-char #\=
  :level 30
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

(define-object-kind "object-134" "constitution"
  :numeric-id 134
  :x-attr #\d
  :x-char #\=
  :level 30
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

(define-object-kind "object-135" "intelligence"
  :numeric-id 135
  :x-attr #\d
  :x-char #\=
  :level 30
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

(define-object-kind "object-136" "speed"
  :numeric-id 136
  :x-attr #\d
  :x-char #\=
  :level 80
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

(define-object-kind "object-137" "searching"
  :numeric-id 137
  :x-attr #\d
  :x-char #\=
  :level 5
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
  :level 5
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

(define-object-kind "object-139" "slow digestion"
  :numeric-id 139
  :x-attr #\d
  :x-char #\=
  :level 5
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
  :level 10
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
  :level 10
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

(define-object-kind "object-142" "feather falling"
  :numeric-id 142
  :x-attr #\d
  :x-char #\=
  :level 5
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
  :level 40
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
  :level 20
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
  :level 5
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
  :level 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 2
  :cost 3000
  :obj-type '(<ring> <protection> <fire>)
  :sort-value 4418
  :the-kind '<ring>
  :game-values (make-instance 'game-values :ac-bonus 15 :ignores '(<fire>) :resists '(<fire>))) 

(define-object-kind "object-147" "acid"
  :numeric-id 147
  :x-attr #\d
  :x-char #\=
  :level 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 2
  :cost 3000
  :obj-type '(<ring> <protection> <acid>)
  :sort-value 4417
  :the-kind '<ring>
  :game-values (make-instance 'game-values :ac-bonus 15 :ignores '(<acid>) :resists '(<acid>))) 

(define-object-kind "object-148" "ice"
  :numeric-id 148
  :x-attr #\d
  :x-char #\=
  :level 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 2
  :cost 3000
  :obj-type '(<ring> <protection> <cold>)
  :sort-value 4419
  :the-kind '<ring>
  :game-values (make-instance 'game-values :ac-bonus 15 :ignores '(<cold>) :resists '(<cold>))) 

(define-object-kind "object-149" "woe"
  :numeric-id 149
  :x-attr #\d
  :x-char #\=
  :level 50
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
  :level 5
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

(define-object-kind "object-151" "damage"
  :numeric-id 151
  :x-attr #\d
  :x-char #\=
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 2
  :cost 500
  :obj-type '(<damage> <ring>)
  :sort-value 4429
  :the-kind '<ring>) 

(define-object-kind "object-152" "accuracy"
  :numeric-id 152
  :x-attr #\d
  :x-char #\=
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 2
  :cost 500
  :obj-type '(<accuracy> <ring>)
  :sort-value 4428
  :the-kind '<ring>) 

(define-object-kind "object-153" "protection"
  :numeric-id 153
  :x-attr #\d
  :x-char #\=
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 2
  :cost 500
  :obj-type '(<protection> <ring>)
  :sort-value 4416
  :the-kind '<ring>) 

(define-object-kind "object-154" "aggravate monster"
  :numeric-id 154
  :x-attr #\d
  :x-char #\=
  :level 5
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
  :level 30
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
  :level 30
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
  :level 30
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
  :level 30
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
  :level 30
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
  :level 30
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
  :level 30
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

(define-object-kind "object-162" "slaying"
  :numeric-id 162
  :x-attr #\d
  :x-char #\=
  :level 40
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

(define-object-kind "object-163" "wisdom"
  :numeric-id 163
  :x-attr #\d
  :x-char #\"
  :level 20
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

(define-object-kind "object-164" "charisma"
  :numeric-id 164
  :x-attr #\d
  :x-char #\"
  :level 20
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

(define-object-kind "object-165" "searching"
  :numeric-id 165
  :x-attr #\d
  :x-char #\"
  :level 30
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

(define-object-kind "object-166" "teleportation"
  :numeric-id 166
  :x-attr #\d
  :x-char #\"
  :level 15
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

(define-object-kind "object-167" "slow digestion"
  :numeric-id 167
  :x-attr #\d
  :x-char #\"
  :level 15
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

(define-object-kind "object-168" "resist acid"
  :numeric-id 168
  :x-attr #\d
  :x-char #\"
  :level 20
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

(define-object-kind "object-169" "adornment"
  :numeric-id 169
  :x-attr #\d
  :x-char #\"
  :level 15
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

(define-object-kind "object-171" "the magi"
  :numeric-id 171
  :x-attr #\d
  :x-char #\"
  :level 50
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

(define-object-kind "object-172" "doom"
  :numeric-id 172
  :x-attr #\d
  :x-char #\"
  :level 50
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

(define-object-kind "object-173" "enchant weapon to-hit"
  :numeric-id 173
  :x-attr #\d
  :x-char #\?
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 5
  :cost 125
  :obj-type '(<scroll> <enchant> <weapon> <to-hit>)
  :sort-value 5017
  :the-kind '<scroll>) 

(define-object-kind "object-174" "enchant weapon to-dam"
  :numeric-id 174
  :x-attr #\d
  :x-char #\?
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 5
  :cost 125
  :obj-type '(<scroll> <enchant> <weapon> <to-dmg>)
  :sort-value 5018
  :the-kind '<scroll>) 

(define-object-kind "object-175" "enchant armor"
  :numeric-id 175
  :x-attr #\d
  :x-char #\?
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 5
  :cost 125
  :obj-type '(<scroll> <enchant> <armour> <normal>)
  :sort-value 5016
  :the-kind '<scroll>) 

(define-object-kind "object-176" "identify"
  :numeric-id 176
  :x-attr #\d
  :x-char #\?
  :level 1
  :rarity 0
  :chance #(1 1 1 1)
  :locale #(1 5 10 30)
  :weight 5
  :cost 50
  :obj-type '(<scroll> <identify> <normal>)
  :sort-value 5012
  :the-kind '<scroll>) 

(define-object-kind "object-177" "*identify*"
  :numeric-id 177
  :x-attr #\d
  :x-char #\?
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 5
  :cost 1000
  :obj-type '(<scroll> <identify> <powerful>)
  :sort-value 5013
  :the-kind '<scroll>) 

(define-object-kind "object-180" "remove curse"
  :numeric-id 180
  :x-attr #\d
  :x-char #\?
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 5
  :cost 100
  :obj-type '(<scroll> <remove-curse> <normal>)
  :sort-value 5014
  :the-kind '<scroll>) 

(define-object-kind "object-181" "light"
  :numeric-id 181
  :x-attr #\d
  :x-char #\?
  :level 0
  :rarity 0
  :chance #(1 1 1 0)
  :locale #(0 3 10 0)
  :weight 5
  :cost 15
  :obj-type '(<illuminate> <scroll>)
  :sort-value 5024
  :the-kind '<scroll>) 

(define-object-kind "object-184" "summon monster"
  :numeric-id 184
  :x-attr #\d
  :x-char #\?
  :level 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 5
  :cost 0
  :obj-type '(<scroll> <summon> <monster>)
  :sort-value 5004
  :the-kind '<scroll>) 

(define-object-kind "object-185" "phase door"
  :numeric-id 185
  :x-attr #\d
  :x-char #\?
  :level 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 5
  :cost 15
  :obj-type '(<phase-door> <scroll>)
  :sort-value 5008
  :the-kind '<scroll>) 

(define-object-kind "object-186" "teleportation"
  :numeric-id 186
  :x-attr #\d
  :x-char #\?
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 5
  :cost 40
  :obj-type '(<teleportation> <scroll>)
  :sort-value 5009
  :the-kind '<scroll>) 

(define-object-kind "object-187" "teleport level"
  :numeric-id 187
  :x-attr #\d
  :x-char #\?
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 5
  :cost 50
  :obj-type '(<teleport-level> <scroll>)
  :sort-value 5010
  :the-kind '<scroll>) 

(define-object-kind "object-188" "monster confusion"
  :numeric-id 188
  :x-attr #\d
  :x-char #\?
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 5
  :cost 30
  :obj-type '(<confuse-monster> <scroll>)
  :sort-value 5036
  :the-kind '<scroll>) 

(define-object-kind "object-189" "magic mapping"
  :numeric-id 189
  :x-attr #\d
  :x-char #\?
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 5
  :cost 40
  :obj-type '(<mapping> <scroll>)
  :sort-value 5025
  :the-kind '<scroll>) 

(define-object-kind "object-190" "rune of protection"
  :numeric-id 190
  :x-attr #\d
  :x-char #\?
  :level 60
  :rarity 0
  :chance #(2 4 0 0)
  :locale #(60 90 0 0)
  :weight 5
  :cost 500
  :obj-type '(<scroll> <protection> <rune>)
  :sort-value 5038
  :the-kind '<scroll>) 

(define-object-kind "object-191" "*remove curse*"
  :numeric-id 191
  :x-attr #\d
  :x-char #\?
  :level 50
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(50 0 0 0)
  :weight 5
  :cost 8000
  :obj-type '(<scroll> <remove-curse> <powerful>)
  :sort-value 5015
  :the-kind '<scroll>) 

(define-object-kind "object-192" "treasure detection"
  :numeric-id 192
  :x-attr #\d
  :x-char #\?
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 5
  :cost 15
  :obj-type '(<scroll> <detect> <money>)
  :sort-value 5026
  :the-kind '<scroll>) 

(define-object-kind "object-193" "object detection"
  :numeric-id 193
  :x-attr #\d
  :x-char #\?
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 5
  :cost 15
  :obj-type '(<scroll> <detect> <item>)
  :sort-value 5027
  :the-kind '<scroll>) 

(define-object-kind "object-194" "trap detection"
  :numeric-id 194
  :x-attr #\d
  :x-char #\?
  :level 5
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(5 10 0 0)
  :weight 5
  :cost 35
  :obj-type '(<scroll> <detect> <trap>)
  :sort-value 5028
  :the-kind '<scroll>) 

(define-object-kind "object-197" "door/stair location"
  :numeric-id 197
  :x-attr #\d
  :x-char #\?
  :level 5
  :rarity 0
  :chance #(1 1 1 0)
  :locale #(5 10 15 0)
  :weight 5
  :cost 35
  :obj-type '(<scroll> <detect> <door>)
  :sort-value 5029
  :the-kind '<scroll>) 

(define-object-kind "object-198" "acquirement"
  :numeric-id 198
  :x-attr #\d
  :x-char #\?
  :level 20
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(20 0 0 0)
  :weight 5
  :cost 100000
  :obj-type '(<scroll> <acquirement> <normal>)
  :sort-value 5046
  :the-kind '<scroll>) 

(define-object-kind "object-199" "*acquirement*"
  :numeric-id 199
  :x-attr #\d
  :x-char #\?
  :level 60
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(60 0 0 0)
  :weight 5
  :cost 200000
  :obj-type '(<scroll> <acquirement> <powerful>)
  :sort-value 5047
  :the-kind '<scroll>) 

(define-object-kind "object-200" "mass genocide"
  :numeric-id 200
  :x-attr #\d
  :x-char #\?
  :level 50
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(50 0 0 0)
  :weight 5
  :cost 1000
  :obj-type '(<scroll> <genocide> <mass>)
  :sort-value 5045
  :the-kind '<scroll>) 

(define-object-kind "object-201" "detect invisible"
  :numeric-id 201
  :x-attr #\d
  :x-char #\?
  :level 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 5
  :cost 15
  :obj-type '(<scroll> <detect> <invisible>)
  :sort-value 5030
  :the-kind '<scroll>) 

(define-object-kind "object-202" "aggravate monster"
  :numeric-id 202
  :x-attr #\d
  :x-char #\?
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 5
  :cost 0
  :obj-type '(<aggravate> <scroll>)
  :sort-value 5001
  :the-kind '<scroll>) 

(define-object-kind "object-203" "trap creation"
  :numeric-id 203
  :x-attr #\d
  :x-char #\?
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 5
  :cost 0
  :obj-type '(<create-trap> <scroll>)
  :sort-value 5007
  :the-kind '<scroll>) 

(define-object-kind "object-204" "trap/door destruction"
  :numeric-id 204
  :x-attr #\d
  :x-char #\?
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 5
  :cost 50
  :obj-type '(<trap/door-destruction> <scroll>)
  :sort-value 5039
  :the-kind '<scroll>) 

(define-object-kind "object-206" "recharging"
  :numeric-id 206
  :x-attr #\d
  :x-char #\?
  :level 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 5
  :cost 200
  :obj-type '(<recharge> <scroll>)
  :sort-value 5022
  :the-kind '<scroll>) 

(define-object-kind "object-207" "genocide"
  :numeric-id 207
  :x-attr #\d
  :x-char #\?
  :level 40
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(40 0 0 0)
  :weight 5
  :cost 750
  :obj-type '(<genocide> <scroll>)
  :sort-value 5044
  :the-kind '<scroll>) 

(define-object-kind "object-208" "darkness"
  :numeric-id 208
  :x-attr #\d
  :x-char #\?
  :level 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 5
  :cost 0
  :obj-type '(<darkness> <scroll>)
  :sort-value 5000
  :the-kind '<scroll>) 

(define-object-kind "object-209" "protection from evil"
  :numeric-id 209
  :x-attr #\d
  :x-char #\?
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 5
  :cost 50
  :obj-type '(<scroll> <protection> <evil>)
  :sort-value 5037
  :the-kind '<scroll>) 

(define-object-kind "object-210" "satisfy hunger"
  :numeric-id 210
  :x-attr #\d
  :x-char #\?
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 5
  :cost 10
  :obj-type '(<satisfy-hunger> <scroll>)
  :sort-value 5032
  :the-kind '<scroll>) 

(define-object-kind "object-211" "dispel undead"
  :numeric-id 211
  :x-attr #\d
  :x-char #\?
  :level 40
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
  :level 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 5
  :cost 500
  :obj-type '(<scroll> <enchant> <weapon> <powerful>)
  :sort-value 5021
  :the-kind '<scroll>) 

(define-object-kind "object-213" "curse weapon"
  :numeric-id 213
  :x-attr #\d
  :x-char #\?
  :level 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 5
  :cost 0
  :obj-type '(<curse-weapon> <scroll>)
  :sort-value 5003
  :the-kind '<scroll>) 

(define-object-kind "object-214" "*enchant armor*"
  :numeric-id 214
  :x-attr #\d
  :x-char #\?
  :level 50
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(50 50 0 0)
  :weight 5
  :cost 500
  :obj-type '(<scroll> <enchant> <armour> <powerful>)
  :sort-value 5020
  :the-kind '<scroll>) 

(define-object-kind "object-215" "curse armor"
  :numeric-id 215
  :x-attr #\d
  :x-char #\?
  :level 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 5
  :cost 0
  :obj-type '(<curse-armour> <scroll>)
  :sort-value 5002
  :the-kind '<scroll>) 

(define-object-kind "object-216" "summon undead"
  :numeric-id 216
  :x-attr #\d
  :x-char #\?
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 5
  :cost 0
  :obj-type '(<scroll> <summon> <undead>)
  :sort-value 5005
  :the-kind '<scroll>) 

(define-object-kind "object-217" "blessing"
  :numeric-id 217
  :x-attr #\d
  :x-char #\?
  :level 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 5
  :cost 15
  :obj-type '(<scroll> <blessing> <light>)
  :sort-value 5033
  :the-kind '<scroll>) 

(define-object-kind "object-218" "holy chant"
  :numeric-id 218
  :x-attr #\d
  :x-char #\?
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 5
  :cost 40
  :obj-type '(<scroll> <blessing> <chant>)
  :sort-value 5034
  :the-kind '<scroll>) 

(define-object-kind "object-219" "holy prayer"
  :numeric-id 219
  :x-attr #\d
  :x-char #\?
  :level 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 5
  :cost 80
  :obj-type '(<scroll> <blessing> <prayer>)
  :sort-value 5035
  :the-kind '<scroll>) 

(define-object-kind "object-220" "word of recall"
  :numeric-id 220
  :x-attr #\d
  :x-char #\?
  :level 5
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
  :level 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 5
  :cost 250
  :obj-type '(<scroll> <destruction> <powerful>)
  :sort-value 5041
  :the-kind '<scroll>) 

(define-object-kind "object-222" "slime mold juice"
  :numeric-id 222
  :x-attr #\d
  :x-char #\!
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 4
  :cost 2
  :obj-type '(<slime-mold> <potion>)
  :sort-value 5502
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :food-val 400)) 

(define-object-kind "object-223" "apple juice"
  :numeric-id 223
  :x-attr #\d
  :x-char #\!
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 4
  :cost 1
  :obj-type '(<apple-juice> <potion>)
  :sort-value 5501
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :food-val 250)) 

(define-object-kind "object-224" "water"
  :numeric-id 224
  :x-attr #\d
  :x-char #\!
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 4
  :cost 1
  :obj-type '(<water> <potion>)
  :sort-value 5500
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :food-val 200)) 

(define-object-kind "object-225" "strength"
  :numeric-id 225
  :x-attr #\d
  :x-char #\!
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 4
  :cost 8000
  :obj-type '(<potion> <increase> <str>)
  :sort-value 5548
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-226" "weakness"
  :numeric-id 226
  :x-attr #\d
  :x-char #\!
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<potion> <reduce> <str>)
  :sort-value 5516
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-227" "restore strength"
  :numeric-id 227
  :x-attr #\d
  :x-char #\!
  :level 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 4
  :cost 300
  :obj-type '(<potion> <restore> <str>)
  :sort-value 5542
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-228" "intelligence"
  :numeric-id 228
  :x-attr #\d
  :x-char #\!
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 4
  :cost 8000
  :obj-type '(<potion> <increase> <int>)
  :sort-value 5549
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-229" "stupidity"
  :numeric-id 229
  :x-attr #\d
  :x-char #\!
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<potion> <reduce> <int>)
  :sort-value 5517
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-230" "restore intelligence"
  :numeric-id 230
  :x-attr #\d
  :x-char #\!
  :level 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 4
  :cost 300
  :obj-type '(<potion> <restore> <int>)
  :sort-value 5543
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-231" "wisdom"
  :numeric-id 231
  :x-attr #\d
  :x-char #\!
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 4
  :cost 8000
  :obj-type '(<potion> <increase> <wis>)
  :sort-value 5550
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-232" "naivety"
  :numeric-id 232
  :x-attr #\d
  :x-char #\!
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<potion> <reduce> <wis>)
  :sort-value 5518
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-233" "restore wisdom"
  :numeric-id 233
  :x-attr #\d
  :x-char #\!
  :level 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 4
  :cost 300
  :obj-type '(<potion> <restore> <wis>)
  :sort-value 5544
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-234" "charisma"
  :numeric-id 234
  :x-attr #\d
  :x-char #\!
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 4
  :cost 1000
  :obj-type '(<potion> <increase> <chr>)
  :sort-value 5553
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-235" "ugliness"
  :numeric-id 235
  :x-attr #\d
  :x-char #\!
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<potion> <reduce> <chr>)
  :sort-value 5521
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-236" "restore charisma"
  :numeric-id 236
  :x-attr #\d
  :x-char #\!
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 4
  :cost 300
  :obj-type '(<potion> <restore> <chr>)
  :sort-value 5547
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-237" "cure light wounds"
  :numeric-id 237
  :x-attr #\d
  :x-char #\!
  :level 0
  :rarity 0
  :chance #(1 1 1 0)
  :locale #(0 1 3 0)
  :weight 4
  :cost 15
  :obj-type '(<potion> <cure> <light>)
  :sort-value 5534
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :food-val 50)) 

(define-object-kind "object-238" "clumsiness"
  :numeric-id 238
  :x-attr #\d
  :x-char #\!
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<potion> <reduce> <dex>)
  :sort-value 5519
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-239" "sickliness"
  :numeric-id 239
  :x-attr #\d
  :x-char #\!
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<potion> <reduce> <con>)
  :sort-value 5520
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-240" "cure serious wounds"
  :numeric-id 240
  :x-attr #\d
  :x-char #\!
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 4
  :cost 40
  :obj-type '(<potion> <cure> <serious>)
  :sort-value 5535
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :food-val 100)) 

(define-object-kind "object-241" "cure critical wounds"
  :numeric-id 241
  :x-attr #\d
  :x-char #\!
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 4
  :cost 100
  :obj-type '(<potion> <cure> <critical>)
  :sort-value 5536
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :food-val 100)) 

(define-object-kind "object-242" "healing"
  :numeric-id 242
  :x-attr #\d
  :x-char #\!
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 4
  :cost 300
  :obj-type '(<potion> <cure> <healing> <normal>)
  :sort-value 5537
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :food-val 200)) 

(define-object-kind "object-243" "constitution"
  :numeric-id 243
  :x-attr #\d
  :x-char #\!
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 4
  :cost 8000
  :obj-type '(<potion> <increase> <con>)
  :sort-value 5552
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-244" "experience"
  :numeric-id 244
  :x-attr #\d
  :x-char #\!
  :level 65
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(65 0 0 0)
  :weight 4
  :cost 25000
  :obj-type '(<xp> <potion>)
  :sort-value 5559
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-245" "sleep"
  :numeric-id 245
  :x-attr #\d
  :x-char #\!
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<sleep> <potion>)
  :sort-value 5511
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :food-val 100)) 

(define-object-kind "object-246" "blindness"
  :numeric-id 246
  :x-attr #\d
  :x-char #\!
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<blindness> <potion>)
  :sort-value 5507
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-247" "confusion"
  :numeric-id 247
  :x-attr #\d
  :x-char #\!
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<confusion> <potion>)
  :sort-value 5509
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :food-val 50)) 

(define-object-kind "object-248" "poison"
  :numeric-id 248
  :x-attr #\d
  :x-char #\!
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<poison> <potion>)
  :sort-value 5506
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-249" "speed"
  :numeric-id 249
  :x-attr #\d
  :x-char #\!
  :level 1
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(1 40 0 0)
  :weight 4
  :cost 75
  :obj-type '(<speed> <potion>)
  :sort-value 5529
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-250" "slowness"
  :numeric-id 250
  :x-attr #\d
  :x-char #\!
  :level 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<slowness> <potion>)
  :sort-value 5504
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :food-val 50)) 

(define-object-kind "object-251" "dexterity"
  :numeric-id 251
  :x-attr #\d
  :x-char #\!
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 4
  :cost 8000
  :obj-type '(<potion> <increase> <dex>)
  :sort-value 5551
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-252" "restore dexterity"
  :numeric-id 252
  :x-attr #\d
  :x-char #\!
  :level 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 4
  :cost 300
  :obj-type '(<potion> <restore> <dex>)
  :sort-value 5545
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-253" "restore constitution"
  :numeric-id 253
  :x-attr #\d
  :x-char #\!
  :level 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 4
  :cost 300
  :obj-type '(<potion> <restore> <con>)
  :sort-value 5546
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-254" "lose memories"
  :numeric-id 254
  :x-attr #\d
  :x-char #\!
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<amnesia> <potion>)
  :sort-value 5513
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-255" "salt water"
  :numeric-id 255
  :x-attr #\d
  :x-char #\!
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<salt-water> <potion>)
  :sort-value 5505
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-256" "enlightenment"
  :numeric-id 256
  :x-attr #\d
  :x-char #\!
  :level 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 4
  :cost 800
  :obj-type '(<potion> <enlightenment> <normal>)
  :sort-value 5556
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-257" "heroism"
  :numeric-id 257
  :x-attr #\d
  :x-char #\!
  :level 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 4
  :cost 35
  :obj-type '(<heroism> <potion>)
  :sort-value 5532
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-258" "berserk strength"
  :numeric-id 258
  :x-attr #\d
  :x-char #\!
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 4
  :cost 100
  :obj-type '(<berserk-strength> <potion>)
  :sort-value 5533
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-259" "boldness"
  :numeric-id 259
  :x-attr #\d
  :x-char #\!
  :level 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 4
  :cost 10
  :obj-type '(<boldness> <potion>)
  :sort-value 5528
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-260" "restore life levels"
  :numeric-id 260
  :x-attr #\d
  :x-char #\!
  :level 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 4
  :cost 400
  :obj-type '(<potion> <restore> <xp>)
  :sort-value 5541
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-261" "resist heat"
  :numeric-id 261
  :x-attr #\d
  :x-char #\!
  :level 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 4
  :cost 30
  :obj-type '(<potion> <resist> <fire>)
  :sort-value 5530
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-262" "resist cold"
  :numeric-id 262
  :x-attr #\d
  :x-char #\!
  :level 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 4
  :cost 30
  :obj-type '(<potion> <resist> <cold>)
  :sort-value 5531
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-263" "detect invisible"
  :numeric-id 263
  :x-attr #\d
  :x-char #\!
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 4
  :cost 50
  :obj-type '(<potion> <detect> <invisible>)
  :sort-value 5525
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-264" "slow poison"
  :numeric-id 264
  :x-attr #\d
  :x-char #\!
  :level 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 4
  :cost 25
  :obj-type '(<slow-poison> <potion>)
  :sort-value 5526
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-265" "neutralize poison"
  :numeric-id 265
  :x-attr #\d
  :x-char #\!
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 4
  :cost 75
  :obj-type '(<potion> <cure> <poison>)
  :sort-value 5527
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-266" "restore mana"
  :numeric-id 266
  :x-attr #\d
  :x-char #\!
  :level 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 4
  :cost 350
  :obj-type '(<potion> <restore> <mana>)
  :sort-value 5540
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-267" "infra-vision"
  :numeric-id 267
  :x-attr #\d
  :x-char #\!
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 4
  :cost 20
  :obj-type '(<infravision> <potion>)
  :sort-value 5524
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-269" "light"
  :numeric-id 269
  :x-attr #\d
  :x-char #\-
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 10
  :cost 200
  :obj-type '(<light> <wand>)
  :sort-value 4807
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-270" "lightning bolts"
  :numeric-id 270
  :x-attr #\d
  :x-char #\-
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 10
  :cost 600
  :obj-type '(<wand> <bolt> <lightning>)
  :sort-value 4817
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-271" "frost bolts"
  :numeric-id 271
  :x-attr #\d
  :x-char #\-
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 10
  :cost 800
  :obj-type '(<wand> <bolt> <cold>)
  :sort-value 4819
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-272" "fire bolts"
  :numeric-id 272
  :x-attr #\d
  :x-char #\-
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 10
  :cost 1000
  :obj-type '(<wand> <bolt> <fire>)
  :sort-value 4818
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-273" "stone to mud"
  :numeric-id 273
  :x-attr #\d
  :x-char #\-
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 10
  :cost 300
  :obj-type '(<stone-to-mud> <wand>)
  :sort-value 4806
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-274" "polymorph"
  :numeric-id 274
  :x-attr #\d
  :x-char #\-
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 10
  :cost 400
  :obj-type '(<polymorph> <wand>)
  :sort-value 4813
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-275" "heal monster"
  :numeric-id 275
  :x-attr #\d
  :x-char #\-
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 10
  :cost 0
  :obj-type '(<heal-monster> <wand>)
  :sort-value 4800
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-276" "haste monster"
  :numeric-id 276
  :x-attr #\d
  :x-char #\-
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 10
  :cost 0
  :obj-type '(<haste-monster> <wand>)
  :sort-value 4801
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-277" "slow monster"
  :numeric-id 277
  :x-attr #\d
  :x-char #\-
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 10
  :cost 500
  :obj-type '(<slow-monster> <wand>)
  :sort-value 4809
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-278" "confuse monster"
  :numeric-id 278
  :x-attr #\d
  :x-char #\-
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 10
  :cost 500
  :obj-type '(<confuse-monster> <wand>)
  :sort-value 4810
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-279" "sleep monster"
  :numeric-id 279
  :x-attr #\d
  :x-char #\-
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 10
  :cost 500
  :obj-type '(<sleep-monster> <wand>)
  :sort-value 4808
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-280" "drain life"
  :numeric-id 280
  :x-attr #\d
  :x-char #\-
  :level 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 10
  :cost 1200
  :obj-type '(<drain-life> <wand>)
  :sort-value 4812
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-281" "trap/door destruction"
  :numeric-id 281
  :x-attr #\d
  :x-char #\-
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 10
  :cost 100
  :obj-type '(<trap/door-destruction> <wand>)
  :sort-value 4805
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-282" "magic missile"
  :numeric-id 282
  :x-attr #\d
  :x-char #\-
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 10
  :cost 200
  :obj-type '(<magic-missile> <wand>)
  :sort-value 4815
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-283" "clone monster"
  :numeric-id 283
  :x-attr #\d
  :x-char #\-
  :level 15
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(15 50 0 0)
  :weight 10
  :cost 0
  :obj-type '(<clone-monster> <wand>)
  :sort-value 4802
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-284" "scare monster"
  :numeric-id 284
  :x-attr #\d
  :x-char #\-
  :level 10
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(10 0 0 0)
  :weight 10
  :cost 500
  :obj-type '(<fear-monster> <wand>)
  :sort-value 4811
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-285" "teleport other"
  :numeric-id 285
  :x-attr #\d
  :x-char #\-
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 10
  :cost 350
  :obj-type '(<teleport-away> <wand>)
  :sort-value 4803
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-286" "disarming"
  :numeric-id 286
  :x-attr #\d
  :x-char #\-
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 10
  :cost 700
  :obj-type '(<disarm> <wand>)
  :sort-value 4804
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-287" "lightning balls"
  :numeric-id 287
  :x-attr #\d
  :x-char #\-
  :level 35
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(35 0 0 0)
  :weight 10
  :cost 1200
  :obj-type '(<wand> <ball> <lightning>)
  :sort-value 4821
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<electricity>))) 

(define-object-kind "object-288" "cold balls"
  :numeric-id 288
  :x-attr #\d
  :x-char #\-
  :level 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 10
  :cost 1500
  :obj-type '(<wand> <ball> <cold>)
  :sort-value 4823
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<cold>))) 

(define-object-kind "object-289" "fire balls"
  :numeric-id 289
  :x-attr #\d
  :x-char #\-
  :level 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 10
  :cost 1800
  :obj-type '(<wand> <ball> <fire>)
  :sort-value 4822
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<fire>))) 

(define-object-kind "object-290" "stinking cloud"
  :numeric-id 290
  :x-attr #\d
  :x-char #\-
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 10
  :cost 400
  :obj-type '(<stinking-cloud> <wand>)
  :sort-value 4814
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-291" "acid balls"
  :numeric-id 291
  :x-attr #\d
  :x-char #\-
  :level 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 10
  :cost 1650
  :obj-type '(<wand> <ball> <acid>)
  :sort-value 4820
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<acid>))) 

(define-object-kind "object-292" "wonder"
  :numeric-id 292
  :x-attr #\d
  :x-char #\-
  :level 3
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(3 0 0 0)
  :weight 10
  :cost 250
  :obj-type '(<wonder> <wand>)
  :sort-value 4824
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "object-294" "acid bolts"
  :numeric-id 294
  :x-attr #\d
  :x-char #\-
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 10
  :cost 950
  :obj-type '(<wand> <bolt> <acid>)
  :sort-value 4816
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-295" "dragon's flame"
  :numeric-id 295
  :x-attr #\d
  :x-char #\-
  :level 50
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(50 0 0 0)
  :weight 10
  :cost 2400
  :obj-type '(<wand> <dragon> <fire>)
  :sort-value 4826
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "object-296" "dragon's frost"
  :numeric-id 296
  :x-attr #\d
  :x-char #\-
  :level 50
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(50 0 0 0)
  :weight 10
  :cost 2400
  :obj-type '(<wand> <dragon> <cold>)
  :sort-value 4827
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "object-297" "dragon's breath"
  :numeric-id 297
  :x-attr #\d
  :x-char #\-
  :level 60
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(60 0 0 0)
  :weight 10
  :cost 2400
  :obj-type '(<wand> <dragon> <breath>)
  :sort-value 4828
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "object-298" "annihilation"
  :numeric-id 298
  :x-attr #\d
  :x-char #\-
  :level 60
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(60 0 0 0)
  :weight 10
  :cost 3000
  :obj-type '(<annihiliation> <wand>)
  :sort-value 4825
  :the-kind '<wand>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "object-300" "trap location"
  :numeric-id 300
  :x-attr #\d
  :x-char #\_
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 50
  :cost 350
  :obj-type '(<staff> <detect> <trap>)
  :sort-value 4612
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-301" "treasure location"
  :numeric-id 301
  :x-attr #\d
  :x-char #\_
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 50
  :cost 200
  :obj-type '(<staff> <detect> <money>)
  :sort-value 4610
  :the-kind '<money>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-302" "object location"
  :numeric-id 302
  :x-attr #\d
  :x-char #\_
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 50
  :cost 200
  :obj-type '(<staff> <detect> <item>)
  :sort-value 4611
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-303" "teleportation"
  :numeric-id 303
  :x-attr #\d
  :x-char #\_
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 50
  :cost 2000
  :obj-type '(<teleportation> <staff>)
  :sort-value 4604
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-304" "earthquakes"
  :numeric-id 304
  :x-attr #\d
  :x-char #\_
  :level 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 50
  :cost 350
  :obj-type '(<earthquake> <staff>)
  :sort-value 4628
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-305" "summoning"
  :numeric-id 305
  :x-attr #\d
  :x-char #\_
  :level 10
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(10 50 0 0)
  :weight 50
  :cost 0
  :obj-type '(<summoning> <staff>)
  :sort-value 4603
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-306" "light"
  :numeric-id 306
  :x-attr #\d
  :x-char #\_
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 50
  :cost 250
  :obj-type '(<light> <staff>)
  :sort-value 4608
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-307" "*destruction*"
  :numeric-id 307
  :x-attr #\d
  :x-char #\_
  :level 50
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(50 70 0 0)
  :weight 50
  :cost 2500
  :obj-type '(<destruction> <staff>)
  :sort-value 4629
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-308" "starlight"
  :numeric-id 308
  :x-attr #\d
  :x-char #\_
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 50
  :cost 800
  :obj-type '(<star-light> <staff>)
  :sort-value 4607
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-309" "haste monsters"
  :numeric-id 309
  :x-attr #\d
  :x-char #\_
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 50
  :cost 0
  :obj-type '(<haste-monster> <staff>)
  :sort-value 4602
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-310" "slow monsters"
  :numeric-id 310
  :x-attr #\d
  :x-char #\_
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 50
  :cost 800
  :obj-type '(<slow-monster> <staff>)
  :sort-value 4621
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-311" "sleep monsters"
  :numeric-id 311
  :x-attr #\d
  :x-char #\_
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 50
  :cost 700
  :obj-type '(<sleep-monster> <staff>)
  :sort-value 4620
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-312" "cure light wounds"
  :numeric-id 312
  :x-attr #\d
  :x-char #\_
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 50
  :cost 350
  :obj-type '(<staff> <cure> <light>)
  :sort-value 4616
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-313" "detect invisible"
  :numeric-id 313
  :x-attr #\d
  :x-char #\_
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 50
  :cost 200
  :obj-type '(<staff> <detect> <invisible>)
  :sort-value 4614
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-314" "speed"
  :numeric-id 314
  :x-attr #\d
  :x-char #\_
  :level 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 50
  :cost 1000
  :obj-type '(<speed> <staff>)
  :sort-value 4622
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-315" "slowness"
  :numeric-id 315
  :x-attr #\d
  :x-char #\_
  :level 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 50
  :cost 0
  :obj-type '(<slowness> <staff>)
  :sort-value 4601
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-316" "door/stair location"
  :numeric-id 316
  :x-attr #\d
  :x-char #\_
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 50
  :cost 350
  :obj-type '(<staff> <detect> <door>)
  :sort-value 4613
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-317" "remove curse"
  :numeric-id 317
  :x-attr #\d
  :x-char #\_
  :level 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 50
  :cost 500
  :obj-type '(<remove-curse> <staff>)
  :sort-value 4606
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-318" "detect evil"
  :numeric-id 318
  :x-attr #\d
  :x-char #\_
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 50
  :cost 350
  :obj-type '(<staff> <detect> <evil>)
  :sort-value 4615
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-319" "curing"
  :numeric-id 319
  :x-attr #\d
  :x-char #\_
  :level 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 50
  :cost 1000
  :obj-type '(<staff> <cure> <curing>)
  :sort-value 4617
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-320" "dispel evil"
  :numeric-id 320
  :x-attr #\d
  :x-char #\_
  :level 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 50
  :cost 1200
  :obj-type '(<staff> <dispel> <evil>)
  :sort-value 4624
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-321" "probing"
  :numeric-id 321
  :x-attr #\d
  :x-char #\_
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 50
  :cost 2000
  :obj-type '(<probing> <staff>)
  :sort-value 4623
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-322" "darkness"
  :numeric-id 322
  :x-attr #\d
  :x-char #\_
  :level 5
  :rarity 0
  :chance #(1 1 0 0)
  :locale #(5 50 0 0)
  :weight 50
  :cost 0
  :obj-type '(<darkness> <staff>)
  :sort-value 4600
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-323" "genocide"
  :numeric-id 323
  :x-attr #\d
  :x-char #\_
  :level 70
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(70 0 0 0)
  :weight 50
  :cost 3500
  :obj-type '(<genocide> <staff>)
  :sort-value 4627
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-324" "power"
  :numeric-id 324
  :x-attr #\d
  :x-char #\_
  :level 70
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(70 0 0 0)
  :weight 50
  :cost 4000
  :obj-type '(<power> <staff>)
  :sort-value 4625
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-325" "the magi"
  :numeric-id 325
  :x-attr #\d
  :x-char #\_
  :level 70
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(70 0 0 0)
  :weight 50
  :cost 4500
  :obj-type '(<magi> <staff>)
  :sort-value 4619
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-326" "perception"
  :numeric-id 326
  :x-attr #\d
  :x-char #\_
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 50
  :cost 400
  :obj-type '(<identify> <staff>)
  :sort-value 4605
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-327" "holiness"
  :numeric-id 327
  :x-attr #\d
  :x-char #\_
  :level 70
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(70 0 0 0)
  :weight 50
  :cost 4500
  :obj-type '(<holiness> <staff>)
  :sort-value 4626
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-328" "enlightenment"
  :numeric-id 328
  :x-attr #\d
  :x-char #\_
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 50
  :cost 750
  :obj-type '(<mapping> <staff>)
  :sort-value 4609
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-329" "healing"
  :numeric-id 329
  :x-attr #\d
  :x-char #\_
  :level 70
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(70 0 0 0)
  :weight 50
  :cost 5000
  :obj-type '(<staff> <cure> <healing>)
  :sort-value 4618
  :the-kind '<staff>
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-330" "[magic for beginners]"
  :numeric-id 330
  :x-attr #\R
  :x-char #\?
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 30
  :cost 25
  :obj-type '(<beginner> <spellbook> <mage>)
  :sort-value 6900
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-331" "[conjurings and tricks]"
  :numeric-id 331
  :x-attr #\R
  :x-char #\?
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 30
  :cost 100
  :obj-type '(<conjuring> <spellbook> <mage>)
  :sort-value 6901
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-332" "[incantations and illusions]"
  :numeric-id 332
  :x-attr #\R
  :x-char #\?
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 30
  :cost 400
  :obj-type '(<illusions> <spellbook> <mage>)
  :sort-value 6902
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-333" "[sorcery and evocations]"
  :numeric-id 333
  :x-attr #\R
  :x-char #\?
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 30
  :cost 800
  :obj-type '(<sorcery> <spellbook> <mage>)
  :sort-value 6903
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-334" "[beginners handbook]"
  :numeric-id 334
  :x-attr #\G
  :x-char #\?
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 30
  :cost 25
  :obj-type '(<beginner> <spellbook> <priest>)
  :sort-value 7000
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-335" "[words of wisdom]"
  :numeric-id 335
  :x-attr #\G
  :x-char #\?
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 30
  :cost 100
  :obj-type '(<words> <spellbook> <priest>)
  :sort-value 7001
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-336" "[chants and blessings]"
  :numeric-id 336
  :x-attr #\G
  :x-char #\?
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 30
  :cost 300
  :obj-type '(<chants> <spellbook> <priest>)
  :sort-value 7002
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-337" "[exorcism and dispelling]"
  :numeric-id 337
  :x-attr #\G
  :x-char #\?
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 30
  :cost 900
  :obj-type '(<exorcism> <spellbook> <priest>)
  :sort-value 7003
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-338" "& small wooden chest~"
  :numeric-id 338
  :x-attr #\s
  :x-char #\~
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 250
  :cost 20
  :obj-type '(<chest>)
  :sort-value 2401
  :the-kind '<chest>
  :game-values (make-instance 'game-values :base-dice 3 :num-dice 2)) 

(define-object-kind "object-339" "& large wooden chest~"
  :numeric-id 339
  :x-attr #\s
  :x-char #\~
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 500
  :cost 60
  :obj-type '(<chest>)
  :sort-value 2405
  :the-kind '<chest>
  :game-values (make-instance 'game-values :base-dice 5 :num-dice 2)) 

(define-object-kind "object-340" "& small iron chest~"
  :numeric-id 340
  :x-attr #\s
  :x-char #\~
  :level 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 300
  :cost 100
  :obj-type '(<chest>)
  :sort-value 2402
  :the-kind '<chest>
  :game-values (make-instance 'game-values :base-dice 4 :num-dice 2)) 

(define-object-kind "object-341" "& large iron chest~"
  :numeric-id 341
  :x-attr #\s
  :x-char #\~
  :level 35
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(35 0 0 0)
  :weight 1000
  :cost 150
  :obj-type '(<chest>)
  :sort-value 2406
  :the-kind '<chest>
  :game-values (make-instance 'game-values :base-dice 6 :num-dice 2)) 

(define-object-kind "object-342" "& small steel chest~"
  :numeric-id 342
  :x-attr #\s
  :x-char #\~
  :level 45
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(45 0 0 0)
  :weight 500
  :cost 200
  :obj-type '(<chest>)
  :sort-value 2403
  :the-kind '<chest>
  :game-values (make-instance 'game-values :base-dice 4 :num-dice 2)) 

(define-object-kind "object-343" "& large steel chest~"
  :numeric-id 343
  :x-attr #\s
  :x-char #\~
  :level 55
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(55 0 0 0)
  :weight 1000
  :cost 250
  :obj-type '(<chest>)
  :sort-value 2407
  :the-kind '<chest>
  :game-values (make-instance 'game-values :base-dice 6 :num-dice 2)) 

(define-object-kind "object-344" "& ruined chest~"
  :numeric-id 344
  :x-attr #\s
  :x-char #\~
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(75 0 0 0)
  :weight 250
  :cost 0
  :obj-type '(<chest>)
  :sort-value 2400
  :the-kind '<chest>) 

(define-object-kind "object-345" "& iron spike~"
  :numeric-id 345
  :x-attr #\W
  :x-char #\~
  :level 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 10
  :cost 1
  :obj-type '(<spike>)
  :sort-value 2300
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-346" "& wooden torch~"
  :numeric-id 346
  :x-attr #\u
  :x-char #\~
  :level 1
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

(define-object-kind "object-347" "& brass lantern~"
  :numeric-id 347
  :x-attr #\U
  :x-char #\~
  :level 3
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

(define-object-kind "object-348" "& flask~ of oil"
  :numeric-id 348
  :x-attr #\y
  :x-char #\!
  :level 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 10
  :cost 3
  :obj-type '(<oil> <flask>)
  :sort-value 5700
  :game-values (make-instance 'game-values :base-dice 6 :num-dice 2 :charges 7500)) 

(define-object-kind "object-349" "& empty bottle~"
  :numeric-id 349
  :x-attr #\w
  :x-char #\!
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 0
  :obj-type '(<bottle>)
  :sort-value 2101
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-351" "door/stair location"
  :numeric-id 351
  :x-attr #\d
  :x-char #\-
  :level 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 15
  :cost 1000
  :obj-type '(<rod> <detect> <door>)
  :sort-value 4501
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-352" "trap location"
  :numeric-id 352
  :x-attr #\d
  :x-char #\-
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 15
  :cost 100
  :obj-type '(<rod> <detect> <trap>)
  :sort-value 4500
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-353" "probing"
  :numeric-id 353
  :x-attr #\d
  :x-char #\-
  :level 40
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(40 0 0 0)
  :weight 15
  :cost 4000
  :obj-type '(<probing> <rod>)
  :sort-value 4507
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-354" "recall"
  :numeric-id 354
  :x-attr #\d
  :x-char #\-
  :level 30
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(30 0 0 0)
  :weight 15
  :cost 4000
  :obj-type '(<recall> <rod>)
  :sort-value 4503
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-355" "illumination"
  :numeric-id 355
  :x-attr #\d
  :x-char #\-
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 15
  :cost 1000
  :obj-type '(<illumination> <rod>)
  :sort-value 4504
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-356" "light"
  :numeric-id 356
  :x-attr #\d
  :x-char #\-
  :level 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 15
  :cost 500
  :obj-type '(<light> <rod>)
  :sort-value 4515
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-357" "lightning bolts"
  :numeric-id 357
  :x-attr #\d
  :x-char #\-
  :level 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 15
  :cost 2000
  :obj-type '(<rod> <bolt> <lightning>)
  :sort-value 4521
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-358" "frost bolts"
  :numeric-id 358
  :x-attr #\d
  :x-char #\-
  :level 25
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(25 0 0 0)
  :weight 15
  :cost 2500
  :obj-type '(<rod> <bolt> <cold>)
  :sort-value 4523
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-359" "fire bolts"
  :numeric-id 359
  :x-attr #\d
  :x-char #\-
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 15
  :cost 3000
  :obj-type '(<rod> <bolt> <fire>)
  :sort-value 4522
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-360" "polymorph"
  :numeric-id 360
  :x-attr #\d
  :x-char #\-
  :level 35
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(35 0 0 0)
  :weight 15
  :cost 1200
  :obj-type '(<polymorph> <rod>)
  :sort-value 4519
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-361" "slow monster"
  :numeric-id 361
  :x-attr #\d
  :x-char #\-
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 15
  :cost 1500
  :obj-type '(<slow-monster> <rod>)
  :sort-value 4517
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-362" "sleep monster"
  :numeric-id 362
  :x-attr #\d
  :x-char #\-
  :level 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 15
  :cost 1500
  :obj-type '(<sleep-monster> <rod>)
  :sort-value 4516
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-363" "drain life"
  :numeric-id 363
  :x-attr #\d
  :x-char #\-
  :level 75
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(75 0 0 0)
  :weight 15
  :cost 3600
  :obj-type '(<drain-life> <rod>)
  :sort-value 4518
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-364" "teleport other"
  :numeric-id 364
  :x-attr #\d
  :x-char #\-
  :level 45
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(45 0 0 0)
  :weight 15
  :cost 1400
  :obj-type '(<teleport-away> <rod>)
  :sort-value 4513
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-365" "disarming"
  :numeric-id 365
  :x-attr #\d
  :x-char #\-
  :level 35
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(35 0 0 0)
  :weight 15
  :cost 2100
  :obj-type '(<disarming> <rod>)
  :sort-value 4514
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-366" "lightning balls"
  :numeric-id 366
  :x-attr #\d
  :x-char #\-
  :level 55
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(55 0 0 0)
  :weight 15
  :cost 4000
  :obj-type '(<rod> <ball> <lightning)
  :sort-value 4525
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-367" "cold balls"
  :numeric-id 367
  :x-attr #\d
  :x-char #\-
  :level 60
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(60 0 0 0)
  :weight 15
  :cost 4500
  :obj-type '(<rod> <ball> <cold)
  :sort-value 4527
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-368" "fire balls"
  :numeric-id 368
  :x-attr #\d
  :x-char #\-
  :level 75
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(75 0 0 0)
  :weight 15
  :cost 5000
  :obj-type '(<rod> <ball> <fire>)
  :sort-value 4526
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-369" "acid balls"
  :numeric-id 369
  :x-attr #\d
  :x-char #\-
  :level 70
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(70 0 0 0)
  :weight 15
  :cost 5500
  :obj-type '(<rod> <ball> <acid>)
  :sort-value 4524
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-370" "acid bolts"
  :numeric-id 370
  :x-attr #\d
  :x-char #\-
  :level 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 15
  :cost 3500
  :obj-type '(<rod> <bolt> <acid>)
  :sort-value 4520
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-371" "enlightenment"
  :numeric-id 371
  :x-attr #\d
  :x-char #\-
  :level 65
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(65 0 0 0)
  :weight 15
  :cost 10000
  :obj-type '(<mapping> <rod>)
  :sort-value 4505
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-372" "perception"
  :numeric-id 372
  :x-attr #\d
  :x-char #\-
  :level 50
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(50 0 0 0)
  :weight 15
  :cost 13000
  :obj-type '(<identify> <rod>)
  :sort-value 4502
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-373" "curing"
  :numeric-id 373
  :x-attr #\d
  :x-char #\-
  :level 65
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(65 0 0 0)
  :weight 15
  :cost 15000
  :obj-type '(<curing> <rod>)
  :sort-value 4508
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-374" "healing"
  :numeric-id 374
  :x-attr #\d
  :x-char #\-
  :level 80
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(80 0 0 0)
  :weight 15
  :cost 20000
  :obj-type '(<healing> <rod>)
  :sort-value 4509
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-375" "detection"
  :numeric-id 375
  :x-attr #\d
  :x-char #\-
  :level 30
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(30 0 0 0)
  :weight 15
  :cost 5000
  :obj-type '(<detection> <rod>)
  :sort-value 4506
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-376" "restoration"
  :numeric-id 376
  :x-attr #\d
  :x-char #\-
  :level 80
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(80 0 0 0)
  :weight 15
  :cost 25000
  :obj-type '(<restoration> <rod>)
  :sort-value 4510
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-377" "speed"
  :numeric-id 377
  :x-attr #\d
  :x-char #\-
  :level 95
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(95 0 0 0)
  :weight 15
  :cost 50000
  :obj-type '(<speed> <rod>)
  :sort-value 4511
  :the-kind '<rod>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-379" "[resistance of scarabtarices]"
  :numeric-id 379
  :x-attr #\r
  :x-char #\?
  :level 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 30
  :cost 5000
  :obj-type '(<resistance> <spellbook> <mage>)
  :sort-value 6904
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "object-380" "[mordenkainen's escapes]"
  :numeric-id 380
  :x-attr #\r
  :x-char #\?
  :level 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 30
  :cost 10000
  :obj-type '(<escapes> <spellbook> <mage>)
  :sort-value 6905
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "object-381" "[kelek's grimoire of power]"
  :numeric-id 381
  :x-attr #\r
  :x-char #\?
  :level 60
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(60 0 0 0)
  :weight 30
  :cost 30000
  :obj-type '(<grimoire> <spellbook> <mage>)
  :sort-value 6906
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "object-382" "[tenser's transformations]"
  :numeric-id 382
  :x-attr #\r
  :x-char #\?
  :level 80
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(80 0 0 0)
  :weight 30
  :cost 50000
  :obj-type '(<transformations> <spellbook> <mage>)
  :sort-value 6907
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "object-383" "[raal's tome of destruction]"
  :numeric-id 383
  :x-attr #\r
  :x-char #\?
  :level 100
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(100 0 0 0)
  :weight 30
  :cost 100000
  :obj-type '(<destruction> <spellbook> <mage>)
  :sort-value 6908
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "object-384" "[ethereal openings]"
  :numeric-id 384
  :x-attr #\g
  :x-char #\?
  :level 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 30
  :cost 5000
  :obj-type '(<openings> <spellbook> <priest>)
  :sort-value 7004
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "object-385" "[godly insights]"
  :numeric-id 385
  :x-attr #\g
  :x-char #\?
  :level 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 30
  :cost 10000
  :obj-type '(<insights> <spellbook> <priest>)
  :sort-value 7005
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "object-386" "[purifications and healing]"
  :numeric-id 386
  :x-attr #\g
  :x-char #\?
  :level 60
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(60 0 0 0)
  :weight 30
  :cost 30000
  :obj-type '(<healing> <spellbook> <priest>)
  :sort-value 7006
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "object-387" "[holy infusions]"
  :numeric-id 387
  :x-attr #\g
  :x-char #\?
  :level 80
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(80 0 0 0)
  :weight 30
  :cost 50000
  :obj-type '(<infusions> <spellbook> <priest>)
  :sort-value 7007
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "object-388" "[wrath of god]"
  :numeric-id 388
  :x-attr #\g
  :x-char #\?
  :level 100
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(100 0 0 0)
  :weight 30
  :cost 100000
  :obj-type '(<wrath> <spellbook> <priest>)
  :sort-value 7008
  :the-kind '<book>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "object-389" "& shard~ of pottery"
  :numeric-id 389
  :x-attr #\r
  :x-char #\~
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 5
  :cost 0
  :obj-type '(<junk>)
  :sort-value 2203
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-390" "& broken stick~"
  :numeric-id 390
  :x-attr #\r
  :x-char #\~
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 3
  :cost 0
  :obj-type '(<junk>)
  :sort-value 2206
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-391" "& broken skull~"
  :numeric-id 391
  :x-attr #\w
  :x-char #\~
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 1
  :cost 0
  :obj-type '(<skeleton>)
  :sort-value 2001
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-392" "& broken bone~"
  :numeric-id 392
  :x-attr #\w
  :x-char #\~
  :level 0
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(0 0 0 0)
  :weight 2
  :cost 0
  :obj-type '(<skeleton>)
  :sort-value 2002
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-393" "& canine skeleton~"
  :numeric-id 393
  :x-attr #\w
  :x-char #\~
  :level 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 10
  :cost 0
  :obj-type '(<skeleton>)
  :sort-value 2004
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-394" "& rodent skeleton~"
  :numeric-id 394
  :x-attr #\w
  :x-char #\~
  :level 1
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(1 0 0 0)
  :weight 10
  :cost 0
  :obj-type '(<skeleton>)
  :sort-value 2003
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-395" "& human skeleton~"
  :numeric-id 395
  :x-attr #\w
  :x-char #\~
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 60
  :cost 0
  :obj-type '(<skeleton>)
  :sort-value 2008
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-396" "& dwarf skeleton~"
  :numeric-id 396
  :x-attr #\w
  :x-char #\~
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 50
  :cost 0
  :obj-type '(<skeleton>)
  :sort-value 2007
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-397" "& elf skeleton~"
  :numeric-id 397
  :x-attr #\w
  :x-char #\~
  :level 5
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
  :level 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 30
  :cost 0
  :obj-type '(<skeleton>)
  :sort-value 2005
  :game-values (make-instance 'game-values :base-dice 2 :num-dice 1)) 

(define-object-kind "object-400" "black dragon scale mail~"
  :numeric-id 400
  :x-attr #\s
  :x-char #\[
  :level 60
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(60 0 0 0)
  :weight 200
  :cost 30000
  :obj-type '(<black> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4101
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 30 :ac-bonus 10 :base-dice 4 :num-dice 2 :tohit-bonus -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<acid>))) 

(define-object-kind "object-401" "blue dragon scale mail~"
  :numeric-id 401
  :x-attr #\b
  :x-char #\[
  :level 40
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(40 0 0 0)
  :weight 200
  :cost 35000
  :obj-type '(<blue> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4102
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 30 :ac-bonus 10 :base-dice 4 :num-dice 2 :tohit-bonus -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<electricity>))) 

(define-object-kind "object-402" "white dragon scale mail~"
  :numeric-id 402
  :x-attr #\w
  :x-char #\[
  :level 50
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(50 0 0 0)
  :weight 200
  :cost 40000
  :obj-type '(<white> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4103
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 30 :ac-bonus 10 :base-dice 4 :num-dice 2 :tohit-bonus -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<cold>))) 

(define-object-kind "object-403" "red dragon scale mail~"
  :numeric-id 403
  :x-attr #\r
  :x-char #\[
  :level 80
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(80 0 0 0)
  :weight 200
  :cost 100000
  :obj-type '(<red> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4104
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 30 :ac-bonus 10 :base-dice 4 :num-dice 2 :tohit-bonus -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<fire>))) 

(define-object-kind "object-404" "green dragon scale mail~"
  :numeric-id 404
  :x-attr #\g
  :x-char #\[
  :level 70
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(70 0 0 0)
  :weight 200
  :cost 80000
  :obj-type '(<green> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4105
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 30 :ac-bonus 10 :base-dice 4 :num-dice 2 :tohit-bonus -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<poison>))) 

(define-object-kind "object-405" "multi-hued dragon scale mail~"
  :numeric-id 405
  :x-attr #\v
  :x-char #\[
  :level 100
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(100 0 0 0)
  :weight 200
  :cost 150000
  :obj-type '(<multihued> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4106
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 30 :ac-bonus 10 :base-dice 4 :num-dice 2 :tohit-bonus -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists
                              '(<poison> <cold> <fire> <electricity> <acid>))) 

(define-object-kind "object-406" "shining dragon scale mail~"
  :numeric-id 406
  :x-attr #\o
  :x-char #\[
  :level 65
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(65 0 0 0)
  :weight 200
  :cost 60000
  :obj-type '(<shining> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4110
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 30 :ac-bonus 10 :base-dice 4 :num-dice 2 :tohit-bonus -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<dark> <light>))) 

(define-object-kind "object-407" "law dragon scale mail~"
  :numeric-id 407
  :x-attr #\B
  :x-char #\[
  :level 80
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(80 0 0 0)
  :weight 200
  :cost 80000
  :obj-type '(<law> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4112
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 30 :ac-bonus 10 :base-dice 4 :num-dice 2 :tohit-bonus -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<shard> <sound>))) 

(define-object-kind "object-408" "bronze dragon scale mail~"
  :numeric-id 408
  :x-attr #\U
  :x-char #\[
  :level 55
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(55 0 0 0)
  :weight 200
  :cost 30000
  :obj-type '(<bronze> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4114
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 30 :ac-bonus 10 :base-dice 4 :num-dice 2 :tohit-bonus -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<confusion>))) 

(define-object-kind "object-409" "gold dragon scale mail~"
  :numeric-id 409
  :x-attr #\y
  :x-char #\[
  :level 65
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(65 0 0 0)
  :weight 200
  :cost 40000
  :obj-type '(<gold> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4116
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 30 :ac-bonus 10 :base-dice 4 :num-dice 2 :tohit-bonus -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<sound>))) 

(define-object-kind "object-410" "chaos dragon scale mail~"
  :numeric-id 410
  :x-attr #\v
  :x-char #\[
  :level 75
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(75 0 0 0)
  :weight 200
  :cost 70000
  :obj-type '(<chaos> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4118
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 30 :ac-bonus 10 :base-dice 4 :num-dice 2 :tohit-bonus -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists '(<disenchant> <chaos> <confusion>))) 

(define-object-kind "object-411" "balance dragon scale mail~"
  :numeric-id 411
  :x-attr #\v
  :x-char #\[
  :level 90
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(90 0 0 0)
  :weight 200
  :cost 100000
  :obj-type '(<balance> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4120
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 30 :ac-bonus 10 :base-dice 4 :num-dice 2 :tohit-bonus -2 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists
                              '(<disenchant> <chaos> <shard> <sound> <confusion>))) 

(define-object-kind "object-412" "power dragon scale mail~"
  :numeric-id 412
  :x-attr #\v
  :x-char #\[
  :level 110
  :rarity 0
  :chance #(64 0 0 0)
  :locale #(110 0 0 0)
  :weight 200
  :cost 300000
  :obj-type '(<power> <body-armour> <dragon-scale>)
  :flags '(<activation>)
  :sort-value 4130
  :the-kind '<body-armour>
  :game-values (make-instance 'game-values :base-ac 40 :ac-bonus 15 :base-dice 4 :num-dice 2 :tohit-bonus -3 :ignores
                              '(<cold> <fire> <electricity> <acid>) :resists
                              '(<chaos> <nether> <nexus> <dark> <light> <confusion> <poison> <electricity> <cold>
                                <fire> <acid>))) 

(define-object-kind "object-415" "death"
  :numeric-id 415
  :x-attr #\d
  :x-char #\!
  :level 55
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(55 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<death> <potion>)
  :sort-value 5523
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-416" "ruination"
  :numeric-id 416
  :x-attr #\d
  :x-char #\!
  :level 40
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(40 0 0 0)
  :weight 4
  :cost 0
  :obj-type '(<ruination> <potion>)
  :sort-value 5515
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-417" "detonations"
  :numeric-id 417
  :x-attr #\d
  :x-char #\!
  :level 60
  :rarity 0
  :chance #(8 0 0 0)
  :locale #(60 0 0 0)
  :weight 4
  :cost 10000
  :obj-type '(<detonations> <potion>)
  :sort-value 5522
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 25 :num-dice 25)) 

(define-object-kind "object-418" "augmentation"
  :numeric-id 418
  :x-attr #\d
  :x-char #\!
  :level 40
  :rarity 0
  :chance #(16 0 0 0)
  :locale #(40 0 0 0)
  :weight 4
  :cost 60000
  :obj-type '(<augmentation> <potion>)
  :sort-value 5555
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-419" "*healing*"
  :numeric-id 419
  :x-attr #\d
  :x-char #\!
  :level 40
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(40 0 0 0)
  :weight 4
  :cost 1500
  :obj-type '(<potion> <cure> <healing> <powerful>)
  :sort-value 5538
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-420" "life"
  :numeric-id 420
  :x-attr #\d
  :x-char #\!
  :level 60
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(60 0 0 0)
  :weight 4
  :cost 5000
  :obj-type '(<life> <potion>)
  :sort-value 5539
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-421" "self knowledge"
  :numeric-id 421
  :x-attr #\d
  :x-char #\!
  :level 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 4
  :cost 2000
  :obj-type '(<self-knowledge> <potion>)
  :sort-value 5558
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-422" "*enlightenment*"
  :numeric-id 422
  :x-attr #\d
  :x-char #\!
  :level 70
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(70 0 0 0)
  :weight 4
  :cost 80000
  :obj-type '(<potion> <enlightenment> <powerful>)
  :sort-value 5557
  :the-kind '<potion>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-480" "copper"
  :numeric-id 480
  :x-attr #\u
  :x-char #\$
  :level 1
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
  :level 1
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
  :level 1
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
  :level 1
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
  :level 1
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
  :level 1
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
  :level 1
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
  :level 1
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
  :level 1
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
  :level 1
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
  :level 1
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
  :level 1
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
  :level 1
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
  :level 1
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
  :level 1
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
  :level 1
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
  :level 1
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
  :level 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 80
  :obj-type '(<money>)
  :sort-value 7118
  :the-kind '<money>) 

(define-object-kind "object-498" "& mighty hammer~"
  :numeric-id 498
  :x-attr #\D
  :x-char #\\
  :level 15
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 200
  :cost 1000
  :obj-type '(<grond> <weapon> <hafted>)
  :flags '(<instant-artifact> <show-modififers>)
  :sort-value 3050
  :the-kind '<weapon>
  :game-values (make-instance 'game-values :base-dice 9 :num-dice 3)) 

(define-object-kind "object-499" "& massive iron crown~"
  :numeric-id 499
  :x-attr #\D
  :x-char #\]
  :level 44
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 20
  :cost 1000
  :obj-type '(<morgoth> <headgear> <crown>)
  :flags '(<instant-artifact>)
  :sort-value 3650
  :the-kind '<headgear>
  :game-values (make-instance 'game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "object-500" "& phial~"
  :numeric-id 500
  :x-attr #\y
  :x-char #\~
  :level 1
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
  :level 30
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
  :level 60
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
  :level 50
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
  :level 60
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
  :level 70
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
  :level 50
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
  :level 90
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
  :level 80
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
  :level 90
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
  :level 100
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
  :level 110
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


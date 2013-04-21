;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/monsters.lisp - monsters for vanilla variant
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

;;; === depth 1
(define-monster-kind "bat-fruit" "fruit bat"
  :numeric-id  31
  :desc "A fast-moving pest."
  :x-char #\b
  :x-attr #\o
  :type '(<animal>)
  :depth 1
  :rarity 1
  :hitpoints '(1 . 6)
  :armour 3
  :speed 120
  :xp 1
  :alertness 10
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 1))))

(define-monster-kind "ant-soldier" "soldier ant"
  :numeric-id  30
  :desc "A large ant with powerful mandibles."
  :x-char #\a
  :x-attr #\W
  :type '(<animal>)
  :depth 1
  :rarity 1
  :hitpoints '(2 . 5)
  :armour 3
  :speed 110
  :xp 3
  :abilities '(<bash-door> <weird-mind>)
  :alertness 10
  :vision 10
  :attacks '((<bite> :type <hurt> :damage (1 . 2))))

(define-monster-kind "jackal" "jackal"
  :numeric-id  29
  :desc "It is a yapping snarling dog, dangerous when in a pack."
  :x-char #\C
  :x-attr #\U
  :type '(<animal>)
  :depth 1
  :rarity 1
  :hitpoints '(1 . 4)
  :armour 3
  :speed 110
  :xp 1
  :alertness 10
  :vision 10
  :attacks '((<bite> :type <hurt> :damage (1 . 1))))

(define-monster-kind "lizard-rock" "rock lizard"
  :numeric-id  28
  :desc "It is a small lizard with a hardened hide."
  :x-char #\R
  :x-attr #\U
  :type '(<animal>)
  :depth 1
  :rarity 1
  :hitpoints '(3 . 4)
  :armour 4
  :speed 110
  :xp 2
  :alertness 15
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 1))))

(define-monster-kind "eye-floating" "floating eye"
  :numeric-id  27
  :desc "A disembodied eye, floating a few feet above the ground."
  :x-char #\e
  :x-attr #\o
  :depth 1
  :rarity 1
  :hitpoints '(3 . 6)
  :armour 6
  :speed 110
  :xp 1
  :abilities '(<never-move>)
  :immunities '(<fear>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 2
  :attacks '((<gaze> :type <paralyse> :damage nil)))

(define-monster-kind "worm-white" "white worm mass"
  :numeric-id  26
  :desc "It is a large slimy mass of worms."
  :x-char #\w
  :x-attr #\w
  :type '(<animal>)
  :depth 1
  :rarity 1
  :hitpoints '(4 . 4)
  :armour 1
  :speed 100
  :xp 2
  :abilities '(<breeder> <weird-mind> <stupid> (<random-mover> 1/4) (<random-mover> 1/2))
  :immunities '(<fear> <poison>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 7
  :attacks '((<crawl> :type <poison> :damage (1 . 2))))

(define-monster-kind "kobold" "kobold"
  :numeric-id  25
  :desc "It is a small, dog-headed humanoid."
  :x-char #\k
  :x-attr #\G
  :alignment '<evil>
  :depth 1
  :rarity 1
  :hitpoints '(3 . 7)
  :armour 16
  :speed 110
  :xp 5
  :abilities '(<bash-door> <open-door>)
  :immunities '(<poison>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop-chance> 3/5)))

(define-monster-kind "kobold-small" "small kobold"
  :numeric-id  24
  :desc "It is a squat and ugly humanoid figure."
  :x-char #\k
  :x-attr #\y
  :alignment '<evil>
  :depth 1
  :rarity 1
  :hitpoints '(2 . 7)
  :armour 16
  :speed 110
  :xp 5
  :abilities '(<bash-door> <open-door>)
  :immunities '(<poison>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 5)))
  :treasures '((<drop-chance> 3/5)))

(define-monster-kind "snake-white" "large white snake"
  :numeric-id  23
  :desc "It is about eight feet long."
  :x-char #\J
  :x-attr #\w
  :type '(<animal>)
  :depth 1
  :rarity 1
  :hitpoints '(3 . 6)
  :armour 30
  :speed 100
  :xp 2
  :abilities '(<bash-door> (<random-mover> 1/2))
  :alertness 99
  :vision 4
  :attacks '((<crush> :type <hurt> :damage (1 . 1))
	     (<bite> :type <hurt> :damage (1 . 1))))

(define-monster-kind "snake-brown" "large brown snake"
  :numeric-id  22
  :desc "It is about eight feet long."
  :x-char #\J
  :x-attr #\u
  :type '(<animal>)
  :depth 1
  :rarity 1
  :hitpoints '(4 . 6)
  :armour 35
  :speed 100
  :xp 3
  :abilities '(<bash-door> (<random-mover> 1/4))
  :alertness 99
  :vision 4
  :attacks '((<crush> :type <hurt> :damage (1 . 4))
	     (<bite> :type <hurt> :damage (1 . 3))))

(define-monster-kind "mouse-white" "giant white mouse"
  :numeric-id  21
  :desc "It is about three feet long with large teeth."
  :x-char #\r
  :x-attr #\w
  :type '(<animal>)
  :depth 1
  :rarity 1
  :hitpoints '(1 . 3)
  :armour 4
  :speed 110
  :xp 1
  :abilities '(<breeder> (<random-mover> 1/2))
  :alertness 20
  :vision 8
  :attacks '((<bite> :type <hurt> :damage (1 . 2))))

(define-monster-kind "icky-clear" "clear icky thing"
  :numeric-id  20
  :desc "It is a smallish, slimy, icky, blobby creature."
  :x-char #\i
  :x-attr #\w
  :depth 1
  :rarity 1
  :hitpoints '(2 . 5)
  :armour 6
  :speed 110
  :xp 1
  :abilities '(<empty-mind> <invisible> (<random-mover> 1/4) (<random-mover> 1/2) <see-through>)
  :alertness 10
  :vision 12
  :attacks '((<touch> :type <hurt> :damage (1 . 2))))

(define-monster-kind "icky-white" "white icky thing"
  :numeric-id  19
  :desc "It is a smallish, slimy, icky creature."
  :x-char #\i
  :x-attr #\w
  :depth 1
  :rarity 1
  :hitpoints '(3 . 5)
  :armour 7
  :speed 110
  :xp 2
  :abilities '(<empty-mind> (<random-mover> 1/4) (<random-mover> 1/2))
  :alertness 10
  :vision 12
  :attacks '((<touch> :type <hurt> :damage (1 . 2))))

(define-monster-kind "mold-grey" "grey mold"
  :numeric-id  15
  :desc "A small strange growth."
  :x-char #\m
  :x-attr #\s
  :depth 1
  :rarity 1
  :hitpoints '(1 . 2)
  :armour 1
  :speed 110
  :xp 3
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion> <poison>)
  :alertness 0
  :vision 2
  :attacks '((<hit> :type <hurt> :damage (1 . 4))
	     (<hit> :type <hurt> :damage (1 . 4))))

(define-monster-kind "centipede-white" "giant white centipede"
  :numeric-id  18
  :desc "It is about four feet long and carnivorous."
  :x-char #\c
  :x-attr #\w
  :type '(<animal>)
  :depth 1
  :rarity 1
  :hitpoints '(3 . 5)
  :armour 10
  :speed 110
  :xp 2
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/2))
  :alertness 40
  :vision 7
  :attacks '((<sting> :type <hurt> :damage (1 . 2))
	     (<bite> :type <hurt> :damage (1 . 2))))

(define-monster-kind "centipede-yellow" "giant yellow centipede"
  :numeric-id  17
  :desc "It is about four feet long and carnivorous."
  :x-char #\c
  :x-attr #\y
  :type '(<animal>)
  :depth 1
  :rarity 1
  :hitpoints '(2 . 6)
  :armour 12
  :speed 110
  :xp 2
  :abilities '(<weird-mind>)
  :alertness 30
  :vision 8
  :attacks '((<sting> :type <hurt> :damage (1 . 3))
	     (<bite> :type <hurt> :damage (1 . 3))))

(define-monster-kind "mushroom-grey" "grey mushroom patch"
  :numeric-id  16
  :desc "Yum!  It looks quite tasty."
  :x-char #\,
  :x-attr #\s
  :depth 1
  :rarity 1
  :hitpoints '(1 . 2)
  :armour 1
  :speed 110
  :xp 1
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion> <poison>)
  :alertness 0
  :vision 2
  :attacks '((<spore> :type <confusion> :damage (1 . 4))))

;;; end depth 1 monsters

;;; === depth 2

(define-monster-kind "worm-green" "green worm mass"
  :numeric-id  48
  :desc "It is a large slimy mass of worms."
  :x-char #\w
  :x-attr #\g
  :type '(<animal>)
  :depth 2
  :rarity 1
  :hitpoints '(6 . 4)
  :armour 3
  :speed 100
  :xp 3
  :abilities '(<breeder> <weird-mind> <stupid> (<random-mover> 1/4) (<random-mover> 1/2))
  :immunities '(<fear> <acid>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 7
  :attacks '((<crawl> :type <acid> :damage (1 . 3))))

(define-monster-kind "harpy-white" "white harpy"
  :numeric-id  44
  :desc "A flying, screeching bird with a woman's face."
  :x-char #\H
  :x-attr #\w
  :alignment '<evil>
  :type '(<animal>)
  :depth 2
  :rarity 1
  :hitpoints '(2 . 5)
  :armour 17
  :speed 110
  :xp 5
  :abilities '((<random-mover> 1/2))
  :alertness 10
  :vision 16
  :attacks '((<bite> :type <hurt> :damage (1 . 2))
	     (<claw> :type <hurt> :damage (1 . 1))
	     (<claw> :type <hurt> :damage (1 . 1)))
  :gender '<female>)

(define-monster-kind "salamander" "salamander"
  :numeric-id  43
  :desc "A small black and orange lizard."
  :x-char #\R
  :x-attr #\o
  :type '(<animal>)
  :depth 2
  :rarity 1
  :hitpoints '(4 . 6)
  :armour 20
  :speed 110
  :xp 10
  :abilities '((<random-mover> 1/4))
  :immunities '(<fire>)
  :alertness 80
  :vision 8
  :attacks '((<bite> :type <fire> :damage (1 . 3))))

(define-monster-kind "snake-yellow" "large yellow snake"
  :numeric-id  49
  :desc "It is about ten feet long."
  :x-char #\J
  :x-attr #\y
  :type '(<animal>)
  :depth 2
  :rarity 1
  :hitpoints '(4 . 8)
  :armour 38
  :speed 100
  :xp 9
  :abilities '(<bash-door> (<random-mover> 1/4))
  :alertness 75
  :vision 5
  :attacks '((<crush> :type <hurt> :damage (1 . 6))
	     (<bite> :type <hurt> :damage (1 . 4))))

(define-monster-kind "ant-black" "giant black ant"
  :numeric-id  42
  :desc "It is about three feet long."
  :x-char #\a
  :x-attr #\D
  :type '(<animal>)
  :depth 2
  :rarity 1
  :hitpoints '(3 . 6)
  :armour 20
  :speed 110
  :xp 8
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/4))
  :alertness 80
  :vision 8
  :attacks '((<bite> :type <hurt> :damage (1 . 4))))

(define-monster-kind "frog-green" "giant green frog"
  :numeric-id  41
  :desc "It is as big as a wolf."
  :x-char #\R
  :x-attr #\g
  :type '(<animal>)
  :depth 2
  :rarity 1
  :hitpoints '(2 . 8)
  :armour 8
  :speed 110
  :xp 6
  :abilities '(<bash-door> (<random-mover> 1/4))
  :alertness 30
  :vision 12
  :attacks '((<bite> :type <hurt> :damage (1 . 3))))

(define-monster-kind "jelly-white" "white jelly"
  :numeric-id  40
  :desc "Its a large pile of white flesh."
  :x-char #\j
  :x-attr #\w
  :depth 2
  :rarity 1
  :hitpoints '(8 . 8)
  :armour 1
  :speed 120
  :xp 10
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion> <poison>)
  :vulnerabilities '(<light>)
  :alertness 99
  :vision 2
  :attacks '((<touch> :type <poison> :damage (1 . 2))))

(define-monster-kind "mushroom-yellow" "yellow mushroom patch"
  :numeric-id  39
  :desc "Yum!  It looks quite tasty."
  :x-char #\,
  :x-attr #\y
  :depth 2
  :rarity 1
  :hitpoints '(1 . 1)
  :armour 1
  :speed 110
  :xp 2
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion> <poison>)
  :alertness 0
  :vision 2
  :attacks '((<spore> :type <terrify> :damage (1 . 6))))

(define-monster-kind "spider-cave" "cave spider"
  :numeric-id  50
  :desc "It is a black spider that moves in fits and starts."
  :x-char #\S
  :x-attr #\D
  :type '(<animal>)
  :depth 2
  :rarity 1
  :hitpoints '(2 . 6)
  :armour 16
  :speed 120
  :xp 7
  :abilities '(<bash-door> <weird-mind>)
  :alertness 80
  :vision 8
  :attacks '((<bite> :type <hurt> :damage (1 . 4))))

(define-monster-kind "cat-wild" "wild cat"
  :numeric-id  51
  :desc "A larger than normal feline, hissing loudly.  Its velvet
claws conceal a fistful of needles."
  :x-char #\f
  :x-attr #\U
  :type '(<animal>)
  :depth 2
  :rarity 2
  :hitpoints '(3 . 5)
  :armour 12
  :speed 120
  :xp 8
  :abilities '(<bash-door>)
  :alertness 0
  :vision 40
  :attacks '((<claw> :type <hurt> :damage (1 . 3)) (<claw> :type <hurt> :damage (1 . 3))))

(define-monster-kind "yeek-blue" "blue yeek"
  :numeric-id  45
  :desc "A small humanoid figure."
  :x-char #\y
  :x-attr #\b
  :type '(<animal>)
  :depth 2
  :rarity 1
  :hitpoints '(2 . 6)
  :armour 14
  :speed 110
  :xp 4
  :abilities '(<bash-door> <open-door>)
  :alertness 10
  :vision 18
  :attacks '((<hit> :type <hurt> :damage (1 . 5)))
  :treasures '((<drop-chance> 3/5)))

(define-monster-kind "priest-novice" "novice priest"
  :numeric-id  89
  :numeric-id  37
  :desc "He is tripping over his priestly robes."
  :x-char #\p
  :x-attr #\g
  :depth 2
  :rarity 1
  :hitpoints '(7 . 4)
  :armour 10
  :speed 110
  :xp 7
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 5)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>
  :special-abilities '((<spell> (<cause> 1)) (<spell> <scare>) (<spell> <heal>) (<frequency> 1/12))
  :appear-in-group? #'van-novice-appears-in-group?)

(define-monster-kind "rogue-novice" "novice rogue"
  :numeric-id  91
  :numeric-id  36
  :desc "A rather shifty individual."
  :x-char #\p
  :x-attr #\b
  :alignment '<evil>
  :depth 2
  :rarity 1
  :hitpoints '(8 . 4)
  :armour 12
  :speed 110
  :xp 6
  :abilities '(<bash-door> <open-door> <pick-up-item>)
  :alertness 5
  :vision 20
  :attacks '((<touch> :type <eat-gold> :damage nil)
	     (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>
  :appear-in-group? #'van-novice-appears-in-group?)

(define-monster-kind "warriour-novice" "novice warriour"
  :numeric-id 35
  :desc "He looks inexperienced but tough."
  :x-char #\p
  :x-attr #\u
  :depth 2
  :rarity 1
  :hitpoints '(9 . 4)
  :armour 16
  :speed 110
  :xp 6
  :abilities '(<bash-door> <open-door>)
  :alertness 5
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 6))
	     (<hit> :type <hurt> :damage (1 . 7)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>
  :appear-in-group? #'van-novice-appears-in-group?)

(define-monster-kind "centipede-green" "metallic green centipede"
  :numeric-id  34
  :desc "It is about four feet long and carnivorous."
  :x-char #\c
  :x-attr #\g
  :type '(<animal>)
  :depth 2
  :rarity 1
  :hitpoints '(4 . 4)
  :armour 4
  :speed 120
  :xp 3
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/2))
  :alertness 10
  :vision 5
  :attacks '((<crawl> :type <hurt> :damage (1 . 1))))

(define-monster-kind "mushroom-shrieker" "shrieker mushroom patch"
  :numeric-id  32
  :desc "Yum!  These look quite tasty."
  :x-char #\,
  :x-attr #\R
  :depth 2
  :rarity 1
  :hitpoints '(1 . 1)
  :armour 1
  :speed 110
  :xp 1
  :abilities '(<empty-mind> <stupid> <never-attack> <never-move> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison>)
  :alertness 0
  :vision 4
  :special-abilities '(<shriek> (<frequency> 1/4)))

(define-monster-kind "icky-blubbering" "blubbering icky thing"
  :numeric-id  33
  :desc "It is a smallish, slimy, icky, hungry creature."
  :x-char #\i
  :x-attr #\W
  :depth 2
  :rarity 1
  :hitpoints '(5 . 6)
  :armour 4
  :speed 110
  :xp 8
  :abilities '(<overrun-others> <pick-up-item> <empty-mind> (<random-mover> 1/2))
  :immunities '(<poison>)
  :alertness 10
  :vision 14
  :attacks '((<crawl> :type <eat-food> :damage nil) (<crawl> :type <poison> :damage (1 . 4)))
  :treasures '((<drop-chance> 9/10)))

(define-monster-kind "mage-novice" "novice mage"
  :numeric-id  77
  :numeric-id  38
  :desc "He is leaving behind a trail of dropped spell components."
  :x-char #\p
  :x-attr #\r
  :depth 2
  :rarity 1
  :hitpoints '(6 . 4)
  :armour 6
  :speed 110
  :xp 7
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :alertness 5
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>
  :special-abilities '((<spell> <missile>) (<spell> <confusion>) (<spell> <blindness>) (<spell> <blink>)
		       (<frequency> 1/12))
  :appear-in-group? #'van-novice-appears-in-group?)
;;; end depth 2 monsters

;;; === depth 3
(define-monster-kind "ooze-green" "green ooze"
  :numeric-id  53
  :desc "It's green and it's oozing."
  :x-char #\j
  :x-attr #\g
  :depth 3
  :rarity 2
  :hitpoints '(3 . 4)
  :armour 16
  :speed 120
  :xp 4
  :abilities '(<empty-mind> <stupid> (<random-mover> 1/4) (<random-mover> 1/2))
  :immunities '(<fear> <sleep> <confusion> <poison> <acid>)
  :alertness 80
  :vision 8
  :attacks '((<crawl> :type <acid> :damage (1 . 3)))
  :treasures '((<drop-chance> 9/10)))

(define-monster-kind "poltergeist" "poltergeist"
  :numeric-id  54
  :desc "It is a ghastly, ghostly form."
  :x-char #\G
  :x-attr #\W
  :alignment '<evil>
  :type '(<undead>)
  :depth 3
  :rarity 1
  :hitpoints '(2 . 5)
  :armour 15
  :speed 130
  :xp 8
  :abilities '(<pick-up-item> <pass-wall> <cold-blood> <invisible> (<random-mover> 1/4) (<random-mover> 1/2))
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 8
  :attacks '((<touch> :type <terrify> :damage nil))
  :treasures '((<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<spell> <blink>) (<frequency> 1/15)))

(define-monster-kind "centipede-blue" "metallic blue centipede"
  :numeric-id  55
  :desc "It is about four feet long and carnivorous."
  :x-char #\c
  :x-attr #\b
  :type '(<animal>)
  :depth 3
  :rarity 1
  :hitpoints '(4 . 5)
  :armour 6
  :speed 120
  :xp 7
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/2))
  :alertness 15
  :vision 6
  :attacks '((<crawl> :type <hurt> :damage (1 . 2))))

(define-monster-kind "louse-white" "giant white louse"
  :numeric-id  56
  :desc "It is six inches long."
  :x-char #\l
  :x-attr #\w
  :type '(<animal>)
  :depth 3
  :rarity 1
  :hitpoints '(1 . 1)
  :armour 5
  :speed 120
  :xp 1
  :abilities '(<weird-mind> <breeder> (<random-mover> 1/4) (<random-mover> 1/2))
  :alertness 10
  :vision 6
  :attacks '((<bite> :type <hurt> :damage (1 . 1))))

(define-monster-kind "naga-black" "black naga"
  :numeric-id  57
  :desc "A large black serpent's body with a female torso."
  :x-char #\n
  :x-attr #\D
  :alignment '<evil>
  :depth 3
  :rarity 1
  :hitpoints '(6 . 8)
  :armour 40
  :speed 110
  :xp 20
  :abilities '(<bash-door> (<random-mover> 1/4))
  :alertness 120
  :vision 16
  :attacks '((<crush> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop-chance> 3/5))
  :gender '<female>)

(define-monster-kind "mushroom-spotted" "spotted mushroom patch"
  :numeric-id  58
  :desc "Yum!  It looks quite tasty."
  :x-char #\,
  :x-attr #\o
  :depth 3
  :rarity 1
  :hitpoints '(1 . 1)
  :armour 1
  :speed 110
  :xp 3
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion> <poison>)
  :alertness 0
  :vision 2
  :attacks '((<spore> :type <poison> :damage (2 . 4))))

(define-monster-kind "jelly-silver" "silver jelly"
  :numeric-id  59
  :desc "It is a large pile of silver flesh that sucks all light from its  surroundings."
  :x-char #\j
  :x-attr #\W
  :depth 3
  :rarity 2
  :hitpoints '(10 . 8)
  :armour 1
  :speed 120
  :xp 12
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion> <poison>)
  :vulnerabilities '(<light>)
  :alertness 99
  :vision 2
  :attacks '((<touch> :type <eat-light> :damage (1 . 3))
	     (<touch> :type <eat-light> :damage (1 . 3)))
  :special-abilities '((<spell> <drain-mana>) (<frequency> 1/15)))

(define-monster-kind "jelly-yellow" "yellow jelly"
  :numeric-id  60
  :desc "It's a large pile of yellow flesh."
  :x-char #\j
  :x-attr #\y
  :depth 3
  :rarity 1
  :hitpoints '(10 . 8)
  :armour 1
  :speed 120
  :xp 12
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion> <poison>)
  :vulnerabilities '(<light>)
  :alertness 99
  :vision 2
  :attacks '((<touch> :type <poison> :damage (1 . 3)))
  :special-abilities '((<spell> <drain-mana>) (<frequency> 1/15)))

(define-monster-kind "hobbit-scruffy" "scruffy looking hobbit"
  :numeric-id  61
  :desc "A short little guy, in bedraggled clothes.  He appears to be looking for a good tavern."
  :x-char #\h
  :x-attr #\b
  :alignment '<evil>
  :depth 3
  :rarity 1
  :hitpoints '(3 . 5)
  :armour 8
  :speed 110
  :xp 4
  :abilities '(<bash-door> <open-door> <pick-up-item>)
  :alertness 10
  :vision 16
  :attacks '((<touch> :type <eat-gold> :damage nil) (<hit> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>)

(define-monster-kind "ant-white" "giant white ant"
  :numeric-id  62
  :desc "It is about two feet long and has sharp pincers."
  :x-char #\a
  :x-attr #\w
  :type '(<animal>)
  :depth 3
  :rarity 1
  :hitpoints '(3 . 6)
  :armour 16
  :speed 110
  :xp 7
  :abilities '(<bash-door> <weird-mind>)
  :alertness 80
  :vision 8
  :attacks '((<bite> :type <hurt> :damage (1 . 4))))

(define-monster-kind "mold-yellow" "yellow mold"
  :numeric-id  63
  :desc "It is a strange growth on the dungeon floor."
  :x-char #\m
  :x-attr #\y
  :depth 3
  :rarity 1
  :hitpoints '(8 . 8)
  :armour 10
  :speed 110
  :xp 9
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion> <poison>)
  :alertness 99
  :vision 2
  :attacks '((<hit> :type <hurt> :damage (1 . 4))))

(define-monster-kind "centipede-red" "metallic red centipede"
  :numeric-id  64
  :desc "It is about four feet long and carnivorous."
  :x-char #\c
  :x-attr #\r
  :type '(<animal>)
  :depth 3
  :rarity 1
  :hitpoints '(4 . 8)
  :armour 9
  :speed 120
  :xp 12
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/4))
  :alertness 20
  :vision 8
  :attacks '((<crawl> :type <hurt> :damage (1 . 2))))

(define-monster-kind "worm-yellow" "yellow worm mass"
  :numeric-id  65
  :desc "It is a large slimy mass of worms."
  :x-char #\w
  :x-attr #\y
  :type '(<animal>)
  :depth 3
  :rarity 2
  :hitpoints '(4 . 8)
  :armour 4
  :speed 100
  :xp 4
  :abilities '(<breeder> <weird-mind> <stupid> (<random-mover> 1/4) (<random-mover> 1/2))
  :immunities '(<fear>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 7
  :attacks '((<crawl> :type <lose-dex> :damage (1 . 3))))

(define-monster-kind "worm-clear" "clear worm mass"
  :numeric-id  66
  :desc "It is a disgusting mass of poisonous worms."
  :x-char #\w
  :x-attr #\w
  :type '(<animal>)
  :depth 3
  :rarity 2
  :hitpoints '(4 . 4)
  :armour 1
  :speed 100
  :xp 4
  :abilities '(<breeder> <invisible> <weird-mind> <stupid> (<random-mover> 1/4) (<random-mover> 1/2) <see-through>)
  :immunities '(<fear> <poison>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 7
  :attacks '((<crawl> :type <poison> :damage (1 . 2))))

(define-monster-kind "eye-radiation" "radiation eye"
  :numeric-id  67
  :desc "A disembodied eye, crackling with energy."
  :x-char #\e
  :x-attr #\R
  :depth 3
  :rarity 1
  :hitpoints '(3 . 6)
  :armour 6
  :speed 110
  :xp 6
  :abilities '(<never-move>)
  :immunities '(<fear>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 2
  :attacks '((<gaze> :type <lose-str> :damage (1 . 6)))
  :special-abilities '((<spell> <drain-mana>) (<frequency> 1/11)))
;;; end depth 3 monsters

;;; === depth 4
(define-monster-kind "lizard-cave" "cave lizard"
  :numeric-id  68
  :desc "It is an armoured lizard with a powerful bite."
  :x-char #\R
  :x-attr #\u
  :type '(<animal>)
  :depth 4
  :rarity 1
  :hitpoints '(3 . 6)
  :armour 16
  :speed 110
  :xp 8
  :alertness 80
  :vision 8
  :attacks '((<bite> :type <hurt> :damage (1 . 5))))

(define-monster-kind "ranger-novice" "novice ranger"
  :numeric-id  114
  :numeric-id  69
  :desc "An agile hunter, ready and relaxed."
  :x-char #\p
  :x-attr #\W
  :depth 4
  :rarity 1
  :hitpoints '(6 . 8)
  :armour 6
  :speed 110
  :xp 18
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :alertness 5
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 5)) (<hit> :type <hurt> :damage (1 . 5)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>
  :special-abilities '((<spell> <missile>) (<arrow> 2) (<frequency> 1/9))
  :appear-in-group? #'van-novice-appears-in-group?)

(define-monster-kind "paladin-novice" "novice paladin"
  :numeric-id  118
  :numeric-id  70
  :desc "An adventurer both devoutly religious and skillful in combat."
  :x-char #\p
  :x-attr #\w
  :depth 4
  :rarity 1
  :hitpoints '(6 . 8)
  :armour 16
  :speed 110
  :xp 20
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :alertness 5
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 7)) (<hit> :type <hurt> :damage (1 . 7)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>
  :special-abilities '((<spell> (<cause> 1)) (<spell> <scare>) (<frequency> 1/9))
  :appear-in-group? #'van-novice-appears-in-group?)

(define-monster-kind "jelly-blue" "blue jelly"
  :numeric-id  71
  :desc "It's a large pile of pulsing blue flesh."
  :x-char #\j
  :x-attr #\b
  :depth 4
  :rarity 1
  :hitpoints '(12 . 8)
  :armour 1
  :speed 110
  :xp 14
  :abilities '(<empty-mind> <stupid> <cold-blood> <never-move>)
  :immunities '(<fear> <sleep> <confusion> <cold>)
  :vulnerabilities '(<light>)
  :alertness 99
  :vision 2
  :attacks '((<touch> :type <cold> :damage (1 . 6))))

(define-monster-kind "creeping-copper" "creeping copper coins"
  :numeric-id  72
  :desc "It is a pile of coins."
  :x-char #\$
  :x-attr #\u
  :type '(<animal>)
  :depth 4
  :rarity 2
  :hitpoints '(7 . 8)
  :armour 24
  :speed 100
  :xp 9
  :abilities '(<bash-door> <cold-blood>)
  :immunities '(<sleep> <confusion> <poison>)
  :alertness 10
  :vision 3
  :attacks '((<touch> :type <poison> :damage (2 . 4)) (<hit> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "1d2") <only-drop-gold>))

(define-monster-kind "rat-white" "giant white rat"
  :numeric-id  73
  :desc "It is a very vicious rodent."
  :x-char #\r
  :x-attr #\W
  :type '(<animal>)
  :depth 4
  :rarity 1
  :hitpoints '(2 . 2)
  :armour 7
  :speed 110
  :xp 1
  :abilities '(<breeder> (<random-mover> 1/4))
  :alertness 30
  :vision 8
  :attacks '((<bite> :type <poison> :damage (1 . 3))))

(define-monster-kind "worm-blue" "blue worm mass"
  :numeric-id  74
  :desc "It is a large slimy mass of worms."
  :x-char #\w
  :x-attr #\b
  :type '(<animal>)
  :depth 4
  :rarity 1
  :hitpoints '(5 . 8)
  :armour 12
  :speed 100
  :xp 5
  :abilities '(<breeder> <cold-blood> <weird-mind> <stupid> (<random-mover> 1/4) (<random-mover> 1/2))
  :immunities '(<fear> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 7
  :attacks '((<crawl> :type <cold> :damage (1 . 4))))

(define-monster-kind "snake-grey" "large grey snake"
  :numeric-id  75
  :desc "It is about ten feet long."
  :x-char #\J
  :x-attr #\s
  :type '(<animal>)
  :depth 4
  :rarity 1
  :hitpoints '(6 . 8)
  :armour 41
  :speed 100
  :xp 14
  :abilities '(<bash-door> (<random-mover> 1/4))
  :alertness 50
  :vision 6
  :attacks '((<crush> :type <hurt> :damage (1 . 8)) (<bite> :type <hurt> :damage (1 . 5))))
;;; end depth 4 monsters

;;; === depth 5
(define-monster-kind "naga-green" "green naga"
  :numeric-id  78
  :desc "A large green serpent with a female's torso.  Her green skin glistens with acid."
  :x-char #\n
  :x-attr #\g
  :alignment '<evil>
  :depth 5
  :rarity 1
  :hitpoints '(9 . 8)
  :armour 40
  :speed 110
  :xp 30
  :abilities '(<bash-door> <pick-up-item> (<random-mover> 1/4))
  :immunities '(<acid>)
  :alertness 120
  :vision 18
  :attacks '((<spit> :type <acid> :damage (2 . 6)) (<crush> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop-chance> 3/5))
  :gender '<female>)

(define-monster-kind "ooze-blue" "blue ooze"
  :numeric-id  79
  :desc "It's blue and it's oozing."
  :x-char #\j
  :x-attr #\b
  :depth 5
  :rarity 1
  :hitpoints '(3 . 4)
  :armour 16
  :speed 110
  :xp 7
  :abilities '(<empty-mind> <stupid> (<random-mover> 1/4) (<random-mover> 1/2))
  :immunities '(<fear> <sleep> <confusion> <cold>)
  :alertness 80
  :vision 8
  :attacks '((<crawl> :type <cold> :damage (1 . 4)))
  :treasures '((<drop-chance> 3/5)))

(define-monster-kind "ghost-glutton" "green glutton ghost"
  :numeric-id  80
  :desc "It is a very ugly green ghost with a voracious appetite."
  :x-char #\G
  :x-attr #\g
  :alignment '<evil>
  :type '(<undead>)
  :depth 5
  :rarity 1
  :hitpoints '(3 . 4)
  :armour 20
  :speed 130
  :xp 15
  :abilities '(<pass-wall> <cold-blood> <invisible> (<random-mover> 1/4) (<random-mover> 1/2))
  :immunities '(<sleep> <confusion>)
  :alertness 10
  :vision 10
  :attacks '((<touch> :type <eat-food> :damage (1 . 1)))
  :treasures '((<drop-chance> 9/10) (<drop-chance> 3/5)))

(define-monster-kind "jelly-green" "green jelly"
  :numeric-id  81
  :desc "It is a large pile of pulsing green flesh."
  :x-char #\j
  :x-attr #\g
  :depth 5
  :rarity 1
  :hitpoints '(22 . 8)
  :armour 1
  :speed 120
  :xp 18
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion> <acid>)
  :vulnerabilities '(<light>)
  :alertness 99
  :vision 2
  :attacks '((<touch> :type <acid> :damage (1 . 2))))

(define-monster-kind "kobold-large" "large kobold"
  :numeric-id  82
  :desc "It a man-sized figure with the all too recognizable face of a kobold."
  :x-char #\k
  :x-attr #\b
  :alignment '<evil>
  :depth 5
  :rarity 1
  :hitpoints '(13 . 9)
  :armour 32
  :speed 110
  :xp 25
  :abilities '(<bash-door> <open-door>)
  :immunities '(<poison>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 10)))
  :treasures '((<drop-chance> 9/10)))

(define-monster-kind "kobold-skeleton" "skeleton kobold"
  :numeric-id  83
  :desc "It is a small animated kobold skeleton."
  :x-char #\s
  :x-attr #\w
  :alignment '<evil>
  :type '(<undead>)
  :depth 5
  :rarity 1
  :hitpoints '(5 . 8)
  :armour 26
  :speed 110
  :xp 12
  :abilities '(<bash-door> <open-door> <empty-mind> <cold-blood>)
  :immunities '(<fear> <sleep> <confusion> <poison> <cold>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 6))))

(define-monster-kind "icky-grey" "grey icky thing"
  :numeric-id  84
  :desc "It is a smallish, slimy, icky, nasty creature."
  :x-char #\i
  :x-attr #\s
  :depth 5
  :rarity 1
  :hitpoints '(4 . 8)
  :armour 12
  :speed 110
  :xp 10
  :abilities '(<empty-mind> (<random-mover> 1/2))
  :alertness 15
  :vision 14
  :attacks '((<touch> :type <hurt> :damage (1 . 5))))

(define-monster-kind "eye-disenchanter" "disenchanter eye"
  :numeric-id  85
  :desc "A disembodied eye, crackling with magic."
  :x-char #\e
  :x-attr #\v
  :depth 5
  :rarity 2
  :hitpoints '(7 . 8)
  :armour 10
  :speed 100
  :xp 20
  :abilities '(<never-move> <colour-changing>)
  :immunities '(<fear>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 2
  :attacks '((<gaze> :type <un-bonus> :damage nil))
  :special-abilities '((<spell> <drain-mana>) (<frequency> 1/9)))

(define-monster-kind "worm-red" "red worm mass"
  :numeric-id  86
  :desc "It is a large slimy mass of worms."
  :x-char #\w
  :x-attr #\r
  :type '(<animal>)
  :depth 5
  :rarity 1
  :hitpoints '(5 . 8)
  :armour 12
  :speed 100
  :xp 6
  :abilities '(<bash-door> <breeder> <empty-mind> <stupid> (<random-mover> 1/4) (<random-mover> 1/2))
  :immunities '(<fear> <fire>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 7
  :attacks '((<crawl> :type <fire> :damage (1 . 6))))

(define-monster-kind "snake-copperhead" "copperhead snake"
  :numeric-id  87
  :desc "It has a copper head and sharp venomous fangs."
  :x-char #\J
  :x-attr #\o
  :type '(<animal>)
  :depth 5
  :rarity 1
  :hitpoints '(4 . 6)
  :armour 20
  :speed 110
  :xp 15
  :abilities '(<bash-door> (<random-mover> 1/2))
  :immunities '(<poison>)
  :alertness 1
  :vision 6
  :attacks '((<bite> :type <poison> :damage (2 . 4))))
;;; end depth 5 monsters

;;; === depth 6
(define-monster-kind "mushroom-purple" "purple mushroom patch"
  :numeric-id  88
  :desc "Yum!  It looks quite tasty."
  :x-char #\,
  :x-attr #\v
  :depth 6
  :rarity 2
  :hitpoints '(1 . 1)
  :armour 1
  :speed 110
  :xp 15
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 2
  :attacks '((<spore> :type <lose-con> :damage (1 . 2)) (<spore> :type <lose-con> :damage (1 . 2))
	     (<spore> :type <lose-con> :damage (1 . 2))))

(define-monster-kind "mold-brown" "brown mold"
  :numeric-id  92
  :desc "A strange brown growth on the dungeon floor."
  :x-char #\m
  :x-attr #\u
  :depth 6
  :rarity 1
  :hitpoints '(15 . 8)
  :armour 12
  :speed 110
  :xp 20
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion> <poison>)
  :alertness 99
  :vision 2
  :attacks '((<hit> :type <confusion> :damage (1 . 4))))

(define-monster-kind "bat-brown" "giant brown bat"
  :numeric-id  93
  :desc "It screeches as it attacks."
  :x-char #\b
  :x-attr #\u
  :type '(<animal>)
  :depth 6
  :rarity 1
  :hitpoints '(3 . 8)
  :armour 15
  :speed 130
  :xp 10
  :abilities '((<random-mover> 1/2))
  :alertness 30
  :vision 10
  :attacks '((<bite> :type <hurt> :damage (1 . 3))))

(define-monster-kind "archer-novice" "novice archer"
  :numeric-id  94
  :desc "A nasty little fellow with a bow and arrow."
  :x-char #\p
  :x-attr #\W
  :depth 6
  :rarity 2
  :hitpoints '(6 . 8)
  :armour 10
  :speed 120
  :xp 20
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :alertness 5
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 4)) (<hit> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "1d2") <only-drop-gold>)
  :gender '<male>
  :special-abilities '((<arrow> 1) (<frequency> 1/3)))

(define-monster-kind "creeping-silver" "creeping silver coins"
  :numeric-id  95
  :desc "It is a pile of coins, crawling forward on thousands of tiny legs."
  :x-char #\$
  :x-attr #\s
  :type '(<animal>)
  :depth 6
  :rarity 2
  :hitpoints '(12 . 8)
  :armour 30
  :speed 100
  :xp 18
  :abilities '(<bash-door> <cold-blood>)
  :immunities '(<sleep> <confusion> <poison>)
  :alertness 10
  :vision 4
  :attacks '((<touch> :type <poison> :damage (2 . 6)) (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5) <only-drop-gold>))

(define-monster-kind "snaga" "snaga"
  :numeric-id  96
  :desc "He is one of the many weaker 'slave' orcs, often mistakenly known as a  goblin."
  :x-char #\o
  :x-attr #\U
  :alignment '<evil>
  :type '(<orc>)
  :depth 6
  :rarity 1
  :hitpoints '(8 . 8)
  :armour 32
  :speed 110
  :xp 15
  :abilities '(<bash-door> <open-door>)
  :vulnerabilities '(<light>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>)

(define-monster-kind "snake-rattle" "rattlesnake"
  :numeric-id  97
  :desc "It is recognized by the hard-scaled end of its body that is often rattled  to frighten its prey."
  :x-char #\J
  :x-attr #\r
  :type '(<animal>)
  :depth 6
  :rarity 1
  :hitpoints '(6 . 7)
  :armour 24
  :speed 110
  :xp 20
  :abilities '(<bash-door> (<random-mover> 1/2))
  :immunities '(<poison>)
  :alertness 1
  :vision 6
  :attacks '((<bite> :type <poison> :damage (2 . 5))))
;;; end depth 6 monsters

;;; === depth 7
(define-monster-kind "orc-cave" "cave orc"
  :numeric-id  98
  :desc "He is often found in huge numbers in deep caves."
  :x-char #\o
  :x-attr #\G
  :alignment '<evil>
  :type '(<orc>)
  :depth 7
  :rarity 1
  :hitpoints '(11 . 9)
  :armour 32
  :speed 110
  :xp 20
  :abilities '(<bash-door> <open-door>)
  :vulnerabilities '(<light>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>)

(define-monster-kind "spider-wood" "wood spider"
  :numeric-id  99
  :desc "It scuttles towards you."
  :x-char #\S
  :x-attr #\U
  :type '(<animal>)
  :depth 7
  :rarity 3
  :hitpoints '(3 . 6)
  :armour 16
  :speed 120
  :xp 15
  :abilities '(<bash-door> <weird-mind>)
  :immunities '(<poison>)
  :alertness 80
  :vision 8
  :attacks '((<sting> :type <poison> :damage (1 . 4)) (<bite> :type <hurt> :damage (1 . 3))))

(define-monster-kind "manes" "manes"
  :numeric-id  100
  :desc "It is a minor but aggressive demon."
  :x-char #\u
  :x-attr #\u
  :alignment '<evil>
  :type '(<demon>)
  :depth 7
  :rarity 2
  :hitpoints '(8 . 8)
  :armour 32
  :speed 110
  :xp 16
  :abilities '(<bash-door> <open-door>)
  :immunities '(<fear> <fire>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 8))))

(define-monster-kind "eye-bloodshot" "bloodshot eye"
  :numeric-id  101
  :desc "A disembodied eye, bloodshot and nasty."
  :x-char #\e
  :x-attr #\r
  :depth 7
  :rarity 3
  :hitpoints '(5 . 8)
  :armour 6
  :speed 110
  :xp 15
  :abilities '(<never-move>)
  :immunities '(<fear>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 2
  :attacks '((<gaze> :type <blind> :damage (2 . 6)))
  :special-abilities '((<spell> <drain-mana>) (<frequency> 1/7)))

(define-monster-kind "naga-pink" "pink naga"
  :numeric-id  102
  :desc "A large pink snake with a woman's torso."
  :x-char #\n
  :x-attr #\R
  :alignment '<evil>
  :depth 7
  :rarity 2
  :hitpoints '(11 . 8)
  :armour 40
  :speed 110
  :xp 40
  :abilities '(<bash-door> <pick-up-item> (<random-mover> 1/4))
  :alertness 120
  :vision 20
  :attacks '((<bite> :type <lose-str> :damage (1 . 4)) (<crush> :type <hurt> :damage (1 . 10)))
  :treasures '((<drop-chance> 3/5))
  :gender '<female>)

(define-monster-kind "jelly-pink" "pink jelly"
  :numeric-id  103
  :desc "It is a large pulsating mound of red flesh."
  :x-char #\j
  :x-attr #\R
  :depth 7
  :rarity 1
  :hitpoints '(26 . 8)
  :armour 1
  :speed 110
  :xp 26
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion>)
  :vulnerabilities '(<light>)
  :alertness 99
  :vision 2
  :attacks '((<touch> :type <lose-str> :damage (1 . 5))))

(define-monster-kind "frog-pink" "giant pink frog"
  :numeric-id  104
  :desc "It looks poisonous."
  :x-char #\R
  :x-attr #\R
  :type '(<animal>)
  :depth 7
  :rarity 1
  :hitpoints '(5 . 8)
  :armour 16
  :speed 110
  :xp 16
  :abilities '(<bash-door> (<random-mover> 1/2))
  :alertness 50
  :vision 12
  :attacks '((<bite> :type <lose-str> :damage (2 . 4))))

(define-monster-kind "icky-green" "green icky thing"
  :numeric-id  105
  :desc "It is a smallish, slimy, icky, acidic creature."
  :x-char #\i
  :x-attr #\g
  :depth 7
  :rarity 2
  :hitpoints '(5 . 8)
  :armour 12
  :speed 110
  :xp 18
  :abilities '(<empty-mind> (<random-mover> 1/2))
  :immunities '(<acid>)
  :alertness 20
  :vision 14
  :attacks '((<touch> :type <acid> :damage (2 . 5))))

(define-monster-kind "kobold-zombie" "zombified kobold"
  :numeric-id  106
  :desc "It is an animated kobold corpse.  Flesh falls off in large chunks as it shambles forward."
  :x-char #\z
  :x-attr #\s
  :alignment '<evil>
  :type '(<undead>)
  :depth 7
  :rarity 1
  :hitpoints '(6 . 8)
  :armour 14
  :speed 110
  :xp 14
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confusion> <poison> <cold>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 2)) (<hit> :type <hurt> :damage (1 . 2))))

(define-monster-kind "soul-lost" "lost soul"
  :numeric-id  107
  :desc "It is almost insubstantial."
  :x-char #\G
  :x-attr #\B
  :alignment '<evil>
  :type '(<undead>)
  :depth 7
  :rarity 2
  :hitpoints '(2 . 8)
  :armour 10
  :speed 110
  :xp 18
  :abilities '(<pass-wall> <pick-up-item> <cold-blood> <invisible> (<random-mover> 1/2))
  :immunities '(<sleep> <confusion> <cold>)
  :alertness 10
  :vision 12
  :attacks '((<touch> :type <lose-wis> :damage nil) (<hit> :type <hurt> :damage (2 . 2)))
  :treasures '((<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<spell> <drain-mana>) (<spell> <teleport>) (<frequency> 1/15)))

(define-monster-kind "dark-elf" "dark elf"
  :numeric-id  108
  :desc "An elven figure with jet black skin and white hair, his eyes are large and 
twisted with evil."
  :x-char #\h
  :x-attr #\D
  :alignment '<evil>
  :depth 7
  :rarity 2
  :hitpoints '(7 . 10)
  :armour 16
  :speed 110
  :xp 25
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :vulnerabilities '(<light>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop-chance> 9/10))
  :gender '<male>
  :special-abilities '((<spell> <darkness>) (<spell> <confusion>) (<frequency> 1/10)))
(define-monster-kind "lizard-night" "night lizard"
  :numeric-id  109
  :desc "It is a black lizard with overlapping scales and a powerful jaw."
  :x-char #\R
  :x-attr #\D
  :type '(<animal>)
  :depth 7
  :rarity 2
  :hitpoints '(4 . 8)
  :armour 16
  :speed 110
  :xp 35
  :alertness 30
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 6)) (<bite> :type <hurt> :damage (1 . 6))))
;;; end depth 7 monsters

;;; === depth 8
(define-monster-kind "yeek-brown" "brown yeek"
  :numeric-id  113
  :desc "It is a strange small humanoid."
  :x-char #\y
  :x-attr #\u
  :type '(<animal>)
  :depth 8
  :rarity 1
  :hitpoints '(4 . 8)
  :armour 18
  :speed 110
  :xp 11
  :abilities '(<bash-door> <open-door>)
  :alertness 10
  :vision 18
  :attacks '((<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop-chance> 3/5)))

(define-monster-kind "salamander-giant" "giant salamander"
  :numeric-id  115
  :desc "A large black and yellow lizard.  You'd better run away!"
  :x-char #\R
  :x-attr #\y
  :type '(<animal>)
  :depth 8
  :rarity 1
  :hitpoints '(6 . 7)
  :armour 40
  :speed 110
  :xp 50
  :abilities '((<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<fire>)
  :alertness 1
  :vision 6
  :attacks '((<bite> :type <fire> :damage (3 . 6)))
  :special-abilities '((<breath> <fire>) (<frequency> 1/9)))

(define-monster-kind "mold-green" "green mold"
  :numeric-id  116
  :desc "It is a strange growth on the dungeon floor."
  :x-char #\m
  :x-attr #\g
  :depth 8
  :rarity 2
  :hitpoints '(21 . 8)
  :armour 14
  :speed 110
  :xp 28
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion> <poison> <acid>)
  :alertness 75
  :vision 2
  :attacks '((<hit> :type <terrify> :damage (1 . 4))))

(define-monster-kind "orc-skeleton" "skeleton orc"
  :numeric-id  117
  :desc "It is an animated orc skeleton."
  :x-char #\s
  :x-attr #\w
  :alignment '<evil>
  :type '(<undead> <orc>)
  :depth 8
  :rarity 1
  :hitpoints '(10 . 8)
  :armour 36
  :speed 110
  :xp 26
  :abilities '(<bash-door> <open-door> <empty-mind> <cold-blood>)
  :immunities '(<fear> <sleep> <confusion> <poison> <cold>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 5))))

(define-monster-kind "lemure" "lemure"
  :numeric-id  119
  :desc "It is the larval form of a major demon."
  :x-char #\u
  :x-attr #\U
  :alignment '<evil>
  :type '(<demon>)
  :depth 8
  :rarity 3
  :hitpoints '(13 . 9)
  :armour 32
  :speed 110
  :xp 16
  :abilities '(<bash-door> <open-door>)
  :immunities '(<fear> <fire>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 8))))

(define-monster-kind "orc-hill" "hill orc"
  :numeric-id  120
  :desc "He is a hardy well-weathered survivor."
  :x-char #\o
  :x-attr #\u
  :alignment '<evil>
  :type '(<orc>)
  :depth 8
  :rarity 1
  :hitpoints '(13 . 9)
  :armour 32
  :speed 110
  :xp 25
  :abilities '(<bash-door> <open-door>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 10)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>)

(define-monster-kind "bandit" "bandit"
  :numeric-id  121
  :desc "He is after your cash!"
  :x-char #\p
  :x-attr #\b
  :alignment '<evil>
  :depth 8
  :rarity 2
  :hitpoints '(8 . 8)
  :armour 24
  :speed 110
  :xp 26
  :abilities '(<bash-door> <open-door> <pick-up-item>)
  :alertness 10
  :vision 20
  :attacks '((<touch> :type <eat-gold> :damage nil) (<hit> :type <hurt> :damage (2 . 4)))
  :treasures '((<drop> "1d2"))
  :gender '<male>)
;;; end depth 8 monsters

;;; === depth 9
(define-monster-kind "yeti" "yeti"
  :numeric-id  122
  :desc "A large white figure covered in shaggy fur."
  :x-char #\Y
  :x-attr #\b
  :type '(<animal>)
  :depth 9
  :rarity 3
  :hitpoints '(11 . 9)
  :armour 24
  :speed 110
  :xp 30
  :abilities '(<bash-door> <open-door>)
  :immunities '(<cold>)
  :alertness 10
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 4)) (<claw> :type <hurt> :damage (1 . 3))
	     (<claw> :type <hurt> :damage (1 . 3))))

(define-monster-kind "icky-bloodshot" "bloodshot icky thing"
  :numeric-id  123
  :desc "It is a strange, slimy, icky creature."
  :x-char #\i
  :x-attr #\r
  :depth 9
  :rarity 3
  :hitpoints '(7 . 8)
  :armour 18
  :speed 110
  :xp 24
  :abilities '(<empty-mind> (<random-mover> 1/2))
  :immunities '(<poison>)
  :alertness 20
  :vision 14
  :attacks '((<crawl> :type <acid> :damage (2 . 4)) (<touch> :type <hurt> :damage (1 . 4)))
  :special-abilities '((<spell> <drain-mana>) (<frequency> 1/11)))

(define-monster-kind "rat-grey" "giant grey rat"
  :numeric-id  124
  :desc "It is a rodent of unusual size."
  :x-char #\r
  :x-attr #\s
  :type '(<animal>)
  :depth 9
  :rarity 1
  :hitpoints '(2 . 3)
  :armour 12
  :speed 110
  :xp 2
  :abilities '(<breeder> (<random-mover> 1/4))
  :immunities '(<poison>)
  :alertness 20
  :vision 8
  :attacks '((<bite> :type <poison> :damage (1 . 4))))
(define-monster-kind "harpy-black" "black harpy"
  :numeric-id  125
  :desc "A woman's face on the body of a vicious black bird."
  :x-char #\H
  :x-attr #\D
  :alignment '<evil>
  :type '(<animal>)
  :depth 9
  :rarity 1
  :hitpoints '(3 . 8)
  :armour 22
  :speed 120
  :xp 19
  :abilities '((<random-mover> 1/4))
  :alertness 10
  :vision 16
  :attacks '((<bite> :type <hurt> :damage (1 . 3)) (<claw> :type <hurt> :damage (1 . 2))
	     (<claw> :type <hurt> :damage (1 . 2)))
  :gender '<female>)

(define-monster-kind "orc-shaman" "orc shaman"
  :numeric-id  126
  :desc "An orc dressed in skins who gestures wildly."
  :x-char #\o
  :x-attr #\r
  :alignment '<evil>
  :type '(<orc>)
  :depth 9
  :rarity 1
  :hitpoints '(9 . 8)
  :armour 15
  :speed 110
  :xp 30
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :vulnerabilities '(<light>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop-chance> 9/10))
  :gender '<male>
  :special-abilities '((<spell> <missile>) (<spell> (<cause> 1)) (<spell> <blink>) (<frequency> 1/8)))

(define-monster-kind "baby-dragon-blue" "baby blue dragon"
  :numeric-id  127
  :desc "This hatchling dragon is still soft, its eyes unaccustomed to light and  its scales a pale blue."
  :x-char #\d
  :x-attr #\b
  :alignment '<evil>
  :type '(<dragon>)
  :depth 9
  :rarity 2
  :hitpoints '(10 . 10)
  :armour 30
  :speed 110
  :xp 35
  :abilities '(<bash-door> <open-door> <initial-sleeper> <max-hitpoints>)
  :immunities '(<electricity>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 5)) (<claw> :type <hurt> :damage (1 . 3))
	     (<claw> :type <hurt> :damage (1 . 3)))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5) <only-drop-gold>)
  :special-abilities '((<breath> <electricity>) (<frequency> 1/11)))

(define-monster-kind "baby-dragon-white" "baby white dragon"
  :numeric-id  128
  :desc "This hatchling dragon is still soft, its eyes unaccustomed to light and its scales a pale white."
  :x-char #\d
  :x-attr #\w
  :alignment '<evil>
  :type '(<dragon>)
  :depth 9
  :rarity 2
  :hitpoints '(10 . 10)
  :armour 30
  :speed 110
  :xp 35
  :abilities '(<bash-door> <open-door> <initial-sleeper> <max-hitpoints>)
  :immunities '(<cold>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 5)) (<claw> :type <hurt> :damage (1 . 3))
	     (<claw> :type <hurt> :damage (1 . 3)))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5) <only-drop-gold>)
  :special-abilities '((<breath> <cold>) (<frequency> 1/11)))

(define-monster-kind "baby-dragon-green" "baby green dragon"
  :numeric-id  129
  :desc "This hatchling dragon is still soft its eyes unaccustomed to light and its scales a sickly green."
  :x-char #\d
  :x-attr #\g
  :alignment '<evil>
  :type '(<dragon>)
  :depth 9
  :rarity 2
  :hitpoints '(10 . 10)
  :armour 30
  :speed 110
  :xp 35
  :abilities '(<bash-door> <open-door> <initial-sleeper> <max-hitpoints>)
  :immunities '(<poison>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 5)) (<claw> :type <hurt> :damage (1 . 3))
	     (<claw> :type <hurt> :damage (1 . 3)))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5) <only-drop-gold>)
  :special-abilities '((<breath> <poison>) (<frequency> 1/11)))

(define-monster-kind "baby-dragon-black" "baby black dragon"
  :numeric-id  130
  :desc "This hatchling dragon is still soft, its eyes unaccustomed to light and its scales a dull black."
  :x-char #\d
  :x-attr #\s
  :alignment '<evil>
  :type '(<dragon>)
  :depth 9
  :rarity 2
  :hitpoints '(10 . 10)
  :armour 30
  :speed 110
  :xp 35
  :abilities '(<bash-door> <open-door> <initial-sleeper> <max-hitpoints>)
  :immunities '(<acid>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 5)) (<claw> :type <hurt> :damage (1 . 3))
	     (<claw> :type <hurt> :damage (1 . 3)))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5) <only-drop-gold>)
  :special-abilities '((<breath> <acid>) (<frequency> 1/11)))

(define-monster-kind "baby-dragon-red" "baby red dragon"
  :numeric-id  131
  :desc "This hatchling dragon is still soft, its eyes unaccustomed to light and its scales a pale red."
  :x-char #\d
  :x-attr #\r
  :alignment '<evil>
  :type '(<dragon>)
  :depth 9
  :rarity 2
  :hitpoints '(11 . 10)
  :armour 30
  :speed 110
  :xp 35
  :abilities '(<bash-door> <open-door> <initial-sleeper> <max-hitpoints>)
  :immunities '(<fire>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 5)) (<claw> :type <hurt> :damage (1 . 3))
	     (<claw> :type <hurt> :damage (1 . 3)))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5) <only-drop-gold>)
  :special-abilities '((<breath> <fire>) (<frequency> 1/11)))

(define-monster-kind "ant-pink" "giant pink ant"
  :numeric-id  132
  :desc "It is large and has venomous mandibles."
  :x-char #\a
  :x-attr #\R
  :type '(<animal>)
  :depth 9
  :rarity 2
  :hitpoints '(4 . 8)
  :armour 34
  :speed 110
  :xp 22
  :abilities '(<bash-door> <weird-mind>)
  :alertness 60
  :vision 12
  :attacks '((<sting> :type <lose-str> :damage (1 . 4)) (<bite> :type <hurt> :damage (1 . 4))))

(define-monster-kind "snake-cobra" "king cobra"
  :numeric-id  134
  :desc "It is a large snake with a hooded face."
  :x-char #\J
  :x-attr #\g
  :type '(<animal>)
  :depth 9
  :rarity 2
  :hitpoints '(8 . 10)
  :armour 30
  :speed 110
  :xp 28
  :abilities '(<bash-door> (<random-mover> 1/2))
  :immunities '(<poison>)
  :alertness 1
  :vision 8
  :attacks '((<bite> :type <poison> :damage (3 . 4)) (<spit> :type <blind> :damage (1 . 2))))
;;; end depth 9 monsters

;;; === depth 10 and up
(define-monster-kind "spider-giant" "giant spider"
  :numeric-id  135
  :desc "It is a vast black spider whose bulbous body is bloated with poison."
  :x-char #\S
  :x-attr #\v
  :type '(<animal>)
  :depth 10
  :rarity 2
  :hitpoints '(10 . 10)
  :armour 16
  :speed 110
  :xp 35
  :abilities '(<bash-door> <weird-mind>)
  :immunities '(<poison>)
  :alertness 80
  :vision 8
  :attacks '((<bite> :type <hurt> :damage (1 . 10)) (<bite> :type <poison> :damage (1 . 6))
	     (<bite> :type <poison> :damage (1 . 6)) (<bite> :type <hurt> :damage (1 . 10))))

(define-monster-kind "dark-elf-mage" "dark elven mage"
  :numeric-id  136
  :desc "A dark elven figure, dressed all in black, hurling spells at you."
  :x-char #\h
  :x-attr #\v
  :alignment '<evil>
  :depth 10
  :rarity 1
  :hitpoints '(7 . 10)
  :armour 16
  :speed 120
  :xp 50
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :immunities '(<poison>)
  :vulnerabilities '(<light>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> (<ball> <poison>)) (<spell> <darkness>) (<spell> <missile>) (<spell> <confusion>)
		       (<spell> <blindness>) (<frequency> 1/5)))

(define-monster-kind "dark-elf-warriour" "dark elven warriour"
  :numeric-id 138
  :desc "A dark elven figure in armour and ready with his sword."
  :x-char #\h
  :x-attr #\u
  :alignment '<evil>
  :depth 10
  :rarity 1
  :hitpoints '(10 . 11)
  :armour 16
  :speed 110
  :xp 50
  :abilities '(<bash-door> <open-door>)
  :vulnerabilities '(<light>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 8)) (<hit> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "1d2"))
  :gender '<male>)

(define-monster-kind "mushroom-clear" "clear mushroom patch"
  :numeric-id  139
  :desc "Yum!  It looks quite tasty."
  :x-char #\,
  :x-attr #\w
  :depth 10
  :rarity 2
  :hitpoints '(1 . 1)
  :armour 1
  :speed 120
  :xp 3
  :abilities '(<empty-mind> <stupid> <breeder> <cold-blood> <invisible> <never-move> <see-through>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 4
  :attacks '((<spore> :type <hurt> :damage (1 . 1))))

(define-monster-kind "tick-white" "giant white tick"
  :numeric-id  141
  :desc "It is moving slowly towards you."
  :x-char #\S
  :x-attr #\w
  :type '(<animal>)
  :depth 10
  :rarity 2
  :hitpoints '(12 . 8)
  :armour 40
  :speed 100
  :xp 27
  :abilities '(<bash-door> <weird-mind>)
  :immunities '(<poison>)
  :alertness 20
  :vision 12
  :attacks '((<bite> :type <poison> :damage (2 . 6))))

(define-monster-kind "mold-hairy" "hairy mold"
  :numeric-id  142
  :desc "It is a strange hairy growth on the dungeon floor."
  :x-char #\m
  :x-attr #\o
  :depth 10
  :rarity 2
  :hitpoints '(15 . 8)
  :armour 15
  :speed 110
  :xp 32
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion> <poison>)
  :alertness 70
  :vision 2
  :attacks '((<hit> :type <poison> :damage (1 . 3))))

(define-monster-kind "mold-disenchanter" "disenchanter mold"
  :numeric-id  143
  :desc "It is a strange glowing growth on the dungeon floor."
  :x-char #\m
  :x-attr #\v
  :depth 10
  :rarity 2
  :hitpoints '(16 . 8)
  :armour 20
  :speed 110
  :xp 40
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion> <poison>)
  :alertness 70
  :vision 2
  :attacks '((<touch> :type <un-bonus> :damage (1 . 6)))
  :special-abilities '((<spell> <drain-mana>) (<frequency> 1/11)))

(define-monster-kind "dragon-pseudo" "pseudo dragon"
  :numeric-id  144
  :desc "A small relative of the dragon that inhabits dark caves."
  :x-char #\d
  :x-attr #\o
  :type '(<dragon>)
  :depth 10
  :rarity 2
  :hitpoints '(20 . 10)
  :armour 30
  :speed 110
  :xp 150
  :abilities '(<bash-door> <initial-sleeper> <max-hitpoints>)
  :alertness 40
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 5)) (<claw> :type <hurt> :damage (1 . 3))
	     (<claw> :type <hurt> :damage (1 . 3)))
  :treasures '((<drop-chance> 3/5))
  :special-abilities '((<breath> <darkness>) (<breath> <light>) (<spell> <scare>) (<spell> <confusion>)
		       (<frequency> 1/11)))

(define-monster-kind "tengu" "tengu"
  :numeric-id  145
  :desc "It is a fast-moving demon that blinks quickly in and out of existence; no  other demon matches its teleporting mastery."
  :x-char #\u
  :x-attr #\R
  :alignment '<evil>
  :type '(<demon>)
  :depth 10
  :rarity 1
  :hitpoints '(16 . 9)
  :armour 32
  :speed 120
  :xp 40
  :abilities '(<bash-door> <open-door>)
  :immunities '(<fear> <fire>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 8)))
  :special-abilities '((<spell> <teleport-player>) (<spell> <blink>) (<frequency> 1/3)))

(define-monster-kind "creeping-gold" "creeping gold coins"
  :numeric-id  146
  :desc "It is a pile of coins, crawling forward on thousands of tiny legs."
  :x-char #\$
  :x-attr #\y
  :type '(<animal>)
  :depth 10
  :rarity 3
  :hitpoints '(18 . 8)
  :armour 36
  :speed 100
  :xp 32
  :abilities '(<bash-door> <cold-blood>)
  :immunities '(<sleep> <confusion> <poison>)
  :alertness 10
  :vision 5
  :attacks '((<touch> :type <poison> :damage (3 . 5)) (<hit> :type <hurt> :damage (2 . 5)))
  :treasures '((<drop> "1d2") (<drop-chance> 9/10) <only-drop-gold>))

(define-monster-kind "wolf" "wolf"
  :numeric-id  147
  :desc "It howls and snaps at you."
  :x-char #\C
  :x-attr #\u
  :type '(<animal>)
  :depth 10
  :rarity 1
  :hitpoints '(6 . 6)
  :armour 30
  :speed 120
  :xp 30
  :abilities '(<bash-door> (<random-mover> 1/4))
  :alertness 20
  :vision 30
  :attacks '((<bite> :type <hurt> :damage (1 . 6))))

(define-monster-kind "fly-fruit" "giant fruit fly"
  :numeric-id  148
  :desc "A fast-breeding, annoying pest."
  :x-char #\I
  :x-attr #\G
  :type '(<animal>)
  :depth 10
  :rarity 6
  :hitpoints '(2 . 2)
  :armour 14
  :speed 120
  :xp 4
  :abilities '(<weird-mind> <breeder> (<random-mover> 1/4) (<random-mover> 1/2))
  :alertness 10
  :vision 8
  :attacks '((<bite> :type <hurt> :damage (1 . 2))))

(define-monster-kind "panther" "panther"
  :numeric-id  149
  :desc "A large black cat, stalking you with intent.  It thinks you're its next  meal."
  :x-char #\f
  :x-attr #\u
  :type '(<animal>)
  :depth 10
  :rarity 2
  :hitpoints '(10 . 8)
  :armour 30
  :speed 120
  :xp 25
  :abilities '(<bash-door>)
  :alertness 0
  :vision 40
  :attacks '((<claw> :type <hurt> :damage (1 . 8)) (<claw> :type <hurt> :damage (1 . 8))))

(define-monster-kind "brigand" "brigand"
  :numeric-id  150
  :desc "He is eyeing your backpack."
  :x-char #\p
  :x-attr #\b
  :alignment '<evil>
  :depth 10
  :rarity 2
  :hitpoints '(9 . 8)
  :armour 32
  :speed 110
  :xp 35
  :abilities '(<bash-door> <open-door> <pick-up-item>)
  :alertness 10
  :vision 20
  :attacks '((<touch> :type <eat-item> :damage nil) (<hit> :type <hurt> :damage (2 . 4)))
  :treasures '((<drop> "1d2"))
  :gender '<male>)

(define-monster-kind "baby-dragon-mh" "baby multi-hued dragon"
  :numeric-id  151
  :desc "This hatchling dragon is still soft, its eyes unaccustomed to light and its scales shimmering with a hint of colour."
  :x-char #\d
  :x-attr #\v
  :alignment '<evil>
  :type '(<dragon>)
  :depth 11
  :rarity 2
  :hitpoints '(13 . 10)
  :armour 30
  :speed 110
  :xp 45
  :abilities '(<bash-door> <open-door> <initial-sleeper> <max-hitpoints> <colour-changing>)
  :immunities '(<poison> <electricity> <cold> <fire> <acid>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 5)) (<claw> :type <hurt> :damage (1 . 3))
	     (<claw> :type <hurt> :damage (1 . 3)))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5) <only-drop-gold>)
  :special-abilities '((<breath> <poison>) (<breath> <electricity>) (<breath> <cold>) (<breath> <fire>) (<breath> <acid>)
		       (<frequency> 1/11)))

(define-monster-kind "hippogriff" "hippogriff"
  :numeric-id  152
  :desc "A strange hybrid of eagle, lion and horse.  It looks weird."
  :x-char #\H
  :x-attr #\U
  :type '(<animal>)
  :depth 11
  :rarity 1
  :hitpoints '(20 . 9)
  :armour 14
  :speed 110
  :xp 30
  :abilities '(<bash-door>)
  :alertness 10
  :vision 12
  :attacks '((<bite> :type <hurt> :damage (2 . 5)) (<hit> :type <hurt> :damage (2 . 5))))

(define-monster-kind "orc-zombie" "zombified orc"
  :numeric-id  153
  :desc "It is a shambling orcish corpse leaving behind a trail of flesh."
  :x-char #\z
  :x-attr #\s
  :alignment '<evil>
  :type '(<undead> <orc>)
  :depth 11
  :rarity 1
  :hitpoints '(11 . 8)
  :armour 24
  :speed 110
  :xp 30
  :abilities '(<bash-door> <open-door> <empty-mind> <cold-blood>)
  :immunities '(<fear> <sleep> <confusion> <poison> <cold>)
  :alertness 25
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 4)) (<hit> :type <hurt> :damage (1 . 4))
	     (<hit> :type <hurt> :damage (1 . 4))))

(define-monster-kind "gnome-mage" "gnome mage"
  :numeric-id  202
  :numeric-id  154
  :desc "A mage of short stature."
  :x-char #\h
  :x-attr #\r
  :alignment '<evil>
  :depth 11
  :rarity 2
  :hitpoints '(7 . 8)
  :armour 20
  :speed 110
  :xp 38
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :alertness 10
  :vision 18
  :attacks '((<hit> :type <hurt> :damage (1 . 5)))
  :treasures '((<drop> "1d2"))
  :gender '<male>
  :special-abilities '((<summon> <monster>) (<spell> (<bolt> <cold>)) (<spell> <darkness>) (<spell> <blink>)
		       (<frequency> 1/4))
  :appear-in-group? #'van-novice-appears-in-group?)

(define-monster-kind "snake-black-mamba" "black mamba"
  :numeric-id  155
  :desc "It has glistening black skin, a sleek body and highly venomous fangs."
  :x-char #\J
  :x-attr #\D
  :type '(<animal>)
  :depth 12
  :rarity 3
  :hitpoints '(10 . 8)
  :armour 32
  :speed 120
  :xp 40
  :abilities '(<bash-door> (<random-mover> 1/2))
  :immunities '(<poison>)
  :alertness 1
  :vision 10
  :attacks '((<bite> :type <poison> :damage (4 . 4))))

(define-monster-kind "wolf-white" "white wolf"
  :numeric-id  156
  :desc "A large and muscled wolf from the northern wastes.  Its breath is cold and  icy and its fur coated in frost."
  :x-char #\C
  :x-attr #\w
  :type '(<animal>)
  :depth 12
  :rarity 1
  :hitpoints '(7 . 7)
  :armour 30
  :speed 120
  :xp 30
  :abilities '(<bash-door> (<random-mover> 1/4))
  :immunities '(<cold>)
  :alertness 20
  :vision 30
  :attacks '((<bite> :type <hurt> :damage (1 . 4)) (<bite> :type <hurt> :damage (1 . 3))))

(define-monster-kind "jelly-grape" "grape jelly"
  :numeric-id  157
  :desc "It is a pulsing mound of glowing flesh."
  :x-char #\j
  :x-attr #\v
  :depth 12
  :rarity 3
  :hitpoints '(52 . 8)
  :armour 1
  :speed 110
  :xp 60
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion> <poison>)
  :vulnerabilities '(<light>)
  :alertness 99
  :vision 2
  :attacks '((<touch> :type <exp-10> :damage nil))
  :special-abilities '((<spell> <drain-mana>) (<frequency> 1/11)))

(define-monster-kind "worm-nether" "nether worm mass"
  :numeric-id  158
  :desc "It is a disgusting mass of dark worms, eating each other, the floor, the air, you...."
  :x-char #\w
  :x-attr #\D
  :type '(<animal>)
  :depth 12
  :rarity 3
  :hitpoints '(5 . 8)
  :armour 15
  :speed 100
  :xp 6
  :abilities '(<bash-door> <breeder> <weird-mind> <stupid> (<random-mover> 1/4) (<random-mover> 1/2))
  :immunities '(<fear>)
  :vulnerabilities '(<light>)
  :alertness 3
  :vision 10
  :attacks '((<touch> :type <exp-10> :damage nil)))

(define-monster-kind "yeek-master" "master yeek"
  :numeric-id  160
  :desc "A small humanoid that radiates some power."
  :x-char #\y
  :x-attr #\U
  :alignment '<evil>
  :type '(<animal>)
  :depth 12
  :rarity 2
  :hitpoints '(12 . 9)
  :armour 24
  :speed 110
  :xp 28
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :alertness 10
  :vision 18
  :attacks '((<hit> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop-chance> 3/5))
  :special-abilities '((<summon> <monster>) (<spell> (<ball> <poison>)) (<spell> <slow>) (<spell> <blindness>)
		       (<spell> <teleport>) (<spell> <blink>) (<frequency> 1/4)))

(define-monster-kind "priest" "priest"
  :numeric-id  161
  :desc "A robed humanoid dedicated to his god."
  :x-char #\p
  :x-attr #\g
  :alignment '<evil>
  :depth 12
  :rarity 1
  :hitpoints '(12 . 8)
  :armour 22
  :speed 110
  :xp 36
  :abilities '(<bash-door> <open-door> <smart> <initial-sleeper>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 3)) (<hit> :type <hurt> :damage (2 . 3)))
  :treasures '((<drop> "1d2"))
  :gender '<male>
  :special-abilities '((<summon> <monster>) (<spell> (<cause> 2)) (<spell> <scare>) (<spell> <heal>) (<frequency> 1/3)))

(define-monster-kind "dark-elf-priest" "dark elven priest"
  :numeric-id  162
  :desc "A dark elven figure, dressed all in black, chanting curses and waiting to
deliver your soul to hell."
  :x-char #\h
  :x-attr #\g
  :alignment '<evil>
  :depth 12
  :rarity 1
  :hitpoints '(7 . 10)
  :armour 30
  :speed 120
  :xp 50
  :abilities '(<bash-door> <open-door> <smart> <initial-sleeper>)
  :vulnerabilities '(<light>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 10)) (<hit> :type <hurt> :damage (1 . 9)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> <darkness>) (<spell> (<cause> 2)) (<spell> <confusion>) (<spell> <blindness>)
		       (<spell> <heal>) (<frequency> 1/5)))

(define-monster-kind "spirit-air" "air spirit"
  :numeric-id  163
  :desc "A whirlwind of sentient air."
  :x-char #\E
  :x-attr #\B
  :alignment '<evil>
  :depth 12
  :rarity 2
  :hitpoints '(8 . 8)
  :armour 40
  :speed 130
  :xp 40
  :abilities '(<bash-door> <cold-blood> <invisible> <empty-mind> (<random-mover> 1/4) (<random-mover> 1/2))
  :immunities '(<fear> <sleep> <confusion> <poison>)
  :alertness 20
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (1 . 3))))

(define-monster-kind "human-skeleton" "skeleton human"
  :numeric-id  164
  :desc "It is an animated human skeleton."
  :x-char #\s
  :x-attr #\w
  :alignment '<evil>
  :type '(<undead>)
  :depth 12
  :rarity 1
  :hitpoints '(10 . 8)
  :armour 30
  :speed 110
  :xp 38
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confusion> <poison> <cold>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 8))))

(define-monster-kind "human-zombie" "zombified human"
  :numeric-id  165
  :desc "It is a shambling human corpse dropping chunks of flesh behind it."
  :x-char #\z
  :x-attr #\s
  :alignment '<evil>
  :type '(<undead>)
  :depth 12
  :rarity 1
  :hitpoints '(12 . 8)
  :armour 24
  :speed 110
  :xp 34
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confusion> <poison> <cold>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 4)) (<hit> :type <hurt> :damage (1 . 4))))

(define-monster-kind "tiger" "tiger"
  :numeric-id  166
  :desc "One of the largest of its species, a sleek orange and black shape creeps
towards you, ready to pounce."
  :x-char #\f
  :x-attr #\o
  :type '(<animal>)
  :depth 12
  :rarity 2
  :hitpoints '(12 . 10)
  :armour 40
  :speed 120
  :xp 40
  :abilities '(<bash-door>)
  :alertness 0
  :vision 40
  :attacks '((<bite> :type <hurt> :damage (1 . 6)) (<claw> :type <hurt> :damage (1 . 8))
	     (<claw> :type <hurt> :damage (1 . 8))))

(define-monster-kind "spirit-moaning" "moaning spirit"
  :numeric-id  167
  :desc "A ghostly apparition that shrieks horribly."
  :x-char #\G
  :x-attr #\u
  :alignment '<evil>
  :type '(<undead>)
  :depth 12
  :rarity 2
  :hitpoints '(5 . 8)
  :armour 20
  :speed 120
  :xp 44
  :abilities '(<pass-wall> <cold-blood> <invisible> (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<sleep> <confusion> <cold>)
  :alertness 10
  :vision 14
  :attacks '((<touch> :type <lose-dex> :damage (1 . 8)) (<wail> :type <terrify> :damage nil))
  :treasures '((<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<spell> <scare>) (<spell> <teleport>) (<frequency> 1/15)))

(define-monster-kind "swordsman" "swordsman"
  :numeric-id  168
  :desc "A warrior of considerable skill."
  :x-char #\p
  :x-attr #\U
  :depth 12
  :rarity 1
  :hitpoints '(12 . 8)
  :armour 34
  :speed 110
  :xp 40
  :abilities '(<bash-door> <open-door>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5)))
  :treasures '((<drop> "1d2"))
  :gender '<male>)

(define-monster-kind "stegocentipede" "stegocentipede"
  :numeric-id  169
  :desc "It is a vast armoured centipede with massive mandibles and a spiked tail."
  :x-char #\c
  :x-attr #\u
  :type '(<animal>)
  :depth 12
  :rarity 2
  :hitpoints '(13 . 8)
  :armour 30
  :speed 120
  :xp 40
  :abilities '(<bash-door> <weird-mind>)
  :alertness 30
  :vision 12
  :attacks '((<sting> :type <hurt> :damage (2 . 4)) (<bite> :type <hurt> :damage (2 . 4))
	     (<bite> :type <hurt> :damage (2 . 4))))

(define-monster-kind "jelly-spotted" "spotted jelly"
  :numeric-id  170
  :desc "A jelly thing."
  :x-char #\j
  :x-attr #\o
  :depth 12
  :rarity 3
  :hitpoints '(13 . 8)
  :armour 18
  :speed 120
  :xp 33
  :abilities '(<cold-blood> <empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion> <poison> <acid>)
  :vulnerabilities '(<light>)
  :alertness 1
  :vision 12
  :attacks '((<touch> :type <acid> :damage (2 . 6)) (<touch> :type <acid> :damage (2 . 6))
	     (<touch> :type <acid> :damage (1 . 10))))

(define-monster-kind "drider" "drider"
  :numeric-id  171
  :desc "A dark elven torso merged with the bloated form of a giant spider."
  :x-char #\S
  :x-attr #\u
  :alignment '<evil>
  :depth 13
  :rarity 2
  :hitpoints '(10 . 13)
  :armour 30
  :speed 110
  :xp 55
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<poison>)
  :alertness 80
  :vision 8
  :attacks '((<bite> :type <poison> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 12))
	     (<hit> :type <hurt> :damage (1 . 12)))
  :special-abilities '((<spell> <darkness>) (<spell> (<cause> 1)) (<spell> <confusion>) (<frequency> 1/8)))

(define-monster-kind "beetle-brown" "killer brown beetle"
  :numeric-id  172
  :desc "It is a vicious insect with a tough carapace."
  :x-char #\K
  :x-attr #\u
  :type '(<animal>)
  :depth 13
  :rarity 2
  :hitpoints '(13 . 8)
  :armour 40
  :speed 110
  :xp 38
  :abilities '(<bash-door> <weird-mind>)
  :alertness 30
  :vision 10
  :attacks '((<bite> :type <hurt> :damage (3 . 4))))

(define-monster-kind "ogre" "ogre"
  :numeric-id  174
  :desc "A hideous, smallish giant that is often found near or with orcs."
  :x-char #\O
  :x-attr #\U
  :alignment '<evil>
  :type '(<giant>)
  :depth 13
  :rarity 2
  :hitpoints '(13 . 9)
  :armour 33
  :speed 110
  :xp 50
  :abilities '(<bash-door> <open-door>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 8)))
  :treasures '((<drop-chance> 3/5)))

(define-monster-kind "creeping-mithril" "creeping mithril coins"
  :numeric-id  175
  :desc "It is a pile of coins, shambling forward on thousands of tiny legs."
  :x-char #\$
  :x-attr #\B
  :type '(<animal>)
  :depth 13
  :rarity 4
  :hitpoints '(20 . 8)
  :armour 50
  :speed 110
  :xp 45
  :abilities '(<bash-door> <cold-blood>)
  :immunities '(<sleep> <confusion> <poison>)
  :alertness 10
  :vision 5
  :attacks '((<touch> :type <poison> :damage (3 . 5)) (<hit> :type <hurt> :damage (2 . 5)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) <only-drop-gold>))

(define-monster-kind "illusionist" "illusionist"
  :numeric-id  176
  :desc "A deceptive spell caster."
  :x-char #\p
  :x-attr #\r
  :alignment '<evil>
  :depth 13
  :rarity 2
  :hitpoints '(12 . 8)
  :armour 10
  :speed 110
  :xp 50
  :abilities '(<bash-door> <open-door> <smart> <initial-sleeper>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 2)))
  :treasures '((<drop> "1d2"))
  :gender '<male>
  :special-abilities '((<spell> <darkness>) (<spell> <confusion>) (<spell> <slow>) (<spell> <paralysis>)
		       (<spell> <blindness>) (<spell> <teleport>) (<spell> <blink>) (<spell> <haste>)
		       (<frequency> 1/3)))

(define-monster-kind "druid" "druid"
  :numeric-id  177
  :desc "A mystic at one with nature.  Om."
  :x-char #\p
  :x-attr #\r
  :alignment '<evil>
  :depth 13
  :rarity 2
  :hitpoints '(12 . 12)
  :armour 10
  :speed 110
  :xp 50
  :abilities '(<bash-door> <open-door> <smart> <initial-sleeper>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 4)) (<hit> :type <hurt> :damage (2 . 4)))
  :treasures '((<drop> "1d2"))
  :gender '<male>
  :special-abilities '((<spell> (<bolt> <electricity>)) (<spell> (<bolt> <fire>)) (<spell> <slow>) (<spell> <paralysis>)
		       (<spell> <blindness>) (<spell> <blink>) (<spell> <haste>) (<frequency> 1/3)))

(define-monster-kind "orc-black" "black orc"
  :numeric-id  178
  :desc "He is a large orc with powerful arms and deep black skin."
  :x-char #\o
  :x-attr #\D
  :alignment '<evil>
  :type '(<orc>)
  :depth 13
  :rarity 2
  :hitpoints '(12 . 10)
  :armour 36
  :speed 110
  :xp 45
  :abilities '(<bash-door> <open-door>)
  :vulnerabilities '(<light>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>)

(define-monster-kind "jelly-ochre" "ochre jelly"
  :numeric-id  179
  :desc "A fast moving highly acidic jelly thing, that is eating away the floor it
rests on."
  :x-char #\j
  :x-attr #\U
  :depth 13
  :rarity 3
  :hitpoints '(13 . 8)
  :armour 18
  :speed 120
  :xp 40
  :abilities '(<bash-door> <open-door> <pick-up-item> <cold-blood> <empty-mind> <stupid>)
  :immunities '(<fear> <sleep> <confusion> <poison> <acid>)
  :alertness 1
  :vision 12
  :attacks '((<touch> :type <acid> :damage (2 . 6)) (<touch> :type <acid> :damage (2 . 6))
	     (<touch> :type <acid> :damage (1 . 10))))

(define-monster-kind "flea-giant" "giant flea"
  :numeric-id  180
  :desc "It makes you itch just to look at it."
  :x-char #\I
  :x-attr #\s
  :type '(<animal>)
  :depth 14
  :rarity 1
  :hitpoints '(2 . 2)
  :armour 25
  :speed 120
  :xp 4
  :abilities '(<bash-door> <weird-mind> <breeder> (<random-mover> 1/4) (<random-mover> 1/2))
  :alertness 10
  :vision 8
  :attacks '((<bite> :type <hurt> :damage (1 . 2))))

(define-monster-kind "dragonfly-white" "giant white dragon fly"
  :numeric-id  182
  :desc "It is a large fly that drips frost."
  :x-char #\F
  :x-attr #\w
  :type '(<animal>)
  :depth 14
  :rarity 3
  :hitpoints '(5 . 8)
  :armour 20
  :speed 110
  :xp 60
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<cold>)
  :alertness 50
  :vision 20
  :attacks '((<bite> :type <cold> :damage (1 . 6)))
  :special-abilities '((<breath> <cold>) (<frequency> 1/10)))

(define-monster-kind "icky-blue" "blue icky thing"
  :numeric-id  183
  :desc "It is a strange, slimy, icky creature, with rudimentary intelligence, 
but evil cunning.  It hungers for food and you look tasty."
  :x-char #\i
  :x-attr #\b
  :alignment '<evil>
  :depth 14
  :rarity 4
  :hitpoints '(10 . 6)
  :armour 20
  :speed 100
  :xp 20
  :abilities '(<bash-door> <open-door> <breeder> (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<poison>)
  :alertness 20
  :vision 15
  :attacks '((<hit> :type <hurt> :damage (1 . 4)) (<hit> :type <hurt> :damage (1 . 4))
	     (<crawl> :type <eat-food> :damage nil) (<crawl> :type <poison> :damage (1 . 4)))
  :special-abilities '((<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/8)))

(define-monster-kind "giant-hill" "hill giant"
  :numeric-id  184
  :desc "A ten foot tall humanoid with powerful muscles."
  :x-char #\P
  :x-attr #\U
  :alignment '<evil>
  :type '(<giant>)
  :depth 14
  :rarity 1
  :hitpoints '(16 . 10)
  :armour 45
  :speed 110
  :xp 60
  :abilities '(<bash-door> <open-door>)
  :alertness 50
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 6)) (<hit> :type <hurt> :damage (3 . 6)))
  :treasures '((<drop-chance> 3/5)))

(define-monster-kind "golem-flesh" "flesh golem"
  :numeric-id  185
  :desc "A shambling humanoid monster with long scars."
  :x-char #\g
  :x-attr #\R
  :depth 14
  :rarity 1
  :hitpoints '(12 . 8)
  :armour 30
  :speed 110
  :xp 50
  :abilities '(<bash-door> <empty-mind>)
  :immunities '(<fear> <sleep> <confusion> <electricity>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 6))))

(define-monster-kind "wolf-warg" "warg"
  :numeric-id  186
  :desc "It is a large wolf with eyes full of cunning."
  :x-char #\C
  :x-attr #\s
  :alignment '<evil>
  :type '(<animal>)
  :depth 14
  :rarity 2
  :hitpoints '(8 . 8)
  :armour 20
  :speed 120
  :xp 40
  :abilities '(<bash-door> (<random-mover> 1/4))
  :alertness 40
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 8))))

(define-monster-kind "louse-black" "giant black louse"
  :numeric-id  187
  :desc "It makes you itch just to look at it."
  :x-char #\l
  :x-attr #\D
  :type '(<animal>)
  :depth 14
  :rarity 1
  :hitpoints '(1 . 2)
  :armour 7
  :speed 120
  :xp 3
  :abilities '(<breeder> <weird-mind> (<random-mover> 1/2))
  :alertness 10
  :vision 6
  :attacks '((<bite> :type <hurt> :damage (1 . 2))))

(define-monster-kind "lurker" "lurker"
  :numeric-id  188
  :desc "A strange creature that merges with the dungeon floor, trapping its
victims by enveloping them within its perfectly disguised form."
  :x-char #\.
  :x-attr #\w
  :depth 14
  :rarity 3
  :hitpoints '(20 . 10)
  :armour 25
  :speed 110
  :xp 80
  :abilities '(<cold-blood> <invisible> <empty-mind> <max-hitpoints> <never-move> <see-through> <absorbs-symbol>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 10
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (1 . 8)) (<hit> :type <hurt> :damage (1 . 8))))

(define-monster-kind "rat-were" "wererat"
  :numeric-id  189
  :desc "A large rat with glowing red eyes.  The wererat is a disgusting creature
relishing in filth and disease."
  :x-char #\r
  :x-attr #\D
  :alignment '<evil>
  :type '(<animal>)
  :depth 15
  :rarity 2
  :hitpoints '(20 . 8)
  :armour 10
  :speed 110
  :xp 45
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :alertness 10
  :vision 10
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<claw> :type <hurt> :damage (1 . 8))
	     (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop-chance> 3/5) <only-drop-gold>)
  :special-abilities '((<spell> (<ball> <poison>)) (<spell> (<bolt> <cold>)) (<spell> (<cause> 2)) (<spell> <blink>)
		       (<frequency> 1/9)))

(define-monster-kind "ogre-black" "black ogre"
  :numeric-id  190
  :desc "A massive orc-like figure with black skin and powerful arms."
  :x-char #\O
  :x-attr #\D
  :alignment '<evil>
  :type '(<giant>)
  :depth 15
  :rarity 2
  :hitpoints '(20 . 9)
  :armour 33
  :speed 110
  :xp 75
  :abilities '(<bash-door> <open-door> (<random-mover> 1/4))
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 8)) (<hit> :type <hurt> :damage (2 . 8)))
  :treasures '((<drop-chance> 3/5)))

(define-monster-kind "mushroom-magic" "magic mushroom patch"
  :numeric-id  191
  :desc "Yum!  It looks quite tasty.  It seems to glow with an unusual light."
  :x-char #\
  :x-attr #\B
  :depth 15
  :rarity 2
  :hitpoints '(1 . 1)
  :armour 10
  :speed 130
  :xp 10
  :abilities '(<stupid> <never-attack> <never-move> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 40
  :special-abilities '((<spell> <darkness>) (<spell> <scare>) (<spell> <slow>) (<spell> <blink>) (<frequency> 1)))

(define-monster-kind "naga-guardian" "guardian naga"
  :numeric-id  192
  :desc "A giant snake-like figure with a woman's torso."
  :x-char #\n
  :x-attr #\B
  :alignment '<evil>
  :depth 15
  :rarity 2
  :hitpoints '(24 . 11)
  :armour 65
  :speed 110
  :xp 80
  :abilities '(<bash-door> <open-door> (<random-mover> 1/4))
  :alertness 120
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 8)) (<bite> :type <hurt> :damage (1 . 8))
	     (<crush> :type <hurt> :damage (2 . 8)))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5))
  :gender '<female>)

(define-monster-kind "hound-light" "light hound"
  :numeric-id  193
  :desc "A brilliant canine form whose light hurts your eyes, even at this distance."
  :x-char #\Z
  :x-attr #\o
  :type '(<animal>)
  :depth 15
  :rarity 1
  :hitpoints '(6 . 6)
  :armour 30
  :speed 110
  :xp 50
  :abilities '(<bash-door> <initial-sleeper>)
  :alertness 0
  :vision 30
  :attacks '((<bite> :type <hurt> :damage (1 . 6)))
  :special-abilities '((<breath> <light>) (<frequency> 1/5)))

(define-monster-kind "hound-dark" "dark hound"
  :numeric-id  194
  :desc "A hole in the air in the shape of a huge hound.  No light falls upon its  form."
  :x-char #\Z
  :x-attr #\D
  :type '(<animal>)
  :depth 15
  :rarity 1
  :hitpoints '(6 . 6)
  :armour 30
  :speed 110
  :xp 50
  :abilities '(<bash-door> <initial-sleeper>)
  :alertness 0
  :vision 30
  :attacks '((<bite> :type <hurt> :damage (1 . 6)))
  :special-abilities '((<breath> <darkness>) (<frequency> 1/5)))

(define-monster-kind "orc-half" "half-orc"
  :numeric-id  195
  :desc "He is a hideous deformed cross-breed with man and orc, combining man's
strength and cunning with orcish evil."
  :x-char #\o
  :x-attr #\s
  :alignment '<evil>
  :type '(<orc>)
  :depth 15
  :rarity 3
  :hitpoints '(16 . 10)
  :armour 40
  :speed 110
  :xp 50
  :abilities '(<bash-door> <open-door>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>)

(define-monster-kind "spider-tarantula" "giant tarantula"
  :numeric-id  196
  :desc "A giant spider with hairy black and red legs."
  :x-char #\S
  :x-attr #\o
  :type '(<animal>)
  :depth 15
  :rarity 3
  :hitpoints '(10 . 15)
  :armour 32
  :speed 120
  :xp 70
  :abilities '(<bash-door> <weird-mind>)
  :immunities '(<poison>)
  :alertness 80
  :vision 8
  :attacks '((<bite> :type <poison> :damage (1 . 6)) (<bite> :type <poison> :damage (1 . 6))
	     (<bite> :type <poison> :damage (1 . 6))))

(define-monster-kind "centipede-clear" "giant clear centipede"
  :numeric-id  197
  :desc "It is about four feet long and carnivorous."
  :x-char #\c
  :x-attr #\w
  :type '(<animal>)
  :depth 15
  :rarity 2
  :hitpoints '(5 . 8)
  :armour 30
  :speed 110
  :xp 30
  :abilities '(<bash-door> <weird-mind> <invisible> <see-through>)
  :alertness 30
  :vision 12
  :attacks '((<sting> :type <hurt> :damage (2 . 4)) (<bite> :type <hurt> :damage (2 . 4))))

(define-monster-kind "spider-mirkwood" "mirkwood spider"
  :numeric-id  198
  :desc "A strong and powerful spider from Mirkwood forest.  Cunning and evil, it
seeks to taste your juicy insides."
  :x-char #\S
  :x-attr #\g
  :alignment '<evil>
  :type '(<animal>)
  :depth 15
  :rarity 2
  :hitpoints '(9 . 8)
  :armour 25
  :speed 120
  :xp 25
  :abilities '(<bash-door> <weird-mind>)
  :immunities '(<poison>)
  :alertness 80
  :vision 15
  :attacks '((<bite> :type <poison> :damage (1 . 6)) (<bite> :type <poison> :damage (1 . 6))
	     (<bite> :type <hurt> :damage (1 . 8))))

(define-monster-kind "giant-frost" "frost giant"
  :numeric-id  199
  :desc "A twelve foot tall giant covered in furs."
  :x-char #\P
  :x-attr #\w
  :alignment '<evil>
  :type '(<giant>)
  :depth 15
  :rarity 1
  :hitpoints '(17 . 10)
  :armour 50
  :speed 110
  :xp 75
  :abilities '(<bash-door> <open-door>)
  :immunities '(<cold>)
  :alertness 50
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 8)) (<hit> :type <cold> :damage (3 . 6)))
  :treasures '((<drop-chance> 3/5)))

(define-monster-kind "griffon" "griffon"
  :numeric-id  200
  :desc "It is half lion, half eagle.  It flies menacingly towards you."
  :x-char #\H
  :x-attr #\u
  :type '(<animal>)
  :depth 15
  :rarity 1
  :hitpoints '(30 . 8)
  :armour 15
  :speed 110
  :xp 70
  :abilities '(<bash-door>)
  :alertness 10
  :vision 12
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<hit> :type <hurt> :damage (3 . 4))))

(define-monster-kind "homonculous" "homonculous"
  :numeric-id  201
  :desc "It is a small demonic spirit full of malevolence."
  :x-char #\u
  :x-attr #\y
  :alignment '<evil>
  :type '(<demon>)
  :depth 15
  :rarity 3
  :hitpoints '(8 . 8)
  :armour 32
  :speed 110
  :xp 40
  :abilities '(<bash-door> <open-door>)
  :immunities '(<fear> <fire>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 10)) (<hit> :type <paralyse> :damage (1 . 2))))

(define-monster-kind "hound-clear" "clear hound"
  :numeric-id  203
  :desc "A completely translucent hound."
  :x-char #\Z
  :x-attr #\w
  :type '(<animal>)
  :depth 15
  :rarity 2
  :hitpoints '(10 . 6)
  :armour 30
  :speed 110
  :xp 50
  :abilities '(<bash-door> <invisible> <see-through>)
  :alertness 0
  :vision 30
  :attacks '((<bite> :type <hurt> :damage (1 . 6)) (<bite> :type <hurt> :damage (1 . 6))
	     (<bite> :type <hurt> :damage (1 . 6))))

(define-monster-kind "golem-clay" "clay golem"
  :numeric-id  204
  :desc "It is a massive animated statue made out of hardened clay."
  :x-char #\g
  :x-attr #\U
  :depth 15
  :rarity 2
  :hitpoints '(14 . 8)
  :armour 30
  :speed 110
  :xp 50
  :abilities '(<bash-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (1 . 8)) (<hit> :type <hurt> :damage (1 . 8))))

(define-monster-kind "umber-hulk" "umber hulk"
  :numeric-id  205
  :desc "This bizarre creature has glaring eyes and large mandibles capable of  slicing through rock."
  :x-char #\X
  :x-attr #\U
  :alignment '<evil>
  :type '(<animal>)
  :depth 16
  :rarity 1
  :hitpoints '(20 . 10)
  :armour 50
  :speed 110
  :xp 75
  :abilities '(<destroy-wall> <bash-door> <cold-blood> <empty-mind>)
  :immunities '(<sleep> <confusion> <poison>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 10
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<hit> :type <hurt> :damage (1 . 6))
	     (<hit> :type <hurt> :damage (1 . 6)) (<gaze> :type <confusion> :damage nil)))
(define-monster-kind "orc-captain" "orc captain"
  :numeric-id  206
  :desc "An armoured orc with an air of authority."
  :x-char #\o
  :x-attr #\o
  :alignment '<evil>
  :type '(<orc>)
  :depth 16
  :rarity 3
  :hitpoints '(20 . 10)
  :armour 59
  :speed 110
  :xp 40
  :abilities '(<bash-door> <open-door>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4))
	     (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop-chance> 9/10))
  :gender '<male>)

(define-monster-kind "gelatinous-cube" "gelatinous cube"
  :numeric-id  207
  :desc "It is a strange, vast gelatinous structure that assumes cubic proportions
as it lines all four walls of the corridors it patrols.  Through its
transparent jelly structure you can see treasures it has engulfed and a
few corpses as well."
  :x-char #\j
  :x-attr #\G
  :depth 16
  :rarity 4
  :hitpoints '(36 . 10)
  :armour 18
  :speed 110
  :xp 80
  :abilities '(<bash-door> <open-door> <pick-up-item> <cold-blood> <empty-mind> <stupid> <max-hitpoints>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 1
  :vision 12
  :attacks '((<touch> :type <acid> :damage (1 . 10)) (<touch> :type <acid> :damage (1 . 10))
	     (<touch> :type <acid> :damage (1 . 10)))
  :treasures '((<drop> "4d2") (<drop> "1d2")))

(define-monster-kind "dragonfly-green" "giant green dragon fly"
  :numeric-id  208
  :desc "A vast, foul-smelling dragonfly."
  :x-char #\F
  :x-attr #\g
  :type '(<animal>)
  :depth 16
  :rarity 2
  :hitpoints '(3 . 8)
  :armour 20
  :speed 110
  :xp 70
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/4) (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<poison>)
  :alertness 50
  :vision 12
  :attacks '((<bite> :type <poison> :damage (1 . 6)))
  :special-abilities '((<breath> <poison>) (<frequency> 1/10)))

(define-monster-kind "giant-fire" "fire giant"
  :numeric-id  209
  :desc "A glowing fourteen foot tall giant.  Flames drip from its red skin."
  :x-char #\P
  :x-attr #\r
  :alignment '<evil>
  :type '(<giant>)
  :depth 16
  :rarity 2
  :hitpoints '(20 . 8)
  :armour 60
  :speed 110
  :xp 54
  :abilities '(<bash-door> <open-door>)
  :immunities '(<fire>)
  :alertness 50
  :vision 20
  :attacks '((<hit> :type <fire> :damage (3 . 7)) (<hit> :type <fire> :damage (3 . 7)))
  :treasures '((<drop-chance> 3/5)))

(define-monster-kind "hummerhorn" "hummerhorn"
  :numeric-id  210
  :desc "A giant buzzing wasp, its stinger drips venom."
  :x-char #\I
  :x-attr #\y
  :type '(<animal>)
  :depth 16
  :rarity 5
  :hitpoints '(2 . 2)
  :armour 14
  :speed 120
  :xp 4
  :abilities '(<weird-mind> <breeder> (<random-mover> 1/4) (<random-mover> 1/2))
  :alertness 10
  :vision 8
  :attacks '((<bite> :type <confusion> :damage (2 . 2))))

(define-monster-kind "imp" "imp"
  :numeric-id  213
  :desc "The lawful evil master's favourite pet."
  :x-char #\u
  :x-attr #\g
  :alignment '<evil>
  :type '(<demon>)
  :depth 17
  :rarity 2
  :hitpoints '(6 . 8)
  :armour 30
  :speed 110
  :xp 55
  :abilities '(<bash-door> <cold-blood> <invisible> <smart> (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<fire>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <poison> :damage (3 . 4)) (<hit> :type <poison> :damage (3 . 4)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :special-abilities '((<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>) (<spell> <teleport-level>)
		       (<spell> <teleport-player>) (<spell> <teleport>) (<spell> <blink>) (<frequency> 1/10)))

(define-monster-kind "troll-forest" "forest troll"
  :numeric-id  214
  :desc "He is green skinned and ugly."
  :x-char #\T
  :x-attr #\g
  :alignment '<evil>
  :type '(<troll>)
  :depth 17
  :rarity 1
  :hitpoints '(20 . 10)
  :armour 50
  :speed 110
  :xp 70
  :abilities '(<bash-door> <open-door>)
  :vulnerabilities '(<light>)
  :alertness 40
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 4))
	     (<hit> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>)

(define-monster-kind "hydra-2" "2-headed hydra"
  :numeric-id  216
  :desc "A strange reptilian hybrid with two heads, guarding its hoard."
  :x-char #\M
  :x-attr #\u
  :type '(<animal>)
  :depth 17
  :rarity 2
  :hitpoints '(100 . 3)
  :armour 60
  :speed 110
  :xp 80
  :abilities '(<push-others> <bash-door> <open-door> <initial-sleeper>)
  :alertness 20
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<bite> :type <hurt> :damage (2 . 6)))
  :treasures '((<drop> "1d2") <only-drop-gold>)
  :special-abilities '((<spell> <scare>) (<frequency> 1/11)))

(define-monster-kind "spirit-water" "water spirit"
  :numeric-id  217
  :desc "A whirlpool of sentient liquid."
  :x-char #\E
  :x-attr #\s
  :alignment '<evil>
  :depth 17
  :rarity 1
  :hitpoints '(9 . 8)
  :armour 28
  :speed 120
  :xp 58
  :abilities '(<bash-door> <cold-blood> <empty-mind> (<random-mover> 1/4))
  :immunities '(<fear> <sleep> <confusion> <poison>)
  :alertness 40
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (2 . 4)) (<hit> :type <hurt> :damage (2 . 4))))

(define-monster-kind "scorpion-pink" "giant pink scorpion"
  :numeric-id  218
  :desc "It is fast and poisonous."
  :x-char #\S
  :x-attr #\R
  :type '(<animal>)
  :depth 17
  :rarity 1
  :hitpoints '(11 . 8)
  :armour 44
  :speed 110
  :xp 62
  :abilities '(<bash-door> <weird-mind>)
  :alertness 20
  :vision 12
  :attacks '((<sting> :type <lose-str> :damage (1 . 7)) (<bite> :type <hurt> :damage (2 . 4))))

(define-monster-kind "spirit-earth" "earth spirit"
  :numeric-id  219
  :desc "A whirling form of sentient rock."
  :x-char #\E
  :x-attr #\u
  :alignment '<evil>
  :depth 17
  :rarity 2
  :hitpoints '(13 . 8)
  :armour 40
  :speed 120
  :xp 64
  :abilities '(<pass-wall> <cold-blood> <empty-mind> (<random-mover> 1/4))
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 50
  :vision 10
  :attacks '((<hit> :type <hurt> :damage (1 . 8)) (<hit> :type <hurt> :damage (1 . 8))))

(define-monster-kind "spirit-fire" "fire spirit"
  :numeric-id  220
  :desc "A whirlwind of sentient flame."
  :x-char #\E
  :x-attr #\r
  :alignment '<evil>
  :depth 18
  :rarity 2
  :hitpoints '(10 . 9)
  :armour 30
  :speed 120
  :xp 75
  :abilities '(<bash-door> <empty-mind> (<random-mover> 1/4))
  :immunities '(<fear> <sleep> <confusion> <poison> <fire>)
  :alertness 20
  :vision 16
  :attacks '((<hit> :type <fire> :damage (2 . 6)) (<hit> :type <fire> :damage (2 . 6))))

(define-monster-kind "hound-fire" "fire hound"
  :numeric-id  221
  :desc "Flames lick at its feet and its tongue is a blade of fire.  You can feel a
furnace heat radiating from the creature."
  :x-char #\Z
  :x-attr #\r
  :type '(<animal>)
  :depth 18
  :rarity 1
  :hitpoints '(10 . 6)
  :armour 30
  :speed 110
  :xp 70
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<fire>)
  :alertness 0
  :vision 30
  :attacks '((<bite> :type <fire> :damage (1 . 3)) (<bite> :type <fire> :damage (1 . 3))
	     (<bite> :type <fire> :damage (1 . 3)))
  :special-abilities '((<breath> <fire>) (<frequency> 1/10)))

(define-monster-kind "hound-cold" "cold hound"
  :numeric-id  222
  :desc "A hound as tall as a man this creature appears to be composed of angular
planes of ice.  Cold radiates from it and freezes your breath in the air."
  :x-char #\Z
  :x-attr #\w
  :type '(<animal>)
  :depth 18
  :rarity 1
  :hitpoints '(10 . 6)
  :armour 30
  :speed 110
  :xp 70
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<cold>)
  :alertness 0
  :vision 30
  :attacks '((<bite> :type <hurt> :damage (1 . 6)) (<claw> :type <hurt> :damage (1 . 8))
	     (<bite> :type <cold> :damage (1 . 6)))
  :special-abilities '((<breath> <cold>) (<frequency> 1/10)))

(define-monster-kind "hound-energy" "energy hound"
  :numeric-id  223
  :desc "Saint Elmo's Fire forms a ghostly halo around this hound, and sparks sting 
your fingers as energy builds up in the air around you."
  :x-char #\Z
  :x-attr #\b
  :type '(<animal>)
  :depth 18
  :rarity 1
  :hitpoints '(10 . 6)
  :armour 30
  :speed 110
  :xp 70
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<electricity>)
  :alertness 0
  :vision 30
  :attacks '((<bite> :type <electricity> :damage (1 . 3)) (<bite> :type <electricity> :damage (1 . 3))
	     (<bite> :type <electricity> :damage (1 . 3)))
  :special-abilities '((<breath> <electricity>) (<frequency> 1/10)))

(define-monster-kind "mimic-potion" "mimic (potion)"
  :numeric-id  224
  :desc "A strange creature that disguises itself as discarded objects to lure
unsuspecting adventurers within reach of its venomous claws."
  :x-char #\!
  :x-attr #\w
  :depth 18
  :rarity 3
  :hitpoints '(10 . 10)
  :armour 30
  :speed 110
  :xp 60
  :abilities '(<cold-blood> <empty-mind> <never-move> <initial-sleeper> <changes-symbol>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 25
  :attacks '((<hit> :type <hurt> :damage (2 . 3)) (<hit> :type <hurt> :damage (2 . 3))
	     (<hit> :type <poison> :damage (3 . 4)))
  :special-abilities '((<spell> (<bolt> <cold>)) (<spell> (<cause> 2)) (<spell> <scare>) (<spell> <confusion>)
		       (<spell> <blindness>) (<frequency> 1/6)))

(define-monster-kind "dog-blink" "blink dog"
  :numeric-id  225
  :desc "A strange magical member of the canine race, its form seems to shimmer and
fade in front of your very eyes."
  :x-char #\C
  :x-attr #\B
  :type '(<animal>)
  :depth 18
  :rarity 2
  :hitpoints '(8 . 8)
  :armour 20
  :speed 120
  :xp 50
  :abilities '(<bash-door> (<random-mover> 1/4))
  :alertness 10
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 8)))
  :special-abilities '((<spell> <teleport-player>) (<spell> <blink>) (<frequency> 1/4)))

(define-monster-kind "orc-uruk" "uruk"
  :numeric-id  226
  :desc "He is a cunning orc of power, as tall as a man, and stronger.  It fears little."
  :x-char #\o
  :x-attr #\B
  :alignment '<evil>
  :type '(<orc>)
  :depth 18
  :rarity 1
  :hitpoints '(8 . 10)
  :armour 50
  :speed 110
  :xp 68
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<poison>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>)

(define-monster-kind "mound-shambling" "shambling mound"
  :numeric-id  229
  :desc "A pile of rotting vegetation that slides towards you with a disgusting stench, 
waking all it nears."
  :x-char #\
  :x-attr #\W
  :alignment '<evil>
  :depth 18
  :rarity 2
  :hitpoints '(20 . 6)
  :armour 16
  :speed 110
  :xp 75
  :abilities '(<bash-door> <open-door> <empty-mind> <stupid>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 8)) (<hit> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop-chance> 9/10) <only-drop-gold>)
  :special-abilities '(<shriek> (<frequency> 1/4)))

(define-monster-kind "giant-stone" "stone giant"
  :numeric-id  230
  :desc "It is eighteen feet tall and looking at you."
  :x-char #\P
  :x-attr #\W
  :alignment '<evil>
  :type '(<giant>)
  :depth 18
  :rarity 1
  :hitpoints '(24 . 8)
  :armour 75
  :speed 110
  :xp 90
  :abilities '(<bash-door> <open-door> <pick-up-item>)
  :alertness 50
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (3 . 8)))
  :treasures '((<drop-chance> 3/5)))

(define-monster-kind "dragonfly-black" "giant black dragon fly"
  :numeric-id  231
  :desc "The size of a large bird this fly drips caustic acid."
  :x-char #\F
  :x-attr #\s
  :type '(<animal>)
  :depth 18
  :rarity 2
  :hitpoints '(3 . 8)
  :armour 20
  :speed 120
  :xp 68
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/4) (<random-mover> 1/2) <never-attack> <initial-sleeper>)
  :immunities '(<acid>)
  :alertness 50
  :vision 12
  :special-abilities '((<breath> <acid>) (<frequency> 1/9)))

(define-monster-kind "golem-stone" "stone golem"
  :numeric-id  232
  :desc "It is a massive animated statue."
  :x-char #\g
  :x-attr #\W
  :depth 19
  :rarity 2
  :hitpoints '(28 . 8)
  :armour 75
  :speed 100
  :xp 100
  :abilities '(<bash-door> <empty-mind> <cold-blood>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (1 . 10)) (<hit> :type <hurt> :damage (1 . 10))))

(define-monster-kind "mold-red" "red mold"
  :numeric-id  233
  :desc "It is a strange red growth on the dungeon floor; it seems to burn with flame."
  :x-char #\m
  :x-attr #\r
  :depth 19
  :rarity 1
  :hitpoints '(17 . 8)
  :armour 16
  :speed 110
  :xp 64
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confusion> <poison> <fire>)
  :alertness 70
  :vision 2
  :attacks '((<touch> :type <fire> :damage (4 . 4))))

(define-monster-kind "dragonfly-gold" "giant gold dragon fly"
  :numeric-id  234
  :desc "Large beating wings support this dazzling insect.  A loud buzzing noise  pervades the air."
  :x-char #\F
  :x-attr #\y
  :type '(<animal>)
  :depth 18
  :rarity 2
  :hitpoints '(3 . 8)
  :armour 20
  :speed 120
  :xp 78
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/4) (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<fire>)
  :alertness 50
  :vision 12
  :attacks '((<bite> :type <hurt> :damage (1 . 3)))
  :special-abilities '((<breath> <sound>) (<frequency> 1/9)))
(define-monster-kind "spider-phase" "phase spider"
  :numeric-id  236
  :desc "A spider that never seems quite there.  Everywhere you look it is just  half-seen in the corner of one eye."
  :x-char #\S
  :x-attr #\B
  :type '(<animal>)
  :depth 20
  :rarity 2
  :hitpoints '(6 . 8)
  :armour 25
  :speed 120
  :xp 60
  :abilities '(<bash-door> <weird-mind>)
  :immunities '(<poison>)
  :alertness 80
  :vision 15
  :attacks '((<bite> :type <poison> :damage (1 . 6)) (<bite> :type <poison> :damage (1 . 6))
	     (<bite> :type <hurt> :damage (1 . 8)))
  :special-abilities '((<spell> <teleport-player>) (<spell> <blink>) (<frequency> 1/5)))
(define-monster-kind "hydra-3" "3-headed hydra"
  :numeric-id  237
  :desc "A strange reptilian hybrid with three heads, guarding its hoard."
  :x-char #\M
  :x-attr #\o
  :type '(<animal>)
  :depth 20
  :rarity 2
  :hitpoints '(100 . 5)
  :armour 65
  :speed 120
  :xp 350
  :abilities '(<push-others> <bash-door> <open-door> <initial-sleeper>)
  :alertness 20
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<bite> :type <hurt> :damage (2 . 6))
	     (<bite> :type <hurt> :damage (2 . 6)))
  :treasures '((<drop> "2d2") (<drop> "1d2") <only-drop-gold>)
  :special-abilities '((<spell> <scare>) (<frequency> 1/9)))

(define-monster-kind "hound-earth" "earth hound"
  :numeric-id  238
  :desc "A beautiful crystalline shape does not disguise the danger this hound  clearly presents.  Your flesh tingles as it approaches."
  :x-char #\Z
  :x-attr #\u
  :type '(<animal>)
  :depth 20
  :rarity 1
  :hitpoints '(15 . 8)
  :armour 30
  :speed 110
  :xp 200
  :abilities '(<bash-door> <initial-sleeper>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<claw> :type <hurt> :damage (3 . 3))
	     (<bite> :type <hurt> :damage (1 . 8)) (<bite> :type <hurt> :damage (1 . 8)))
  :special-abilities '((<breath> <shards>) (<frequency> 1/10)))

(define-monster-kind "hound-air" "air hound"
  :numeric-id  239
  :desc "Swirling vapours surround this beast as it floats towards you, seemingly
walking on air.  Noxious gases sting your throat."
  :x-char #\Z
  :x-attr #\g
  :type '(<animal>)
  :depth 20
  :rarity 1
  :hitpoints '(15 . 8)
  :armour 30
  :speed 110
  :xp 200
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<poison>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<claw> :type <hurt> :damage (3 . 3))
	     (<bite> :type <hurt> :damage (1 . 8)) (<bite> :type <hurt> :damage (1 . 8)))
  :special-abilities '((<breath> <poison>) (<frequency> 1/10)))

(define-monster-kind "tiger-sabre-tooth" "sabre-tooth tiger"
  :numeric-id  240
  :desc "A fierce and dangerous cat, its huge tusks and sharp claws would lacerate  even the strongest armour."
  :x-char #\f
  :x-attr #\y
  :type '(<animal>)
  :depth 20
  :rarity 2
  :hitpoints '(20 . 14)
  :armour 50
  :speed 120
  :xp 120
  :abilities '(<bash-door>)
  :alertness 0
  :vision 40
  :attacks '((<bite> :type <hurt> :damage (1 . 10)) (<bite> :type <hurt> :damage (1 . 10))
	     (<claw> :type <hurt> :damage (1 . 10)) (<claw> :type <hurt> :damage (1 . 10))))

(define-monster-kind "hound-water" "water hound"
  :numeric-id  241
  :desc "Liquid footprints follow this hound as it pads around the dungeon.  An acrid smell of acid rises from the dog's pelt."
  :x-char #\Z
  :x-attr #\s
  :type '(<animal>)
  :depth 20
  :rarity 2
  :hitpoints '(15 . 8)
  :armour 30
  :speed 110
  :xp 200
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<acid>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<claw> :type <hurt> :damage (3 . 3))
	     (<bite> :type <acid> :damage (2 . 8)) (<bite> :type <acid> :damage (2 . 8)))
  :special-abilities '((<breath> <acid>) (<frequency> 1/10)))

(define-monster-kind "chimera" "chimera"
  :numeric-id  242
  :desc "It is a strange concoction of lion, dragon and goat.  It looks very odd, 
but very avoidable."
  :x-char #\H
  :x-attr #\r
  :depth 20
  :rarity 1
  :hitpoints '(13 . 8)
  :armour 15
  :speed 110
  :xp 200
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<fire>)
  :alertness 10
  :vision 12
  :attacks '((<bite> :type <fire> :damage (1 . 3)) (<bite> :type <fire> :damage (1 . 3))
	     (<bite> :type <hurt> :damage (1 . 10)))
  :special-abilities '((<breath> <fire>) (<frequency> 1/10)))

(define-monster-kind "quylthulg" "quylthulg"
  :numeric-id  243
  :desc "It is a strange pulsing mound of flesh."
  :x-char #\Q
  :x-attr #\y
  :depth 20
  :rarity 1
  :hitpoints '(6 . 8)
  :armour 1
  :speed 110
  :xp 250
  :abilities '(<invisible> <empty-mind> <never-attack> <never-move> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 10
  :special-abilities '((<summon> <monster>) (<spell> <blink>) (<frequency> 1/4)))

(define-monster-kind "sasquatch" "sasquatch"
  :numeric-id  244
  :desc "A tall shaggy, furry humanoid, it could call the yeti brother."
  :x-char #\Y
  :x-attr #\g
  :type '(<animal>)
  :depth 20
  :rarity 3
  :hitpoints '(20 . 19)
  :armour 40
  :speed 120
  :xp 180
  :abilities '(<bash-door> <open-door>)
  :immunities '(<cold>)
  :alertness 10
  :vision 15
  :attacks '((<bite> :type <hurt> :damage (2 . 8)) (<claw> :type <hurt> :damage (1 . 10))
	     (<claw> :type <hurt> :damage (1 . 10))))

(define-monster-kind "wolf-were" "werewolf"
  :numeric-id  245
  :desc "It is a huge wolf with eyes that glow with manly intelligence."
  :x-char #\C
  :x-attr #\D
  :alignment '<evil>
  :type '(<animal>)
  :depth 20
  :rarity 1
  :hitpoints '(20 . 22)
  :armour 30
  :speed 110
  :xp 150
  :abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/4))
  :alertness 70
  :vision 15
  :attacks '((<bite> :type <hurt> :damage (1 . 10)) (<bite> :type <hurt> :damage (1 . 6))
	     (<bite> :type <hurt> :damage (1 . 6))))

(define-monster-kind "dark-elf-lord" "dark elven lord"
  :numeric-id  246
  :desc "A dark elven figure in armour and radiating evil power."
  :x-char #\h
  :x-attr #\D
  :alignment '<evil>
  :depth 20
  :rarity 2
  :hitpoints '(18 . 15)
  :armour 40
  :speed 120
  :xp 500
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :vulnerabilities '(<light>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 8)))
  :treasures '((<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> (<bolt> <cold>)) (<spell> (<bolt> <fire>)) (<spell> <darkness>) (<spell> <confusion>)
		       (<spell> <blindness>) (<spell> <haste>) (<frequency> 1/5)))

(define-monster-kind "giant-cloud" "cloud giant"
  :numeric-id  247
  :desc "It is a twenty foot tall giant wreathed in clouds."
  :x-char #\P
  :x-attr #\b
  :alignment '<evil>
  :type '(<giant>)
  :depth 20
  :rarity 1
  :hitpoints '(24 . 10)
  :armour 60
  :speed 110
  :xp 125
  :abilities '(<bash-door> <open-door> <pick-up-item>)
  :immunities '(<electricity>)
  :alertness 50
  :vision 20
  :attacks '((<hit> :type <electricity> :damage (3 . 8)) (<hit> :type <electricity> :damage (3 . 8)))
  :treasures '((<drop-chance> 9/10)))

(define-monster-kind "bat-blue" "blue dragon bat"
  :numeric-id  250
  :desc "It is a glowing blue bat with a sharp tail."
  :x-char #\b
  :x-attr #\b
  :type '(<animal>)
  :depth 21
  :rarity 1
  :hitpoints '(4 . 4)
  :armour 26
  :speed 130
  :xp 54
  :abilities '(<bash-door> (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<electricity>)
  :alertness 50
  :vision 12
  :attacks '((<bite> :type <electricity> :damage (1 . 3)))
  :special-abilities '((<breath> <electricity>) (<frequency> 1/4)))

(define-monster-kind "mimic-scroll" "mimic (scroll)"
  :numeric-id  251
  :desc "A strange creature that disguises itself as discarded objects to lure  unsuspecting adventurers within reach of its venomous claws."
  :x-char #\?
  :x-attr #\w
  :depth 21
  :rarity 3
  :hitpoints '(10 . 14)
  :armour 40
  :speed 110
  :xp 60
  :abilities '(<cold-blood> <empty-mind> <never-move> <initial-sleeper> <changes-symbol>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (2 . 3)) (<hit> :type <hurt> :damage (2 . 3))
	     (<hit> :type <poison> :damage (3 . 4)) (<hit> :type <poison> :damage (3 . 4)))
  :special-abilities '((<summon> <monster>) (<spell> (<bolt> <fire>)) (<spell> (<cause> 2)) (<spell> <scare>)
		       (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/5)))

(define-monster-kind "vortex-fire" "fire vortex"
  :numeric-id  252
  :desc "A whirling maelstrom of fire."
  :x-char #\v
  :x-attr #\r
  :depth 21
  :rarity 1
  :hitpoints '(9 . 9)
  :armour 30
  :speed 110
  :xp 100
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fear> <fire>)
  :alertness 0
  :vision 100
  :attacks '((<engulf> :type <fire> :damage (3 . 3)))
  :special-abilities '((<breath> <fire>) (<frequency> 1/6)))

(define-monster-kind "vortex-water" "water vortex"
  :numeric-id  253
  :desc "A caustic spinning whirlpool of water."
  :x-char #\v
  :x-attr #\s
  :depth 21
  :rarity 1
  :hitpoints '(9 . 9)
  :armour 30
  :speed 110
  :xp 100
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fear> <acid>)
  :alertness 0
  :vision 100
  :attacks '((<engulf> :type <acid> :damage (3 . 3)))
  :special-abilities '((<breath> <acid>) (<frequency> 1/6)))

(define-monster-kind "vortex-cold" "cold vortex"
  :numeric-id  254
  :desc "A twisting whirlpool of frost."
  :x-char #\v
  :x-attr #\w
  :depth 21
  :rarity 1
  :hitpoints '(9 . 9)
  :armour 30
  :speed 110
  :xp 100
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fear> <cold>)
  :alertness 0
  :vision 100
  :attacks '((<engulf> :type <cold> :damage (3 . 3)))
  :special-abilities '((<breath> <cold>) (<frequency> 1/6)))

(define-monster-kind "vortex-energy" "energy vortex"
  :numeric-id  255
  :desc "A shimmering tornado of air, sparks crackle along its length."
  :x-char #\v
  :x-attr #\b
  :depth 21
  :rarity 1
  :hitpoints '(12 . 12)
  :armour 30
  :speed 110
  :xp 130
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fear> <electricity>)
  :alertness 0
  :vision 100
  :attacks '((<engulf> :type <electricity> :damage (5 . 5)))
  :special-abilities '((<breath> <electricity>) (<frequency> 1/6)))

(define-monster-kind "orc-mummy" "mummified orc"
  :numeric-id  256
  :desc "It is an orcish figure covered in wrappings."
  :x-char #\z
  :x-attr #\w
  :alignment '<evil>
  :type '(<undead> <orc>)
  :depth 21
  :rarity 1
  :hitpoints '(15 . 8)
  :armour 28
  :speed 110
  :xp 56
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confusion> <poison> <cold>)
  :alertness 75
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 4)) (<hit> :type <hurt> :damage (2 . 4)))
  :treasures '((<drop-chance> 9/10)))

(define-monster-kind "beetle-stag" "killer stag beetle"
  :numeric-id  257
  :desc "It is a giant beetle with vicious claws."
  :x-char #\K
  :x-attr #\g
  :type '(<animal>)
  :depth 22
  :rarity 1
  :hitpoints '(20 . 8)
  :armour 55
  :speed 110
  :xp 80
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/4))
  :alertness 30
  :vision 12
  :attacks '((<claw> :type <hurt> :damage (1 . 12)) (<claw> :type <hurt> :damage (1 . 12))))

(define-monster-kind "golem-iron" "iron golem"
  :numeric-id  258
  :desc "It is a massive metal statue that moves steadily towards you."
  :x-char #\g
  :x-attr #\s
  :depth 22
  :rarity 2
  :hitpoints '(80 . 12)
  :armour 80
  :speed 110
  :xp 160
  :abilities '(<bash-door> <cold-blood> <empty-mind> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (1 . 12)))
  :special-abilities '((<spell> <slow>) (<frequency> 1/7)))
(define-monster-kind "scorpion-yellow" "giant yellow scorpion"
  :numeric-id  259
  :desc "It is a giant scorpion with a sharp stinger."
  :x-char #\S
  :x-attr #\y
  :type '(<animal>)
  :depth 22
  :rarity 1
  :hitpoints '(12 . 8)
  :armour 38
  :speed 110
  :xp 60
  :abilities '(<bash-door> <weird-mind>)
  :alertness 20
  :vision 12
  :attacks '((<sting> :type <poison> :damage (2 . 5)) (<bite> :type <hurt> :damage (1 . 8))))

(define-monster-kind "warriour-hardened" "hardened warriour"
  :numeric-id 261
  :desc "A scarred warriour who moves with confidence."
  :x-char #\p
  :x-attr #\u
  :alignment '<evil>
  :depth 23
  :rarity 1
  :hitpoints '(15 . 11)
  :armour 40
  :speed 110
  :xp 60
  :abilities '(<bash-door> <open-door> <pick-up-item>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5)))
  :treasures '((<drop> "1d2"))
  :gender '<male>)
(define-monster-kind "rogue-master" "master rogue"
  :numeric-id  263
  :desc "A thief of great power and shifty speed."
  :x-char #\p
  :x-attr #\B
  :alignment '<evil>
  :depth 23
  :rarity 2
  :hitpoints '(15 . 9)
  :armour 30
  :speed 120
  :xp 110
  :abilities '(<bash-door> <open-door> <pick-up-item>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <eat-gold> :damage (4 . 4)) (<hit> :type <hurt> :damage (2 . 8))
	     (<hit> :type <hurt> :damage (2 . 8)))
  :treasures '((<drop> "2d2"))
  :gender '<male>)
(define-monster-kind "bat-red" "red dragon bat"
  :numeric-id  264
  :desc "It is a sharp-tailed bat, wreathed in fire."
  :x-char #\b
  :x-attr #\r
  :type '(<animal>)
  :depth 23
  :rarity 1
  :hitpoints '(3 . 8)
  :armour 28
  :speed 130
  :xp 60
  :abilities '(<bash-door> (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<fire>)
  :alertness 50
  :vision 12
  :attacks '((<bite> :type <fire> :damage (1 . 3)))
  :special-abilities '((<breath> <fire>) (<frequency> 1/4)))
(define-monster-kind "beetle-white" "killer white beetle"
  :numeric-id  265
  :desc "It is looking for prey."
  :x-char #\K
  :x-attr #\w
  :type '(<animal>)
  :depth 23
  :rarity 1
  :hitpoints '(20 . 8)
  :armour 55
  :speed 110
  :xp 85
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/4))
  :alertness 30
  :vision 14
  :attacks '((<bite> :type <hurt> :damage (4 . 5))))
(define-monster-kind "dragonfly-bronze" "giant bronze dragon fly"
  :numeric-id  266
  :desc "This vast gleaming bronze fly has wings which beat mesmerically fast."
  :x-char #\F
  :x-attr #\U
  :type '(<animal>)
  :depth 18
  :rarity 1
  :hitpoints '(3 . 8)
  :armour 20
  :speed 120
  :xp 70
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/4) (<random-mover> 1/2) <never-attack> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 50
  :vision 12
  :special-abilities '((<breath> <confusion>) (<frequency> 1/9)))

(define-monster-kind "wight-forest" "forest wight"
  :numeric-id  267
  :desc "It is a ghostly apparition with a humanoid form."
  :x-char #\W
  :x-attr #\g
  :alignment '<evil>
  :type '(<undead>)
  :depth 24
  :rarity 1
  :hitpoints '(12 . 8)
  :armour 30
  :speed 110
  :xp 140
  :abilities '(<bash-door> <open-door> <cold-blood> (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 30
  :vision 20
  :attacks '((<touch> :type <exp-20> :damage nil) (<hit> :type <hurt> :damage (1 . 6))
	     (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<spell> <drain-mana>) (<spell> <scare>) (<frequency> 1/10)))

(define-monster-kind "hydra-4" "4-headed hydra"
  :numeric-id  270
  :desc "A strange reptilian hybrid with four heads, guarding its hoard."
  :x-char #\M
  :x-attr #\y
  :type '(<animal>)
  :depth 24
  :rarity 2
  :hitpoints '(100 . 6)
  :armour 70
  :speed 120
  :xp 350
  :abilities '(<push-others> <bash-door> <open-door> <initial-sleeper>)
  :alertness 20
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<bite> :type <hurt> :damage (2 . 6))
	     (<bite> :type <hurt> :damage (2 . 6)) (<bite> :type <hurt> :damage (2 . 6)))
  :treasures '((<drop> "4d2") <only-drop-gold>)
  :special-abilities '((<spell> <scare>) (<frequency> 1/7)))

(define-monster-kind "human-mummy" "mummified human"
  :numeric-id  271
  :desc "It is a human form encased in mouldy wrappings."
  :x-char #\z
  :x-attr #\w
  :alignment '<evil>
  :type '(<undead>)
  :depth 24
  :rarity 1
  :hitpoints '(17 . 9)
  :armour 34
  :speed 110
  :xp 70
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confusion> <poison> <cold>)
  :alertness 60
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 4)) (<hit> :type <hurt> :damage (2 . 4)))
  :treasures '((<drop-chance> 9/10) <only-drop-items>))

(define-monster-kind "bat-vampire" "vampire bat"
  :numeric-id  272
  :desc "An undead bat that flies at your neck hungrily."
  :x-char #\b
  :x-attr #\v
  :alignment '<evil>
  :type '(<undead> <animal>)
  :depth 24
  :rarity 2
  :hitpoints '(9 . 10)
  :armour 40
  :speed 120
  :xp 150
  :abilities '(<regenerate> <cold-blood> (<random-mover> 1/2))
  :immunities '(<fear> <sleep> <confusion> <poison> <cold>)
  :alertness 50
  :vision 12
  :attacks '((<bite> :type <exp-40> :damage (1 . 4)) (<bite> :type <exp-40> :damage (1 . 4))))

(define-monster-kind "banshee" "banshee"
  :numeric-id  275
  :desc "It is a ghostly woman's form that wails mournfully."
  :x-char #\G
  :x-attr #\b
  :alignment '<evil>
  :type '(<undead>)
  :depth 24
  :rarity 2
  :hitpoints '(6 . 8)
  :armour 24
  :speed 120
  :xp 60
  :abilities '(<pass-wall> <pick-up-item> <cold-blood> <invisible> (<random-mover> 1/2))
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :alertness 10
  :vision 20
  :attacks '((<touch> :type <exp-20> :damage nil) (<wail> :type <terrify> :damage nil))
  :treasures '((<drop> "1d2"))
  :gender '<female>
  :special-abilities '((<spell> <drain-mana>) (<spell> <teleport>) (<frequency> 1/15)))

(define-monster-kind "pukelman" "pukelman"
  :numeric-id  276
  :desc "A stumpy figure carved from stone, with glittering eyes, this sentinel  strides towards you with deadly intent."
  :x-char #\g
  :x-attr #\D
  :depth 25
  :rarity 3
  :hitpoints '(80 . 12)
  :armour 80
  :speed 110
  :xp 600
  :abilities '(<bash-door> <cold-blood> <empty-mind> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (3 . 6)) (<hit> :type <hurt> :damage (1 . 12)))
  :special-abilities '((<spell> (<bolt> <acid>)) (<spell> <confusion>) (<spell> <slow>) (<frequency> 1/4)))

(define-monster-kind "dark-elf-druid" "dark elven druid"
  :numeric-id  277
  :desc "A powerful dark elf, with mighty nature-controlling enchantments."
  :x-char #\h
  :x-attr #\g
  :alignment '<evil>
  :depth 25
  :rarity 3
  :hitpoints '(20 . 20)
  :armour 75
  :speed 120
  :xp 500
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 15
  :attacks '((<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (1 . 7))
	     (<hit> :type <hurt> :damage (1 . 7)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <spider>) (<summon> <monster>) (<spell> <darkness>) (<spell> <confusion>)
		       (<spell> <heal>) (<frequency> 1/6)))

(define-monster-kind "troll-stone" "stone troll"
  :numeric-id  278
  :desc "He is a giant troll with scabrous black skin."
  :x-char #\T
  :x-attr #\W
  :alignment '<evil>
  :type '(<troll>)
  :depth 25
  :rarity 1
  :hitpoints '(23 . 10)
  :armour 40
  :speed 110
  :xp 85
  :abilities '(<bash-door> <open-door>)
  :vulnerabilities '(<earth-destruction> <light>)
  :alertness 50
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (1 . 6))
	     (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>)
(define-monster-kind "troll-priest" "troll priest"
  :numeric-id  279
  :desc "A troll who is so bright he knows how to read."
  :x-char #\T
  :x-attr #\G
  :alignment '<evil>
  :type '(<troll>)
  :depth 25
  :rarity 1
  :hitpoints '(30 . 10)
  :armour 50
  :speed 110
  :xp 100
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :vulnerabilities '(<light>)
  :alertness 30
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (1 . 8))
	     (<hit> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop-chance> 9/10))
  :gender '<male>
  :special-abilities '((<spell> <darkness>) (<spell> <missile>) (<spell> (<cause> 1)) (<spell> <scare>)
		       (<spell> <blink>) (<frequency> 1/5)))
(define-monster-kind "worm-were" "wereworm"
  :numeric-id  280
  :desc "A huge wormlike shape dripping acid
twisted by evil sorcery into a foul  monster that breeds on death."
  :x-char #\w
  :x-attr #\D
  :type '(<animal>)
  :depth 25
  :rarity 3
  :hitpoints '(100 . 11)
  :armour 70
  :speed 110
  :xp 300
  :abilities '(<bash-door>)
  :immunities '(<acid>)
  :alertness 20
  :vision 15
  :attacks '((<bite> :type <poison> :damage (1 . 6)) (<bite> :type <hurt> :damage (1 . 10))
	     (<crawl> :type <acid> :damage (2 . 4)) (<gaze> :type <exp-20> :damage nil)))
(define-monster-kind "carrion-crawler" "carrion crawler"
  :numeric-id  353
  :numeric-id  281
  :desc "A hideous centipede covered in slime and with glowing tentacles around its  head."
  :x-char #\c
  :x-attr #\o
  :type '(<animal>)
  :depth 25
  :rarity 2
  :hitpoints '(20 . 12)
  :armour 40
  :speed 110
  :xp 60
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/4))
  :immunities '(<poison>)
  :alertness 10
  :vision 15
  :attacks '((<sting> :type <paralyse> :damage (2 . 6)) (<sting> :type <paralyse> :damage (2 . 6)))
  ;; might need to be slightly adjusted
  :appear-in-group? #'van-novice-appears-in-group?)
(define-monster-kind "beetle-pink" "killer pink beetle"
  :numeric-id  282
  :desc "It is a giant beetle with poisonous mandibles."
  :x-char #\K
  :x-attr #\R
  :type '(<animal>)
  :depth 25
  :rarity 2
  :hitpoints '(20 . 8)
  :armour 50
  :speed 110
  :xp 85
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/4))
  :alertness 30
  :vision 14
  :attacks '((<bite> :type <lose-str> :damage (4 . 4))))
(define-monster-kind "ant-grey" "giant grey ant"
  :numeric-id  283
  :desc "It is an ant encased in shaggy grey fur."
  :x-char #\a
  :x-attr #\s
  :type '(<animal>)
  :depth 26
  :rarity 1
  :hitpoints '(19 . 8)
  :armour 40
  :speed 110
  :xp 90
  :abilities '(<bash-door> <weird-mind> <overrun-others> (<random-mover> 1/4))
  :alertness 40
  :vision 10
  :attacks '((<bite> :type <hurt> :damage (2 . 12))))

(define-monster-kind "displacer-beast" "displacer beast"
  :numeric-id  285
  :desc "It is a huge black panther, clubbed tentacles sprouting from its shoulders."
  :x-char #\f
  :x-attr #\D
  :type '(<animal>)
  :depth 26
  :rarity 2
  :hitpoints '(25 . 10)
  :armour 100
  :speed 110
  :xp 100
  :abilities '(<bash-door> <invisible>)
  :alertness 20
  :vision 35
  :attacks '((<hit> :type <hurt> :damage (1 . 10)) (<hit> :type <hurt> :damage (1 . 10))
	     (<hit> :type <hurt> :damage (1 . 10)) (<bite> :type <hurt> :damage (2 . 8))))

(define-monster-kind "tick-red" "giant red tick"
  :numeric-id  286
  :desc "It is smoking and burning with great heat."
  :x-char #\S
  :x-attr #\r
  :type '(<animal>)
  :depth 26
  :rarity 1
  :hitpoints '(16 . 8)
  :armour 54
  :speed 110
  :xp 90
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/4))
  :immunities '(<fire>)
  :alertness 20
  :vision 14
  :attacks '((<bite> :type <fire> :damage (3 . 6))))
(define-monster-kind "ogre-cave" "cave ogre"
  :numeric-id  287
  :desc "A giant orc-like figure with an awesomely muscled frame."
  :x-char #\O
  :x-attr #\u
  :alignment '<evil>
  :type '(<giant>)
  :depth 26
  :rarity 1
  :hitpoints '(30 . 9)
  :armour 33
  :speed 110
  :xp 42
  :abilities '(<bash-door> <open-door>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (3 . 8)))
  :treasures '((<drop-chance> 3/5)))
(define-monster-kind "wraith-white" "white wraith"
  :numeric-id  288
  :desc "It is a tangible but ghostly form made of white fog."
  :x-char #\W
  :x-attr #\w
  :alignment '<evil>
  :type '(<undead>)
  :depth 26
  :rarity 1
  :hitpoints '(15 . 8)
  :armour 40
  :speed 110
  :xp 175
  :abilities '(<bash-door> <open-door> <cold-blood> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<touch> :type <exp-20> :damage nil) (<hit> :type <hurt> :damage (1 . 6))
	     (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop> "1d2"))
  :special-abilities '((<spell> <darkness>) (<spell> (<cause> 2)) (<spell> <scare>) (<frequency> 1/8)))
(define-monster-kind "deva-monadic" "monadic deva"
  :numeric-id  289
  :desc "A lesser angel wearing little more than a loincloth - its steely skin  provides all the protection it needs."
  :x-char #\A
  :x-attr #\o
  :depth 26
  :rarity 6
  :hitpoints '(30 . 10)
  :armour 60
  :speed 110
  :xp 220
  :abilities '(<powerful-breath> <bash-door> <open-door> <pick-up-item> <initial-sleeper> <max-hitpoints>)
  :immunities '(<sleep> <confusion> <poison> <acid>)
  :alertness 255
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4))
	     (<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop> "2d2") <only-drop-items>)
  :special-abilities '((<spell> <forget>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
		       (<frequency> 1/3)))
(define-monster-kind "beetle-red" "killer red beetle"
  :numeric-id  291
  :desc "It is a giant beetle wreathed in flames."
  :x-char #\K
  :x-attr #\r
  :type '(<animal>)
  :depth 27
  :rarity 1
  :hitpoints '(13 . 8)
  :armour 45
  :speed 110
  :xp 95
  :abilities '(<bash-door> <weird-mind>)
  :immunities '(<fire>)
  :alertness 30
  :vision 14
  :attacks '((<spit> :type <fire> :damage (4 . 5)) (<bite> :type <hurt> :damage (3 . 4))))

(define-monster-kind "creeping-adamantite" "creeping adamantite coins"
  :numeric-id  292
  :desc "It is a pile of coins, slithering forward on thousands of tiny legs."
  :x-char #\$
  :x-attr #\G
  :type '(<animal>)
  :depth 27
  :rarity 4
  :hitpoints '(20 . 25)
  :armour 50
  :speed 120
  :xp 45
  :abilities '(<bash-door> <cold-blood>)
  :immunities '(<sleep> <confusion> <poison>)
  :alertness 10
  :vision 5
  :attacks '((<hit> :type <hurt> :damage (1 . 12)) (<hit> :type <hurt> :damage (1 . 12))
	     (<touch> :type <poison> :damage (3 . 5)) (<bite> :type <poison> :damage (3 . 4)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) <only-drop-gold>))

(define-monster-kind "algroth" "algroth"
  :numeric-id  293
  :desc "A powerful troll form.  Venom drips from its needlelike claws."
  :x-char #\T
  :x-attr #\o
  :alignment '<evil>
  :type '(<troll>)
  :depth 27
  :rarity 1
  :hitpoints '(21 . 12)
  :armour 60
  :speed 110
  :xp 150
  :abilities '(<bash-door> <open-door>)
  :immunities '(<poison>)
  :alertness 40
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 6)) (<claw> :type <poison> :damage (3 . 3))
	     (<claw> :type <poison> :damage (3 . 3)))
  :treasures '((<drop-chance> 3/5)))

(define-monster-kind "hound-vibration" "vibration hound"
  :numeric-id  294
  :desc "A blurry canine form which seems to be moving as fast as the eye can  follow.  You can feel the earth resonating beneath your feet."
  :x-char #\Z
  :x-attr #\y
  :type '(<animal>)
  :depth 27
  :rarity 3
  :hitpoints '(25 . 10)
  :armour 30
  :speed 110
  :xp 250
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<claw> :type <hurt> :damage (3 . 3))
	     (<bite> :type <hurt> :damage (2 . 6)) (<bite> :type <hurt> :damage (2 . 6)))
  :special-abilities '((<breath> <sound>) (<frequency> 1/5)))

(define-monster-kind "hound-nexus" "nexus hound"
  :numeric-id  295
  :desc "A locus of conflicting points coalesce to form the vague shape of a huge  hound.  Or is it just your imagination?"
  :x-char #\Z
  :x-attr #\R
  :type '(<animal>)
  :depth 27
  :rarity 3
  :hitpoints '(25 . 10)
  :armour 30
  :speed 110
  :xp 250
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<claw> :type <hurt> :damage (3 . 3))
	     (<bite> :type <hurt> :damage (2 . 8)) (<bite> :type <hurt> :damage (2 . 8)))
  :special-abilities '((<breath> <nexus>) (<frequency> 1/5)))
(define-monster-kind "ogre-mage" "ogre mage"
  :numeric-id  296
  :desc "A hideous ogre wrapped in black sorcerous robes."
  :x-char #\O
  :x-attr #\r
  :alignment '<evil>
  :type '(<giant>)
  :depth 27
  :rarity 2
  :hitpoints '(30 . 12)
  :armour 40
  :speed 110
  :xp 300
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (3 . 8))
	     (<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (3 . 8)))
  :treasures '((<drop> "1d2"))
  :special-abilities '((<summon> <monster>) (<spell> (<ball> <cold>)) (<spell> <traps>) (<spell> <paralysis>)
		       (<spell> <heal>) (<frequency> 1/4)))
(define-monster-kind "vampire" "vampire"
  :numeric-id  298
  :desc "It is a humanoid with an aura of power.  You notice a sharp set of front  teeth."
  :x-char #\V
  :x-attr #\v
  :alignment '<evil>
  :type '(<undead>)
  :depth 27
  :rarity 1
  :hitpoints '(25 . 12)
  :armour 45
  :speed 110
  :xp 175
  :abilities '(<regenerate> <bash-door> <open-door> <cold-blood> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<bite> :type <exp-20> :damage (1 . 4)) (<bite> :type <exp-20> :damage (1 . 4))
	     (<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5))
  :special-abilities '((<spell> <darkness>) (<spell> <forget>) (<spell> <mind-blast>) (<spell> (<cause> 2))
		       (<spell> <scare>) (<spell> <paralysis>) (<spell> <teleport-player>) (<frequency> 1/9)))

(define-monster-kind "gorgimera" "gorgimera"
  :numeric-id  299
  :desc "The result of evil experiments, this travesty of nature should never be 
alive.  It has 3 heads - gorgon, goat and dragon - all attached to a lion's body."
  :x-char #\H
  :x-attr #\o
  :depth 27
  :rarity 2
  :hitpoints '(25 . 20)
  :armour 55
  :speed 110
  :xp 200
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<fire>)
  :alertness 10
  :vision 12
  :attacks '((<gaze> :type <paralyse> :damage (2 . 4)) (<bite> :type <hurt> :damage (1 . 10))
	     (<bite> :type <fire> :damage (1 . 3)) (<bite> :type <fire> :damage (1 . 3)))
  :special-abilities '((<breath> <fire>) (<frequency> 1/8)))

(define-monster-kind "colbran" "colbran"
  :numeric-id  300
  :desc "A man-shaped form of living lightning, sparks and shocks crackle all over this madly capering figure, as it leaps and whirls around and about you."
  :x-char #\g
  :x-attr #\y
  :depth 27
  :rarity 2
  :hitpoints '(80 . 12)
  :armour 80
  :speed 120
  :xp 900
  :abilities '(<bash-door> <cold-blood> <empty-mind> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <electricity> :damage (3 . 8)) (<hit> :type <electricity> :damage (3 . 8)))
  :special-abilities '((<spell> (<bolt> <electricity>)) (<frequency> 1/3)))

(define-monster-kind "naga-spirit" "spirit naga"
  :numeric-id  301
  :desc "A wraithly snake-like form with the torso of a beautiful woman, it is the  most powerful of its kind."
  :x-char #\n
  :x-attr #\w
  :alignment '<evil>
  :depth 28
  :rarity 2
  :hitpoints '(30 . 15)
  :armour 75
  :speed 110
  :xp 60
  :abilities '(<bash-door> <open-door> <invisible> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 120
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 8)) (<bite> :type <hurt> :damage (1 . 8))
	     (<crush> :type <hurt> :damage (2 . 8)) (<crush> :type <hurt> :damage (2 . 8)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) <only-drop-items>)
  :gender '<female>
  :special-abilities '((<spell> <darkness>) (<spell> <mind-blast>) (<spell> <blindness>) (<spell> <heal>)
		       (<frequency> 1/4)))

(define-monster-kind "hydra-5" "5-headed hydra"
  :numeric-id  302
  :desc "A strange reptilian hybrid with five heads dripping venom."
  :x-char #\M
  :x-attr #\g
  :type '(<animal>)
  :depth 28
  :rarity 2
  :hitpoints '(100 . 8)
  :armour 80
  :speed 120
  :xp 350
  :abilities '(<push-others> <bash-door> <initial-sleeper>)
  :immunities '(<poison>)
  :alertness 20
  :vision 20
  :attacks '((<bite> :type <poison> :damage (4 . 4)) (<bite> :type <poison> :damage (4 . 4))
	     (<bite> :type <poison> :damage (4 . 4)) (<bite> :type <poison> :damage (4 . 4)))
  :treasures '((<drop> "4d2") (<drop> "1d2") <only-drop-gold>)
  :special-abilities '((<spell> (<ball> <poison>)) (<spell> <scare>) (<frequency> 1/5)))
(define-monster-kind "knight-black" "black knight"
  :numeric-id  303
  :desc "He is a figure encased in deep black plate armour; he looks at you  menacingly."
  :x-char #\p
  :x-attr #\s
  :alignment '<evil>
  :depth 28
  :rarity 1
  :hitpoints '(30 . 10)
  :armour 70
  :speed 120
  :xp 240
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5))
	     (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '((<drop> "1d2"))
  :gender '<male>
  :special-abilities '((<spell> <darkness>) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <blindness>)
		       (<frequency> 1/8)))
(define-monster-kind "mage" "mage"
  :numeric-id  305
  :desc "A mage of some power - you can tell by the size of his hat."
  :x-char #\p
  :x-attr #\r
  :alignment '<evil>
  :depth 28
  :rarity 1
  :hitpoints '(15 . 8)
  :armour 40
  :speed 110
  :xp 150
  :abilities '(<bash-door> <open-door> <smart> <initial-sleeper>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 5)) (<hit> :type <hurt> :damage (2 . 5)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <monster>) (<spell> (<bolt> <electricity>)) (<spell> (<bolt> <cold>))
		       (<spell> (<bolt> <fire>)) (<spell> <confusion>) (<spell> <blindness>)
		       (<spell> <teleport-player>) (<spell> <teleport>) (<spell> <haste>) (<frequency> 1/3)))

(define-monster-kind "mind-flayer" "mind flayer"
  :numeric-id  306
  :desc "A humanoid form with a gruesome head, tentacular mouth, and piercing  eyes.  Claws reach out for you and you feel a presence invade your mind."
  :x-char #\h
  :x-attr #\R
  :alignment '<evil>
  :depth 28
  :rarity 1
  :hitpoints '(15 . 10)
  :armour 60
  :speed 110
  :xp 200
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 10
  :vision 20
  :attacks '((<gaze> :type <lose-int> :damage (2 . 6)) (<gaze> :type <lose-int> :damage (2 . 6)))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5) <only-drop-items>)
  :special-abilities '((<spell> <forget>) (<spell> <brain-smash>) (<spell> <mind-blast>) (<spell> <scare>)
		       (<spell> <paralysis>) (<spell> <blindness>) (<frequency> 1/8)))

(define-monster-kind "basilisk" "basilisk"
  :numeric-id  308
  :desc "An evil reptile that preys on unsuspecting travellers.  Its eyes stare  deeply at you and your soul starts to wilt!"
  :x-char #\R
  :x-attr #\b
  :type '(<animal>)
  :depth 28
  :rarity 3
  :hitpoints '(20 . 30)
  :armour 90
  :speed 120
  :xp 300
  :abilities '(<bash-door> <open-door>)
  :immunities '(<sleep> <confusion>)
  :alertness 30
  :vision 15
  :attacks '((<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12))
	     (<bite> :type <hurt> :damage (2 . 12)) (<gaze> :type <paralyse> :damage nil))
  :treasures '((<drop> "1d2") <only-drop-items>))

(define-monster-kind "troll-ice" "ice troll"
  :numeric-id  309
  :desc "He is a white troll with powerfully clawed hands."
  :x-char #\T
  :x-attr #\w
  :alignment '<evil>
  :type '(<troll>)
  :depth 28
  :rarity 1
  :hitpoints '(24 . 10)
  :armour 56
  :speed 110
  :xp 160
  :abilities '(<bash-door> <open-door>)
  :immunities '(<cold>)
  :vulnerabilities '(<light>)
  :alertness 50
  :vision 20
  :attacks '((<bite> :type <cold> :damage (3 . 6)) (<hit> :type <hurt> :damage (1 . 5))
	     (<hit> :type <hurt> :damage (1 . 5)) (<hit> :type <hurt> :damage (1 . 5)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>)
(define-monster-kind "worm-purple" "giant purple worm"
  :numeric-id  310
  :desc "It is a massive worm form, many feet in length.  Its vast maw drips acid and poison."
  :x-char #\w
  :x-attr #\v
  :type '(<animal>)
  :depth 29
  :rarity 3
  :hitpoints '(65 . 8)
  :armour 65
  :speed 110
  :xp 400
  :abilities '(<bash-door>)
  :immunities '(<poison> <acid>)
  :alertness 30
  :vision 14
  :attacks '((<sting> :type <poison> :damage (1 . 8)) (<bite> :type <acid> :damage (2 . 8))
	     (<hit> :type <hurt> :damage (1 . 8))))
(define-monster-kind "deva-movanic" "movanic deva"
  :numeric-id  311
  :desc "A lesser angel protected by an aura of holiness.  Its muscular form looks  extremely powerful next to your own frail body."
  :x-char #\A
  :x-attr #\B
  :depth 29
  :rarity 6
  :hitpoints '(40 . 10)
  :armour 68
  :speed 110
  :xp 400
  :abilities '(<powerful-breath> <bash-door> <open-door> <pick-up-item> <smart> <initial-sleeper> <max-hitpoints>)
  :immunities '(<sleep> <confusion> <poison> <cold> <fire>)
  :alertness 255
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5))
	     (<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5)))
  :treasures '((<drop> "2d2") <only-drop-items>)
  :special-abilities '((<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>) (<spell> <haste>) (<spell> <heal>)
		       (<frequency> 1/3)))

(define-monster-kind "catoblepas" "catoblepas"
  :numeric-id  312
  :desc "A strange ox-like form with a huge head but a thin, weak neck, it looks  likes the creation of some deranged alchemist."
  :x-char #\q
  :x-attr #\g
  :type '(<animal>)
  :depth 29
  :rarity 2
  :hitpoints '(30 . 10)
  :armour 55
  :speed 110
  :xp 400
  :abilities '(<bash-door>)
  :immunities '(<poison>)
  :alertness 40
  :vision 15
  :attacks '((<bite> :type <hurt> :damage (2 . 12)) (<butt> :type <hurt> :damage (2 . 6))
	     (<gaze> :type <blind> :damage (2 . 4)) (<gaze> :type <terrify> :damage (2 . 4)))
  :treasures '((<drop> "2d2") <only-drop-gold>))

(define-monster-kind "mimic-ring" "mimic (ring)"
  :numeric-id  313
  :desc "A strange creature that disguises itself as discarded objects to lure  unsuspecting adventurers within reach of its venomous claws."
  :x-char #\=
  :x-attr #\w
  :depth 29
  :rarity 3
  :hitpoints '(10 . 35)
  :armour 60
  :speed 120
  :xp 200
  :abilities '(<cold-blood> <empty-mind> <never-move> <initial-sleeper> <changes-symbol>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 100
  :vision 30
  :attacks '((<hit> :type <poison> :damage (3 . 4)) (<hit> :type <poison> :damage (3 . 4))
	     (<hit> :type <poison> :damage (3 . 4)) (<hit> :type <poison> :damage (3 . 4)))
  :special-abilities '((<summon> <monster>) (<spell> (<bolt> <electricity>)) (<spell> (<bolt> <cold>))
		       (<spell> (<bolt> <fire>)) (<spell> (<bolt> <acid>)) (<spell> <forget>) (<spell> (<cause> 2))
		       (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/4)))
(define-monster-kind "young-dragon-blue" "young blue dragon"
  :numeric-id  314
  :desc "It has a form that legends are made of.  Its still-tender scales are a  deep blue in hue.  Sparks crackle along its length."
  :x-char #\d
  :x-attr #\b
  :alignment '<evil>
  :type '(<dragon>)
  :depth 29
  :rarity 1
  :hitpoints '(27 . 10)
  :armour 50
  :speed 110
  :xp 300
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<electricity>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 6)) (<claw> :type <hurt> :damage (1 . 4))
	     (<claw> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "1d2") (<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<breath> <electricity>) (<spell> <scare>) (<frequency> 1/11)))
(define-monster-kind "young-dragon-white" "young white dragon"
  :numeric-id  315
  :desc "It has a form that legends are made of.  Its still-tender scales are a  frosty white in hue.  Icy blasts of cold air come from it as it breathes."
  :x-char #\d
  :x-attr #\w
  :alignment '<evil>
  :type '(<dragon>)
  :depth 29
  :rarity 1
  :hitpoints '(27 . 10)
  :armour 50
  :speed 110
  :xp 275
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<cold>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 6)) (<claw> :type <hurt> :damage (1 . 4))
	     (<claw> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "1d2") (<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<breath> <cold>) (<spell> <scare>) (<frequency> 1/11)))
(define-monster-kind "young-dragon-green" "young green dragon"
  :numeric-id  316
  :desc "It has a form that legends are made of.  Its still-tender scales are a  deep green in hue.  Foul gas seeps through its scales."
  :x-char #\d
  :x-attr #\g
  :alignment '<evil>
  :type '(<dragon>)
  :depth 29
  :rarity 1
  :hitpoints '(27 . 10)
  :armour 60
  :speed 110
  :xp 290
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<poison>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 6)) (<claw> :type <hurt> :damage (1 . 4))
	     (<claw> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "1d2") (<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<breath> <poison>) (<spell> <scare>) (<frequency> 1/11)))
(define-monster-kind "young-dragon-bronze" "young bronze dragon"
  :numeric-id  317
  :desc "It has a form that legends are made of.  Its still-tender scales are a rich bronze hue, and its shape masks its true form."
  :x-char #\d
  :x-attr #\U
  :type '(<dragon>)
  :depth 29
  :rarity 3
  :hitpoints '(27 . 10)
  :armour 63
  :speed 110
  :xp 310
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 150
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 6)) (<claw> :type <hurt> :damage (1 . 4))
	     (<claw> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<breath> <confusion>) (<spell> <scare>) (<frequency> 1/11)))
(define-monster-kind "golem-mithril" "mithril golem"
  :numeric-id  318
  :desc "It is a massive statue of purest mithril.  It looks expensive!"
  :x-char #\g
  :x-attr #\B
  :depth 30
  :rarity 4
  :hitpoints '(80 . 15)
  :armour 100
  :speed 110
  :xp 500
  :abilities '(<bash-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5))
	     (<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (3 . 8)))
  :treasures '((<drop> "2d2") <only-drop-gold>))
(define-monster-kind "drake-shadow" "shadow drake"
  :numeric-id  319
  :desc "It is a dragon-like form wrapped in shadow.  Glowing red eyes shine out in  the dark."
  :x-char #\d
  :x-attr #\D
  :alignment '<evil>
  :type '(<dragon> <animal>)
  :depth 30
  :rarity 2
  :hitpoints '(20 . 10)
  :armour 50
  :speed 110
  :xp 700
  :abilities '(<bash-door> <open-door> <invisible> <pick-up-item> (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<cold>)
  :alertness 30
  :vision 25
  :attacks '((<bite> :type <cold> :damage (1 . 6)) (<bite> :type <cold> :damage (1 . 6))
	     (<bite> :type <cold> :damage (1 . 6)))
  :treasures '((<drop> "2d2") <only-drop-items>)
  :special-abilities '((<spell> <darkness>) (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>) (<spell> <haste>)
		       (<frequency> 1/6)))
(define-monster-kind "troll-skeleton" "skeleton troll"
  :numeric-id  320
  :desc "It is a troll skeleton animated by dark dweomers."
  :x-char #\s
  :x-attr #\w
  :alignment '<evil>
  :type '(<undead> <troll>)
  :depth 30
  :rarity 1
  :hitpoints '(20 . 10)
  :armour 55
  :speed 110
  :xp 225
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confusion> <poison> <cold>)
  :alertness 20
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (1 . 6))
	     (<hit> :type <hurt> :damage (1 . 6))))
(define-monster-kind "manticore" "manticore"
  :numeric-id  321
  :desc "It is a winged lion's body with a human torso and a tail covered in  vicious spikes."
  :x-char #\H
  :x-attr #\y
  :alignment '<evil>
  :depth 30
  :rarity 2
  :hitpoints '(25 . 10)
  :armour 15
  :speed 120
  :xp 300
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4))
	     (<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4)))
  :special-abilities '((<arrow> 4) (<frequency> 1/5)))
(define-monster-kind "ant-blue" "giant blue ant"
  :numeric-id  322
  :desc "It is a giant ant that crackles with energy."
  :x-char #\a
  :x-attr #\b
  :type '(<animal>)
  :depth 30
  :rarity 2
  :hitpoints '(8 . 8)
  :armour 50
  :speed 110
  :xp 80
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/4))
  :immunities '(<electricity>)
  :alertness 60
  :vision 10
  :attacks '((<bite> :type <electricity> :damage (5 . 5))))

(define-monster-kind "ant-army" "giant army ant"
  :numeric-id  323
  :desc "An armoured form moving with purpose.  Powerful on its own, flee when hordes of them march."
  :x-char #\a
  :x-attr #\o
  :type '(<animal>)
  :depth 30
  :rarity 3
  :hitpoints '(19 . 6)
  :armour 40
  :speed 120
  :xp 90
  :abilities '(<bash-door> <weird-mind> <overrun-others> (<random-mover> 1/4))
  :alertness 40
  :vision 10
  :attacks '((<bite> :type <hurt> :damage (2 . 12))))

(define-monster-kind "wight-grave" "grave wight"
  :numeric-id  324
  :desc "It is a ghostly form with eyes that haunt you."
  :x-char #\W
  :x-attr #\b
  :alignment '<evil>
  :type '(<undead>)
  :depth 30
  :rarity 1
  :hitpoints '(12 . 10)
  :armour 50
  :speed 110
  :xp 325
  :abilities '(<bash-door> <open-door> <cold-blood> (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 30
  :vision 20
  :attacks '((<touch> :type <exp-20> :damage nil) (<hit> :type <hurt> :damage (1 . 7))
	     (<hit> :type <hurt> :damage (1 . 7)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :special-abilities '((<spell> <darkness>) (<spell> (<cause> 3)) (<spell> <scare>) (<frequency> 1/8)))
(define-monster-kind "beetle-slicer" "killer slicer beetle"
  :numeric-id  325
  :desc "It is a beetle with deadly sharp cutting mandibles and a rock-hard  carapace."
  :x-char #\K
  :x-attr #\y
  :type '(<animal>)
  :depth 30
  :rarity 2
  :hitpoints '(22 . 10)
  :armour 60
  :speed 110
  :xp 200
  :abilities '(<bash-door> <weird-mind>)
  :alertness 30
  :vision 14
  :attacks '((<bite> :type <hurt> :damage (5 . 8)) (<bite> :type <hurt> :damage (5 . 8))))
(define-monster-kind "ghost" "ghost"
  :numeric-id  326
  :desc "You don't believe in them."
  :x-char #\G
  :x-attr #\w
  :alignment '<evil>
  :type '(<undead>)
  :depth 31
  :rarity 1
  :hitpoints '(13 . 8)
  :armour 30
  :speed 120
  :xp 350
  :abilities '(<pass-wall> <pick-up-item> <cold-blood> <invisible> (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :alertness 10
  :vision 20
  :attacks '((<claw> :type <lose-wis> :damage (1 . 6)) (<claw> :type <lose-int> :damage (1 . 6))
	     (<touch> :type <exp-20> :damage nil) (<wail> :type <terrify> :damage nil))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5))
  :special-abilities '((<spell> <drain-mana>) (<spell> <paralysis>) (<spell> <blindness>) (<frequency> 1/15)))
(define-monster-kind "beetle-death" "death watch beetle"
  :numeric-id  327
  :desc "It is a giant beetle that produces a chilling sound."
  :x-char #\K
  :x-attr #\D
  :type '(<animal>)
  :depth 31
  :rarity 3
  :hitpoints '(25 . 12)
  :armour 60
  :speed 110
  :xp 190
  :abilities '(<bash-door> <weird-mind>)
  :alertness 30
  :vision 16
  :attacks '((<wail> :type <terrify> :damage (5 . 6)) (<bite> :type <hurt> :damage (5 . 4))))
(define-monster-kind "ogre-shaman" "ogre shaman"
  :numeric-id  328
  :desc "It is an ogre wrapped in furs and covered in grotesque body paints."
  :x-char #\O
  :x-attr #\o
  :alignment '<evil>
  :type '(<giant>)
  :depth 32
  :rarity 2
  :hitpoints '(14 . 10)
  :armour 55
  :speed 110
  :xp 250
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 6)) (<hit> :type <hurt> :damage (3 . 6))
	     (<hit> :type <hurt> :damage (3 . 6)))
  :treasures '((<drop-chance> 9/10) <only-drop-items>)
  :special-abilities '((<summon> <monster>) (<spell> (<bolt> <fire>)) (<spell> <traps>) (<spell> (<cause> 2))
		       (<spell> <scare>) (<spell> <paralysis>) (<spell> <teleport>) (<frequency> 1/5)))

(define-monster-kind "quylthulg-nexus" "nexus quylthulg"
  :numeric-id  329
  :desc "It is a very unstable, strange pulsing mound of flesh."
  :x-char #\Q
  :x-attr #\R
  :depth 32
  :rarity 1
  :hitpoints '(10 . 12)
  :armour 1
  :speed 110
  :xp 300
  :abilities '(<empty-mind> <invisible> <never-attack> <never-move> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 10
  :special-abilities '((<spell> <teleport-away>) (<spell> <blink>) (<frequency> 1)))

(define-monster-kind "ninja" "ninja"
  :numeric-id  331
  :desc "A humanoid clothed in black who moves with blinding speed."
  :x-char #\p
  :x-attr #\y
  :alignment '<evil>
  :depth 32
  :rarity 2
  :hitpoints '(13 . 12)
  :armour 60
  :speed 120
  :xp 300
  :abilities '(<bash-door> <open-door>)
  :immunities '(<sleep> <confusion>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <lose-str> :damage (3 . 4)) (<hit> :type <lose-str> :damage (3 . 4))
	     (<hit> :type <poison> :damage (3 . 4)))
  :treasures '((<drop> "1d2"))
  :gender '<male>)

(define-monster-kind "moss-memory" "memory moss"
  :numeric-id  332
  :desc "A mass of green vegetation.  You don't remember seeing anything like it  before."
  :x-char #\,
  :x-attr #\r
  :depth 32
  :rarity 3
  :hitpoints '(1 . 2)
  :armour 1
  :speed 110
  :xp 150
  :abilities '(<empty-mind> <stupid> <never-move> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 5
  :vision 30
  :attacks '((<hit> :type <confusion> :damage (1 . 4)) (<hit> :type <confusion> :damage (1 . 4)))
  :special-abilities '((<spell> <forget>) (<frequency> 1/6)))

(define-monster-kind "giant-storm" "storm giant"
  :numeric-id  333
  :desc "It is a twenty-five foot tall giant wreathed in lighting."
  :x-char #\P
  :x-attr #\B
  :alignment '<evil>
  :type '(<giant>)
  :depth 32
  :rarity 1
  :hitpoints '(38 . 10)
  :armour 60
  :speed 110
  :xp 1500
  :abilities '(<bash-door> <open-door> <pick-up-item> <max-hitpoints> <initial-sleeper>)
  :immunities '(<electricity> <cold>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <electricity> :damage (3 . 8)) (<hit> :type <electricity> :damage (3 . 8))
	     (<hit> :type <electricity> :damage (3 . 8)))
  :treasures '((<drop> "1d2"))
  :special-abilities '((<spell> (<ball> <electricity>)) (<spell> (<bolt> <electricity>)) (<spell> <scare>)
		       (<spell> <confusion>) (<spell> <teleport-player>) (<spell> <blink>) (<frequency> 1/8)))

(define-monster-kind "troll-cave" "cave troll"
  :numeric-id  334
  :desc "He is a vicious monster, feared for his ferocity."
  :x-char #\T
  :x-attr #\u
  :alignment '<evil>
  :type '(<troll>)
  :depth 33
  :rarity 1
  :hitpoints '(24 . 12)
  :armour 50
  :speed 110
  :xp 350
  :abilities '(<bash-door> <open-door>)
  :immunities '(<poison>)
  :vulnerabilities '(<light>)
  :alertness 50
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 8)) (<hit> :type <hurt> :damage (1 . 8))
	     (<hit> :type <hurt> :damage (1 . 8)) (<hit> :type <hurt> :damage (3 . 5)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>)

(define-monster-kind "troll-half" "half-troll"
  :numeric-id  335
  :desc "A huge, ugly, half-human in search of plunder."
  :x-char #\T
  :x-attr #\U
  :alignment '<evil>
  :type '(<troll>)
  :depth 33
  :rarity 2
  :hitpoints '(25 . 14)
  :armour 50
  :speed 110
  :xp 300
  :abilities '(<bash-door> <open-door>)
  :immunities '(<poison>)
  :alertness 50
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<claw> :type <hurt> :damage (1 . 5))
	     (<claw> :type <hurt> :damage (1 . 5)) (<claw> :type <hurt> :damage (1 . 5)))
  :treasures '((<drop-chance> 9/10) <only-drop-items>)
  :gender '<male>)

(define-monster-kind "mystic" "mystic"
  :numeric-id  336
  :desc "An adept at unarmed combat, the mystic strikes with stunning power.  He  can summon help from nature and is able to focus his power to ease any  pain."
  :x-char #\p
  :x-attr #\o
  :depth 33
  :rarity 3
  :hitpoints '(35 . 10)
  :armour 50
  :speed 120
  :xp 500
  :abilities '(<bash-door> <open-door> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <acid>)
  :alertness 5
  :vision 30
  :attacks '((<kick> :type <hurt> :damage (10 . 2)) (<kick> :type <hurt> :damage (10 . 2))
	     (<kick> :type <hurt> :damage (10 . 2)) (<kick> :type <hurt> :damage (10 . 2)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <spider>) (<spell> <heal>) (<frequency> 1/6)))

(define-monster-kind "wight-barrow" "barrow wight"
  :numeric-id  337
  :desc "It is a ghostly nightmare of a entity."
  :x-char #\W
  :x-attr #\v
  :alignment '<evil>
  :type '(<undead>)
  :depth 33
  :rarity 3
  :hitpoints '(15 . 10)
  :armour 40
  :speed 110
  :xp 375
  :abilities '(<bash-door> <open-door> <cold-blood> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<touch> :type <exp-40> :damage nil) (<hit> :type <hurt> :damage (1 . 8))
	     (<hit> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop-chance> 3/5))
  :special-abilities '((<spell> <darkness>) (<spell> (<cause> 2)) (<spell> <scare>) (<spell> <paralysis>)
		       (<frequency> 1/8)))

(define-monster-kind "troll-giant-skeleton" "giant skeleton troll"
  :numeric-id  338
  :desc "It is the animated form of a massive troll."
  :x-char #\s
  :x-attr #\w
  :alignment '<evil>
  :type '(<undead> <troll>)
  :depth 33
  :rarity 1
  :hitpoints '(45 . 10)
  :armour 50
  :speed 110
  :xp 325
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind> <max-hitpoints>)
  :immunities '(<fear> <sleep> <confusion> <poison> <cold>)
  :alertness 20
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 5)) (<bite> :type <hurt> :damage (1 . 5))
	     (<hit> :type <hurt> :damage (1 . 9)) (<hit> :type <hurt> :damage (1 . 9))))

(define-monster-kind "drake-chaos" "chaos drake"
  :numeric-id  339
  :desc "A dragon twisted by the forces of chaos.  It seems first ugly, then fair, as its form shimmers and changes in front of your eyes."
  :x-char #\d
  :x-attr #\v
  :alignment '<evil>
  :type '(<dragon>)
  :depth 33
  :rarity 3
  :hitpoints '(50 . 10)
  :armour 100
  :speed 110
  :xp 700
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confusion> <fire>)
  :alertness 30
  :vision 25
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<claw> :type <hurt> :damage (1 . 8))
	     (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <chaos>) (<breath> <disenchant>) (<spell> <scare>) (<spell> <confusion>)
		       (<spell> <slow>) (<frequency> 1/6)))

(define-monster-kind "drake-law" "law drake"
  :numeric-id  340
  :desc "This dragon is clever and cunning.  It laughs at your puny efforts to  disturb it."
  :x-char #\d
  :x-attr #\B
  :alignment '<evil>
  :type '(<dragon>)
  :depth 33
  :rarity 3
  :hitpoints '(55 . 10)
  :armour 100
  :speed 110
  :xp 700
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <cold>)
  :alertness 30
  :vision 25
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<claw> :type <hurt> :damage (1 . 8))
	     (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <shards>) (<breath> <sound>) (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>)
		       (<frequency> 1/6)))

(define-monster-kind "drake-balance" "balance drake"
  :numeric-id  341
  :desc "A mighty dragon, the balance drake seeks to maintain the Cosmic Balance, and despises your feeble efforts to destroy evil."
  :x-char #\d
  :x-attr #\v
  :alignment '<evil>
  :type '(<dragon>)
  :depth 33
  :rarity 3
  :hitpoints '(60 . 10)
  :armour 100
  :speed 110
  :xp 700
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confusion> <cold> <fire>)
  :alertness 30
  :vision 25
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<claw> :type <hurt> :damage (1 . 8))
	     (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <chaos>) (<breath> <disenchant>) (<breath> <shards>) (<breath> <sound>)
		       (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>) (<frequency> 1/6)))

(define-monster-kind "drake-ethereal" "ethereal drake"
  :numeric-id  342
  :desc "A dragon of elemental power, with control over light and dark, the ethereal drake's eyes glare with white hatred from the shadows."
  :x-char #\d
  :x-attr #\o
  :alignment '<evil>
  :type '(<dragon>)
  :depth 33
  :rarity 3
  :hitpoints '(45 . 10)
  :armour 100
  :speed 110
  :xp 700
  :abilities '(<pass-wall> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 15
  :vision 25
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<claw> :type <hurt> :damage (1 . 8))
	     (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <darkness>) (<breath> <light>) (<spell> <scare>) (<spell> <confusion>)
		       (<spell> <slow>) (<frequency> 1/6)))
(define-monster-kind "shade" "shade"
  :numeric-id  346
  :desc "A shadowy form clutches at you from the darkness.  A powerful undead with  a deadly touch."
  :x-char #\G
  :x-attr #\D
  :alignment '<evil>
  :type '(<undead>)
  :depth 33
  :rarity 3
  :hitpoints '(14 . 20)
  :armour 30
  :speed 120
  :xp 350
  :abilities '(<pass-wall> <pick-up-item> <cold-blood> <invisible> (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :alertness 10
  :vision 20
  :attacks '((<claw> :type <lose-int> :damage (1 . 10)) (<touch> :type <exp-40> :damage nil)
	     (<wail> :type <terrify> :damage nil))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) <only-drop-items>)
  :special-abilities '((<spell> <forget>) (<spell> <drain-mana>) (<spell> <paralysis>) (<spell> <blindness>)
		       (<frequency> 1/15)))
(define-monster-kind "spectre" "spectre"
  :numeric-id  347
  :desc "A phantasmal shrieking spirit.  Its wail drives the intense cold of pure  evil deep within your body."
  :x-char #\G
  :x-attr #\U
  :alignment '<evil>
  :type '(<undead>)
  :depth 33
  :rarity 3
  :hitpoints '(14 . 20)
  :armour 30
  :speed 120
  :xp 350
  :abilities '(<pass-wall> <pick-up-item> <cold-blood> <invisible> (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :alertness 10
  :vision 20
  :attacks '((<claw> :type <lose-wis> :damage (5 . 5)) (<touch> :type <exp-40> :damage nil)
	     (<wail> :type <terrify> :damage nil))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) <only-drop-items>)
  :special-abilities '((<spell> <forget>) (<spell> <drain-mana>) (<spell> <paralysis>) (<spell> <blindness>)
		       (<frequency> 1/15)))
(define-monster-kind "troll-water" "water troll"
  :numeric-id  348
  :desc "He is a troll that reeks of brine."
  :x-char #\T
  :x-attr #\s
  :alignment '<evil>
  :type '(<troll>)
  :depth 33
  :rarity 1
  :hitpoints '(36 . 10)
  :armour 50
  :speed 110
  :xp 420
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 50
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 2)) (<hit> :type <hurt> :damage (2 . 2))
	     (<hit> :type <hurt> :damage (1 . 9)) (<hit> :type <hurt> :damage (1 . 9)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>)
(define-monster-kind "elemental-fire" "fire elemental"
  :numeric-id  349
  :desc "It is a towering inferno of flames."
  :x-char #\E
  :x-attr #\r
  :alignment '<evil>
  :depth 33
  :rarity 2
  :hitpoints '(30 . 8)
  :armour 50
  :speed 110
  :xp 350
  :abilities '(<powerful-breath> <bash-door> <overrun-others> <overrun-items> <empty-mind> (<random-mover> 1/4)
	       <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <fire>)
  :alertness 50
  :vision 12
  :attacks '((<hit> :type <fire> :damage (4 . 6)) (<hit> :type <fire> :damage (4 . 6)))
  :special-abilities '((<spell> (<bolt> <fire>)) (<frequency> 1/6)))
(define-monster-kind "deva-astral" "astral deva"
  :numeric-id  350
  :desc "It is an angel moving very quickly, wielding a holy war hammer and casting  a volley of powerful spells in your direction."
  :x-char #\A
  :x-attr #\G
  :depth 33
  :rarity 6
  :hitpoints '(45 . 10)
  :armour 68
  :speed 120
  :xp 400
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <pick-up-item> <smart> <max-hitpoints>
	       <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 255
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (4 . 3))
	     (<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (4 . 3)))
  :treasures '((<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :special-abilities '((<summon> <monsters>) (<spell> (<bolt> <fire>)) (<spell> <mind-blast>) (<spell> <scare>)
		       (<spell> <blindness>) (<spell> <haste>) (<spell> <heal>) (<frequency> 1/3)))

(define-monster-kind "elemental-water" "water elemental"
  :numeric-id  351
  :desc "It is a towering tempest of water."
  :x-char #\E
  :x-attr #\s
  :alignment '<evil>
  :depth 33
  :rarity 2
  :hitpoints '(25 . 8)
  :armour 40
  :speed 110
  :xp 325
  :abilities '(<powerful-breath> <bash-door> <overrun-items> <overrun-others> <cold-blood> <empty-mind>
	       (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison>)
  :alertness 50
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (1 . 10)) (<hit> :type <hurt> :damage (1 . 10))
	     (<hit> :type <hurt> :damage (1 . 10)))
  :special-abilities '((<spell> (<bolt> <cold>)) (<frequency> 1/6)))
(define-monster-kind "invisible-stalker" "invisible stalker"
  :numeric-id  352
  :desc "It is impossible to define its form but its violence is legendary."
  :x-char #\E
  :x-attr #\y
  :alignment '<evil>
  :depth 34
  :rarity 3
  :hitpoints '(19 . 12)
  :armour 46
  :speed 130
  :xp 300
  :abilities '(<powerful-breath> <bash-door> <open-door> <cold-blood> <invisible> <empty-mind> (<random-mover> 1/2))
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 6))
	     (<hit> :type <hurt> :damage (1 . 6))))
(define-monster-kind "thief-master" "master thief"
  :numeric-id  354
  :desc "Cool and confident, fast and lithe; protect your possessions quickly!"
  :x-char #\p
  :x-attr #\B
  :alignment '<evil>
  :depth 34
  :rarity 2
  :hitpoints '(18 . 10)
  :armour 30
  :speed 130
  :xp 350
  :abilities '(<bash-door> <open-door> <pick-up-item>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <eat-item> :damage (4 . 5)) (<hit> :type <eat-gold> :damage (4 . 4))
	     (<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (2 . 8)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10))
  :gender '<male>)
(define-monster-kind "lich" "lich"
  :numeric-id  356
  :desc "It is a skeletal form dressed in robes.  It radiates vastly evil power."
  :x-char #\L
  :x-attr #\o
  :alignment '<evil>
  :type '(<undead>)
  :depth 34
  :rarity 3
  :hitpoints '(30 . 10)
  :armour 60
  :speed 110
  :xp 800
  :abilities '(<bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 60
  :vision 20
  :attacks '((<touch> :type <lose-dex> :damage (2 . 8)) (<touch> :type <lose-dex> :damage (2 . 8))
	     (<touch> :type <un-power> :damage nil) (<touch> :type <exp-40> :damage nil))
  :treasures '((<drop> "1d2"))
  :special-abilities '((<spell> <brain-smash>) (<spell> <drain-mana>) (<spell> (<cause> 3)) (<spell> <scare>)
		       (<spell> <slow>) (<spell> <paralysis>) (<spell> <blindness>) (<spell> <teleport-away>)
		       (<spell> <teleport-player>) (<spell> <blink>) (<frequency> 1/4)))
(define-monster-kind "vampire-master" "master vampire"
  :numeric-id  357
  :desc "It is a humanoid form dressed in robes.  Power emanates from its chilling  frame."
  :x-char #\V
  :x-attr #\g
  :alignment '<evil>
  :type '(<undead>)
  :depth 34
  :rarity 3
  :hitpoints '(34 . 10)
  :armour 60
  :speed 110
  :xp 750
  :abilities '(<regenerate> <bash-door> <open-door> <cold-blood> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<bite> :type <exp-40> :damage (1 . 4)) (<bite> :type <exp-40> :damage (1 . 4))
	     (<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop> "4d2"))
  :special-abilities '((<spell> (<bolt> <nether>)) (<spell> <darkness>) (<spell> <forget>) (<spell> <mind-blast>)
		       (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <confusion>) (<spell> <paralysis>)
		       (<spell> <teleport-player>) (<frequency> 1/6)))
(define-monster-kind "scorpion-grey" "giant grey scorpion"
  :numeric-id  358
  :desc "It is a giant grey scorpion.  It looks poisonous."
  :x-char #\S
  :x-attr #\s
  :type '(<animal>)
  :depth 34
  :rarity 4
  :hitpoints '(18 . 20)
  :armour 50
  :speed 120
  :xp 275
  :abilities '(<bash-door> <weird-mind>)
  :alertness 40
  :vision 12
  :attacks '((<sting> :type <poison> :damage (1 . 4)) (<bite> :type <hurt> :damage (1 . 6))))
(define-monster-kind "elemental-earth" "earth elemental"
  :numeric-id  359
  :desc "It is a towering form composed of rock with fists of awesome power."
  :x-char #\E
  :x-attr #\u
  :alignment '<evil>
  :depth 34
  :rarity 2
  :hitpoints '(30 . 10)
  :armour 60
  :speed 100
  :xp 375
  :abilities '(<powerful-breath> <pass-wall> <overrun-others> <overrun-items> <cold-blood> <empty-mind>
	       <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 90
  :vision 10
  :attacks '((<hit> :type <hurt> :damage (4 . 6)) (<hit> :type <hurt> :damage (4 . 6))
	     (<hit> :type <hurt> :damage (4 . 6)))
  :special-abilities '((<spell> (<bolt> <acid>)) (<frequency> 1/8)))
(define-monster-kind "elemental-air" "air elemental"
  :numeric-id  360
  :desc "It is a towering tornado of winds."
  :x-char #\E
  :x-attr #\B
  :alignment '<evil>
  :depth 34
  :rarity 2
  :hitpoints '(30 . 5)
  :armour 50
  :speed 120
  :xp 390
  :abilities '(<powerful-breath> <bash-door> <overrun-items> <overrun-others> <cold-blood> <empty-mind>
	       (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 50
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (1 . 10)) (<hit> :type <confusion> :damage (1 . 4))
	     (<hit> :type <hurt> :damage (1 . 10)))
  :special-abilities '((<spell> (<bolt> <electricity>)) (<frequency> 1/8)))
(define-monster-kind "hound-hell" "hell hound"
  :numeric-id  537
  :numeric-id  361
  :desc "It is a giant dog that glows with heat.  Flames pour from its nostrils."
  :x-char #\C
  :x-attr #\r
  :alignment '<evil>
  :type '(<animal>)
  :depth 35
  :rarity 3
  :hitpoints '(40 . 10)	;; (48 . 10) for later version
  :armour 80
  :speed 120
  :xp 600
  :abilities '(<push-others> <bash-door> (<random-mover> 1/4) <max-hitpoints> <initial-sleeper>)
  :immunities '(<fire>)
  :alertness 0 ;; 30 for later version
  :vision 25
  :attacks '((<bite> :type <fire> :damage (3 . 12)) (<bite> :type <fire> :damage (3 . 12))
	     (<bite> :type <fire> :damage (3 . 12)))
  :special-abilities '((<breath> <fire>) (<frequency> 1/5))
  :appear-in-group? #'van-novice-appears-in-group?)
(define-monster-kind "golem-eog" "eog golem"
  :numeric-id  362
  :desc "It is a massive deep brown statue
striding towards you with an  all-too-familiar purpose.  Your magic surprisingly feels much less  powerful now."
  :x-char #\g
  :x-attr #\u
  :depth 35
  :rarity 4
  :hitpoints '(100 . 20)
  :armour 125
  :speed 100
  :xp 1200
  :abilities '(<bash-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (6 . 6)) (<hit> :type <hurt> :damage (6 . 6))
	     (<hit> :type <hurt> :damage (8 . 6)) (<hit> :type <hurt> :damage (8 . 6)))
  :treasures '((<drop> "2d2") <only-drop-gold>))
(define-monster-kind "troll-olog" "olog"
  :numeric-id  363
  :desc "It is a massive intelligent troll with needle sharp fangs."
  :x-char #\T
  :x-attr #\y
  :alignment '<evil>
  :type '(<troll>)
  :depth 35
  :rarity 1
  :hitpoints '(42 . 10)
  :armour 50
  :speed 110
  :xp 400
  :abilities '(<bash-door> <smart> <open-door> <max-hitpoints>)
  :immunities '(<poison>)
  :alertness 50
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 3)) (<bite> :type <hurt> :damage (2 . 3))
	     (<hit> :type <hurt> :damage (1 . 12)) (<hit> :type <hurt> :damage (1 . 12)))
  :treasures '((<drop-chance> 3/5)))

(define-monster-kind "dagashi" "dagashi"
  :numeric-id  364
  :desc "A human warrior, moving with lightning speed."
  :x-char #\p
  :x-attr #\y
  :alignment '<evil>
  :depth 35
  :rarity 4
  :hitpoints '(13 . 25)
  :armour 70
  :speed 120
  :xp 500
  :abilities '(<bash-door> <open-door>)
  :immunities '(<sleep> <confusion>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <poison> :damage (3 . 4)) (<hit> :type <lose-str> :damage (3 . 4))
	     (<hit> :type <lose-str> :damage (3 . 4)) (<hit> :type <poison> :damage (3 . 4)))
  :treasures '((<drop> "1d2"))
  :gender '<male>)
(define-monster-kind "hound-gravity" "gravity hound"
  :numeric-id  365
  :desc "Unfettered by the usual constraints of gravity
these unnatural creatures  are walking on the walls and even the ceiling!  The earth suddenly feels  rather less solid as you see gravity warp all round the monsters."
  :x-char #\Z
  :x-attr #\W
  :type '(<animal>)
  :depth 35
  :rarity 2
  :hitpoints '(35 . 10)
  :armour 30
  :speed 110
  :xp 500
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<bite> :type <hurt> :damage (2 . 12))
	     (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <gravity>) (<frequency> 1/5)))
(define-monster-kind "cytoplasm-acidic" "acidic cytoplasm"
  :numeric-id  366
  :desc "A disgusting animated blob of destruction.  Flee its gruesome hunger!"
  :x-char #\j
  :x-attr #\s
  :depth 35
  :rarity 5
  :hitpoints '(40 . 10)
  :armour 18
  :speed 120
  :xp 36
  :abilities '(<bash-door> <open-door> <empty-mind> <stupid> <cold-blood> <pick-up-item> <max-hitpoints>)
  :immunities '(<sleep> <confusion> <fear> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 1
  :vision 12
  :attacks '((<touch> :type <acid> :damage (1 . 10)) (<touch> :type <acid> :damage (1 . 10))
	     (<touch> :type <acid> :damage (1 . 10)) (<touch> :type <acid> :damage (1 . 10)))
  :treasures '((<drop> "4d2") (<drop> "1d2")))

(define-monster-kind "hound-inertia" "inertia hound"
  :numeric-id  367
  :desc "Bizarrely, this hound seems to be hardly moving at all, yet it approaches  you with deadly menace.  It makes you tired just to look at it."
  :x-char #\Z
  :x-attr #\W
  :type '(<animal>)
  :depth 35
  :rarity 2
  :hitpoints '(35 . 10)
  :armour 30
  :speed 110
  :xp 500
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<bite> :type <hurt> :damage (2 . 12))
	     (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <inertia>) (<frequency> 1/5)))

(define-monster-kind "hound-impact" "impact hound"
  :numeric-id  368
  :desc "A deep brown shape is visible before you, its canine form strikes you with  an almost physical force.  The dungeon floor buckles as if struck by a  powerful blow as it stalks towards you."
  :x-char #\Z
  :x-attr #\u
  :type '(<animal>)
  :depth 35
  :rarity 2
  :hitpoints '(35 . 10)
  :armour 30
  :speed 110
  :xp 500
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<bite> :type <hurt> :damage (2 . 12))
	     (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <force>) (<frequency> 1/8)))

(define-monster-kind "dread" "dread"
  :numeric-id  460
  :numeric-id  369
  :desc "It is a form that screams its presence against the eye.  Death incarnate, its hideous black body seems to struggle against reality as the universe  itself struggles to banish it."
  :x-char #\G
  :x-attr #\o
  :alignment '<evil>
  :type '(<undead>)
  :depth 35
  :rarity 2
  :hitpoints '(25 . 20)
  :armour 30
  :speed 120
  :xp 600
  :abilities '(<pass-wall> <cold-blood> <invisible> <pick-up-item> (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <lose-str> :damage (3 . 4)) (<hit> :type <hurt> :damage (6 . 6))
	     (<hit> :type <hurt> :damage (6 . 6)))
  :treasures '((<drop> "2d2") (<drop-chance> 3/5) <only-drop-items>)
  :special-abilities '((<spell> (<bolt> <nether>)) (<spell> <drain-mana>) (<spell> <confusion>) (<spell> <paralysis>)
		       (<spell> <blindness>) (<frequency> 1/15))
  :appear-in-group? #'van-novice-appears-in-group?)

(define-monster-kind "elemental-ooze" "ooze elemental"
  :numeric-id  370
  :desc "It is a towering mass of filth, an eyesore of ooze."
  :x-char #\E
  :x-attr #\g
  :alignment '<evil>
  :depth 35
  :rarity 3
  :hitpoints '(13 . 10)
  :armour 80
  :speed 110
  :xp 300
  :abilities '(<powerful-breath> <bash-door> <overrun-others> <overrun-items> <cold-blood> <empty-mind>
	       <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 90
  :vision 10
  :attacks '((<touch> :type <acid> :damage (1 . 10)) (<touch> :type <acid> :damage (1 . 10))
	     (<touch> :type <acid> :damage (1 . 10)))
  :special-abilities '((<spell> (<ball> <acid>)) (<spell> (<bolt> <acid>)) (<frequency> 1/5)))

(define-monster-kind "elemental-smoke" "smoke elemental"
  :numeric-id  371
  :desc "It is a towering blackened form, crackling with heat."
  :x-char #\E
  :x-attr #\R
  :alignment '<evil>
  :depth 35
  :rarity 3
  :hitpoints '(15 . 10)
  :armour 80
  :speed 120
  :xp 375
  :abilities '(<powerful-breath> <bash-door> <overrun-others> <overrun-items> <empty-mind> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :alertness 90
  :vision 10
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<bite> :type <hurt> :damage (2 . 6)))
  :special-abilities '((<spell> (<bolt> <fire>)) (<spell> <darkness>) (<frequency> 1/5)))

(define-monster-kind "young-dragon-black" "young black dragon"
  :numeric-id  372
  :desc "It has a form that legends are made of.  Its still-tender scales are a  darkest black hue.  Acid drips from its body."
  :x-char #\d
  :x-attr #\s
  :alignment '<evil>
  :type '(<dragon>)
  :depth 35
  :rarity 1
  :hitpoints '(25 . 10)
  :armour 60
  :speed 110
  :xp 620
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<acid>)
  :alertness 50
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 6)) (<claw> :type <hurt> :damage (1 . 5))
	     (<claw> :type <hurt> :damage (1 . 5)))
  :treasures '((<drop> "1d2") (<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<breath> <acid>) (<spell> <scare>) (<frequency> 1/11)))

(define-monster-kind "mumak" "mumak"
  :numeric-id  461
  :numeric-id  373
  :desc "A massive elephantine form with eyes twisted by madness."
  :x-char #\q
  :x-attr #\s
  :type '(<animal>)
  :depth 35
  :rarity 3
  :hitpoints '(90 . 10)
  :armour 55
  :speed 110
  :xp 2100
  :abilities '(<bash-door>)
  :alertness 100
  :vision 20
  :attacks '((<crush> :type <hurt> :damage (4 . 4)) (<butt> :type <hurt> :damage (4 . 6))
	     (<butt> :type <hurt> :damage (4 . 6)))
  :appear-in-group? #'van-novice-appears-in-group?)

(define-monster-kind "ant-red" "giant red ant"
  :numeric-id  374
  :desc "A giant ant covered in shaggy fur.  Its powerful jaws glow with heat."
  :x-char #\a
  :x-attr #\r
  :type '(<animal>)
  :depth 35
  :rarity 1
  :hitpoints '(20 . 10)
  :armour 49
  :speed 110
  :xp 350
  :abilities '(<bash-door> <weird-mind> <overrun-others> <max-hitpoints>)
  :immunities '(<fire>)
  :alertness 40
  :vision 14
  :attacks '((<bite> :type <fire> :damage (3 . 12)) (<bite> :type <fire> :damage (3 . 12))))

(define-monster-kind "mature-dragon-white" "mature white dragon"
  :numeric-id  375
  :desc "A large dragon, scales gleaming bright white."
  :x-char #\d
  :x-attr #\w
  :alignment '<evil>
  :type '(<dragon>)
  :depth 35
  :rarity 1
  :hitpoints '(40 . 10)
  :armour 65
  :speed 110
  :xp 1000
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <cold>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 8)) (<claw> :type <hurt> :damage (1 . 8))
	     (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "2d2"))
  :special-abilities '((<breath> <cold>) (<spell> <scare>) (<frequency> 1/10)))

(define-monster-kind "xorn" "xorn"
  :numeric-id  376
  :desc "A huge creature of the element Earth.  Able to merge with its element, 
it has four huge arms protruding from its enormous torso."
  :x-char #\X
  :x-attr #\u
  :depth 36
  :rarity 2
  :hitpoints '(16 . 10)
  :armour 80
  :speed 110
  :xp 650
  :abilities '(<pass-wall> <overrun-items> <cold-blood> <empty-mind> <max-hitpoints>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 6))
	     (<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 6))))

(define-monster-kind "shadow" "shadow"
  :numeric-id  377
  :desc "A mighty spirit of darkness of vaguely humanoid form.  Razor-edged claws
reach out to end your life as it glides towards you, seeking to suck the
energy from your soul to feed its power."
  :x-char #\G
  :x-attr #\D
  :alignment '<evil>
  :type '(<undead>)
  :depth 36
  :rarity 3
  :hitpoints '(10 . 20)
  :armour 30
  :speed 120
  :xp 400
  :abilities '(<pass-wall> <cold-blood> <invisible> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :alertness 20
  :vision 30
  :attacks '((<claw> :type <lose-wis> :damage (1 . 10)) (<claw> :type <lose-int> :damage (1 . 10))
	     (<touch> :type <exp-40> :damage nil) (<touch> :type <exp-80> :damage nil))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :special-abilities '((<spell> <slow>) (<spell> <teleport-player>) (<frequency> 1/8)))

(define-monster-kind "phantom" "phantom"
  :numeric-id  378
  :desc "An unholy creature of darkness, the aura emanating from this evil being  saps your very soul."
  :x-char #\G
  :x-attr #\v
  :alignment '<evil>
  :type '(<undead>)
  :depth 36
  :rarity 3
  :hitpoints '(20 . 25)
  :armour 30
  :speed 120
  :xp 400
  :abilities '(<pass-wall> <cold-blood> <invisible> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :alertness 20
  :vision 30
  :attacks '((<claw> :type <lose-wis> :damage (1 . 10)) (<claw> :type <lose-int> :damage (1 . 10))
	     (<touch> :type <exp-40> :damage nil) (<touch> :type <exp-80> :damage nil))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :special-abilities '((<spell> <forget>) (<frequency> 1/5)))

(define-monster-kind "wraith-grey" "grey wraith"
  :numeric-id  379
  :desc "A tangible but ghostly form made of grey fog.  The air around it feels  deathly cold."
  :x-char #\W
  :x-attr #\s
  :alignment '<evil>
  :type '(<undead>)
  :depth 36
  :rarity 1
  :hitpoints '(19 . 10)
  :armour 50
  :speed 110
  :xp 700
  :abilities '(<bash-door> <open-door> <cold-blood> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<touch> :type <exp-40> :damage nil) (<hit> :type <hurt> :damage (1 . 10))
	     (<hit> :type <hurt> :damage (1 . 10)))
  :treasures '((<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<spell> <darkness>) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <paralysis>)
		       (<frequency> 1/7)))

(define-monster-kind "young-dragon-mh" "young multi-hued dragon"
  :numeric-id  380
  :desc "It has a form that legends are made of.  Beautiful scales of shimmering  and magical colours cover it."
  :x-char #\d
  :x-attr #\v
  :alignment '<evil>
  :type '(<dragon>)
  :depth 36
  :rarity 1
  :hitpoints '(32 . 10)
  :armour 60
  :speed 110
  :xp 1320
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 50
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 10)) (<claw> :type <hurt> :damage (1 . 9))
	     (<claw> :type <hurt> :damage (1 . 9)))
  :treasures '((<drop> "4d2") (<drop> "1d2"))
  :special-abilities '((<breath> <poison>) (<breath> <electricity>) (<breath> <cold>) (<breath> <fire>) (<breath> <acid>)
		       (<spell> <scare>) (<frequency> 1/5)))

(define-monster-kind "colossus" "colossus"
  :numeric-id  381
  :desc "An enormous construct resembling a titan made from stone.  It strides  purposefully towards you, swinging its slow fists with earth-shattering  power."
  :x-char #\g
  :x-attr #\G
  :depth 36
  :rarity 4
  :hitpoints '(30 . 100)
  :armour 150
  :speed 100
  :xp 850
  :abilities '(<bash-door> <cold-blood> <empty-mind> <max-hitpoints>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (6 . 6)) (<hit> :type <hurt> :damage (6 . 6))
	     (<hit> :type <hurt> :damage (10 . 10)) (<hit> :type <hurt> :damage (10 . 10))))
(define-monster-kind "young-dragon-gold" "young gold dragon"
  :numeric-id  382
  :desc "It has a form that legends are made of.  Its still-tender scales are a tarnished gold hue
and light is reflected from its form."
  :x-char #\d
  :x-attr #\y
  :type '(<dragon>)
  :depth 36
  :rarity 2
  :hitpoints '(30 . 10)
  :armour 63
  :speed 110
  :xp 950
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :alertness 150
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 8)) (<claw> :type <hurt> :damage (1 . 8))
	     (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<breath> <sound>) (<spell> <scare>) (<frequency> 1/11)))

(define-monster-kind "mature-dragon-blue" "mature blue dragon"
  :numeric-id  384
  :desc "A large dragon, scales tinted deep blue."
  :x-char #\d
  :x-attr #\b
  :alignment '<evil>
  :type '(<dragon>)
  :depth 36
  :rarity 1
  :hitpoints '(40 . 10)
  :armour 75
  :speed 110
  :xp 1200
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <electricity>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 10)) (<claw> :type <hurt> :damage (1 . 8))
	     (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<breath> <electricity>) (<spell> <scare>) (<frequency> 1/9)))
(define-monster-kind "mature-dragon-green" "mature green dragon"
  :numeric-id  385
  :desc "A large dragon, scales tinted deep green."
  :x-char #\d
  :x-attr #\g
  :alignment '<evil>
  :type '(<dragon>)
  :depth 36
  :rarity 1
  :hitpoints '(40 . 10)
  :armour 70
  :speed 110
  :xp 1100
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 6)) (<claw> :type <hurt> :damage (1 . 4))
	     (<claw> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<breath> <poison>) (<spell> <scare>) (<frequency> 1/9)))

(define-monster-kind "mature-dragon-bronze" "mature bronze dragon"
  :numeric-id  386
  :desc "A large dragon with scales of rich bronze."
  :x-char #\d
  :x-attr #\U
  :type '(<dragon>)
  :depth 36
  :rarity 2
  :hitpoints '(44 . 10)
  :armour 70
  :speed 110
  :xp 1300
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 150
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 10)) (<claw> :type <hurt> :damage (1 . 8))
	     (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "4d2") (<drop> "1d2"))
  :special-abilities '((<breath> <confusion>) (<spell> <scare>) (<spell> <confusion>) (<frequency> 1/9)))

(define-monster-kind "young-dragon-red" "young red dragon"
  :numeric-id  387
  :desc "It has a form that legends are made of.  Its still-tender scales are a  deepest red hue.  Heat radiates from its form."
  :x-char #\d
  :x-attr #\r
  :alignment '<evil>
  :type '(<dragon>)
  :depth 36
  :rarity 1
  :hitpoints '(29 . 10)
  :armour 63
  :speed 110
  :xp 640
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fire>)
  :alertness 50
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 8)) (<claw> :type <hurt> :damage (1 . 8))
	     (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "1d2") (<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<breath> <fire>) (<spell> <scare>) (<frequency> 1/11)))

(define-monster-kind "trapper" "trapper"
  :numeric-id  388
  :desc "A larger cousin of the lurker, this creature traps unsuspecting victims  and paralyses them, to be slowly digested later."
  :x-char #\.
  :x-attr #\w
  :depth 36
  :rarity 3
  :hitpoints '(60 . 10)
  :armour 75
  :speed 120
  :xp 580
  :abilities '(<cold-blood> <empty-mind> <invisible> <max-hitpoints> <never-move> <see-through> <absorbs-symbol>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 10
  :vision 30
  :attacks '((<hit> :type <paralyse> :damage (15 . 1)) (<hit> :type <paralyse> :damage (15 . 1))
	     (<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (3 . 8))))
(define-monster-kind "bodak" "bodak"
  :numeric-id  389
  :desc "It is a humanoid form composed of flames and hatred."
  :x-char #\u
  :x-attr #\r
  :alignment '<evil>
  :type '(<demon>)
  :depth 36
  :rarity 2
  :hitpoints '(35 . 10)
  :armour 68
  :speed 110
  :xp 750
  :abilities '(<bash-door> <open-door> <pick-up-item> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <fire>)
  :alertness 90
  :vision 10
  :attacks '((<gaze> :type <exp-20> :damage nil) (<hit> :type <fire> :damage (4 . 6))
	     (<hit> :type <fire> :damage (4 . 6)))
  :special-abilities '((<summon> <demon>) (<spell> (<ball> <fire>)) (<spell> (<bolt> <fire>)) (<frequency> 1/4)))

(define-monster-kind "elemental-ice" "ice elemental"
  :numeric-id  390
  :desc "It is a towering glacier of ice."
  :x-char #\E
  :x-attr #\w
  :alignment '<evil>
  :depth 36
  :rarity 2
  :hitpoints '(35 . 10)
  :armour 60
  :speed 110
  :xp 650
  :abilities '(<powerful-breath> <bash-door> <overrun-others> <overrun-items> <cold-blood> <empty-mind>
	       <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold>)
  :alertness 90
  :vision 10
  :attacks '((<bite> :type <cold> :damage (1 . 3)) (<hit> :type <hurt> :damage (4 . 6))
	     (<bite> :type <cold> :damage (1 . 3)))
  :special-abilities '((<spell> (<ball> <cold>)) (<spell> (<bolt> <ice>)) (<frequency> 1/5)))

(define-monster-kind "necromancer" "necromancer"
  :numeric-id  391
  :desc "A gaunt figure, clothed in black robes."
  :x-char #\p
  :x-attr #\R
  :alignment '<evil>
  :depth 36
  :rarity 2
  :hitpoints '(28 . 10)
  :armour 50
  :speed 110
  :xp 630
  :abilities '(<bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 6)) (<hit> :type <hurt> :damage (2 . 6)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <undead>) (<spell> (<bolt> <nether>)) (<spell> (<cause> 3)) (<spell> <scare>)
		       (<spell> <paralysis>) (<spell> <blindness>) (<spell> <teleport-player>) (<spell> <teleport>)
		       (<spell> <haste>) (<frequency> 1/3)))

(define-monster-kind "demonologist" "demonologist"
  :numeric-id  393
  :desc "A figure twisted by evil standing in robes of deepest crimson."
  :x-char #\p
  :x-attr #\R
  :alignment '<evil>
  :depth 36
  :rarity 2
  :hitpoints '(28 . 10)
  :armour 50
  :speed 120
  :xp 700
  :abilities '(<bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 5)) (<hit> :type <hurt> :damage (2 . 6))
	     (<hit> :type <hurt> :damage (2 . 6)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <demon>) (<spell> <paralysis>) (<spell> <teleport>) (<frequency> 1/2)))

(define-monster-kind "troll-mummy" "mummified troll"
  :numeric-id  394
  :desc "It is a massive figure clothed in wrappings.  You are wary of its massive  fists."
  :x-char #\z
  :x-attr #\w
  :alignment '<evil>
  :type '(<undead> <troll>)
  :depth 37
  :rarity 1
  :hitpoints '(19 . 10)
  :armour 50
  :speed 110
  :xp 420
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind> <max-hitpoints>)
  :immunities '(<fear> <sleep> <confusion> <poison> <cold>)
  :alertness 50
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 6)) (<hit> :type <hurt> :damage (2 . 6)))
  :treasures '((<drop-chance> 3/5)))

(define-monster-kind "will-o-wisp" "will o' the wisp"
  :numeric-id  396
  :desc "A strange ball of glowing light.  It disappears and reappears and seems to  draw you to it.  You seem somehow compelled to stand still and watch its  strange dancing motion."
  :x-char #\E
  :x-attr #\W
  :depth 37
  :rarity 4
  :hitpoints '(20 . 10)
  :armour 150
  :speed 130
  :xp 500
  :abilities '(<powerful-breath> <pass-wall> <invisible> <empty-mind> <smart> (<random-mover> 1/2) <max-hitpoints>
	       <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 0
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (1 . 9)) (<hit> :type <hurt> :damage (1 . 9))
	     (<hit> :type <hurt> :damage (1 . 9)) (<hit> :type <hurt> :damage (1 . 9)))
  :special-abilities '((<spell> (<cause> 2)) (<spell> <confusion>) (<spell> <teleport>) (<spell> <blink>)
		       (<frequency> 1/2)))

(define-monster-kind "elemental-magma" "magma elemental"
  :numeric-id  397
  :desc "It is a towering glowing form of molten hate."
  :x-char #\E
  :x-attr #\r
  :alignment '<evil>
  :depth 37
  :rarity 2
  :hitpoints '(35 . 10)
  :armour 70
  :speed 110
  :xp 950
  :abilities '(<powerful-breath> <pass-wall> <overrun-others> <overrun-items> <empty-mind> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <fire>)
  :alertness 90
  :vision 10
  :attacks '((<hit> :type <fire> :damage (3 . 7)) (<hit> :type <hurt> :damage (4 . 6))
	     (<hit> :type <fire> :damage (3 . 7)))
  :special-abilities '((<spell> (<ball> <fire>)) (<spell> (<bolt> <plasma>)) (<frequency> 1/7)))

(define-monster-kind "pudding-black" "black pudding"
  :numeric-id  398
  :desc "A lump of rotting black flesh that slurrrrrrrps across the dungeon floor."
  :x-char #\j
  :x-attr #\D
  :depth 37
  :rarity 5
  :hitpoints '(40 . 10)
  :armour 18
  :speed 110
  :xp 36
  :abilities '(<bash-door> <open-door> <pick-up-item> <cold-blood> <empty-mind> <stupid> <max-hitpoints>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 1
  :vision 12
  :attacks '((<touch> :type <acid> :damage (1 . 10)) (<touch> :type <acid> :damage (1 . 10))
	     (<touch> :type <acid> :damage (1 . 10)) (<touch> :type <acid> :damage (1 . 10)))
  :treasures '((<drop> "1d2") (<drop-chance> 9/10) (<drop-chance> 3/5)))

(define-monster-kind "beetle-blue" "killer blue beetle"
  :numeric-id  399
  :desc "It is a giant beetle, whose carapace shimmers with vibrant energies."
  :x-char #\K
  :x-attr #\b
  :type '(<animal>)
  :depth 37
  :rarity 2
  :hitpoints '(25 . 10)
  :armour 60
  :speed 110
  :xp 850
  :abilities '(<bash-door> <weird-mind> <max-hitpoints>)
  :immunities '(<electricity>)
  :alertness 30
  :vision 16
  :attacks '((<gaze> :type <paralyse> :damage nil) (<claw> :type <electricity> :damage (1 . 12))
	     (<claw> :type <electricity> :damage (1 . 12))))

(define-monster-kind "vortex-nexus" "nexus vortex"
  :numeric-id  400
  :desc "A maelstrom of potent magical energy."
  :x-char #\v
  :x-attr #\R
  :depth 37
  :rarity 1
  :hitpoints '(32 . 10)
  :armour 40
  :speed 120
  :xp 800
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/4) (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 100
  :attacks '((<engulf> :type <hurt> :damage (5 . 5)))
  :special-abilities '((<breath> <nexus>) (<frequency> 1/6)))

(define-monster-kind "vortex-plasma" "plasma vortex"
  :numeric-id  401
  :desc "A whirlpool of intense flame, charring the stones at your feet."
  :x-char #\v
  :x-attr #\r
  :depth 37
  :rarity 1
  :hitpoints '(32 . 10)
  :armour 40
  :speed 120
  :xp 800
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/4) (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <fire>)
  :alertness 0
  :vision 100
  :attacks '((<engulf> :type <fire> :damage (8 . 8)))
  :special-abilities '((<breath> <plasma>) (<frequency> 1/6)))

(define-monster-kind "mature-dragon-red" "mature red dragon"
  :numeric-id  402
  :desc "A large dragon, scales tinted deep red."
  :x-char #\d
  :x-attr #\r
  :alignment '<evil>
  :type '(<dragon>)
  :depth 37
  :rarity 1
  :hitpoints '(48 . 10)
  :armour 80
  :speed 110
  :xp 1400
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire>)
  :alertness 30
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 12)) (<claw> :type <hurt> :damage (1 . 10))
	     (<claw> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "4d2") (<drop> "1d2"))
  :special-abilities '((<breath> <fire>) (<spell> <scare>) (<spell> <confusion>) (<frequency> 1/9)))

(define-monster-kind "mature-dragon-gold" "mature gold dragon"
  :numeric-id  403
  :desc "A large dragon with scales of gleaming gold."
  :x-char #\d
  :x-attr #\y
  :type '(<dragon>)
  :depth 37
  :rarity 2
  :hitpoints '(56 . 10)
  :armour 80
  :speed 110
  :xp 1500
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 150
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 12)) (<claw> :type <hurt> :damage (1 . 10))
	     (<claw> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "4d2") (<drop> "1d2"))
  :special-abilities '((<breath> <sound>) (<spell> <scare>) (<spell> <confusion>) (<frequency> 1/9)))

(define-monster-kind "drake-crystal" "crystal drake"
  :numeric-id  404
  :desc "A dragon of strange crystalline form.  Light shines through it, dazzling your eyes with spectrums of colour."
  :x-char #\d
  :x-attr #\v
  :alignment '<evil>
  :type '(<dragon>)
  :depth 37
  :rarity 2
  :hitpoints '(50 . 10)
  :armour 100
  :speed 120
  :xp 1500
  :abilities '(<bash-door> <open-door> <invisible> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confusion> <cold>)
  :alertness 30
  :vision 25
  :attacks '((<bite> :type <hurt> :damage (2 . 5)) (<claw> :type <hurt> :damage (1 . 4))
	     (<claw> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "4d2") <only-drop-items>)
  :special-abilities '((<breath> <shards>) (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>) (<frequency> 1/6)))
(define-monster-kind "mature-dragon-black" "mature black dragon"
  :numeric-id  405
  :desc "A large dragon, with scales of deepest black."
  :x-char #\d
  :x-attr #\s
  :alignment '<evil>
  :type '(<dragon>)
  :depth 37
  :rarity 1
  :hitpoints '(46 . 10)
  :armour 55
  :speed 110
  :xp 1350
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <acid>)
  :alertness 30
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 10)) (<claw> :type <hurt> :damage (1 . 8))
	     (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<breath> <acid>) (<spell> <scare>) (<frequency> 1/9)))
(define-monster-kind "mature-dragon-mh" "mature multi-hued dragon"
  :numeric-id  406
  :desc "A large dragon, scales shimmering many colours."
  :x-char #\d
  :x-attr #\v
  :alignment '<evil>
  :type '(<dragon>)
  :depth 38
  :rarity 2
  :hitpoints '(64 . 10)
  :armour 65
  :speed 110
  :xp 1700
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 50
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 12)) (<claw> :type <hurt> :damage (1 . 10))
	     (<claw> :type <hurt> :damage (1 . 10)))
  :treasures '((<drop> "4d2") (<drop> "3d2"))
  :special-abilities '((<breath> <poison>) (<breath> <electricity>) (<breath> <cold>) (<breath> <fire>) (<breath> <acid>)
		       (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/5)))

(define-monster-kind "knight-death" "death knight"
  :numeric-id  407
  :desc "It is a humanoid form dressed in armour of an ancient form.  From beneath its helmet, eyes glow a baleful red and seem to pierce you like lances of fire."
  :x-char #\p
  :x-attr #\D
  :alignment '<evil>
  :depth 38
  :rarity 1
  :hitpoints '(60 . 10)
  :armour 100
  :speed 120
  :xp 1000
  :abilities '(<bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<cold>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5))
	     (<hit> :type <hurt> :damage (6 . 6)))
  :treasures '((<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :special-abilities '((<summon> <monsters>) (<spell> (<bolt> <nether>)) (<spell> (<cause> 3)) (<spell> <scare>)
		       (<spell> <blindness>) (<frequency> 1/5)))

(define-monster-kind "vortex-time" "time vortex"
  :numeric-id  409
  :desc "You haven't seen it yet."
  :x-char #\v
  :x-attr #\B
  :depth 38
  :rarity 4
  :hitpoints '(32 . 10)
  :armour 40
  :speed 130
  :xp 900
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/4) (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 100
  :attacks '((<engulf> :type <hurt> :damage (5 . 5)))
  :special-abilities '((<breath> <time>) (<frequency> 1/6)))

(define-monster-kind "vortex-shimmering" "shimmering vortex"
  :numeric-id  410
  :desc "A strange pillar of shining light that hurts your eyes.  Its shape changes  constantly as it cuts through the air towards you.  It is like a beacon, waking monsters from their slumber."
  :x-char #\v
  :x-attr #\v
  :depth 38
  :rarity 4
  :hitpoints '(6 . 12)
  :armour 30
  :speed 140
  :xp 200
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/4) (<random-mover> 1/2) <never-attack>
	       <initial-sleeper> <colour-changing>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 100
  :special-abilities '(<shriek> (<breath> <light>) (<frequency> 1/4)))

(define-monster-kind "ancient-dragon-blue" "ancient blue dragon"
  :numeric-id  411
  :desc "A huge draconic form.  Lightning crackles along its length."
  :x-char #\D
  :x-attr #\b
  :alignment '<evil>
  :type '(<dragon>)
  :depth 38
  :rarity 1
  :hitpoints '(70 . 10)
  :armour 80
  :speed 120
  :xp 1500
  :abilities '(<push-others> <powerful-breath> <bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <electricity>)
  :alertness 80
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 8)) (<claw> :type <hurt> :damage (1 . 8))
	     (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "4d2") (<drop> "1d2"))
  :special-abilities '((<breath> <electricity>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
		       (<frequency> 1/9)))

(define-monster-kind "ancient-dragon-bronze" "ancient bronze dragon"
  :numeric-id  412
  :desc "A huge draconic form enveloped in a cascade of colour."
  :x-char #\D
  :x-attr #\U
  :type '(<dragon>)
  :depth 38
  :rarity 2
  :hitpoints '(73 . 10)
  :armour 100
  :speed 120
  :xp 1700
  :abilities '(<push-others> <powerful-breath> <bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 200
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 10))
	     (<claw> :type <hurt> :damage (1 . 8))
	     (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "4d2") (<drop> "3d2"))
  :special-abilities '((<breath> <confusion>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
		       (<frequency> 1/6)))

(define-monster-kind "beholder" "beholder"
  :numeric-id  413
  :desc "A disembodied eye, surrounded by twelve smaller eyes on stalks."
  :x-char #\e
  :x-attr #\U
  :alignment '<evil>
  :depth 38
  :rarity 4
  :hitpoints '(16 . 100)
  :armour 80
  :speed 120
  :xp 6000
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison>)
  :alertness 10
  :vision 30
  :attacks '((<gaze> :type <un-power> :damage (2 . 6))
	     (<gaze> :type <lose-int> :damage (2 . 6))
	     (<gaze> :type <paralyse> :damage (2 . 4))
	     (<gaze> :type <exp-20> :damage (2 . 4)))
  :special-abilities '((<spell> (<bolt> <cold>)) (<spell> (<bolt> <fire>)) (<spell> (<bolt> <acid>))
		       (<spell> <darkness>) (<spell> <forget>) (<spell> <mind-blast>) (<spell> <drain-mana>)
		       (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>) (<spell> <blindness>)
		       (<frequency> 1/2)))

(define-monster-kind "wight-emperor" "emperor wight"
  :numeric-id  414
  :desc "Your life force is torn from your body as this powerful unearthly being  approaches."
  :x-char #\W
  :x-attr #\r
  :alignment '<evil>
  :type '(<undead>)
  :depth 38
  :rarity 2
  :hitpoints '(38 . 10)
  :armour 40
  :speed 120
  :xp 1600
  :abilities '(<bash-door> <open-door> <cold-blood> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<touch> :type <exp-80> :damage nil) (<touch> :type <exp-80> :damage nil)
	     (<hit> :type <hurt> :damage (1 . 12)) (<hit> :type <hurt> :damage (1 . 12)))
  :treasures '((<drop> "4d2") (<drop-chance> 9/10) <only-drop-items>)
  :special-abilities '((<spell> (<bolt> <nether>)) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <paralysis>)
		       (<frequency> 1/6)))
(define-monster-kind "angel-planetar" "planetar"
  :numeric-id  415
  :desc "It is an angel
fast and strong.  You are stunned by its extreme holiness  and try to resist all desires to obey it."
  :x-char #\A
  :x-attr #\r
  :depth 38
  :rarity 6
  :hitpoints '(50 . 10)
  :armour 68
  :speed 120
  :xp 1800
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <pick-up-item> <smart> <max-hitpoints>
	       <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 255
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (4 . 6)) (<hit> :type <hurt> :damage (5 . 5))
	     (<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (4 . 6)))
  :treasures '((<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :special-abilities '((<summon> <angel>) (<summon> <monsters>) (<spell> (<bolt> <plasma>)) (<spell> (<bolt> <mana>))
		       (<spell> <confusion>) (<spell> <teleport-away>) (<spell> <haste>) (<spell> <heal>)
		       (<frequency> 1/11)))
(define-monster-kind "wraith-black" "black wraith"
  :numeric-id  417
  :desc "A figure that seems made of void
its strangely human shape is cloaked in  shadow.  It reaches out at you."
  :x-char #\W
  :x-attr #\D
  :alignment '<evil>
  :type '(<undead>)
  :depth 38
  :rarity 2
  :hitpoints '(50 . 10)
  :armour 55
  :speed 120
  :xp 1700
  :abilities '(<bash-door> <open-door> <cold-blood> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<touch> :type <exp-40> :damage nil) (<touch> :type <exp-40> :damage nil)
	     (<hit> :type <hurt> :damage (1 . 12)) (<hit> :type <hurt> :damage (1 . 12)))
  :treasures '((<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :special-abilities '((<spell> (<bolt> <nether>)) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <paralysis>)
		       (<spell> <blindness>) (<frequency> 1/7)))
(define-monster-kind "erinyes" "erinyes"
  :numeric-id  418
  :desc "It is a lesser demon of female form; however
she takes little time to  show her true colours."
  :x-char #\U
  :x-attr #\u
  :alignment '<evil>
  :type '(<demon>)
  :depth 38
  :rarity 2
  :hitpoints '(24 . 10)
  :armour 50
  :speed 110
  :xp 1000
  :abilities '(<powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<touch> :type <lose-str> :damage (1 . 5)) (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop-chance> 3/5) <only-drop-items>)
  :gender '<female>
  :special-abilities '((<spell> (<bolt> <fire>)) (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/7)))
(define-monster-kind "wraith-nether" "nether wraith"
  :numeric-id  419
  :desc "A form that hurts the eye
death permeates the air around it.  As it nears  you
a coldness saps your soul."
  :x-char #\W
  :x-attr #\G
  :alignment '<evil>
  :type '(<undead>)
  :depth 39
  :rarity 2
  :hitpoints '(48 . 10)
  :armour 55
  :speed 120
  :xp 1700
  :abilities '(<pass-wall> <cold-blood> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<touch> :type <exp-80> :damage nil) (<touch> :type <exp-80> :damage nil)
	     (<hit> :type <hurt> :damage (1 . 12)) (<hit> :type <hurt> :damage (1 . 12)))
  :treasures '((<drop> "4d2") (<drop-chance> 9/10) <only-drop-items>)
  :special-abilities '((<spell> (<bolt> <nether>)) (<spell> <darkness>) (<spell> <mind-blast>) (<spell> (<cause> 3))
		       (<spell> <scare>) (<spell> <blindness>) (<frequency> 1/6)))

(define-monster-kind "troll-eldrak" "eldrak"
  :numeric-id  420
  :desc "A massive troll, larger and stronger than many men together."
  :x-char #\T
  :x-attr #\r
  :alignment '<evil>
  :type '(<troll>)
  :depth 39
  :rarity 3
  :hitpoints '(75 . 10)
  :armour 80
  :speed 110
  :xp 800
  :abilities '(<bash-door> <open-door> <pick-up-item> <max-hitpoints>)
  :immunities '(<sleep> <confusion> <poison>)
  :alertness 50
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4))
	     (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop-chance> 3/5)))

(define-monster-kind "troll-ettin" "ettin"
  :numeric-id  421
  :desc "A massive troll of huge strength.  Ettins are stupid but violent."
  :x-char #\T
  :x-attr #\b
  :alignment '<evil>
  :type '(<troll>)
  :depth 39
  :rarity 3
  :hitpoints '(15 . 100)
  :armour 100
  :speed 110
  :xp 1000
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<sleep> <confusion> <poison>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 6)) (<hit> :type <hurt> :damage (3 . 6))
	     (<hit> :type <hurt> :damage (3 . 6)))
  :treasures '((<drop> "1d2") <only-drop-items>))

(define-monster-kind "ancient-dragon-white" "ancient white dragon"
  :numeric-id  424
  :desc "A huge draconic form.  Frost covers it from head to tail."
  :x-char #\D
  :x-attr #\w
  :alignment '<evil>
  :type '(<dragon>)
  :depth 39
  :rarity 1
  :hitpoints '(70 . 10)
  :armour 90
  :speed 120
  :xp 2500
  :abilities '(<push-others> <powerful-breath> <bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <cold>)
  :alertness 80
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 12)) (<claw> :type <hurt> :damage (1 . 9))
	     (<claw> :type <hurt> :damage (1 . 9)))
  :treasures '((<drop> "4d2") (<drop> "3d2"))
  :special-abilities '((<breath> <cold>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
		       (<frequency> 1/9)))

(define-monster-kind "ancient-dragon-green" "ancient green dragon"
  :numeric-id  425
  :desc "A huge draconic form enveloped in clouds of poisonous vapour."
  :x-char #\D
  :x-attr #\g
  :alignment '<evil>
  :type '(<dragon>)
  :depth 39
  :rarity 1
  :hitpoints '(72 . 10)
  :armour 85
  :speed 120
  :xp 2400
  :abilities '(<push-others> <powerful-breath> <bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison>)
  :alertness 80
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 10)) (<claw> :type <hurt> :damage (1 . 8))
	     (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "4d2") (<drop> "3d2"))
  :special-abilities '((<breath> <poison>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
		       (<frequency> 1/9)))

(define-monster-kind "hydra-7" "7-headed hydra"
  :numeric-id  426
  :desc "A strange reptilian hybrid with seven heads dripping venom."
  :x-char #\M
  :x-attr #\G
  :type '(<animal>)
  :depth 39
  :rarity 2
  :hitpoints '(100 . 10)
  :armour 90
  :speed 120
  :xp 2000
  :abilities '(<push-others> <bash-door> <initial-sleeper>)
  :immunities '(<poison>)
  :alertness 20
  :vision 20
  :attacks '((<spit> :type <blind> :damage (1 . 2)) (<bite> :type <poison> :damage (3 . 9))
	     (<bite> :type <poison> :damage (3 . 9)) (<bite> :type <poison> :damage (3 . 9)))
  :treasures '((<drop> "4d2") (<drop> "2d2") <only-drop-gold>)
  :special-abilities '((<breath> <poison>) (<spell> (<ball> <poison>)) (<spell> <scare>) (<frequency> 1/5)))

(define-monster-kind "night-mare" "night mare"
  :numeric-id  427
  :desc "A fearsome skeletal horse with glowing eyes, that watch you with little  more than a hatred of all that lives."
  :x-char #\q
  :x-attr #\D
  :alignment '<evil>
  :type '(<undead>)
  :depth 39
  :rarity 3
  :hitpoints '(15 . 100)
  :armour 85
  :speed 120
  :xp 2900
  :abilities '(<bash-door> <open-door> <cold-blood> <max-hitpoints>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :alertness 0
  :vision 30
  :attacks '((<hit> :type <confusion> :damage (6 . 6)) (<hit> :type <hurt> :damage (3 . 8))
	     (<hit> :type <hurt> :damage (3 . 8)) (<bite> :type <exp-80> :damage (2 . 6)))
  :treasures '((<drop> "2d2") <only-drop-gold>))

(define-monster-kind "vampire-lord" "vampire lord"
  :numeric-id  428
  :desc "A foul wind chills your bones as this ghastly figure approaches."
  :x-char #\V
  :x-attr #\b
  :alignment '<evil>
  :type '(<undead>)
  :depth 39
  :rarity 3
  :hitpoints '(16 . 100)
  :armour 70
  :speed 120
  :xp 1800
  :abilities '(<regenerate> <bash-door> <open-door> <cold-blood> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<bite> :type <exp-80> :damage (1 . 6)) (<bite> :type <exp-80> :damage (1 . 6))
	     (<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop> "4d2") (<drop-chance> 3/5))
  :special-abilities '((<spell> (<bolt> <nether>)) (<spell> <darkness>) (<spell> <brain-smash>) (<spell> <drain-mana>)
		       (<spell> (<cause> 4)) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <paralysis>)
		       (<spell> <blindness>) (<frequency> 1/7)))

(define-monster-kind "ancient-dragon-black" "ancient black dragon"
  :numeric-id  429
  :desc "A huge draconic form.  Pools of acid melt the floor around it."
  :x-char #\D
  :x-attr #\s
  :alignment '<evil>
  :type '(<dragon>)
  :depth 39
  :rarity 1
  :hitpoints '(72 . 10)
  :armour 90
  :speed 120
  :xp 2500
  :abilities '(<push-others> <powerful-breath> <bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <acid>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 10)) (<claw> :type <hurt> :damage (1 . 9))
	     (<claw> :type <hurt> :damage (1 . 9)))
  :treasures '((<drop> "4d2") (<drop> "3d2"))
  :special-abilities '((<breath> <acid>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
		       (<frequency> 1/9)))

(define-monster-kind "worm-disenchanter" "disenchanter worm mass"
  :numeric-id  430
  :desc "It is a strange mass of squirming worms.  Magical energy crackles  around its disgusting form."
  :x-char #\w
  :x-attr #\v
  :type '(<animal>)
  :depth 40
  :rarity 3
  :hitpoints '(10 . 8)
  :armour 5
  :speed 100
  :xp 30
  :abilities '(<bash-door> <breeder> <weird-mind> <stupid> (<random-mover> 1/2))
  :immunities '(<fear>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 7
  :attacks '((<crawl> :type <un-bonus> :damage (1 . 4))))

(define-monster-kind "quylthulg-rotting" "rotting quylthulg"
  :numeric-id  431
  :desc "It is a pulsing flesh mound that reeks of death and putrefaction."
  :x-char #\Q
  :x-attr #\u
  :alignment '<evil>
  :type '(<animal>)
  :depth 40
  :rarity 1
  :hitpoints '(16 . 10)
  :armour 1
  :speed 120
  :xp 1500
  :abilities '(<empty-mind> <invisible> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 20
  :special-abilities '((<summon> <undead>) (<spell> <teleport>) (<spell> <blink>) (<frequency> 1/2)))

(define-monster-kind "troll-spirit" "spirit troll"
  :numeric-id  432
  :desc "A weird troll from the elemental planes."
  :x-char #\T
  :x-attr #\B
  :alignment '<evil>
  :type '(<troll>)
  :depth 40
  :rarity 3
  :hitpoints '(10 . 100)
  :armour 90
  :speed 110
  :xp 900
  :abilities '(<pass-wall> <invisible> <max-hitpoints>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold>)
  :alertness 5
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5))
	     (<hit> :type <hurt> :damage (3 . 6)))
  :treasures '((<drop-chance> 9/10)))

(define-monster-kind "titan-lesser" "lesser titan"
  :numeric-id  433
  :desc "It is a humanoid figure thirty feet tall that gives off an aura of power  and hate."
  :x-char #\P
  :x-attr #\y
  :alignment '<evil>
  :type '(<giant>)
  :depth 40
  :rarity 3
  :hitpoints '(10 . 100)
  :armour 80
  :speed 120
  :xp 3500
  :abilities '(<bash-door> <open-door> <pick-up-item> <smart> <max-hitpoints> <initial-sleeper>)
  :alertness 15
  :vision 30
  :attacks '((<hit> :type <confusion> :damage (6 . 6))
	     (<hit> :type <confusion> :damage (6 . 6))
	     (<hit> :type <confusion> :damage (6 . 6))
	     (<hit> :type <confusion> :damage (6 . 6)))
  :treasures '((<drop> "4d2") (<drop> "2d2"))
  :special-abilities '((<summon> <monsters>) (<spell> <scare>) (<spell> <teleport-player>) (<spell> <heal>)
		       (<frequency> 1/3)))

(define-monster-kind "hydra-9" "9-headed hydra"
  :numeric-id  434
  :desc "A strange reptilian hybrid with nine smouldering heads."
  :x-char #\M
  :x-attr #\r
  :type '(<animal>)
  :depth 40
  :rarity 2
  :hitpoints '(100 . 12)
  :armour 95
  :speed 120
  :xp 3000
  :abilities '(<push-others> <bash-door> <open-door> <initial-sleeper>)
  :immunities '(<fire>)
  :alertness 20
  :vision 20
  :attacks '((<bite> :type <fire> :damage (3 . 6))
	     (<bite> :type <fire> :damage (3 . 6))
	     (<bite> :type <fire> :damage (3 . 6))
	     (<bite> :type <fire> :damage (3 . 6)))
  :treasures '((<drop> "4d2") (<drop> "2d2") <only-drop-gold>)
  :special-abilities '((<breath> <fire>) (<spell> (<bolt> <fire>)) (<spell> <scare>) (<frequency> 1/4)))

(define-monster-kind "enchantress" "enchantress"
  :numeric-id  435
  :desc "This elusive female spellcaster has a special affinity for dragons, whom she rarely fights without."
  :x-char #\p
  :x-attr #\R
  :alignment '<evil>
  :depth 40
  :rarity 4
  :hitpoints '(52 . 10)
  :armour 60
  :speed 130
  :xp 2100
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 8))
	     (<hit> :type <hurt> :damage (2 . 6))
	     (<hit> :type <hurt> :damage (2 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<female>
  :special-abilities '((<summon> <dragon>) (<spell> <blindness>) (<frequency> 1/2)))

(define-monster-kind "priest-arch" "archpriest"
  :numeric-id  436
  :desc "An evil priest, dressed all in black.  Deadly spells hit you at an  alarming rate as his black spiked mace rains down blow after blow on your  pitiful frame."
  :x-char #\p
  :x-attr #\G
  :alignment '<evil>
  :depth 40
  :rarity 2
  :hitpoints '(52 . 10)
  :armour 60
  :speed 120
  :xp 1800
  :abilities '(<bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 5))
	     (<hit> :type <hurt> :damage (3 . 4))
	     (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <undead>) (<summon> <monster>) (<spell> (<cause> 3)) (<spell> <confusion>)
		       (<spell> <paralysis>) (<spell> <blindness>) (<spell> <heal>) (<frequency> 1/2)))

(define-monster-kind "sorcerer" "sorcerer"
  :numeric-id  437
  :desc "A human figure in robes, he moves with magically improved speed, and his hands are ablur with spell casting."
  :x-char #\p
  :x-attr #\v
  :alignment '<evil>
  :depth 40
  :rarity 2
  :hitpoints '(52 . 10)
  :armour 60
  :speed 130
  :xp 2150
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 8))
	     (<hit> :type <hurt> :damage (2 . 8))
	     (<hit> :type <hurt> :damage (2 . 8)))
  :treasures '((<drop> "4d2") (<drop-chance> 9/10) <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <dragon>) (<summon> <undead>) (<summon> <monster>) (<spell> (<ball> <cold>))
		       (<spell> (<ball> <fire>)) (<spell> (<bolt> <acid>)) (<spell> <traps>) (<spell> (<cause> 3))
		       (<spell> <confusion>) (<spell> <blindness>) (<spell> <teleport-player>) (<spell> <blink>)
		       (<frequency> 1/2)))

(define-monster-kind "xaren" "xaren"
  :numeric-id  438
  :desc "It is a tougher relative of the Xorn.  Its hide glitters with metal ores."
  :x-char #\X
  :x-attr #\s
  :depth 40
  :rarity 1
  :hitpoints '(32 . 10)
  :armour 80
  :speed 120
  :xp 1200
  :abilities '(<pass-wall> <overrun-items> <cold-blood> <empty-mind> <max-hitpoints>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 4))
	     (<hit> :type <hurt> :damage (3 . 4))
	     (<hit> :type <hurt> :damage (3 . 4))
	     (<hit> :type <hurt> :damage (3 . 4))))

(define-monster-kind "roc-giant" "giant roc"
  :numeric-id  439
  :desc "A vast legendary bird, its iron talons rake the most impenetrable of  surfaces and its screech echoes through the many winding dungeon corridors."
  :x-char #\B
  :x-attr #\u
  :type '(<animal>)
  :depth 40
  :rarity 3
  :hitpoints '(80 . 13)
  :armour 70
  :speed 110
  :xp 1000
  :abilities '(<bash-door>)
  :immunities '(<electricity>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <electricity> :damage (12 . 12))
	     (<crush> :type <hurt> :damage (8 . 12))
	     (<crush> :type <hurt> :damage (8 . 12))))

(define-monster-kind "minotaur" "minotaur"
  :numeric-id  441
  :desc "It is a cross between a human and a bull."
  :x-char #\H
  :x-attr #\s
  :alignment '<evil>
  :depth 40
  :rarity 2
  :hitpoints '(100 . 10)
  :armour 25
  :speed 130
  :xp 2100
  :abilities '(<bash-door>)
  :alertness 10
  :vision 13
  :attacks '((<butt> :type <hurt> :damage (2 . 6))
	     (<butt> :type <hurt> :damage (2 . 6))
	     (<butt> :type <hurt> :damage (4 . 6))
	     (<butt> :type <hurt> :damage (4 . 6))))

(define-monster-kind "drake-death" "death drake"
  :numeric-id  443
  :desc "It is a dragon-like form wrapped in darkness.  You cannot make out its  true form but you sense its evil."
  :x-char #\D
  :x-attr #\G
  :alignment '<evil>
  :type '(<dragon>)
  :depth 40
  :rarity 2
  :hitpoints '(10 . 100)
  :armour 100
  :speed 120
  :xp 3500
  :abilities '(<push-others> <powerful-breath> <pass-wall> <pick-up-item> <invisible> <max-hitpoints>
	       <initial-sleeper>)
  :immunities '(<sleep> <confusion> <cold>)
  :alertness 30
  :vision 25
  :attacks '((<bite> :type <exp-80> :damage (1 . 6))
	     (<bite> :type <exp-80> :damage (1 . 6))
	     (<claw> :type <hurt> :damage (1 . 10))
	     (<claw> :type <hurt> :damage (1 . 10)))
  :treasures '((<drop> "4d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <nether>) (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>) (<frequency> 1/6)))

(define-monster-kind "ancient-dragon-red" "ancient red dragon"
  :numeric-id  444
  :desc "A huge draconic form.  Wisps of smoke steam from its nostrils and the  extreme heat surrounding it makes you gasp for breath."
  :x-char #\D
  :x-attr #\r
  :alignment '<evil>
  :type '(<dragon>)
  :depth 40
  :rarity 1
  :hitpoints '(10 . 100)
  :armour 100
  :speed 120
  :xp 2750
  :abilities '(<push-others> <powerful-breath> <bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 14))
	     (<claw> :type <hurt> :damage (1 . 10))
	     (<claw> :type <hurt> :damage (1 . 10)))
  :treasures '((<drop> "4d2") (<drop> "3d2"))
  :special-abilities '((<breath> <fire>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
		       (<frequency> 1/6)))

(define-monster-kind "ancient-dragon-gold" "ancient gold dragon"
  :numeric-id  445
  :desc "A huge draconic form wreathed in a nimbus of light."
  :x-char #\D
  :x-attr #\y
  :type '(<dragon>)
  :depth 40
  :rarity 2
  :hitpoints '(15 . 100)
  :armour 100
  :speed 120
  :xp 4000
  :abilities '(<push-others> <powerful-breath> <bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 200
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 14))
	     (<claw> :type <hurt> :damage (1 . 10))
	     (<claw> :type <hurt> :damage (1 . 10)))
  :treasures '((<drop> "4d2") (<drop> "3d2"))
  :special-abilities '((<breath> <sound>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
		       (<frequency> 1/6)))

(define-monster-kind "drake-great-crystal" "great crystal drake"
  :numeric-id  446
  :desc "A huge crystalline dragon.  Its claws could cut you to shreds and its  teeth are razor sharp.  Strange colours ripple through it as it moves in  the light."
  :x-char #\D
  :x-attr #\v
  :alignment '<evil>
  :type '(<dragon>)
  :depth 40
  :rarity 2
  :hitpoints '(15 . 100)
  :armour 100
  :speed 120
  :xp 3500
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <invisible> <max-hitpoints> <initial-sleeper>
	       <colour-changing>)
  :immunities '(<sleep> <confusion> <cold>)
  :alertness 30
  :vision 25
  :attacks '((<bite> :type <hurt> :damage (2 . 12))
	     (<claw> :type <hurt> :damage (1 . 9))
	     (<claw> :type <hurt> :damage (1 . 9)))
  :treasures '((<drop> "4d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <shards>) (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>) (<frequency> 1/6)))

(define-monster-kind "vrock" "vrock"
  :numeric-id  447
  :desc "It is a demon with a long neck and raking claws."
  :x-char #\U
  :x-attr #\s
  :alignment '<evil>
  :type '(<demon>)
  :depth 40
  :rarity 2
  :hitpoints '(40 . 10)
  :armour 50
  :speed 110
  :xp 1000
  :abilities '(<powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<crush> :type <hurt> :damage (8 . 12))
	     (<crush> :type <hurt> :damage (8 . 12))
	     (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop-chance> 3/5) <only-drop-items>)
  :special-abilities '((<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/8)))

(define-monster-kind "quasit-death" "death quasit"
  :numeric-id  448
  :desc "It is a demon of small stature
but its armoured frame moves with  lightning speed and its powers make it a tornado of death and destruction."
  :x-char #\u
  :x-attr #\D
  :alignment '<evil>
  :type '(<demon>)
  :depth 40
  :rarity 3
  :hitpoints '(44 . 10)
  :armour 80
  :speed 130
  :xp 1000
  :abilities '(<pass-wall> <invisible> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <fire>)
  :alertness 0
  :vision 20
  :attacks '((<claw> :type <hurt> :damage (3 . 3))
	     (<claw> :type <hurt> :damage (3 . 3))
	     (<bite> :type <lose-dex> :damage (3 . 6)))
  :treasures '((<drop> "4d2") (<drop> "2d2") (<drop-chance> 9/10) <only-drop-items>)
  :special-abilities '((<summon> <demon>) (<spell> <forget>) (<spell> (<cause> 3)) (<spell> <scare>)
		       (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/10)))

(define-monster-kind "dark-elf-sorceror" "dark elven sorceror"
  :numeric-id  450
  :desc "A dark elven figure, dressed in deepest black.  Power seems to crackle  from his slender frame."
  :x-char #\h
  :x-attr #\v
  :alignment '<evil>
  :depth 41
  :rarity 2
  :hitpoints '(80 . 10)
  :armour 70
  :speed 130
  :xp 3000
  :abilities '(<bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 8))
	     (<hit> :type <hurt> :damage (2 . 8))
	     (<hit> :type <hurt> :damage (2 . 8)))
  :treasures '((<drop> "4d2") (<drop-chance> 9/10) <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <demon>) (<summon> <undead>) (<summon> <monster>) (<spell> (<ball> <cold>))
		       (<spell> (<ball> <fire>)) (<spell> (<bolt> <acid>)) (<spell> <darkness>) (<spell> (<cause> 3))
		       (<spell> <confusion>) (<spell> <blindness>) (<spell> <teleport-player>) (<spell> <blink>)
		       (<spell> <heal>) (<frequency> 1/2)))

(define-monster-kind "lich-master" "master lich"
  :numeric-id  451
  :desc "A skeletal form wrapped in robes.  Powerful magic crackles along its bony  fingers."
  :x-char #\L
  :x-attr #\r
  :alignment '<evil>
  :type '(<undead>)
  :depth 41
  :rarity 2
  :hitpoints '(18 . 100)
  :armour 80
  :speed 120
  :xp 10000
  :abilities '(<bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :alertness 50
  :vision 20
  :attacks '((<touch> :type <lose-dex> :damage (2 . 12))
	     (<touch> :type <lose-dex> :damage (2 . 12))
	     (<touch> :type <un-power> :damage nil)
	     (<touch> :type <exp-80> :damage nil))
  :treasures '((<drop> "4d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<summon> <undead>) (<spell> <brain-smash>) (<spell> <drain-mana>) (<spell> (<cause> 4))
		       (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <confusion>) (<spell> <paralysis>)
		       (<spell> <blindness>) (<spell> <teleport-player>) (<spell> <blink>) (<frequency> 1/3)))

(define-monster-kind "hezrou" "hezrou"
  :numeric-id  452
  :desc "It is a demon of lizard form with cruel-looking jaws."
  :x-char #\U
  :x-attr #\v
  :alignment '<evil>
  :type '(<demon>)
  :depth 41
  :rarity 3
  :hitpoints '(52 . 10)
  :armour 40
  :speed 110
  :xp 1500
  :abilities '(<powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 4))
	     (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop> "2d2") <only-drop-items>)
  :special-abilities '((<summon> <demon>) (<spell> (<bolt> <fire>)) (<frequency> 1/9)))

(define-monster-kind "angel-solar" "solar"
  :numeric-id  455
  :desc "Never a more heavenly being have you seen.  The very holiness of its  presence makes you deeply respect it.  Few creatures can match the powers  of a Solar; fewer still live to tell the tale after attacking one."
  :x-char #\A
  :x-attr #\y
  :depth 41
  :rarity 6
  :hitpoints '(100 . 35)
  :armour 140
  :speed 130
  :xp 15000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <pick-up-item> <smart> <initial-sleeper>)
  :immunities '(<poison> <electricity> <cold> <fire> <acid>)
  :alertness 255
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (8 . 6))
	     (<hit> :type <hurt> :damage (8 . 6))
	     (<gaze> :type <terrify> :damage (4 . 4))
	     (<gaze> :type <terrify> :damage (4 . 4)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :special-abilities '((<summon> <angel>) (<spell> (<bolt> <mana>)) (<spell> (<cause> 4)) (<spell> (<cause> 2))
		       (<spell> <scare>) (<spell> <blindness>) (<spell> <teleport-player>) (<frequency> 1/3)))
(define-monster-kind "glabrezu" "glabrezu"
  :numeric-id  456
  :desc "It is demon with arms and pincers, its form a true mockery of life."
  :x-char #\U
  :x-attr #\o
  :alignment '<evil>
  :type '(<demon>)
  :depth 41
  :rarity 2
  :hitpoints '(60 . 10)
  :armour 40
  :speed 110
  :xp 1750
  :abilities '(<powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 4))
	     (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop-chance> 9/10) <only-drop-items>)
  :special-abilities '((<summon> <demon>) (<spell> (<bolt> <fire>)) (<frequency> 1/9)))
(define-monster-kind "nalfeshnee" "nalfeshnee"
  :numeric-id  458
  :desc "It is a large demon with the head of a giant boar.  Flames run up and down  its length."
  :x-char #\U
  :x-attr #\r
  :alignment '<evil>
  :type '(<demon>)
  :depth 42
  :rarity 2
  :hitpoints '(67 . 10)
  :armour 50
  :speed 110
  :xp 2000
  :abilities '(<powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 4))
	     (<hit> :type <hurt> :damage (3 . 4))
	     (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :special-abilities '((<summon> <demon>) (<breath> <fire>) (<spell> <confusion>) (<spell> <blindness>)
		       (<frequency> 1/9)))
(define-monster-kind "beholder-undead" "undead beholder"
  :numeric-id  459
  :desc "A disembodied eye
floating in the air.  Black nether storms rage around  its bloodshot pupil and light seems to bend as it sucks its power from the  very air around it.  Your soul chills as it drains your vitality for its  evil enchantments."
  :x-char #\e
  :x-attr #\u
  :alignment '<evil>
  :type '(<undead>)
  :depth 42
  :rarity 4
  :hitpoints '(27 . 100)
  :armour 100
  :speed 120
  :xp 4000
  :abilities '(<bash-door> <cold-blood> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 10
  :vision 30
  :attacks '((<gaze> :type <un-power> :damage (2 . 6))
	     (<gaze> :type <lose-int> :damage (2 . 6))
	     (<gaze> :type <paralyse> :damage nil)
	     (<gaze> :type <exp-40> :damage nil))
  :special-abilities '((<summon> <undead>) (<spell> (<bolt> <mana>)) (<spell> <forget>) (<spell> <brain-smash>)
		       (<spell> <mind-blast>) (<spell> <drain-mana>) (<spell> (<cause> 4)) (<spell> <slow>)
		       (<frequency> 1/2)))
(define-monster-kind "ancient-dragon-mh" "ancient multi-hued dragon"
  :numeric-id  462
  :desc "A huge draconic form.  Many colours ripple down its massive frame.  Few  live to see another."
  :x-char #\D
  :x-attr #\v
  :alignment '<evil>
  :type '(<dragon>)
  :depth 43
  :rarity 1
  :hitpoints '(21 . 100)
  :armour 100
  :speed 120
  :xp 13000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper>
	       <colour-changing>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (3 . 12))
	     (<claw> :type <hurt> :damage (1 . 12))
	     (<claw> :type <hurt> :damage (1 . 12)))
  :treasures '((<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <poison>) (<breath> <electricity>) (<breath> <cold>) (<breath> <fire>) (<breath> <acid>)
		       (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/5)))
(define-monster-kind "dragon-ethereal" "ethereal dragon"
  :numeric-id  463
  :desc "A huge dragon emanating from the elemental plains
the ethereal dragon is  a master of light and dark.  Its form disappears from sight as it cloaks  itself in unearthly shadows."
  :x-char #\D
  :x-attr #\o
  :type '(<dragon>)
  :depth 43
  :rarity 2
  :hitpoints '(21 . 100)
  :armour 100
  :speed 120
  :xp 11000
  :abilities '(<push-others> <powerful-breath> <pass-wall> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 15
  :vision 25
  :attacks '((<bite> :type <hurt> :damage (3 . 12))
	     (<claw> :type <hurt> :damage (1 . 12))
	     (<claw> :type <hurt> :damage (1 . 12)))
  :treasures '((<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <confusion>) (<breath> <darkness>) (<breath> <light>) (<spell> <confusion>)
		       (<spell> <blindness>) (<frequency> 1/5)))
(define-monster-kind "marilith" "marilith"
  :numeric-id  465
  :desc "She is a demon of female form with many arms
each bearing deadly weapons."
  :x-char #\U
  :x-attr #\y
  :alignment '<evil>
  :type '(<demon>)
  :depth 43
  :rarity 2
  :hitpoints '(12 . 100)
  :armour 75
  :speed 120
  :xp 5000
  :abilities '(<powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 6))
	     (<hit> :type <hurt> :damage (3 . 6))
	     (<hit> :type <hurt> :damage (3 . 6))
	     (<hit> :type <hurt> :damage (3 . 6)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :gender '<female>
  :special-abilities '((<summon> <demon>) (<spell> (<cause> 2)) (<spell> <blindness>) (<frequency> 1/9)))
(define-monster-kind "balrog-lesser" "lesser balrog"
  :numeric-id  467
  :desc "It is a massive humanoid demon wreathed in flames."
  :x-char #\U
  :x-attr #\R
  :alignment '<evil>
  :type '(<demon>)
  :depth 44
  :rarity 3
  :hitpoints '(18 . 100)
  :armour 50
  :speed 120
  :xp 8000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (5 . 5))
	     (<hit> :type <fire> :damage (2 . 6))
	     (<hit> :type <hurt> :damage (4 . 6))
	     (<hit> :type <fire> :damage (2 . 6)))
  :treasures '((<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :special-abilities '((<summon> <demon>) (<breath> <fire>) (<spell> <confusion>) (<spell> <blindness>)
		       (<frequency> 1/4)))
(define-monster-kind "hydra-11" "11-headed hydra"
  :numeric-id  469
  :desc "A strange reptilian hybrid with eleven smouldering heads."
  :x-char #\M
  :x-attr #\R
  :type '(<animal>)
  :depth 44
  :rarity 2
  :hitpoints '(100 . 18)
  :armour 100
  :speed 120
  :xp 6000
  :abilities '(<push-others> <bash-door> <open-door> <initial-sleeper>)
  :immunities '(<fire>)
  :alertness 20
  :vision 20
  :attacks '((<bite> :type <fire> :damage (3 . 12))
	     (<bite> :type <fire> :damage (3 . 12))
	     (<bite> :type <fire> :damage (3 . 12))
	     (<bite> :type <fire> :damage (3 . 12)))
  :treasures '((<drop> "4d2") (<drop> "2d2") <only-drop-gold>)
  :special-abilities '((<breath> <fire>) (<spell> (<ball> <fire>)) (<spell> (<bolt> <plasma>))
		       (<spell> (<bolt> <fire>)) (<spell> <scare>) (<frequency> 1/4)))

(define-monster-kind "priest-patriarch" "patriarch"
  :numeric-id  470
  :desc "A dark priest of the highest order.  Powerful and evil, beware his many spells."
  :x-char #\p
  :x-attr #\G
  :alignment '<evil>
  :depth 44
  :rarity 2
  :hitpoints '(80 . 10)
  :armour 60
  :speed 120
  :xp 5000
  :abilities '(<bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 5))
	     (<hit> :type <hurt> :damage (3 . 4))
	     (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop> "4d2") (<drop-chance> 9/10) <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <undead>) (<summon> <monsters>) (<spell> <brain-smash>) (<spell> (<cause> 4))
		       (<spell> <paralysis>) (<spell> <blindness>) (<spell> <heal>) (<frequency> 1/2)))

(define-monster-kind "dreadmaster" "dreadmaster"
  :numeric-id  471
  :desc "It is an unlife of power almost unequaled.  An affront to existence, its very touch abuses and disrupts the flow of life, and its unearthly limbs, of purest black, crush rock and flesh with ease."
  :x-char #\G
  :x-attr #\y
  :alignment '<evil>
  :type '(<undead>)
  :depth 44
  :rarity 2
  :hitpoints '(12 . 100)
  :armour 100
  :speed 120
  :xp 8000
  :abilities '(<pass-wall> <cold-blood> <invisible> <pick-up-item> <smart> (<random-mover> 1/4) <max-hitpoints>
	       <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <lose-str> :damage (3 . 4))
	     (<hit> :type <lose-str> :damage (3 . 4))
	     (<hit> :type <hurt> :damage (6 . 6))
	     (<hit> :type <hurt> :damage (6 . 6)))
  :treasures '((<drop> "4d2") (<drop> "1d2") <only-drop-items>)
  :special-abilities '((<summon> <undead>) (<spell> (<bolt> <nether>)) (<spell> <drain-mana>) (<spell> (<cause> 4))
		       (<spell> <confusion>) (<spell> <paralysis>) (<spell> <blindness>) (<spell> <teleport-level>)
		       (<frequency> 1/9)))
(define-monster-kind "drolem" "drolem"
  :numeric-id  472
  :desc "A constructed dragon, the drolem has massive strength.  Powerful spells  weaved during its creation make it a fearsome adversary.  Its eyes show  little intelligence, but it has been instructed to destroy all it meets."
  :x-char #\g
  :x-attr #\g
  :type '(<dragon>)
  :depth 44
  :rarity 3
  :hitpoints '(30 . 100)
  :armour 130
  :speed 120
  :xp 12000
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :alertness 30
  :vision 25
  :attacks '((<claw> :type <poison> :damage (3 . 3))
	     (<claw> :type <poison> :damage (3 . 3))
	     (<bite> :type <hurt> :damage (5 . 8))
	     (<bite> :type <hurt> :damage (5 . 8)))
  :special-abilities '((<breath> <poison>) (<arrow> 3) (<spell> <confusion>) (<spell> <slow>) (<spell> <blindness>)
		       (<frequency> 1/6)))
(define-monster-kind "titan-greater" "greater titan"
  :numeric-id  477
  :desc "A forty foot tall humanoid that shakes the ground as it walks.  The power  radiating from its frame shakes your courage, its hatred inspired by your  defiance."
  :x-char #\P
  :x-attr #\o
  :alignment '<evil>
  :type '(<giant>)
  :depth 46
  :rarity 3
  :hitpoints '(38 . 100)
  :armour 125
  :speed 120
  :xp 13500
  :abilities '(<bash-door> <open-door> <pick-up-item> <smart> <max-hitpoints> <initial-sleeper>)
  :alertness 15
  :vision 30
  :attacks '((<hit> :type <confusion> :damage (12 . 12))
	     (<hit> :type <confusion> :damage (12 . 12))
	     (<hit> :type <confusion> :damage (12 . 12))
	     (<hit> :type <confusion> :damage (12 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :special-abilities '((<summon> <monsters>) (<spell> <teleport-player>) (<spell> <heal>) (<frequency> 1/3)))
(define-monster-kind "dracolisk" "dracolisk"
  :numeric-id  478
  :desc "A mixture of dragon and basilisk, the dracolisk stares at you with deep  piercing eyes, its evil breath burning the ground where it stands."
  :x-char #\D
  :x-attr #\G
  :alignment '<evil>
  :type '(<dragon> <animal>)
  :depth 46
  :rarity 2
  :hitpoints '(35 . 100)
  :armour 120
  :speed 120
  :xp 14000
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire> <acid>)
  :alertness 30
  :vision 25
  :attacks '((<gaze> :type <paralyse> :damage nil)
	     (<bite> :type <hurt> :damage (5 . 8))
	     (<bite> :type <hurt> :damage (2 . 12))
	     (<bite> :type <hurt> :damage (2 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :special-abilities '((<breath> <nether>) (<breath> <fire>) (<spell> <scare>) (<spell> <paralysis>) (<frequency> 1/6)))

(define-monster-kind "mold-death" "death mold"
  :numeric-id  479
  :desc "It is the epitome of all that is evil, in a mold.  Its lifeless form draws  power from sucking the souls of those that approach it, a nimbus of pure  evil surrounds it.  Luckily for you, it can't move."
  :x-char #\m
  :x-attr #\D
  :alignment '<evil>
  :depth 47
  :rarity 1
  :hitpoints '(100 . 20)
  :armour 60
  :speed 140
  :xp 1000
  :abilities '(<never-move> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 0
  :vision 200
  :attacks '((<hit> :type <exp-80> :damage (5 . 5))
	     (<hit> :type <un-bonus> :damage (7 . 7))
	     (<hit> :type <un-bonus> :damage (7 . 7))
	     (<hit> :type <un-bonus> :damage (7 . 7))))

(define-monster-kind "mystic-master" "master mystic"
  :numeric-id  482
  :desc "A lord of all that is natural, skilled in the mystic ways.  He is a master  of martial arts and is at one with nature, able to summon help from the  wild if need be."
  :x-char #\p
  :x-attr #\o
  :depth 50
  :rarity 3
  :hitpoints '(11 . 100)
  :armour 60
  :speed 130
  :xp 6000
  :abilities '(<bash-door> <open-door> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 5
  :vision 30
  :attacks '((<hit> :type <paralyse> :damage (15 . 1))
	     (<hit> :type <poison> :damage (20 . 1))
	     (<kick> :type <hurt> :damage (10 . 2))
	     (<kick> :type <hurt> :damage (10 . 2)))
  :treasures '((<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <spider>) (<spell> <heal>) (<frequency> 1/3)))

(define-monster-kind "nightwing" "nightwing"
  :numeric-id  484
  :desc "Everywhere colours seem paler and the air chiller.  At the centre of the  cold stands a mighty figure.  Its wings envelop you in the chill of death  as the nightwing reaches out to draw you into oblivion.  Your muscles sag  and your mind loses all will to fight as you stand in awe of this mighty  being."
  :x-char #\W
  :x-attr #\D
  :alignment '<evil>
  :type '(<undead>)
  :depth 50
  :rarity 4
  :hitpoints '(60 . 30)
  :armour 120
  :speed 120
  :xp 6000
  :abilities '(<bash-door> <open-door> <cold-blood> <smart> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <un-bonus> :damage (6 . 8))
	     (<hit> :type <un-bonus> :damage (6 . 8))
	     (<touch> :type <poison> :damage (3 . 5))
	     (<touch> :type <poison> :damage (3 . 5)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<summon> <undead>) (<spell> (<ball> <nether>)) (<spell> (<bolt> <nether>))
		       (<spell> (<bolt> <mana>)) (<spell> <brain-smash>) (<spell> (<cause> 4)) (<spell> <scare>)
		       (<spell> <blindness>) (<frequency> 1/4)))

(define-monster-kind "hound-nether" "nether hound"
  :numeric-id  485
  :desc "You feel a soul-tearing chill upon viewing this beast, a ghostly form of  darkness in the shape of a large dog."
  :x-char #\Z
  :x-attr #\G
  :type '(<animal>)
  :depth 51
  :rarity 2
  :hitpoints '(60 . 10)
  :armour 100
  :speed 120
  :xp 5000
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3))
	     (<bite> :type <hurt> :damage (2 . 12))
	     (<bite> :type <hurt> :damage (2 . 12))
	     (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <nether>) (<frequency> 1/5)))
(define-monster-kind "hound-time" "time hound"
  :numeric-id  486
  :desc "You get a terrible sense of deja vu
or is it a premonition?  All at once  you see a little puppy and a toothless old dog.  Perhaps you should give  up and go to bed."
  :x-char #\Z
  :x-attr #\B
  :type '(<animal>)
  :depth 51
  :rarity 4
  :hitpoints '(60 . 10)
  :armour 100
  :speed 130
  :xp 5000
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3))
	     (<bite> :type <hurt> :damage (2 . 12))
	     (<bite> :type <hurt> :damage (2 . 12))
	     (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <time>) (<frequency> 1/8)))
(define-monster-kind "hound-plasma" "plasma hound"
  :numeric-id  487
  :desc "The very air warps as pure elemental energy stalks towards you in the  shape of a giant hound.  Your hair stands on end and your palms itch as  you sense trouble."
  :x-char #\Z
  :x-attr #\r
  :type '(<animal>)
  :depth 51
  :rarity 2
  :hitpoints '(60 . 10)
  :armour 100
  :speed 120
  :xp 5000
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3))
	     (<bite> :type <hurt> :damage (2 . 12))
	     (<bite> :type <hurt> :damage (2 . 12))
	     (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <plasma>) (<frequency> 1/5)))

(define-monster-kind "quylthulg-demonic" "demonic quylthulg"
  :numeric-id  488
  :desc "A pile of pulsing flesh that glows with an inner hellish fire.  The world  itself seems to cry out against it."
  :x-char #\Q
  :x-attr #\r
  :alignment '<evil>
  :type '(<animal>)
  :depth 51
  :rarity 1
  :hitpoints '(48 . 10)
  :armour 1
  :speed 120
  :xp 3000
  :abilities '(<empty-mind> <invisible> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 20
  :special-abilities '((<summon> <demon>) (<spell> <teleport>) (<spell> <blink>) (<frequency> 1/2)))

(define-monster-kind "wyrm-storm" "great storm wyrm"
  :numeric-id  489
  :desc "A vast dragon of power.  Storms and lightning crash around its titanic
form.  Deep blue scales reflect the flashes and highlight the creature's
  great muscles.  It regards you with contempt."
  :x-char #\D
  :x-attr #\b
  :alignment '<evil>
  :type '(<dragon>)
  :depth 51
  :rarity 2
  :hitpoints '(30 . 100)
  :armour 150
  :speed 120
  :xp 17000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <electricity>)
  :alertness 80
  :vision 30
  :attacks '((<bite> :type <hurt> :damage (4 . 14))
	     (<claw> :type <hurt> :damage (1 . 12))
	     (<claw> :type <hurt> :damage (1 . 12))
	     (<claw> :type <hurt> :damage (1 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :special-abilities '((<breath> <electricity>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
		       (<frequency> 1/6)))

(define-monster-kind "mystic-grand-master" "grand master mystic"
  :numeric-id  493
  :desc "He is one of the few true masters of the art being extremely skillful in
all forms of unarmed combat and controlling the world's natural creatures
with disdainful ease."
  :x-char #\p
  :x-attr #\o
  :depth 53
  :rarity 3
  :hitpoints '(22 . 100)
  :armour 80
  :speed 130
  :xp 15000
  :abilities '(<bash-door> <open-door> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 5
  :vision 30
  :attacks '((<hit> :type <paralyse> :damage (15 . 1))
	     (<hit> :type <poison> :damage (20 . 1))
	     (<kick> :type <hurt> :damage (10 . 2))
	     (<kick> :type <hurt> :damage (20 . 2)))
  :treasures '((<drop> "4d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <hound>) (<summon> <spider>) (<spell> <mind-blast>) (<spell> <heal>)
		       (<frequency> 1/2)))

(define-monster-kind "hound-ethereal" "ethereal hound"
  :numeric-id  495
  :desc "A pale green hound.  Pulsing red lines and strange fluorescent light hints at internal organs best left to the imagination."
  :x-char #\Z
  :x-attr #\G
  :type '(<animal>)
  :depth 54
  :rarity 3
  :hitpoints '(60 . 15)
  :armour 100
  :speed 120
  :xp 5000
  :abilities '(<pass-wall> <invisible> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3))
	     (<bite> :type <hurt> :damage (2 . 12))
	     (<bite> :type <hurt> :damage (2 . 12))
	     (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <nether>) (<frequency> 1/5)))

(define-monster-kind "wyrm-ice" "great ice wyrm"
  :numeric-id  496
  :desc "An immense dragon capable of awesome destruction.  You have never felt  such extreme cold
or witnessed such an icy stare.  Begone quickly or feel  its wrath!"
  :x-char #\D
  :x-attr #\w
  :alignment '<evil>
  :type '(<dragon>)
  :depth 54
  :rarity 2
  :hitpoints '(30 . 100)
  :armour 170
  :speed 120
  :xp 20000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <cold>)
  :alertness 80
  :vision 30
  :attacks '((<bite> :type <hurt> :damage (4 . 14))
	     (<claw> :type <hurt> :damage (3 . 12))
	     (<claw> :type <hurt> :damage (1 . 12))
	     (<claw> :type <hurt> :damage (1 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :special-abilities '((<breath> <cold>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
		       (<frequency> 1/6)))

(define-monster-kind "nightcrawler" "nightcrawler"
  :numeric-id  498
  :desc "This intensely evil creature bears the form of a gargantuan black worm.   Its gaping maw is a void of blackness
acid drips from its steely hide.   It is like nothing you have ever seen before
and a terrible chill runs  down your spine as you face it."
  :x-char #\W
  :x-attr #\D
  :alignment '<evil>
  :type '(<undead>)
  :depth 54
  :rarity 4
  :hitpoints '(80 . 60)
  :armour 160
  :speed 120
  :xp 8000
  :abilities '(<bash-door> <open-door> <cold-blood> <smart> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold> <fire>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<bite> :type <acid> :damage (10 . 10))
	     (<bite> :type <acid> :damage (10 . 10))
	     (<sting> :type <lose-con> :damage (8 . 8))
	     (<sting> :type <lose-con> :damage (8 . 8)))
  :treasures '(<drop-good> (<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :special-abilities '((<summon> <undead>) (<breath> <nether>) (<spell> (<ball> <nether>)) (<spell> (<bolt> <nether>))
		       (<spell> (<bolt> <mana>)) (<spell> <brain-smash>) (<spell> <scare>) (<spell> <blindness>)
		       (<frequency> 1/4)))

(define-monster-kind "druj-hand" "hand druj"
  :numeric-id  499
  :desc "A skeletal hand floating in the air motionless except for its flexing fingers."
  :x-char #\s
  :x-attr #\y
  :alignment '<evil>
  :type '(<undead>)
  :depth 55
  :rarity 4
  :hitpoints '(60 . 10)
  :armour 110
  :speed 130
  :xp 12000
  :abilities '(<cold-blood> <smart> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <cold>)
  :alertness 10
  :vision 20
  :special-abilities '((<spell> <darkness>) (<spell> <forget>) (<spell> (<cause> 3)) (<spell> <scare>)
		       (<spell> <confusion>) (<spell> <blindness>) (<spell> <teleport-away>) (<frequency> 1)))

(define-monster-kind "druj-eye" "eye druj"
  :numeric-id  500
  :desc "A bloodshot eyeball floating in the air you'd be forgiven for assuming it harmless."
  :x-char #\s
  :x-attr #\r
  :alignment '<evil>
  :type '(<undead>)
  :depth 55
  :rarity 4
  :hitpoints '(10 . 100)
  :armour 90
  :speed 130
  :xp 24000
  :abilities '(<cold-blood> <smart> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <cold> <fire>)
  :alertness 10
  :vision 20
  :attacks '((<gaze> :type <exp-80> :damage nil)
	     (<gaze> :type <exp-80> :damage nil))
  :special-abilities '((<summon> <undead>) (<spell> (<ball> <nether>)) (<spell> (<bolt> <nether>))
		       (<spell> (<bolt> <mana>)) (<frequency> 1)))

(define-monster-kind "druj-skull" "skull druj"
  :numeric-id  501
  :desc "A glowing skull possessed by sorcerous power.  It need not move
but merely blast you with mighty magic."
  :x-char #\s
  :x-attr #\o
  :alignment '<evil>
  :type '(<undead>)
  :depth 55
  :rarity 4
  :hitpoints '(14 . 100)
  :armour 120
  :speed 130
  :xp 25000
  :abilities '(<cold-blood> <smart> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <cold> <fire>)
  :alertness 10
  :vision 20
  :attacks '((<bite> :type <lose-wis> :damage (4 . 4))
	     (<bite> :type <lose-int> :damage (4 . 4))
	     (<bite> :type <paralyse> :damage (4 . 4))
	     (<bite> :type <exp-80> :damage (4 . 4)))
  :special-abilities '((<summon> <undead>) (<spell> (<ball> <water>)) (<spell> (<bolt> <nether>))
		       (<spell> (<bolt> <plasma>)) (<spell> <traps>) (<spell> <brain-smash>) (<spell> <mind-blast>)
		       (<spell> (<cause> 4)) (<spell> <slow>) (<frequency> 1)))

(define-monster-kind "vortex-chaos" "chaos vortex"
  :numeric-id  502
  :desc "Void nothingness spinning destructively."
  :x-char #\v
  :x-attr #\v
  :depth 55
  :rarity 1
  :hitpoints '(32 . 20)
  :armour 80
  :speed 140
  :xp 4000
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/4) (<random-mover> 1/2) <never-attack>
	       <initial-sleeper> <colour-changing>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 100
  :special-abilities '((<breath> <chaos>) (<frequency> 1/6)))

(define-monster-kind "vortex-aether" "aether vortex"
  :numeric-id  503
  :desc "An awesome vortex of pure magic power radiates from its frame."
  :x-char #\v
  :x-attr #\v
  :depth 55
  :rarity 2
  :hitpoints '(32 . 20)
  :armour 40
  :speed 130
  :xp 4500
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/4) (<random-mover> 1/2) <initial-sleeper>
	       <colour-changing>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 0
  :vision 100
  :attacks '((<engulf> :type <cold> :damage (3 . 3))
	     (<engulf> :type <acid> :damage (3 . 3))
	     (<engulf> :type <fire> :damage (3 . 3))
	     (<engulf> :type <electricity> :damage (5 . 5)))
  :special-abilities '((<breath> <nexus>) (<breath> <plasma>) (<breath> <gravity>) (<breath> <time>)
		       (<breath> <inertia>) (<breath> <force>) (<breath> <nether>) (<breath> <shards>)
		       (<breath> <chaos>) (<breath> <confusion>) (<breath> <sound>) (<breath> <darkness>)
		       (<breath> <light>) (<breath> <poison>) (<breath> <electricity>) (<breath> <cold>)
		       (<breath> <fire>) (<breath> <acid>) (<frequency> 1/6)))

(define-monster-kind "wyrm-hell" "great hell wyrm"
  :numeric-id  506
  :desc "A vast dragon of immense power.  Fire leaps continuously from its huge
form.  The air around it scalds you.  Its slightest glance burns you and you
truly realize how insignificant you are."
  :x-char #\D
  :x-attr #\r
  :alignment '<evil>
  :type '(<dragon>)
  :depth 55
  :rarity 2
  :hitpoints '(54 . 100)
  :armour 170
  :speed 120
  :xp 23000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire>)
  :alertness 40
  :vision 40
  :attacks '((<bite> :type <hurt> :damage (4 . 14)) (<claw> :type <hurt> :damage (3 . 12))
	     (<claw> :type <hurt> :damage (1 . 12)) (<claw> :type <hurt> :damage (1 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <fire>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
		       (<frequency> 1/6)))

(define-monster-kind "quylthulg-draconic" "draconic quylthulg"
  :numeric-id  507
  :desc "It looks like it was once a dragon corpse now deeply infected with
magical bacteria that make it pulse in a foul and degrading way."
  :x-char #\Q
  :x-attr #\g
  :alignment '<evil>
  :type '(<animal>)
  :depth 55
  :rarity 3
  :hitpoints '(72 . 10)
  :armour 1
  :speed 120
  :xp 5500
  :abilities '(<empty-mind> <invisible> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 20
  :special-abilities '((<summon> <dragon>) (<spell> <teleport>) (<spell> <blink>) (<frequency> 1/2)))

(define-monster-kind "nightwalker" "nightwalker"
  :numeric-id  512
  :desc "A huge giant garbed in black more massive than a titan and stronger than a dragon.
With terrible blows it breaks your armour from your back leaving you defenseless against
its evil wrath.  It can smell your fear and you in turn smell the awful stench of death
as this ghastly figure strides towards you menacingly."
  :x-char #\W
  :x-attr #\D
  :alignment '<evil>
  :type '(<undead>)
  :depth 59
  :rarity 4
  :hitpoints '(50 . 65)
  :armour 175
  :speed 130
  :xp 15000
  :abilities '(<bash-door> <open-door> <cold-blood> <smart> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <un-bonus> :damage (7 . 7)) (<hit> :type <un-bonus> :damage (7 . 7))
	     (<hit> :type <un-bonus> :damage (10 . 10)) (<hit> :type <un-bonus> :damage (10 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :special-abilities '((<summon> <undead>) (<spell> (<ball> <nether>)) (<spell> (<bolt> <nether>))
		       (<spell> (<bolt> <mana>)) (<spell> <brain-smash>) (<spell> <scare>) (<spell> <blindness>)
		       (<frequency> 1/4)))

(define-monster-kind "dread-lord" "dreadlord"
  :numeric-id  515
  :desc "It is a massive form of animated death its colour deeper than black.  It drinks in light
and space around it is twisted and torn by the weight of its evil.  It is unlife and it knows
nothing but the stealing of souls and the stench of death.  Flee its hunger!"
  :x-char #\G
  :x-attr #\r
  :alignment '<evil>
  :type '(<undead>)
  :depth 62
  :rarity 2
  :hitpoints '(30 . 100)
  :armour 150
  :speed 120
  :xp 20000
  :abilities '(<pass-wall> <pick-up-item> <cold-blood> <invisible> (<random-mover> 1/4) <max-hitpoints>
	       <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <lose-str> :damage (4 . 6)) (<hit> :type <lose-str> :damage (4 . 6))
	     (<hit> :type <hurt> :damage (6 . 6)) (<hit> :type <hurt> :damage (6 . 6)))
  :treasures '((<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<summon> <undead>) (<spell> (<ball> <nether>)) (<spell> <drain-mana>) (<spell> <confusion>)
		       (<spell> <paralysis>) (<spell> <blindness>) (<frequency> 1/4)))

(define-monster-kind "beetle-chaos" "chaos beetle"
  :numeric-id  517
  :desc "With biting jaws and catching claws this immense beetle is like
death incarnate chasing behind you!"
  :x-char #\K
  :x-attr #\v
  :type '(<animal>)
  :depth 65
  :rarity 4
  :hitpoints '(32 . 100)
  :armour 125
  :speed 130
  :xp 19000
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :alertness 255
  :vision 35
  :attacks '((<bite> :type <hurt> :damage (10 . 10)) (<bite> :type <hurt> :damage (10 . 10))
	     (<claw> :type <hurt> :damage (10 . 10)) (<claw> :type <hurt> :damage (10 . 10)))
  :treasures '((<drop-chance> 9/10) (<drop-chance> 3/5) <only-drop-items>)
  :special-abilities '((<breath> <chaos>) (<spell> (<cause> 4)) (<frequency> 1/5)))

(define-monster-kind "hound-chaos" "chaos hound"
  :numeric-id  518
  :desc "A constantly changing canine form this hound rushes towards you as if expecting
mayhem and chaos ahead.  It appears to have an almost kamikaze relish for combat.
You suspect all may not be as it seems."
  :x-char #\Z
  :x-attr #\v
  :type '(<animal>)
  :depth 65
  :rarity 1
  :hitpoints '(60 . 30)
  :armour 100
  :speed 120
  :xp 10000
  :abilities '(<bash-door> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confusion>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<bite> :type <hurt> :damage (2 . 12))
	     (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <chaos>) (<frequency> 1/5)))

(define-monster-kind "wyrm-chaos" "great wyrm of chaos"
  :numeric-id  519
  :desc "A massive dragon of changing form.  As you watch it appears first fair
and then foul.  Its body is twisted by chaotic forces as it strives to  stay real.
Its very existence distorts the universe around it."
  :x-char #\D
  :x-attr #\v
  :alignment '<evil>
  :type '(<dragon>)
  :depth 67
  :rarity 2
  :hitpoints '(45 . 100)
  :armour 170
  :speed 120
  :xp 29000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confusion>)
  :alertness 20
  :vision 40
  :attacks '((<bite> :type <hurt> :damage (8 . 14)) (<claw> :type <hurt> :damage (6 . 12))
	     (<claw> :type <hurt> :damage (5 . 12)) (<claw> :type <hurt> :damage (5 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<summon> <dragon>) (<breath> <disenchant>) (<breath> <chaos>) (<spell> <scare>)
		       (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/3)))

(define-monster-kind "wyrm-law" "great wyrm of law"
  :numeric-id  520
  :desc "A massive dragon of powerful intellect.  It seeks to dominate the universe
and despises all other life.  It sees all who do not obey it as mere insects to be crushed underfoot."
  :x-char #\D
  :x-attr #\B
  :type '(<dragon>)
  :depth 67
  :rarity 2
  :hitpoints '(45 . 100)
  :armour 170
  :speed 120
  :xp 29000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 255
  :vision 40
  :attacks '((<bite> :type <hurt> :damage (8 . 14)) (<claw> :type <hurt> :damage (6 . 12))
	     (<claw> :type <hurt> :damage (5 . 12)) (<claw> :type <hurt> :damage (5 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<summon> <dragon>) (<breath> <shards>) (<breath> <sound>) (<spell> <scare>)
		       (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/3)))

(define-monster-kind "wyrm-balance" "great wyrm of balance"
  :numeric-id  521
  :desc "A massive dragon
one of the mightiest of dragonkind.  It is thousands of years old and seeks to
maintain the Cosmic Balance.  It sees you as an upstart troublemaker without the
wisdom to control your actions.  It will destroy you."
  :x-char #\D
  :x-attr #\v
  :type '(<dragon>)
  :depth 67
  :rarity 4
  :hitpoints '(49 . 100)
  :armour 170
  :speed 120
  :xp 31000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confusion>)
  :alertness 255
  :vision 40
  :attacks '((<bite> :type <hurt> :damage (8 . 14)) (<claw> :type <hurt> :damage (6 . 12))
	     (<claw> :type <hurt> :damage (5 . 12)) (<claw> :type <hurt> :damage (5 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<summon> <high-dragon>) (<summon> <dragon>) (<breath> <disenchant>) (<breath> <shards>)
		       (<breath> <chaos>) (<breath> <sound>) (<spell> <scare>) (<spell> <confusion>)
		       (<spell> <blindness>) (<frequency> 1/3)))

(define-monster-kind "reaver-black" "black reaver"
  :numeric-id  524
  :desc "A humanoid form black as night advancing steadily and unstoppably.  Flee!"
  :x-char #\L
  :x-attr #\D
  :alignment '<evil>
  :type '(<undead>)
  :depth 71
  :rarity 3
  :hitpoints '(35 . 100)
  :armour 170
  :speed 120
  :xp 23000
  :abilities '(<destroy-wall> <bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :alertness 50
  :vision 20
  :attacks '((<hit> :type <lose-str> :damage (4 . 6)) (<hit> :type <lose-str> :damage (4 . 6))
	     (<hit> :type <un-bonus> :damage (6 . 8)) (<hit> :type <un-bonus> :damage (6 . 8)))
  :treasures '(<drop-good> (<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :special-abilities '((<summon> <undead>) (<spell> (<ball> <nether>)) (<spell> (<ball> <mana>))
		       (<spell> <brain-smash>) (<spell> <drain-mana>) (<spell> (<cause> 4)) (<spell> (<cause> 3))
		       (<spell> <confusion>) (<spell> <paralysis>) (<spell> <blindness>) (<spell> <teleport-player>)
		       (<frequency> 1/3)))

(define-monster-kind "quylthulg-master" "master quylthulg"
  :numeric-id  525
  :desc "A pulsating mound of flesh shining with silver pulses of throbbing light."
  :x-char #\Q
  :x-attr #\B
  :alignment '<evil>
  :type '(<animal>)
  :depth 71
  :rarity 3
  :hitpoints '(20 . 100)
  :armour 1
  :speed 120
  :xp 12000
  :abilities '(<empty-mind> <invisible> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 20
  :special-abilities '((<summon> <high-demon>) (<summon> <high-dragon>) (<summon> <high-undead>) (<summon> <dragon>)
		       (<summon> <undead>) (<summon> <monsters>) (<summon> <monster>) (<frequency> 1/2)))

(define-monster-kind "quylthulg-greater-draconic" "greater draconic quylthulg"
  :numeric-id  526
  :desc "A massive mound of scaled flesh throbbing and pulsating with multi-hued light."
  :x-char #\Q
  :x-attr #\G
  :alignment '<evil>
  :type '(<animal>)
  :depth 71
  :rarity 3
  :hitpoints '(15 . 100)
  :armour 1
  :speed 120
  :xp 10500
  :abilities '(<empty-mind> <invisible> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 20
  :special-abilities '((<summon> <high-dragon>) (<spell> <teleport-player>) (<spell> <blink>) (<frequency> 1/2)))
(define-monster-kind "quylthulg-greater-rotting" "greater rotting quylthulg"
  :numeric-id  527
  :desc "A massive pile of rotting flesh.  A disgusting stench fills the air as it  throbs and writhes."
  :x-char #\Q
  :x-attr #\U
  :alignment '<evil>
  :type '(<animal>)
  :depth 71
  :rarity 3
  :hitpoints '(15 . 100)
  :armour 1
  :speed 120
  :xp 10500
  :abilities '(<empty-mind> <invisible> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 20
  :special-abilities '((<summon> <high-undead>) (<spell> <teleport-player>) (<spell> <blink>) (<frequency> 1/2)))
(define-monster-kind "hound-aether" "aether hound"
  :numeric-id  531
  :desc "A shifting swirling form.  It seems to be all colours and sizes and shapes
though the dominant form is that of a huge dog.  You feel very uncertain all of a sudden."
  :x-char #\Z
  :x-attr #\v
  :type '(<animal>)
  :depth 75
  :rarity 2
  :hitpoints '(60 . 30)
  :armour 100
  :speed 120
  :xp 10000
  :abilities '(<bash-door> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<bite> :type <hurt> :damage (2 . 12))
	     (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <nexus>) (<breath> <plasma>) (<breath> <gravity>) (<breath> <time>)
		       (<breath> <inertia>) (<breath> <force>) (<breath> <disenchant>) (<breath> <nether>)
		       (<breath> <shards>) (<breath> <chaos>) (<breath> <confusion>) (<breath> <sound>)
		       (<breath> <darkness>) (<breath> <light>) (<breath> <poison>) (<breath> <electricity>)
		       (<breath> <cold>) (<breath> <fire>) (<breath> <acid>) (<frequency> 1/5)))

(define-monster-kind "lich-draco" "dracolich"
  :numeric-id  476
  :desc "The skeletal form of a once-great dragon enchanted by magic most perilous.
Its animated form strikes with speed and drains life from its prey to satisfy its hunger."
  :x-char #\D
  :x-attr #\G
  :alignment '<evil>
  :type '(<undead> <dragon>)
  :depth 46
  :rarity 2
  :hitpoints '(35 . 100)
  :armour 120
  :speed 120
  :xp 18000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <pick-up-item> <cold-blood> <max-hitpoints>
	       <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :alertness 30
  :vision 25
  :attacks '((<bite> :type <exp-80> :damage (1 . 6)) (<bite> :type <exp-80> :damage (1 . 6))
	     (<claw> :type <hurt> :damage (1 . 12)) (<claw> :type <hurt> :damage (1 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :special-abilities '((<breath> <nether>) (<breath> <cold>) (<spell> <scare>) (<spell> <confusion>) (<frequency> 1/6)))

(define-monster-kind "quasit" "quasit"
  :numeric-id  212
  :desc "The chaotic evil master's favourite pet."
  :x-char #\u
  :x-attr #\o
  :alignment '<evil>
  :type '(<demon>)
  :depth 16
  :rarity 2
  :hitpoints '(6 . 8)
  :armour 30
  :speed 110
  :xp 50
  :abilities '(<bash-door> <invisible> <smart> (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<fire>)
  :alertness 20
  :vision 20
  :attacks '((<claw> :type <hurt> :damage (1 . 3)) (<claw> :type <hurt> :damage (1 . 3))
	     (<bite> :type <lose-dex> :damage (1 . 6)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :special-abilities '((<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>) (<spell> <teleport-level>)
		       (<spell> <teleport-player>) (<spell> <teleport>) (<spell> <blink>) (<frequency> 1/10)))

(define-monster-kind "ooze-black" "black ooze"
  :numeric-id  260
  :desc "It is a strangely moving puddle."
  :x-char #\j
  :x-attr #\D
  :depth 23
  :rarity 1
  :hitpoints '(6 . 8)
  :armour 6
  :speed 90
  :xp 7
  :abilities '(<bash-door> <open-door> <overrun-others> <pick-up-item> <breeder> <empty-mind> <stupid>
	       (<random-mover> 1/2))
  :immunities '(<fear> <poison>)
  :alertness 1
  :vision 10
  :attacks '((<touch> :type <acid> :damage (2 . 6)))
  :treasures '((<drop-chance> 3/5))
  :special-abilities '((<spell> <drain-mana>) (<frequency> 1/11)))


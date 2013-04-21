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
(define-monster-kind "fruit-bat" "fruit bat"
  :desc "A fast-moving pest."
  :symbol #\b
  :colour #\o
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

(define-monster-kind "soldier-ant" "soldier ant"
  :desc "A large ant with powerful mandibles."
  :symbol #\a
  :colour #\W
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
  :desc "It is a yapping snarling dog, dangerous when in a pack."
  :symbol #\C
  :colour #\U
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

(define-monster-kind "rock-lizard" "rock lizard"
  :desc "It is a small lizard with a hardened hide."
  :symbol #\R
  :colour #\U
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

(define-monster-kind "floating-eye" "floating eye"
  :desc "A disembodied eye, floating a few feet above the ground."
  :symbol #\e
  :colour #\o
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
  :attacks '((<gaze> :type <paralyze> :damage nil))) 

(define-monster-kind "white-worm" "white worm mass"
  :desc "It is a large slimy mass of worms."
  :symbol #\w
  :colour #\w
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
  :desc "It is a small, dog-headed humanoid."
  :symbol #\k
  :colour #\G
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

(define-monster-kind "small-kobold" "small kobold"
  :desc "It is a squat and ugly humanoid figure."
  :symbol #\k
  :colour #\y
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

(define-monster-kind "white-snake" "large white snake"
  :desc "It is about eight feet long."
  :symbol #\J
  :colour #\w
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
  :attacks '((<crush> :type <hurt> :damage (1 . 1)) (<bite> :type <hurt> :damage (1 . 1)))) 

(define-monster-kind "brown-snake" "large brown snake"
  :desc "It is about eight feet long."
  :symbol #\J
  :colour #\u
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
  :attacks '((<crush> :type <hurt> :damage (1 . 4)) (<bite> :type <hurt> :damage (1 . 3)))) 

(define-monster-kind "white-mouse" "giant white mouse"
  :desc "It is about three feet long with large teeth."
  :symbol #\r
  :colour #\w
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

(define-monster-kind "clear-icky" "clear icky thing"
  :desc "It is a smallish, slimy, icky, blobby creature."
  :symbol #\i
  :colour #\w
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

(define-monster-kind "white-icky" "white icky thing"
  :desc "It is a smallish, slimy, icky creature."
  :symbol #\i
  :colour #\w
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

(define-monster-kind "grey-mold" "grey mold"
  :desc "A small strange growth."
  :symbol #\m
  :colour #\s
  :depth 1
  :rarity 1
  :hitpoints '(1 . 2)
  :armour 1
  :speed 110
  :xp 3
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse> <poison>)
  :alertness 0
  :vision 2
  :attacks '((<hit> :type <hurt> :damage (1 . 4)) (<hit> :type <hurt> :damage (1 . 4)))) 

(define-monster-kind "white-centipede" "giant white centipede"
  :desc "It is about four feet long and carnivorous."
  :symbol #\c
  :colour #\w
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
  :attacks '((<sting> :type <hurt> :damage (1 . 2)) (<bite> :type <hurt> :damage (1 . 2)))) 

(define-monster-kind "yellow-centipede" "giant yellow centipede"
  :desc "It is about four feet long and carnivorous."
  :symbol #\c
  :colour #\y
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
  :attacks '((<sting> :type <hurt> :damage (1 . 3)) (<bite> :type <hurt> :damage (1 . 3)))) 

(define-monster-kind "grey-mushroom-patch" "grey mushroom patch"
  :desc "Yum!  It looks quite tasty."
  :symbol #\,
  :colour #\s
  :depth 1
  :rarity 1
  :hitpoints '(1 . 2)
  :armour 1
  :speed 110
  :xp 1
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse> <poison>)
  :alertness 0
  :vision 2
  :attacks '((<spore> :type <confuse> :damage (1 . 4)))) 

;;; end depth 1 monsters

;;; === depth 2

(define-monster-kind "green-worm" "green worm mass"
  :desc "It is a large slimy mass of worms."
  :symbol #\w
  :colour #\g
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


(define-monster-kind "white-harpy" "white harpy"
  :desc "A flying, screeching bird with a woman's face."
  :symbol #\H
  :colour #\w
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
  :attacks '((<bite> :type <hurt> :damage (1 . 2)) (<claw> :type <hurt> :damage (1 . 1))
             (<claw> :type <hurt> :damage (1 . 1)))
  :sex '<female>) 


(define-monster-kind "salamander" "salamander"
  :desc "A small black and orange lizard."
  :symbol #\R
  :colour #\o
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

(define-monster-kind "yellow-snake" "large yellow snake"
  :desc "It is about ten feet long."
  :symbol #\J
  :colour #\y
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
  :attacks '((<crush> :type <hurt> :damage (1 . 6)) (<bite> :type <hurt> :damage (1 . 4)))) 


(define-monster-kind "black-ant" "giant black ant"
  :desc "It is about three feet long."
  :symbol #\a
  :colour #\D
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

(define-monster-kind "green-frog" "giant green frog"
  :desc "It is as big as a wolf."
  :symbol #\R
  :colour #\g
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

(define-monster-kind "white-jelly" "white jelly"
  :desc "Its a large pile of white flesh."
  :symbol #\j
  :colour #\w
  :depth 2
  :rarity 1
  :hitpoints '(8 . 8)
  :armour 1
  :speed 120
  :xp 10
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse> <poison>)
  :vulnerabilities '(<light>)
  :alertness 99
  :vision 2
  :attacks '((<touch> :type <poison> :damage (1 . 2)))) 

(define-monster-kind "yellow-mushroom" "yellow mushroom patch"
  :desc "Yum!  It looks quite tasty."
  :symbol #\,
  :colour #\y
  :depth 2
  :rarity 1
  :hitpoints '(1 . 1)
  :armour 1
  :speed 110
  :xp 2
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse> <poison>)
  :alertness 0
  :vision 2
  :attacks '((<spore> :type <terrify> :damage (1 . 6)))) 

(define-monster-kind "cave-spider" "cave spider"
  :desc "It is a black spider that moves in fits and starts."
  :symbol #\S
  :colour #\D
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

(define-monster-kind "wild-cat" "wild cat"
  :desc "A larger than normal feline, hissing loudly.  Its velvet claws conceal a  fistful of needles."
  :symbol #\f
  :colour #\U
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

(define-monster-kind "blue-yeek" "blue yeek"
  :desc "A small humanoid figure."
  :symbol #\y
  :colour #\b
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

(define-monster-kind "novice-priest" "novice priest"
  :desc "He is tripping over his priestly robes."
  :symbol #\p
  :colour #\g
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
  :sex '<male>
  :special-abilities '((<spell> (<cause> 1)) (<spell> <scare>) (<spell> <heal>) (<frequency> 1/12))
  :appear-in-group? #'van-novice-appears-in-group?)

(define-monster-kind "novice-rogue" "novice rogue"
  :desc "A rather shifty individual."
  :symbol #\p
  :colour #\b
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
  :attacks '((<touch> :type <eat-gold> :damage nil) (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop-chance> 3/5))
  :sex '<male>
  :appear-in-group? #'van-novice-appears-in-group?)

(define-monster-kind "novice-warriour" "novice warriour"
  :desc "He looks inexperienced but tough."
  :symbol #\p
  :colour #\u
  :depth 2
  :rarity 1
  :hitpoints '(9 . 4)
  :armour 16
  :speed 110
  :xp 6
  :abilities '(<bash-door> <open-door>)
  :alertness 5
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 7)))
  :treasures '((<drop-chance> 3/5))
  :sex '<male>
  :appear-in-group? #'van-novice-appears-in-group?)


(define-monster-kind "metallic-green-centipede" "metallic green centipede"
  :desc "It is about four feet long and carnivorous."
  :symbol #\c
  :colour #\g
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

(define-monster-kind "shrieker-mushroom" "shrieker mushroom patch"
  :desc "Yum!  These look quite tasty."
  :symbol #\,
  :colour #\R
  :depth 2
  :rarity 1
  :hitpoints '(1 . 1)
  :armour 1
  :speed 110
  :xp 1
  :abilities '(<empty-mind> <stupid> <never-attack> <never-move> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison>)
  :alertness 0
  :vision 4
  :special-abilities '(<shriek> (<frequency> 1/4))) 

(define-monster-kind "blubbering-icky" "blubbering icky thing"
  :desc "It is a smallish, slimy, icky, hungry creature."
  :symbol #\i
  :colour #\W
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


(define-monster-kind "novice-mage" "novice mage"
  :desc "He is leaving behind a trail of dropped spell components."
  :symbol #\p
  :colour #\r
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
  :sex '<male>
  :special-abilities '((<spell> <missile>) (<spell> <confusion>) (<spell> <blindness>) (<spell> <blink>)
                       (<frequency> 1/12))
  :appear-in-group? #'van-novice-appears-in-group?)


;;; end depth 2 monsters

;;; === depth 3

(define-monster-kind "green-ooze" "green ooze"
  :desc "It's green and it's oozing."
  :symbol #\j
  :colour #\g
  :depth 3
  :rarity 2
  :hitpoints '(3 . 4)
  :armour 16
  :speed 120
  :xp 4
  :abilities '(<empty-mind> <stupid> (<random-mover> 1/4) (<random-mover> 1/2))
  :immunities '(<fear> <sleep> <confuse> <poison> <acid>)
  :alertness 80
  :vision 8
  :attacks '((<crawl> :type <acid> :damage (1 . 3)))
  :treasures '((<drop-chance> 9/10))) 


(define-monster-kind "poltergeist" "poltergeist"
  :desc "It is a ghastly, ghostly form."
  :symbol #\G
  :colour #\W
  :alignment '<evil>
  :type '(<undead>)
  :depth 3
  :rarity 1
  :hitpoints '(2 . 5)
  :armour 15
  :speed 130
  :xp 8
  :abilities '(<pick-up-item> <pass-wall> <cold-blood> <invisible> (<random-mover> 1/4) (<random-mover> 1/2))
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 8
  :attacks '((<touch> :type <terrify> :damage nil))
  :treasures '((<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<spell> <blink>) (<frequency> 1/15))) 


(define-monster-kind "blue-centipede" "metallic blue centipede"
  :desc "It is about four feet long and carnivorous."
  :symbol #\c
  :colour #\b
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

(define-monster-kind "white-louse" "giant white louse"
  :desc "It is six inches long."
  :symbol #\l
  :colour #\w
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

(define-monster-kind "black-naga" "black naga"
  :desc "A large black serpent's body with a female torso."
  :symbol #\n
  :colour #\D
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
  :sex '<female>) 

(define-monster-kind "spotted-mushroom" "spotted mushroom patch"
  :desc "Yum!  It looks quite tasty."
  :symbol #\,
  :colour #\o
  :depth 3
  :rarity 1
  :hitpoints '(1 . 1)
  :armour 1
  :speed 110
  :xp 3
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse> <poison>)
  :alertness 0
  :vision 2
  :attacks '((<spore> :type <poison> :damage (2 . 4)))) 

(define-monster-kind "silver-jelly" "silver jelly"
  :desc "It is a large pile of silver flesh that sucks all light from its  surroundings."
  :symbol #\j
  :colour #\W
  :depth 3
  :rarity 2
  :hitpoints '(10 . 8)
  :armour 1
  :speed 120
  :xp 12
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse> <poison>)
  :vulnerabilities '(<light>)
  :alertness 99
  :vision 2
  :attacks '((<touch> :type <eat_lite> :damage (1 . 3)) (<touch> :type <eat_lite> :damage (1 . 3)))
  :special-abilities '((<spell> <drain-mana>) (<frequency> 1/15))) 


(define-monster-kind "yellow-jelly" "yellow jelly"
  :desc "It's a large pile of yellow flesh."
  :symbol #\j
  :colour #\y
  :depth 3
  :rarity 1
  :hitpoints '(10 . 8)
  :armour 1
  :speed 120
  :xp 12
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse> <poison>)
  :vulnerabilities '(<light>)
  :alertness 99
  :vision 2
  :attacks '((<touch> :type <poison> :damage (1 . 3)))
  :special-abilities '((<spell> <drain-mana>) (<frequency> 1/15))) 

(define-monster-kind "scruffy-hobbit" "scruffy looking hobbit"
  :desc "A short little guy, in bedraggled clothes.  He appears to be looking  for a good tavern."
  :symbol #\h
  :colour #\b
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
  :sex '<male>) 

(define-monster-kind "white-ant" "giant white ant"
  :desc "It is about two feet long and has sharp pincers."
  :symbol #\a
  :colour #\w
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

(define-monster-kind "yellow-mold" "yellow mold"
  :desc "It is a strange growth on the dungeon floor."
  :symbol #\m
  :colour #\y
  :depth 3
  :rarity 1
  :hitpoints '(8 . 8)
  :armour 10
  :speed 110
  :xp 9
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse> <poison>)
  :alertness 99
  :vision 2
  :attacks '((<hit> :type <hurt> :damage (1 . 4)))) 

(define-monster-kind "red-centipede" "metallic red centipede"
  :desc "It is about four feet long and carnivorous."
  :symbol #\c
  :colour #\r
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

(define-monster-kind "yellow-worm" "yellow worm mass"
  :desc "It is a large slimy mass of worms."
  :symbol #\w
  :colour #\y
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
  :attacks '((<crawl> :type <lose_dex> :damage (1 . 3)))) 

(define-monster-kind "clear-worm" "clear worm mass"
  :desc "It is a disgusting mass of poisonous worms."
  :symbol #\w
  :colour #\w
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

(define-monster-kind "radiation-eye" "radiation eye"
  :desc "A disembodied eye, crackling with energy."
  :symbol #\e
  :colour #\R
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


(define-monster-kind "cave-lizard" "cave lizard"
  :desc "It is an armoured lizard with a powerful bite."
  :symbol #\R
  :colour #\u
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

(define-monster-kind "novice-ranger" "novice ranger"
  :desc "An agile hunter, ready and relaxed."
  :symbol #\p
  :colour #\W
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
  :sex '<male>
  :special-abilities '((<spell> <missile>) (<arrow> 2) (<frequency> 1/9))
  :appear-in-group? #'van-novice-appears-in-group?)



(define-monster-kind "novice-paladin" "novice paladin"
  :desc "An adventurer both devoutly religious and skillful in combat."
  :symbol #\p
  :colour #\w
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
  :sex '<male>
  :special-abilities '((<spell> (<cause> 1)) (<spell> <scare>) (<frequency> 1/9))
  :appear-in-group? #'van-novice-appears-in-group?)


(define-monster-kind "blue-jelly" "blue jelly"
  :desc "It's a large pile of pulsing blue flesh."
  :symbol #\j
  :colour #\b
  :depth 4
  :rarity 1
  :hitpoints '(12 . 8)
  :armour 1
  :speed 110
  :xp 14
  :abilities '(<empty-mind> <stupid> <cold-blood> <never-move>)
  :immunities '(<fear> <sleep> <confuse> <cold>)
  :vulnerabilities '(<light>)
  :alertness 99
  :vision 2
  :attacks '((<touch> :type <cold> :damage (1 . 6)))) 

(define-monster-kind "creeping-copper" "creeping copper coins"
  :desc "It is a pile of coins."
  :symbol #\$
  :colour #\u
  :type '(<animal>)
  :depth 4
  :rarity 2
  :hitpoints '(7 . 8)
  :armour 24
  :speed 100
  :xp 9
  :abilities '(<bash-door> <cold-blood>)
  :immunities '(<sleep> <confuse> <poison>)
  :alertness 10
  :vision 3
  :attacks '((<touch> :type <poison> :damage (2 . 4)) (<hit> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "1d2") <only-drop-gold>)) 

(define-monster-kind "white-rat" "giant white rat"
  :desc "It is a very vicious rodent."
  :symbol #\r
  :colour #\W
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

(define-monster-kind "blue-worm" "blue worm mass"
  :desc "It is a large slimy mass of worms."
  :symbol #\w
  :colour #\b
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

(define-monster-kind "grey-snake" "large grey snake"
  :desc "It is about ten feet long."
  :symbol #\J
  :colour #\s
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


(define-monster-kind "green-naga" "green naga"
  :desc "A large green serpent with a female's torso.  Her green skin glistens with  acid."
  :symbol #\n
  :colour #\g
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
  :sex '<female>) 

(define-monster-kind "blue-ooze" "blue ooze"
  :desc "It's blue and it's oozing."
  :symbol #\j
  :colour #\b
  :depth 5
  :rarity 1
  :hitpoints '(3 . 4)
  :armour 16
  :speed 110
  :xp 7
  :abilities '(<empty-mind> <stupid> (<random-mover> 1/4) (<random-mover> 1/2))
  :immunities '(<fear> <sleep> <confuse> <cold>)
  :alertness 80
  :vision 8
  :attacks '((<crawl> :type <cold> :damage (1 . 4)))
  :treasures '((<drop-chance> 3/5))) 


(define-monster-kind "glutton-ghost" "green glutton ghost"
  :desc "It is a very ugly green ghost with a voracious appetite."
  :symbol #\G
  :colour #\g
  :alignment '<evil>
  :type '(<undead>)
  :depth 5
  :rarity 1
  :hitpoints '(3 . 4)
  :armour 20
  :speed 130
  :xp 15
  :abilities '(<pass-wall> <cold-blood> <invisible> (<random-mover> 1/4) (<random-mover> 1/2))
  :immunities '(<sleep> <confuse>)
  :alertness 10
  :vision 10
  :attacks '((<touch> :type <eat_food> :damage (1 . 1)))
  :treasures '((<drop-chance> 9/10) (<drop-chance> 3/5))) 

(define-monster-kind "green-jelly" "green jelly"
  :desc "It is a large pile of pulsing green flesh."
  :symbol #\j
  :colour #\g
  :depth 5
  :rarity 1
  :hitpoints '(22 . 8)
  :armour 1
  :speed 120
  :xp 18
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse> <acid>)
  :vulnerabilities '(<light>)
  :alertness 99
  :vision 2
  :attacks '((<touch> :type <acid> :damage (1 . 2)))) 

(define-monster-kind "large-kobold" "large kobold"
  :desc "It a man-sized figure with the all too recognizable face of a kobold."
  :symbol #\k
  :colour #\b
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

(define-monster-kind "skeleton-kobold" "skeleton kobold"
  :desc "It is a small animated kobold skeleton."
  :symbol #\s
  :colour #\w
  :alignment '<evil>
  :type '(<undead>)
  :depth 5
  :rarity 1
  :hitpoints '(5 . 8)
  :armour 26
  :speed 110
  :xp 12
  :abilities '(<bash-door> <open-door> <empty-mind> <cold-blood>)
  :immunities '(<fear> <sleep> <confuse> <poison> <cold>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 6)))) 

(define-monster-kind "grey-icky" "grey icky thing"
  :desc "It is a smallish, slimy, icky, nasty creature."
  :symbol #\i
  :colour #\s
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

(define-monster-kind "disenchanter-eye" "disenchanter eye"
  :desc "A disembodied eye, crackling with magic."
  :symbol #\e
  :colour #\v
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
  :attacks '((<gaze> :type <un_bonus> :damage nil))
  :special-abilities '((<spell> <drain-mana>) (<frequency> 1/9))) 

(define-monster-kind "red-worm" "red worm mass"
  :desc "It is a large slimy mass of worms."
  :symbol #\w
  :colour #\r
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

(define-monster-kind "copper-snake" "copperhead snake"
  :desc "It has a copper head and sharp venomous fangs."
  :symbol #\J
  :colour #\o
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


(define-monster-kind "purple-mushroom" "purple mushroom patch"
  :desc "Yum!  It looks quite tasty."
  :symbol #\,
  :colour #\v
  :depth 6
  :rarity 2
  :hitpoints '(1 . 1)
  :armour 1
  :speed 110
  :xp 15
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 2
  :attacks '((<spore> :type <lose_con> :damage (1 . 2)) (<spore> :type <lose_con> :damage (1 . 2))
             (<spore> :type <lose_con> :damage (1 . 2)))) 


(define-monster-kind "brown-mold" "brown mold"
  :desc "A strange brown growth on the dungeon floor."
  :symbol #\m
  :colour #\u
  :depth 6
  :rarity 1
  :hitpoints '(15 . 8)
  :armour 12
  :speed 110
  :xp 20
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse> <poison>)
  :alertness 99
  :vision 2
  :attacks '((<hit> :type <confuse> :damage (1 . 4)))) 

(define-monster-kind "brown-bat" "giant brown bat"
  :desc "It screeches as it attacks."
  :symbol #\b
  :colour #\u
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

(define-monster-kind "novice-archer" "novice archer"
  :desc "A nasty little fellow with a bow and arrow."
  :symbol #\p
  :colour #\W
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
  :sex '<male>
  :special-abilities '((<arrow> 1) (<frequency> 1/3))) 

(define-monster-kind "creeping-silver" "creeping silver coins"
  :desc "It is a pile of coins, crawling forward on thousands of tiny legs."
  :symbol #\$
  :colour #\s
  :type '(<animal>)
  :depth 6
  :rarity 2
  :hitpoints '(12 . 8)
  :armour 30
  :speed 100
  :xp 18
  :abilities '(<bash-door> <cold-blood>)
  :immunities '(<sleep> <confuse> <poison>)
  :alertness 10
  :vision 4
  :attacks '((<touch> :type <poison> :damage (2 . 6)) (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5) <only-drop-gold>)) 

(define-monster-kind "snaga" "snaga"
  :desc "He is one of the many weaker 'slave' orcs, often mistakenly known as a  goblin."
  :symbol #\o
  :colour #\U
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
  :sex '<male>) 

(define-monster-kind "rattlesnake" "rattlesnake"
  :desc "It is recognized by the hard-scaled end of its body that is often rattled  to frighten its prey."
  :symbol #\J
  :colour #\r
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

(define-monster-kind "cave-orc" "cave orc"
  :desc "He is often found in huge numbers in deep caves."
  :symbol #\o
  :colour #\G
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
  :sex '<male>) 

(define-monster-kind "wood-spider" "wood spider"
  :desc "It scuttles towards you."
  :symbol #\S
  :colour #\U
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
  :desc "It is a minor but aggressive demon."
  :symbol #\u
  :colour #\u
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

(define-monster-kind "bloody-eye" "bloodshot eye"
  :desc "A disembodied eye, bloodshot and nasty."
  :symbol #\e
  :colour #\r
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

(define-monster-kind "pink-naga" "pink naga"
  :desc "A large pink snake with a woman's torso."
  :symbol #\n
  :colour #\R
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
  :sex '<female>) 

(define-monster-kind "pink-jelly" "pink jelly"
  :desc "It is a large pulsating mound of red flesh."
  :symbol #\j
  :colour #\R
  :depth 7
  :rarity 1
  :hitpoints '(26 . 8)
  :armour 1
  :speed 110
  :xp 26
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse>)
  :vulnerabilities '(<light>)
  :alertness 99
  :vision 2
  :attacks '((<touch> :type <lose-str> :damage (1 . 5)))) 

(define-monster-kind "pink-frog" "giant pink frog"
  :desc "It looks poisonous."
  :symbol #\R
  :colour #\R
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

(define-monster-kind "green-icky-thing" "green icky thing"
  :desc "It is a smallish, slimy, icky, acidic creature."
  :symbol #\i
  :colour #\g
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

(define-monster-kind "zombie-kobold" "zombified kobold"
  :desc "It is an animated kobold corpse.  Flesh falls off in large chunks as it  shambles forward."
  :symbol #\z
  :colour #\s
  :alignment '<evil>
  :type '(<undead>)
  :depth 7
  :rarity 1
  :hitpoints '(6 . 8)
  :armour 14
  :speed 110
  :xp 14
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confuse> <poison> <cold>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 2)) (<hit> :type <hurt> :damage (1 . 2)))) 

(define-monster-kind "lost-soul" "lost soul"
  :desc "It is almost insubstantial."
  :symbol #\G
  :colour #\B
  :alignment '<evil>
  :type '(<undead>)
  :depth 7
  :rarity 2
  :hitpoints '(2 . 8)
  :armour 10
  :speed 110
  :xp 18
  :abilities '(<pass-wall> <pick-up-item> <cold-blood> <invisible> (<random-mover> 1/2))
  :immunities '(<sleep> <confuse> <cold>)
  :alertness 10
  :vision 12
  :attacks '((<touch> :type <lose-wis> :damage nil) (<hit> :type <hurt> :damage (2 . 2)))
  :treasures '((<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<spell> <drain-mana>) (<spell> <teleport>) (<frequency> 1/15))) 

(define-monster-kind "dark-elf" "dark elf"
  :desc "An elven figure with jet black skin and white hair, his eyes are large and  twisted with evil."
  :symbol #\h
  :colour #\D
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
  :sex '<male>
  :special-abilities '((<spell> <darkness>) (<spell> <confusion>) (<frequency> 1/10))) 

(define-monster-kind "night-lizard" "night lizard"
  :desc "It is a black lizard with overlapping scales and a powerful jaw."
  :symbol #\R
  :colour #\D
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


(define-monster-kind "brown-yeek" "brown yeek"
  :desc "It is a strange small humanoid."
  :symbol #\y
  :colour #\u
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


(define-monster-kind "giant-salamander" "giant salamander"
  :desc "A large black and yellow lizard.  You'd better run away!"
  :symbol #\R
  :colour #\y
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

(define-monster-kind "green-mold" "green mold"
  :desc "It is a strange growth on the dungeon floor."
  :symbol #\m
  :colour #\g
  :depth 8
  :rarity 2
  :hitpoints '(21 . 8)
  :armour 14
  :speed 110
  :xp 28
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse> <poison> <acid>)
  :alertness 75
  :vision 2
  :attacks '((<hit> :type <terrify> :damage (1 . 4)))) 

(define-monster-kind "skeleton-orc" "skeleton orc"
  :desc "It is an animated orc skeleton."
  :symbol #\s
  :colour #\w
  :alignment '<evil>
  :type '(<undead> <orc>)
  :depth 8
  :rarity 1
  :hitpoints '(10 . 8)
  :armour 36
  :speed 110
  :xp 26
  :abilities '(<bash-door> <open-door> <empty-mind> <cold-blood>)
  :immunities '(<fear> <sleep> <confuse> <poison> <cold>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 5)))) 


(define-monster-kind "lemure" "lemure"
  :desc "It is the larval form of a major demon."
  :symbol #\u
  :colour #\U
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


(define-monster-kind "hill-orc" "hill orc"
  :desc "He is a hardy well-weathered survivor."
  :symbol #\o
  :colour #\u
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
  :sex '<male>) 

(define-monster-kind "bandit" "bandit"
  :desc "He is after your cash!"
  :symbol #\p
  :colour #\b
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
  :sex '<male>) 


;;; end depth 8 monsters

;;; === depth 9


(define-monster-kind "yeti" "yeti"
  :desc "A large white figure covered in shaggy fur."
  :symbol #\Y
  :colour #\b
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

(define-monster-kind "bloodshot-icky" "bloodshot icky thing"
  :desc "It is a strange, slimy, icky creature."
  :symbol #\i
  :colour #\r
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

(define-monster-kind "grey-rat" "giant grey rat"
  :desc "It is a rodent of unusual size."
  :symbol #\r
  :colour #\s
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

(define-monster-kind "black-harpy" "black harpy"
  :desc "A woman's face on the body of a vicious black bird."
  :symbol #\H
  :colour #\D
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
  :sex '<female>) 

(define-monster-kind "orc-shaman" "orc shaman"
  :desc "An orc dressed in skins who gestures wildly."
  :symbol #\o
  :colour #\r
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
  :sex '<male>
  :special-abilities '((<spell> <missile>) (<spell> (<cause> 1)) (<spell> <blink>) (<frequency> 1/8))) 

(define-monster-kind "baby-blue-dragon" "baby blue dragon"
  :desc "This hatchling dragon is still soft, its eyes unaccustomed to light and  its scales a pale blue."
  :symbol #\d
  :colour #\b
  :alignment '<evil>
  :type '(<dragon>)
  :depth 9
  :rarity 2
  :hitpoints '(10 . 10)
  :armour 30
  :speed 110
  :xp 35
  :abilities '(<bash-door> <open-door> <initial-sleeper> <max-hitpoints>)
  :immunities '(<lightning>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 5)) (<claw> :type <hurt> :damage (1 . 3))
             (<claw> :type <hurt> :damage (1 . 3)))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5) <only-drop-gold>)
  :special-abilities '((<breath> <lightning>) (<frequency> 1/11))) 

(define-monster-kind "baby-white-dragon" "baby white dragon"
  :desc "This hatchling dragon is still soft, its eyes unaccustomed to light and  its scales a pale white."
  :symbol #\d
  :colour #\w
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

(define-monster-kind "baby-green-dragon" "baby green dragon"
  :desc "This hatchling dragon is still soft, its eyes unaccustomed to light and  its scales a sickly green."
  :symbol #\d
  :colour #\g
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


(define-monster-kind "baby-black-dragon" "baby black dragon"
  :desc "This hatchling dragon is still soft, its eyes unaccustomed to light and  its scales a dull black."
  :symbol #\d
  :colour #\s
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

(define-monster-kind "baby-red-dragon" "baby red dragon"
  :desc "This hatchling dragon is still soft, its eyes unaccustomed to light and  its scales a pale red."
  :symbol #\d
  :colour #\r
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

(define-monster-kind "pink-ant" "giant pink ant"
  :desc "It is large and has venomous mandibles."
  :symbol #\a
  :colour #\R
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


(define-monster-kind "king-cobra" "king cobra"
  :desc "It is a large snake with a hooded face."
  :symbol #\J
  :colour #\g
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



(define-monster-kind "giant-spider" "giant spider"
  :desc "It is a vast black spider whose bulbous body is bloated with poison."
  :symbol #\S
  :colour #\v
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
  :desc "A dark elven figure, dressed all in black, hurling spells at you."
  :symbol #\h
  :colour #\v
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
  :sex '<male>
  :special-abilities '((<spell> (<ball> <poison>)) (<spell> <darkness>) (<spell> <missile>) (<spell> <confusion>)
                       (<spell> <blindness>) (<frequency> 1/5))) 


(define-monster-kind "dark-elf-warriour" "dark elven warriour"
  :desc "A dark elven figure in armour and ready with his sword."
  :symbol #\h
  :colour #\u
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
  :sex '<male>) 

(define-monster-kind "clear-mushroom" "clear mushroom patch"
  :desc "Yum!  It looks quite tasty."
  :symbol #\,
  :colour #\w
  :depth 10
  :rarity 2
  :hitpoints '(1 . 1)
  :armour 1
  :speed 120
  :xp 3
  :abilities '(<empty-mind> <stupid> <breeder> <cold-blood> <invisible> <never-move> <see-through>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 4
  :attacks '((<spore> :type <hurt> :damage (1 . 1)))) 

(define-monster-kind "white-tick" "giant white tick"
  :desc "It is moving slowly towards you."
  :symbol #\S
  :colour #\w
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

(define-monster-kind "hairy-mold" "hairy mold"
  :desc "It is a strange hairy growth on the dungeon floor."
  :symbol #\m
  :colour #\o
  :depth 10
  :rarity 2
  :hitpoints '(15 . 8)
  :armour 15
  :speed 110
  :xp 32
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse> <poison>)
  :alertness 70
  :vision 2
  :attacks '((<hit> :type <poison> :damage (1 . 3)))) 

(define-monster-kind "disenchanter-mold" "disenchanter mold"
  :desc "It is a strange glowing growth on the dungeon floor."
  :symbol #\m
  :colour #\v
  :depth 10
  :rarity 2
  :hitpoints '(16 . 8)
  :armour 20
  :speed 110
  :xp 40
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse> <poison>)
  :alertness 70
  :vision 2
  :attacks '((<touch> :type <un-bonus> :damage (1 . 6)))
  :special-abilities '((<spell> <drain-mana>) (<frequency> 1/11))) 

(define-monster-kind "pseudo-dragon" "pseudo dragon"
  :desc "A small relative of the dragon that inhabits dark caves."
  :symbol #\d
  :colour #\o
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
  :desc "It is a fast-moving demon that blinks quickly in and out of existence; no  other demon matches its teleporting mastery."
  :symbol #\u
  :colour #\R
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
  :desc "It is a pile of coins, crawling forward on thousands of tiny legs."
  :symbol #\$
  :colour #\y
  :type '(<animal>)
  :depth 10
  :rarity 3
  :hitpoints '(18 . 8)
  :armour 36
  :speed 100
  :xp 32
  :abilities '(<bash-door> <cold-blood>)
  :immunities '(<sleep> <confuse> <poison>)
  :alertness 10
  :vision 5
  :attacks '((<touch> :type <poison> :damage (3 . 5)) (<hit> :type <hurt> :damage (2 . 5)))
  :treasures '((<drop> "1d2") (<drop-chance> 9/10) <only-drop-gold>)) 

(define-monster-kind "wolf" "wolf"
  :desc "It howls and snaps at you."
  :symbol #\C
  :colour #\u
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

(define-monster-kind "giant-fruit-fly" "giant fruit fly"
  :desc "A fast-breeding, annoying pest."
  :symbol #\I
  :colour #\G
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
  :desc "A large black cat, stalking you with intent.  It thinks you're its next  meal."
  :symbol #\f
  :colour #\u
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
  :desc "He is eyeing your backpack."
  :symbol #\p
  :colour #\b
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
  :sex '<male>) 

(define-monster-kind "baby-mh-dragon" "baby multi-hued dragon"
  :desc "This hatchling dragon is still soft, its eyes unaccustomed to light and  its scales shimmering with a hint of colour."
  :symbol #\d
  :colour #\v
  :alignment '<evil>
  :type '(<dragon>)
  :depth 11
  :rarity 2
  :hitpoints '(13 . 10)
  :armour 30
  :speed 110
  :xp 45
  :abilities '(<bash-door> <open-door> <initial-sleeper> <max-hitpoints> <colour-changing>)
  :immunities '(<poison> <lightning> <cold> <fire> <acid>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 5)) (<claw> :type <hurt> :damage (1 . 3))
             (<claw> :type <hurt> :damage (1 . 3)))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5) <only-drop-gold>)
  :special-abilities '((<breath> <poison>) (<breath> <lightning>) (<breath> <cold>) (<breath> <fire>) (<breath> <acid>)
                       (<frequency> 1/11))) 

(define-monster-kind "hippogriff" "hippogriff"
  :desc "A strange hybrid of eagle, lion and horse.  It looks weird."
  :symbol #\H
  :colour #\U
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

(define-monster-kind "zombie-orc" "zombified orc"
  :desc "It is a shambling orcish corpse leaving behind a trail of flesh."
  :symbol #\z
  :colour #\s
  :alignment '<evil>
  :type '(<undead> <orc>)
  :depth 11
  :rarity 1
  :hitpoints '(11 . 8)
  :armour 24
  :speed 110
  :xp 30
  :abilities '(<bash-door> <open-door> <empty-mind> <cold-blood>)
  :immunities '(<fear> <sleep> <confuse> <poison> <cold>)
  :alertness 25
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 4)) (<hit> :type <hurt> :damage (1 . 4))
             (<hit> :type <hurt> :damage (1 . 4)))) 

(define-monster-kind "gnome-mage" "gnome mage"
  :desc "A mage of short stature."
  :symbol #\h
  :colour #\r
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
  :sex '<male>
  :special-abilities '((<summon> <monster>) (<spell> (<bolt> <cold>)) (<spell> <darkness>) (<spell> <blink>)
                       (<frequency> 1/4))
  :appear-in-group? #'van-novice-appears-in-group?)


(define-monster-kind "black-mamba" "black mamba"
  :desc "It has glistening black skin, a sleek body and highly venomous fangs."
  :symbol #\J
  :colour #\D
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

(define-monster-kind "white-wolf" "white wolf"
  :desc "A large and muscled wolf from the northern wastes.  Its breath is cold and  icy and its fur coated in frost."
  :symbol #\C
  :colour #\w
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

(define-monster-kind "grape-jelly" "grape jelly"
  :desc "It is a pulsing mound of glowing flesh."
  :symbol #\j
  :colour #\v
  :depth 12
  :rarity 3
  :hitpoints '(52 . 8)
  :armour 1
  :speed 110
  :xp 60
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse> <poison>)
  :vulnerabilities '(<light>)
  :alertness 99
  :vision 2
  :attacks '((<touch> :type <exp-10> :damage nil))
  :special-abilities '((<spell> <drain-mana>) (<frequency> 1/11))) 

(define-monster-kind "nether-worm" "nether worm mass"
  :desc "It is a disgusting mass of dark worms, eating each other, the floor,  the air, you...."
  :symbol #\w
  :colour #\D
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

(define-monster-kind "master-yeek" "master yeek"
  :desc "A small humanoid that radiates some power."
  :symbol #\y
  :colour #\U
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
  :desc "A robed humanoid dedicated to his god."
  :symbol #\p
  :colour #\g
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
  :sex '<male>
  :special-abilities '((<summon> <monster>) (<spell> (<cause> 2)) (<spell> <scare>) (<spell> <heal>) (<frequency> 1/3))) 

(define-monster-kind "dark-elf-priest" "dark elven priest"
  :desc "A dark elven figure, dressed all in black, chanting curses and waiting to  deliver your soul to hell."
  :symbol #\h
  :colour #\g
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
  :sex '<male>
  :special-abilities '((<spell> <darkness>) (<spell> (<cause> 2)) (<spell> <confusion>) (<spell> <blindness>)
                       (<spell> <heal>) (<frequency> 1/5))) 

(define-monster-kind "air-spirit" "air spirit"
  :desc "A whirlwind of sentient air."
  :symbol #\E
  :colour #\B
  :alignment '<evil>
  :depth 12
  :rarity 2
  :hitpoints '(8 . 8)
  :armour 40
  :speed 130
  :xp 40
  :abilities '(<bash-door> <cold-blood> <invisible> <empty-mind> (<random-mover> 1/4) (<random-mover> 1/2))
  :immunities '(<fear> <sleep> <confuse> <poison>)
  :alertness 20
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (1 . 3)))) 

(define-monster-kind "monster-164" "skeleton human"
  :desc "It is an animated human skeleton."
  :symbol #\s
  :colour #\w
  :alignment '<evil>
  :type '(<undead>)
  :depth 12
  :rarity 1
  :hitpoints '(10 . 8)
  :armour 30
  :speed 110
  :xp 38
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confuse> <poison> <cold>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 8)))) 

(define-monster-kind "monster-165" "zombified human"
  :desc "It is a shambling human corpse dropping chunks of flesh behind it."
  :symbol #\z
  :colour #\s
  :alignment '<evil>
  :type '(<undead>)
  :depth 12
  :rarity 1
  :hitpoints '(12 . 8)
  :armour 24
  :speed 110
  :xp 34
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confuse> <poison> <cold>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 4)) (<hit> :type <hurt> :damage (1 . 4)))) 

(define-monster-kind "monster-166" "tiger"
  :desc "One of the largest of its species, a sleek orange and black shape creeps  towards you, ready to pounce."
  :symbol #\f
  :colour #\o
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

(define-monster-kind "monster-167" "moaning spirit"
  :desc "A ghostly apparition that shrieks horribly."
  :symbol #\G
  :colour #\u
  :alignment '<evil>
  :type '(<undead>)
  :depth 12
  :rarity 2
  :hitpoints '(5 . 8)
  :armour 20
  :speed 120
  :xp 44
  :abilities '(<pass-wall> <cold-blood> <invisible> (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<sleep> <confuse> <cold>)
  :alertness 10
  :vision 14
  :attacks '((<touch> :type <lose-dex> :damage (1 . 8)) (<wail> :type <terrify> :damage nil))
  :treasures '((<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<spell> <scare>) (<spell> <teleport>) (<frequency> 1/15))) 

(define-monster-kind "monster-168" "swordsman"
  :desc "A warrior of considerable skill."
  :symbol #\p
  :colour #\U
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
  :sex '<male>) 

(define-monster-kind "monster-169" "stegocentipede"
  :desc "It is a vast armoured centipede with massive mandibles and a spiked tail."
  :symbol #\c
  :colour #\u
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


(define-monster-kind "monster-170" "spotted jelly"
  :desc "A jelly thing."
  :symbol #\j
  :colour #\o
  :depth 12
  :rarity 3
  :hitpoints '(13 . 8)
  :armour 18
  :speed 120
  :xp 33
  :abilities '(<cold-blood> <empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse> <poison> <acid>)
  :vulnerabilities '(<light>)
  :alertness 1
  :vision 12
  :attacks '((<touch> :type <acid> :damage (2 . 6)) (<touch> :type <acid> :damage (2 . 6))
             (<touch> :type <acid> :damage (1 . 10)))) 

(define-monster-kind "monster-171" "drider"
  :desc "A dark elven torso merged with the bloated form of a giant spider."
  :symbol #\S
  :colour #\u
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

(define-monster-kind "monster-172" "killer brown beetle"
  :desc "It is a vicious insect with a tough carapace."
  :symbol #\K
  :colour #\u
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
  :desc "A hideous, smallish giant that is often found near or with orcs."
  :symbol #\O
  :colour #\U
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

(define-monster-kind "mithril-coins" "creeping mithril coins"
  :desc "It is a pile of coins, shambling forward on thousands of tiny legs."
  :symbol #\$
  :colour #\B
  :type '(<animal>)
  :depth 13
  :rarity 4
  :hitpoints '(20 . 8)
  :armour 50
  :speed 110
  :xp 45
  :abilities '(<bash-door> <cold-blood>)
  :immunities '(<sleep> <confuse> <poison>)
  :alertness 10
  :vision 5
  :attacks '((<touch> :type <poison> :damage (3 . 5)) (<hit> :type <hurt> :damage (2 . 5)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) <only-drop-gold>)) 

(define-monster-kind "illusionist" "illusionist"
  :desc "A deceptive spell caster."
  :symbol #\p
  :colour #\r
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
  :sex '<male>
  :special-abilities '((<spell> <darkness>) (<spell> <confusion>) (<spell> <slow>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<spell> <teleport>) (<spell> <blink>) (<spell> <haste>)
                       (<frequency> 1/3))) 

(define-monster-kind "druid" "druid"
  :desc "A mystic at one with nature.  Om."
  :symbol #\p
  :colour #\r
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
  :sex '<male>
  :special-abilities '((<spell> (<bolt> <lightning>)) (<spell> (<bolt> <fire>)) (<spell> <slow>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<spell> <blink>) (<spell> <haste>) (<frequency> 1/3))) 

(define-monster-kind "black-orc" "black orc"
  :desc "He is a large orc with powerful arms and deep black skin."
  :symbol #\o
  :colour #\D
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
  :sex '<male>) 

(define-monster-kind "ochre-jelly" "ochre jelly"
  :desc "A fast moving highly acidic jelly thing, that is eating away the floor it  rests on."
  :symbol #\j
  :colour #\U
  :depth 13
  :rarity 3
  :hitpoints '(13 . 8)
  :armour 18
  :speed 120
  :xp 40
  :abilities '(<bash-door> <open-door> <pick-up-item> <cold-blood> <empty-mind> <stupid>)
  :immunities '(<fear> <sleep> <confuse> <poison> <acid>)
  :alertness 1
  :vision 12
  :attacks '((<touch> :type <acid> :damage (2 . 6)) (<touch> :type <acid> :damage (2 . 6))
             (<touch> :type <acid> :damage (1 . 10)))) 


(define-monster-kind "giant-flea" "giant flea"
  :desc "It makes you itch just to look at it."
  :symbol #\I
  :colour #\s
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


(define-monster-kind "monster-182" "giant white dragon fly"
  :desc "It is a large fly that drips frost."
  :symbol #\F
  :colour #\w
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

(define-monster-kind "blue-icky" "blue icky thing"
  :desc "It is a strange, slimy, icky creature, with rudimentary intelligence,  but evil cunning.  It hungers for food, and you look tasty."
  :symbol #\i
  :colour #\b
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

(define-monster-kind "hill-giant" "hill giant"
  :desc "A ten foot tall humanoid with powerful muscles."
  :symbol #\P
  :colour #\U
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

(define-monster-kind "flesh-golem" "flesh golem"
  :desc "A shambling humanoid monster with long scars."
  :symbol #\g
  :colour #\R
  :depth 14
  :rarity 1
  :hitpoints '(12 . 8)
  :armour 30
  :speed 110
  :xp 50
  :abilities '(<bash-door> <empty-mind>)
  :immunities '(<fear> <sleep> <confuse> <lightning>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 6)))) 

(define-monster-kind "warg" "warg"
  :desc "It is a large wolf with eyes full of cunning."
  :symbol #\C
  :colour #\s
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

(define-monster-kind "black-louse" "giant black louse"
  :desc "It makes you itch just to look at it."
  :symbol #\l
  :colour #\D
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
  :desc "A strange creature that merges with the dungeon floor, trapping its  victims by enveloping them within its perfectly disguised form."
  :symbol #\.
  :colour #\w
  :depth 14
  :rarity 3
  :hitpoints '(20 . 10)
  :armour 25
  :speed 110
  :xp 80
  :abilities '(<cold-blood> <invisible> <empty-mind> <max-hitpoints> <never-move> <see-through> <special-symbol>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 10
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (1 . 8)) (<hit> :type <hurt> :damage (1 . 8)))) 

(define-monster-kind "wererat" "wererat"
  :desc "A large rat with glowing red eyes.  The wererat is a disgusting creature,  relishing in filth and disease."
  :symbol #\r
  :colour #\D
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

(define-monster-kind "black-ogre" "black ogre"
  :desc "A massive orc-like figure with black skin and powerful arms."
  :symbol #\O
  :colour #\D
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

(define-monster-kind "magic-mushroom" "magic mushroom patch"
  :desc "Yum!  It looks quite tasty.  It seems to glow with an unusual light."
  :symbol #\,
  :colour #\B
  :depth 15
  :rarity 2
  :hitpoints '(1 . 1)
  :armour 10
  :speed 130
  :xp 10
  :abilities '(<stupid> <never-attack> <never-move> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 40
  :special-abilities '((<spell> <darkness>) (<spell> <scare>) (<spell> <slow>) (<spell> <blink>) (<frequency> 1))) 

(define-monster-kind "guardian-naga" "guardian naga"
  :desc "A giant snake-like figure with a woman's torso."
  :symbol #\n
  :colour #\B
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
  :sex '<female>) 

(define-monster-kind "light-hound" "light hound"
  :desc "A brilliant canine form whose light hurts your eyes, even at this distance."
  :symbol #\Z
  :colour #\o
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

(define-monster-kind "dark-hound" "dark hound"
  :desc "A hole in the air in the shape of a huge hound.  No light falls upon its  form."
  :symbol #\Z
  :colour #\D
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

(define-monster-kind "half-orc" "half-orc"
  :desc "He is a hideous deformed cross-breed with man and orc, combining man's  strength and cunning with orcish evil."
  :symbol #\o
  :colour #\s
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
  :sex '<male>) 

(define-monster-kind "tarantula" "giant tarantula"
  :desc "A giant spider with hairy black and red legs."
  :symbol #\S
  :colour #\o
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

(define-monster-kind "clear-centipede" "giant clear centipede"
  :desc "It is about four feet long and carnivorous."
  :symbol #\c
  :colour #\w
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

(define-monster-kind "mirkwood-spider" "mirkwood spider"
  :desc "A strong and powerful spider from Mirkwood forest.  Cunning and evil, it  seeks to taste your juicy insides."
  :symbol #\S
  :colour #\g
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

(define-monster-kind "frost-giant" "frost giant"
  :desc "A twelve foot tall giant covered in furs."
  :symbol #\P
  :colour #\w
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
  :desc "It is half lion, half eagle.  It flies menacingly towards you."
  :symbol #\H
  :colour #\u
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
  :desc "It is a small demonic spirit full of malevolence."
  :symbol #\u
  :colour #\y
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
  :attacks '((<hit> :type <hurt> :damage (1 . 10)) (<hit> :type <paralyze> :damage (1 . 2)))) 


(define-monster-kind "monster-203" "clear hound"
  :desc "A completely translucent hound."
  :symbol #\Z
  :colour #\w
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

(define-monster-kind "monster-204" "clay golem"
  :desc "It is a massive animated statue made out of hardened clay."
  :symbol #\g
  :colour #\U
  :depth 15
  :rarity 2
  :hitpoints '(14 . 8)
  :armour 30
  :speed 110
  :xp 50
  :abilities '(<bash-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (1 . 8)) (<hit> :type <hurt> :damage (1 . 8)))) 

(define-monster-kind "monster-205" "umber hulk"
  :desc "This bizarre creature has glaring eyes and large mandibles capable of  slicing through rock."
  :symbol #\X
  :colour #\U
  :alignment '<evil>
  :type '(<animal>)
  :depth 16
  :rarity 1
  :hitpoints '(20 . 10)
  :armour 50
  :speed 110
  :xp 75
  :abilities '(<destroy-wall> <bash-door> <cold-blood> <empty-mind>)
  :immunities '(<sleep> <confuse> <poison>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 10
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<hit> :type <hurt> :damage (1 . 6))
             (<hit> :type <hurt> :damage (1 . 6)) (<gaze> :type <confuse> :damage nil))) 

(define-monster-kind "monster-206" "orc captain"
  :desc "An armoured orc with an air of authority."
  :symbol #\o
  :colour #\o
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
  :sex '<male>) 

(define-monster-kind "monster-207" "gelatinous cube"
  :desc "It is a strange, vast gelatinous structure that assumes cubic proportions  as it lines all four walls of the corridors it patrols.  Through its  transparent jelly structure you can see treasures it has engulfed, and a  few corpses as well."
  :symbol #\j
  :colour #\G
  :depth 16
  :rarity 4
  :hitpoints '(36 . 10)
  :armour 18
  :speed 110
  :xp 80
  :abilities '(<bash-door> <open-door> <pick-up-item> <cold-blood> <empty-mind> <stupid> <max-hitpoints>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 1
  :vision 12
  :attacks '((<touch> :type <acid> :damage (1 . 10)) (<touch> :type <acid> :damage (1 . 10))
             (<touch> :type <acid> :damage (1 . 10)))
  :treasures '((<drop> "4d2") (<drop> "1d2"))) 

(define-monster-kind "monster-208" "giant green dragon fly"
  :desc "A vast, foul-smelling dragonfly."
  :symbol #\F
  :colour #\g
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

(define-monster-kind "monster-209" "fire giant"
  :desc "A glowing fourteen foot tall giant.  Flames drip from its red skin."
  :symbol #\P
  :colour #\r
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


(define-monster-kind "monster-210" "hummerhorn"
  :desc "A giant buzzing wasp, its stinger drips venom."
  :symbol #\I
  :colour #\y
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
  :attacks '((<bite> :type <confuse> :damage (2 . 2)))) 


(define-monster-kind "imp" "imp"
  :desc "The lawful evil master's favourite pet."
  :symbol #\u
  :colour #\g
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

(define-monster-kind "forest-troll" "forest troll"
  :desc "He is green skinned and ugly."
  :symbol #\T
  :colour #\g
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
  :sex '<male>) 


(define-monster-kind "monster-216" "2-headed hydra"
  :desc "A strange reptilian hybrid with two heads, guarding its hoard."
  :symbol #\M
  :colour #\u
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

(define-monster-kind "monster-217" "water spirit"
  :desc "A whirlpool of sentient liquid."
  :symbol #\E
  :colour #\s
  :alignment '<evil>
  :depth 17
  :rarity 1
  :hitpoints '(9 . 8)
  :armour 28
  :speed 120
  :xp 58
  :abilities '(<bash-door> <cold-blood> <empty-mind> (<random-mover> 1/4))
  :immunities '(<fear> <sleep> <confuse> <poison>)
  :alertness 40
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (2 . 4)) (<hit> :type <hurt> :damage (2 . 4)))) 

(define-monster-kind "pink-scorpion" "giant pink scorpion"
  :desc "It is fast and poisonous."
  :symbol #\S
  :colour #\R
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

(define-monster-kind "monster-219" "earth spirit"
  :desc "A whirling form of sentient rock."
  :symbol #\E
  :colour #\u
  :alignment '<evil>
  :depth 17
  :rarity 2
  :hitpoints '(13 . 8)
  :armour 40
  :speed 120
  :xp 64
  :abilities '(<pass-wall> <cold-blood> <empty-mind> (<random-mover> 1/4))
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 50
  :vision 10
  :attacks '((<hit> :type <hurt> :damage (1 . 8)) (<hit> :type <hurt> :damage (1 . 8)))) 


(define-monster-kind "fire-spirit" "fire spirit"
  :desc "A whirlwind of sentient flame."
  :symbol #\E
  :colour #\r
  :alignment '<evil>
  :depth 18
  :rarity 2
  :hitpoints '(10 . 9)
  :armour 30
  :speed 120
  :xp 75
  :abilities '(<bash-door> <empty-mind> (<random-mover> 1/4))
  :immunities '(<fear> <sleep> <confuse> <poison> <fire>)
  :alertness 20
  :vision 16
  :attacks '((<hit> :type <fire> :damage (2 . 6)) (<hit> :type <fire> :damage (2 . 6)))) 

(define-monster-kind "fire-hound" "fire hound"
  :desc "Flames lick at its feet and its tongue is a blade of fire.  You can feel a  furnace heat radiating from the creature."
  :symbol #\Z
  :colour #\r
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

(define-monster-kind "cold-hound" "cold hound"
  :desc "A hound as tall as a man, this creature appears to be composed of angular  planes of ice.  Cold radiates from it and freezes your breath in the air."
  :symbol #\Z
  :colour #\w
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

(define-monster-kind "energy-hound" "energy hound"
  :desc "Saint Elmo's Fire forms a ghostly halo around this hound, and sparks sting  your fingers as energy builds up in the air around you."
  :symbol #\Z
  :colour #\b
  :type '(<animal>)
  :depth 18
  :rarity 1
  :hitpoints '(10 . 6)
  :armour 30
  :speed 110
  :xp 70
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<lightning>)
  :alertness 0
  :vision 30
  :attacks '((<bite> :type <elec> :damage (1 . 3)) (<bite> :type <elec> :damage (1 . 3))
             (<bite> :type <elec> :damage (1 . 3)))
  :special-abilities '((<breath> <lightning>) (<frequency> 1/10))) 

(define-monster-kind "potion-mimic" "mimic (potion)"
  :desc "A strange creature that disguises itself as discarded objects to lure  unsuspecting adventurers within reach of its venomous claws."
  :symbol #\!
  :colour #\w
  :depth 18
  :rarity 3
  :hitpoints '(10 . 10)
  :armour 30
  :speed 110
  :xp 60
  :abilities '(<cold-blood> <empty-mind> <never-move> <initial-sleeper> <special-symbol-2>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 25
  :attacks '((<hit> :type <hurt> :damage (2 . 3)) (<hit> :type <hurt> :damage (2 . 3))
             (<hit> :type <poison> :damage (3 . 4)))
  :special-abilities '((<spell> (<bolt> <cold>)) (<spell> (<cause> 2)) (<spell> <scare>) (<spell> <confusion>)
                       (<spell> <blindness>) (<frequency> 1/6))) 

(define-monster-kind "blink-dog" "blink dog"
  :desc "A strange magical member of the canine race, its form seems to shimmer and  fade in front of your very eyes."
  :symbol #\C
  :colour #\B
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

(define-monster-kind "uruk" "uruk"
  :desc "He is a cunning orc of power, as tall as a man, and stronger.  It fears  little."
  :symbol #\o
  :colour #\B
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
  :sex '<male>) 

(define-monster-kind "monster-229" "shambling mound"
  :desc "A pile of rotting vegetation that slides towards you with a disgusting  stench, waking all it nears."
  :symbol #\,
  :colour #\W
  :alignment '<evil>
  :depth 18
  :rarity 2
  :hitpoints '(20 . 6)
  :armour 16
  :speed 110
  :xp 75
  :abilities '(<bash-door> <open-door> <empty-mind> <stupid>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 8)) (<hit> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop-chance> 9/10) <only-drop-gold>)
  :special-abilities '(<shriek> (<frequency> 1/4))) 


(define-monster-kind "monster-230" "stone giant"
  :desc "It is eighteen feet tall and looking at you."
  :symbol #\P
  :colour #\W
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

(define-monster-kind "monster-231" "giant black dragon fly"
  :desc "The size of a large bird, this fly drips caustic acid."
  :symbol #\F
  :colour #\s
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

(define-monster-kind "monster-232" "stone golem"
  :desc "It is a massive animated statue."
  :symbol #\g
  :colour #\W
  :depth 19
  :rarity 2
  :hitpoints '(28 . 8)
  :armour 75
  :speed 100
  :xp 100
  :abilities '(<bash-door> <empty-mind> <cold-blood>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (1 . 10)) (<hit> :type <hurt> :damage (1 . 10)))) 

(define-monster-kind "monster-233" "red mold"
  :desc "It is a strange red growth on the dungeon floor; it seems to burn with  flame."
  :symbol #\m
  :colour #\r
  :depth 19
  :rarity 1
  :hitpoints '(17 . 8)
  :armour 16
  :speed 110
  :xp 64
  :abilities '(<empty-mind> <stupid> <never-move>)
  :immunities '(<fear> <sleep> <confuse> <poison> <fire>)
  :alertness 70
  :vision 2
  :attacks '((<touch> :type <fire> :damage (4 . 4)))) 

(define-monster-kind "monster-234" "giant gold dragon fly"
  :desc "Large beating wings support this dazzling insect.  A loud buzzing noise  pervades the air."
  :symbol #\F
  :colour #\y
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


(define-monster-kind "phase-spiders" "phase spider"
  :desc "A spider that never seems quite there.  Everywhere you look it is just  half-seen in the corner of one eye."
  :symbol #\S
  :colour #\B
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

(define-monster-kind "monster-237" "3-headed hydra"
  :desc "A strange reptilian hybrid with three heads, guarding its hoard."
  :symbol #\M
  :colour #\o
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

(define-monster-kind "monster-238" "earth hound"
  :desc "A beautiful crystalline shape does not disguise the danger this hound  clearly presents.  Your flesh tingles as it approaches."
  :symbol #\Z
  :colour #\u
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

(define-monster-kind "monster-239" "air hound"
  :desc "Swirling vapours surround this beast as it floats towards you, seemingly  walking on air.  Noxious gases sting your throat."
  :symbol #\Z
  :colour #\g
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


(define-monster-kind "monster-240" "sabre-tooth tiger"
  :desc "A fierce and dangerous cat, its huge tusks and sharp claws would lacerate  even the strongest armour."
  :symbol #\f
  :colour #\y
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

(define-monster-kind "water-hound" "water hound"
  :desc "Liquid footprints follow this hound as it pads around the dungeon.  An  acrid smell of acid rises from the dog's pelt."
  :symbol #\Z
  :colour #\s
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
  :desc "It is a strange concoction of lion, dragon and goat.  It looks very odd  but very avoidable."
  :symbol #\H
  :colour #\r
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

(define-monster-kind "monster-243" "quylthulg"
  :desc "It is a strange pulsing mound of flesh."
  :symbol #\Q
  :colour #\y
  :depth 20
  :rarity 1
  :hitpoints '(6 . 8)
  :armour 1
  :speed 110
  :xp 250
  :abilities '(<invisible> <empty-mind> <never-attack> <never-move> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 10
  :special-abilities '((<summon> <monster>) (<spell> <blink>) (<frequency> 1/4))) 

(define-monster-kind "monster-244" "sasquatch"
  :desc "A tall shaggy, furry humanoid, it could call the yeti brother."
  :symbol #\Y
  :colour #\g
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

(define-monster-kind "werewolf" "werewolf"
  :desc "It is a huge wolf with eyes that glow with manly intelligence."
  :symbol #\C
  :colour #\D
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

(define-monster-kind "monster-246" "dark elven lord"
  :desc "A dark elven figure in armour and radiating evil power."
  :symbol #\h
  :colour #\D
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
  :sex '<male>
  :special-abilities '((<spell> (<bolt> <cold>)) (<spell> (<bolt> <fire>)) (<spell> <darkness>) (<spell> <confusion>)
                       (<spell> <blindness>) (<spell> <haste>) (<frequency> 1/5))) 

(define-monster-kind "monster-247" "cloud giant"
  :desc "It is a twenty foot tall giant wreathed in clouds."
  :symbol #\P
  :colour #\b
  :alignment '<evil>
  :type '(<giant>)
  :depth 20
  :rarity 1
  :hitpoints '(24 . 10)
  :armour 60
  :speed 110
  :xp 125
  :abilities '(<bash-door> <open-door> <pick-up-item>)
  :immunities '(<lightning>)
  :alertness 50
  :vision 20
  :attacks '((<hit> :type <elec> :damage (3 . 8)) (<hit> :type <elec> :damage (3 . 8)))
  :treasures '((<drop-chance> 9/10))) 


(define-monster-kind "monster-250" "blue dragon bat"
  :desc "It is a glowing blue bat with a sharp tail."
  :symbol #\b
  :colour #\b
  :type '(<animal>)
  :depth 21
  :rarity 1
  :hitpoints '(4 . 4)
  :armour 26
  :speed 130
  :xp 54
  :abilities '(<bash-door> (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<lightning>)
  :alertness 50
  :vision 12
  :attacks '((<bite> :type <elec> :damage (1 . 3)))
  :special-abilities '((<breath> <lightning>) (<frequency> 1/4))) 

(define-monster-kind "scroll-mimic" "mimic (scroll)"
  :desc "A strange creature that disguises itself as discarded objects to lure  unsuspecting adventurers within reach of its venomous claws."
  :symbol #\?
  :colour #\w
  :depth 21
  :rarity 3
  :hitpoints '(10 . 14)
  :armour 40
  :speed 110
  :xp 60
  :abilities '(<cold-blood> <empty-mind> <never-move> <initial-sleeper> <special-symbol-2>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (2 . 3)) (<hit> :type <hurt> :damage (2 . 3))
             (<hit> :type <poison> :damage (3 . 4)) (<hit> :type <poison> :damage (3 . 4)))
  :special-abilities '((<summon> <monster>) (<spell> (<bolt> <fire>)) (<spell> (<cause> 2)) (<spell> <scare>)
                       (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/5))) 

(define-monster-kind "monster-252" "fire vortex"
  :desc "A whirling maelstrom of fire."
  :symbol #\v
  :colour #\r
  :depth 21
  :rarity 1
  :hitpoints '(9 . 9)
  :armour 30
  :speed 110
  :xp 100
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fear> <fire>)
  :alertness 0
  :vision 100
  :attacks '((<engulf> :type <fire> :damage (3 . 3)))
  :special-abilities '((<breath> <fire>) (<frequency> 1/6))) 

(define-monster-kind "monster-253" "water vortex"
  :desc "A caustic spinning whirlpool of water."
  :symbol #\v
  :colour #\s
  :depth 21
  :rarity 1
  :hitpoints '(9 . 9)
  :armour 30
  :speed 110
  :xp 100
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fear> <acid>)
  :alertness 0
  :vision 100
  :attacks '((<engulf> :type <acid> :damage (3 . 3)))
  :special-abilities '((<breath> <acid>) (<frequency> 1/6))) 

(define-monster-kind "monster-254" "cold vortex"
  :desc "A twisting whirlpool of frost."
  :symbol #\v
  :colour #\w
  :depth 21
  :rarity 1
  :hitpoints '(9 . 9)
  :armour 30
  :speed 110
  :xp 100
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fear> <cold>)
  :alertness 0
  :vision 100
  :attacks '((<engulf> :type <cold> :damage (3 . 3)))
  :special-abilities '((<breath> <cold>) (<frequency> 1/6))) 

(define-monster-kind "monster-255" "energy vortex"
  :desc "A shimmering tornado of air, sparks crackle along its length."
  :symbol #\v
  :colour #\b
  :depth 21
  :rarity 1
  :hitpoints '(12 . 12)
  :armour 30
  :speed 110
  :xp 130
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fear> <lightning>)
  :alertness 0
  :vision 100
  :attacks '((<engulf> :type <elec> :damage (5 . 5)))
  :special-abilities '((<breath> <lightning>) (<frequency> 1/6))) 

(define-monster-kind "monster-256" "mummified orc"
  :desc "It is an orcish figure covered in wrappings."
  :symbol #\z
  :colour #\w
  :alignment '<evil>
  :type '(<undead> <orc>)
  :depth 21
  :rarity 1
  :hitpoints '(15 . 8)
  :armour 28
  :speed 110
  :xp 56
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confuse> <poison> <cold>)
  :alertness 75
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 4)) (<hit> :type <hurt> :damage (2 . 4)))
  :treasures '((<drop-chance> 9/10))) 

(define-monster-kind "monster-257" "killer stag beetle"
  :desc "It is a giant beetle with vicious claws."
  :symbol #\K
  :colour #\g
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

(define-monster-kind "iron-golem" "iron golem"
  :desc "It is a massive metal statue that moves steadily towards you."
  :symbol #\g
  :colour #\s
  :depth 22
  :rarity 2
  :hitpoints '(80 . 12)
  :armour 80
  :speed 110
  :xp 160
  :abilities '(<bash-door> <cold-blood> <empty-mind> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (1 . 12)))
  :special-abilities '((<spell> <slow>) (<frequency> 1/7))) 

(define-monster-kind "monster-259" "giant yellow scorpion"
  :desc "It is a giant scorpion with a sharp stinger."
  :symbol #\S
  :colour #\y
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


(define-monster-kind "monster-261" "hardened warrior"
  :desc "A scarred warrior who moves with confidence."
  :symbol #\p
  :colour #\u
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
  :sex '<male>) 


(define-monster-kind "monster-263" "master rogue"
  :desc "A thief of great power and shifty speed."
  :symbol #\p
  :colour #\B
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
  :sex '<male>) 

(define-monster-kind "monster-264" "red dragon bat"
  :desc "It is a sharp-tailed bat, wreathed in fire."
  :symbol #\b
  :colour #\r
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

(define-monster-kind "monster-265" "killer white beetle"
  :desc "It is looking for prey."
  :symbol #\K
  :colour #\w
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

(define-monster-kind "monster-266" "giant bronze dragon fly"
  :desc "This vast gleaming bronze fly has wings which beat mesmerically fast."
  :symbol #\F
  :colour #\U
  :type '(<animal>)
  :depth 18
  :rarity 1
  :hitpoints '(3 . 8)
  :armour 20
  :speed 120
  :xp 70
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/4) (<random-mover> 1/2) <never-attack> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 50
  :vision 12
  :special-abilities '((<breath> <confusion>) (<frequency> 1/9))) 

(define-monster-kind "monster-267" "forest wight"
  :desc "It is a ghostly apparition with a humanoid form."
  :symbol #\W
  :colour #\g
  :alignment '<evil>
  :type '(<undead>)
  :depth 24
  :rarity 1
  :hitpoints '(12 . 8)
  :armour 30
  :speed 110
  :xp 140
  :abilities '(<bash-door> <open-door> <cold-blood> (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 30
  :vision 20
  :attacks '((<touch> :type <exp_20> :damage nil) (<hit> :type <hurt> :damage (1 . 6))
             (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<spell> <drain-mana>) (<spell> <scare>) (<frequency> 1/10))) 


(define-monster-kind "monster-270" "4-headed hydra"
  :desc "A strange reptilian hybrid with four heads, guarding its hoard."
  :symbol #\M
  :colour #\y
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

(define-monster-kind "monster-271" "mummified human"
  :desc "It is a human form encased in mouldy wrappings."
  :symbol #\z
  :colour #\w
  :alignment '<evil>
  :type '(<undead>)
  :depth 24
  :rarity 1
  :hitpoints '(17 . 9)
  :armour 34
  :speed 110
  :xp 70
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confuse> <poison> <cold>)
  :alertness 60
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 4)) (<hit> :type <hurt> :damage (2 . 4)))
  :treasures '((<drop-chance> 9/10) <only-drop-items>)) 

(define-monster-kind "monster-272" "vampire bat"
  :desc "An undead bat that flies at your neck hungrily."
  :symbol #\b
  :colour #\v
  :alignment '<evil>
  :type '(<undead> <animal>)
  :depth 24
  :rarity 2
  :hitpoints '(9 . 10)
  :armour 40
  :speed 120
  :xp 150
  :abilities '(<regenerate> <cold-blood> (<random-mover> 1/2))
  :immunities '(<fear> <sleep> <confuse> <poison> <cold>)
  :alertness 50
  :vision 12
  :attacks '((<bite> :type <exp_40> :damage (1 . 4)) (<bite> :type <exp_40> :damage (1 . 4)))) 


(define-monster-kind "monster-275" "banshee"
  :desc "It is a ghostly woman's form that wails mournfully."
  :symbol #\G
  :colour #\b
  :alignment '<evil>
  :type '(<undead>)
  :depth 24
  :rarity 2
  :hitpoints '(6 . 8)
  :armour 24
  :speed 120
  :xp 60
  :abilities '(<pass-wall> <pick-up-item> <cold-blood> <invisible> (<random-mover> 1/2))
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :alertness 10
  :vision 20
  :attacks '((<touch> :type <exp_20> :damage nil) (<wail> :type <terrify> :damage nil))
  :treasures '((<drop> "1d2"))
  :sex '<female>
  :special-abilities '((<spell> <drain-mana>) (<spell> <teleport>) (<frequency> 1/15))) 

(define-monster-kind "pukelman" "pukelman"
  :desc "A stumpy figure carved from stone, with glittering eyes, this sentinel  strides towards you with deadly intent."
  :symbol #\g
  :colour #\D
  :depth 25
  :rarity 3
  :hitpoints '(80 . 12)
  :armour 80
  :speed 110
  :xp 600
  :abilities '(<bash-door> <cold-blood> <empty-mind> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (3 . 6)) (<hit> :type <hurt> :damage (1 . 12)))
  :special-abilities '((<spell> (<bolt> <acid>)) (<spell> <confusion>) (<spell> <slow>) (<frequency> 1/4))) 

(define-monster-kind "monster-277" "dark elven druid"
  :desc "A powerful dark elf, with mighty nature-controlling enchantments."
  :symbol #\h
  :colour #\g
  :alignment '<evil>
  :depth 25
  :rarity 3
  :hitpoints '(20 . 20)
  :armour 75
  :speed 120
  :xp 500
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 15
  :attacks '((<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (1 . 7))
             (<hit> :type <hurt> :damage (1 . 7)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :sex '<male>
  :special-abilities '((<summon> <spider>) (<summon> <monster>) (<spell> <darkness>) (<spell> <confusion>)
                       (<spell> <heal>) (<frequency> 1/6))) 

(define-monster-kind "stone-troll" "stone troll"
  :desc "He is a giant troll with scabrous black skin."
  :symbol #\T
  :colour #\W
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
  :sex '<male>) 

(define-monster-kind "monster-279" "troll priest"
  :desc "A troll who is so bright he knows how to read."
  :symbol #\T
  :colour #\G
  :alignment '<evil>
  :type '(<troll>)
  :depth 25
  :rarity 1
  :hitpoints '(30 . 10)
  :armour 50
  :speed 110
  :xp 100
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :vulnerabilities '(<light>)
  :alertness 30
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (1 . 8))
             (<hit> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop-chance> 9/10))
  :sex '<male>
  :special-abilities '((<spell> <darkness>) (<spell> <missile>) (<spell> (<cause> 1)) (<spell> <scare>)
                       (<spell> <blink>) (<frequency> 1/5))) 


(define-monster-kind "wereworm" "wereworm"
  :desc "A huge wormlike shape dripping acid, twisted by evil sorcery into a foul  monster that breeds on death."
  :symbol #\w
  :colour #\D
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
             (<crawl> :type <acid> :damage (2 . 4)) (<gaze> :type <exp_20> :damage nil))) 

(define-monster-kind "monster-281" "carrion crawler"
  :desc "A hideous centipede covered in slime and with glowing tentacles around its  head."
  :symbol #\c
  :colour #\o
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
  :attacks '((<sting> :type <paralyze> :damage (2 . 6)) (<sting> :type <paralyze> :damage (2 . 6)))) 

(define-monster-kind "monster-282" "killer pink beetle"
  :desc "It is a giant beetle with poisonous mandibles."
  :symbol #\K
  :colour #\R
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

(define-monster-kind "monster-283" "giant grey ant"
  :desc "It is an ant encased in shaggy grey fur."
  :symbol #\a
  :colour #\s
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


(define-monster-kind "monster-285" "displacer beast"
  :desc "It is a huge black panther, clubbed tentacles sprouting from its shoulders."
  :symbol #\f
  :colour #\D
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

(define-monster-kind "monster-286" "giant red tick"
  :desc "It is smoking and burning with great heat."
  :symbol #\S
  :colour #\r
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

(define-monster-kind "monster-287" "cave ogre"
  :desc "A giant orc-like figure with an awesomely muscled frame."
  :symbol #\O
  :colour #\u
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

(define-monster-kind "monster-288" "white wraith"
  :desc "It is a tangible but ghostly form made of white fog."
  :symbol #\W
  :colour #\w
  :alignment '<evil>
  :type '(<undead>)
  :depth 26
  :rarity 1
  :hitpoints '(15 . 8)
  :armour 40
  :speed 110
  :xp 175
  :abilities '(<bash-door> <open-door> <cold-blood> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<touch> :type <exp_20> :damage nil) (<hit> :type <hurt> :damage (1 . 6))
             (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop> "1d2"))
  :special-abilities '((<spell> <darkness>) (<spell> (<cause> 2)) (<spell> <scare>) (<frequency> 1/8))) 

(define-monster-kind "monster-289" "monadic deva"
  :desc "A lesser angel wearing little more than a loincloth - its steely skin  provides all the protection it needs."
  :symbol #\A
  :colour #\o
  :depth 26
  :rarity 6
  :hitpoints '(30 . 10)
  :armour 60
  :speed 110
  :xp 220
  :abilities '(<powerful-breath> <bash-door> <open-door> <pick-up-item> <initial-sleeper> <max-hitpoints>)
  :immunities '(<sleep> <confuse> <poison> <acid>)
  :alertness 255
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4))
             (<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop> "2d2") <only-drop-items>)
  :special-abilities '((<spell> <forget>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<frequency> 1/3))) 


(define-monster-kind "monster-291" "killer red beetle"
  :desc "It is a giant beetle wreathed in flames."
  :symbol #\K
  :colour #\r
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

(define-monster-kind "monster-292" "creeping adamantite coins"
  :desc "It is a pile of coins, slithering forward on thousands of tiny legs."
  :symbol #\$
  :colour #\G
  :type '(<animal>)
  :depth 27
  :rarity 4
  :hitpoints '(20 . 25)
  :armour 50
  :speed 120
  :xp 45
  :abilities '(<bash-door> <cold-blood>)
  :immunities '(<sleep> <confuse> <poison>)
  :alertness 10
  :vision 5
  :attacks '((<hit> :type <hurt> :damage (1 . 12)) (<hit> :type <hurt> :damage (1 . 12))
             (<touch> :type <poison> :damage (3 . 5)) (<bite> :type <poison> :damage (3 . 4)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) <only-drop-gold>)) 

(define-monster-kind "algroth" "algroth"
  :desc "A powerful troll form.  Venom drips from its needlelike claws."
  :symbol #\T
  :colour #\o
  :alignment '<evil>
  :type '(<troll>)
  :depth 27
  :rarity 1
  :hitpoints '(21 . 12)
  :armour 60
  :speed 110
  :xp 150
  :abilities '(<bash-door> <open-door>)
  :alertness 40
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 6)) (<claw> :type <poison> :damage (3 . 3))
             (<claw> :type <poison> :damage (3 . 3)))
  :treasures '((<drop-chance> 3/5))) 

(define-monster-kind "monster-294" "vibration hound"
  :desc "A blurry canine form which seems to be moving as fast as the eye can  follow.  You can feel the earth resonating beneath your feet."
  :symbol #\Z
  :colour #\y
  :type '(<animal>)
  :depth 27
  :rarity 3
  :hitpoints '(25 . 10)
  :armour 30
  :speed 110
  :xp 250
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<claw> :type <hurt> :damage (3 . 3))
             (<bite> :type <hurt> :damage (2 . 6)) (<bite> :type <hurt> :damage (2 . 6)))
  :special-abilities '((<breath> <sound>) (<frequency> 1/5))) 

(define-monster-kind "monster-295" "nexus hound"
  :desc "A locus of conflicting points coalesce to form the vague shape of a huge  hound.  Or is it just your imagination?"
  :symbol #\Z
  :colour #\R
  :type '(<animal>)
  :depth 27
  :rarity 3
  :hitpoints '(25 . 10)
  :armour 30
  :speed 110
  :xp 250
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<claw> :type <hurt> :damage (3 . 3))
             (<bite> :type <hurt> :damage (2 . 8)) (<bite> :type <hurt> :damage (2 . 8)))
  :special-abilities '((<breath> <nexus>) (<frequency> 1/5))) 

(define-monster-kind "monster-296" "ogre mage"
  :desc "A hideous ogre wrapped in black sorcerous robes."
  :symbol #\O
  :colour #\r
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


(define-monster-kind "monster-298" "vampire"
  :desc "It is a humanoid with an aura of power.  You notice a sharp set of front  teeth."
  :symbol #\V
  :colour #\v
  :alignment '<evil>
  :type '(<undead>)
  :depth 27
  :rarity 1
  :hitpoints '(25 . 12)
  :armour 45
  :speed 110
  :xp 175
  :abilities '(<regenerate> <bash-door> <open-door> <cold-blood> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<bite> :type <exp_20> :damage (1 . 4)) (<bite> :type <exp_20> :damage (1 . 4))
             (<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5))
  :special-abilities '((<spell> <darkness>) (<spell> <forget>) (<spell> <mind-blast>) (<spell> (<cause> 2))
                       (<spell> <scare>) (<spell> <paralysis>) (<spell> <teleport-player>) (<frequency> 1/9))) 

(define-monster-kind "gorgimera" "gorgimera"
  :desc "The result of evil experiments, this travesty of nature should never be  alive.  It has 3 heads - gorgon, goat and dragon - all attached to a  lion's body."
  :symbol #\H
  :colour #\o
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
  :attacks '((<gaze> :type <paralyze> :damage (2 . 4)) (<bite> :type <hurt> :damage (1 . 10))
             (<bite> :type <fire> :damage (1 . 3)) (<bite> :type <fire> :damage (1 . 3)))
  :special-abilities '((<breath> <fire>) (<frequency> 1/8))) 


(define-monster-kind "colbran" "colbran"
  :desc "A man-shaped form of living lightning, sparks and shocks crackle all over  this madly capering figure, as it leaps and whirls around and about you."
  :symbol #\g
  :colour #\y
  :depth 27
  :rarity 2
  :hitpoints '(80 . 12)
  :armour 80
  :speed 120
  :xp 900
  :abilities '(<bash-door> <cold-blood> <empty-mind> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <elec> :damage (3 . 8)) (<hit> :type <elec> :damage (3 . 8)))
  :special-abilities '((<spell> (<bolt> <lightning>)) (<frequency> 1/3))) 

(define-monster-kind "monster-301" "spirit naga"
  :desc "A wraithly snake-like form with the torso of a beautiful woman, it is the  most powerful of its kind."
  :symbol #\n
  :colour #\w
  :alignment '<evil>
  :depth 28
  :rarity 2
  :hitpoints '(30 . 15)
  :armour 75
  :speed 110
  :xp 60
  :abilities '(<bash-door> <open-door> <invisible> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 120
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 8)) (<bite> :type <hurt> :damage (1 . 8))
             (<crush> :type <hurt> :damage (2 . 8)) (<crush> :type <hurt> :damage (2 . 8)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) <only-drop-items>)
  :sex '<female>
  :special-abilities '((<spell> <darkness>) (<spell> <mind-blast>) (<spell> <blindness>) (<spell> <heal>)
                       (<frequency> 1/4))) 

(define-monster-kind "monster-302" "5-headed hydra"
  :desc "A strange reptilian hybrid with five heads dripping venom."
  :symbol #\M
  :colour #\g
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

(define-monster-kind "monster-303" "black knight"
  :desc "He is a figure encased in deep black plate armour; he looks at you  menacingly."
  :symbol #\p
  :colour #\s
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
  :sex '<male>
  :special-abilities '((<spell> <darkness>) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <blindness>)
                       (<frequency> 1/8))) 



(define-monster-kind "mage" "mage"
  :desc "A mage of some power - you can tell by the size of his hat."
  :symbol #\p
  :colour #\r
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
  :sex '<male>
  :special-abilities '((<summon> <monster>) (<spell> (<bolt> <lightning>)) (<spell> (<bolt> <cold>))
                       (<spell> (<bolt> <fire>)) (<spell> <confusion>) (<spell> <blindness>)
                       (<spell> <teleport-player>) (<spell> <teleport>) (<spell> <haste>) (<frequency> 1/3))) 

(define-monster-kind "monster-306" "mind flayer"
  :desc "A humanoid form with a gruesome head, tentacular mouth, and piercing  eyes.  Claws reach out for you and you feel a presence invade your mind."
  :symbol #\h
  :colour #\R
  :alignment '<evil>
  :depth 28
  :rarity 1
  :hitpoints '(15 . 10)
  :armour 60
  :speed 110
  :xp 200
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 10
  :vision 20
  :attacks '((<gaze> :type <lose-int> :damage (2 . 6)) (<gaze> :type <lose-int> :damage (2 . 6)))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5) <only-drop-items>)
  :special-abilities '((<spell> <forget>) (<spell> <brain-smash>) (<spell> <mind-blast>) (<spell> <scare>)
                       (<spell> <paralysis>) (<spell> <blindness>) (<frequency> 1/8))) 


(define-monster-kind "basilisk" "basilisk"
  :desc "An evil reptile that preys on unsuspecting travellers.  Its eyes stare  deeply at you and your soul starts to wilt!"
  :symbol #\R
  :colour #\b
  :type '(<animal>)
  :depth 28
  :rarity 3
  :hitpoints '(20 . 30)
  :armour 90
  :speed 120
  :xp 300
  :abilities '(<bash-door> <open-door>)
  :immunities '(<sleep> <confuse>)
  :alertness 30
  :vision 15
  :attacks '((<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12))
             (<bite> :type <hurt> :damage (2 . 12)) (<gaze> :type <paralyze> :damage nil))
  :treasures '((<drop> "1d2") <only-drop-items>)) 

(define-monster-kind "ice-troll" "ice troll"
  :desc "He is a white troll with powerfully clawed hands."
  :symbol #\T
  :colour #\w
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
  :sex '<male>) 


(define-monster-kind "monster-310" "giant purple worm"
  :desc "It is a massive worm form, many feet in length.  Its vast maw drips acid  and poison."
  :symbol #\w
  :colour #\v
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

(define-monster-kind "monster-311" "movanic deva"
  :desc "A lesser angel protected by an aura of holiness.  Its muscular form looks  extremely powerful next to your own frail body."
  :symbol #\A
  :colour #\B
  :depth 29
  :rarity 6
  :hitpoints '(40 . 10)
  :armour 68
  :speed 110
  :xp 400
  :abilities '(<powerful-breath> <bash-door> <open-door> <pick-up-item> <smart> <initial-sleeper> <max-hitpoints>)
  :immunities '(<sleep> <confuse> <poison> <cold> <fire>)
  :alertness 255
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5))
             (<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5)))
  :treasures '((<drop> "2d2") <only-drop-items>)
  :special-abilities '((<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>) (<spell> <haste>) (<spell> <heal>)
                       (<frequency> 1/3))) 

(define-monster-kind "monster-312" "catoblepas"
  :desc "A strange ox-like form with a huge head but a thin, weak neck, it looks  likes the creation of some deranged alchemist."
  :symbol #\q
  :colour #\g
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

(define-monster-kind "ring-mimic" "mimic (ring)"
  :desc "A strange creature that disguises itself as discarded objects to lure  unsuspecting adventurers within reach of its venomous claws."
  :symbol #\=
  :colour #\w
  :depth 29
  :rarity 3
  :hitpoints '(10 . 35)
  :armour 60
  :speed 120
  :xp 200
  :abilities '(<cold-blood> <empty-mind> <never-move> <initial-sleeper> <special-symbol-2>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 100
  :vision 30
  :attacks '((<hit> :type <poison> :damage (3 . 4)) (<hit> :type <poison> :damage (3 . 4))
             (<hit> :type <poison> :damage (3 . 4)) (<hit> :type <poison> :damage (3 . 4)))
  :special-abilities '((<summon> <monster>) (<spell> (<bolt> <lightning>)) (<spell> (<bolt> <cold>))
                       (<spell> (<bolt> <fire>)) (<spell> (<bolt> <acid>)) (<spell> <forget>) (<spell> (<cause> 2))
                       (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/4))) 

(define-monster-kind "monster-314" "young blue dragon"
  :desc "It has a form that legends are made of.  Its still-tender scales are a  deep blue in hue.  Sparks crackle along its length."
  :symbol #\d
  :colour #\b
  :alignment '<evil>
  :type '(<dragon>)
  :depth 29
  :rarity 1
  :hitpoints '(27 . 10)
  :armour 50
  :speed 110
  :xp 300
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<lightning>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 6)) (<claw> :type <hurt> :damage (1 . 4))
             (<claw> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "1d2") (<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<breath> <lightning>) (<spell> <scare>) (<frequency> 1/11))) 

(define-monster-kind "monster-315" "young white dragon"
  :desc "It has a form that legends are made of.  Its still-tender scales are a  frosty white in hue.  Icy blasts of cold air come from it as it breathes."
  :symbol #\d
  :colour #\w
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

(define-monster-kind "monster-316" "young green dragon"
  :desc "It has a form that legends are made of.  Its still-tender scales are a  deep green in hue.  Foul gas seeps through its scales."
  :symbol #\d
  :colour #\g
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

(define-monster-kind "monster-317" "young bronze dragon"
  :desc "It has a form that legends are made of.  Its still-tender scales are a  rich bronze hue, and its shape masks its true form."
  :symbol #\d
  :colour #\U
  :type '(<dragon>)
  :depth 29
  :rarity 3
  :hitpoints '(27 . 10)
  :armour 63
  :speed 110
  :xp 310
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 150
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 6)) (<claw> :type <hurt> :damage (1 . 4))
             (<claw> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<breath> <confusion>) (<spell> <scare>) (<frequency> 1/11))) 

(define-monster-kind "monster-318" "mithril golem"
  :desc "It is a massive statue of purest mithril.  It looks expensive!"
  :symbol #\g
  :colour #\B
  :depth 30
  :rarity 4
  :hitpoints '(80 . 15)
  :armour 100
  :speed 110
  :xp 500
  :abilities '(<bash-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5))
             (<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (3 . 8)))
  :treasures '((<drop> "2d2") <only-drop-gold>)) 

(define-monster-kind "monster-319" "shadow drake"
  :desc "It is a dragon-like form wrapped in shadow.  Glowing red eyes shine out in  the dark."
  :symbol #\d
  :colour #\D
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


(define-monster-kind "monster-320" "skeleton troll"
  :desc "It is a troll skeleton animated by dark dweomers."
  :symbol #\s
  :colour #\w
  :alignment '<evil>
  :type '(<undead> <troll>)
  :depth 30
  :rarity 1
  :hitpoints '(20 . 10)
  :armour 55
  :speed 110
  :xp 225
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confuse> <poison> <cold>)
  :alertness 20
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (1 . 6))
             (<hit> :type <hurt> :damage (1 . 6)))) 

(define-monster-kind "monster-321" "manticore"
  :desc "It is a winged lion's body with a human torso and a tail covered in  vicious spikes."
  :symbol #\H
  :colour #\y
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

(define-monster-kind "monster-322" "giant blue ant"
  :desc "It is a giant ant that crackles with energy."
  :symbol #\a
  :colour #\b
  :type '(<animal>)
  :depth 30
  :rarity 2
  :hitpoints '(8 . 8)
  :armour 50
  :speed 110
  :xp 80
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/4))
  :immunities '(<lightning>)
  :alertness 60
  :vision 10
  :attacks '((<bite> :type <elec> :damage (5 . 5)))) 

(define-monster-kind "monster-323" "giant army ant"
  :desc "An armoured form moving with purpose.  Powerful on its own, flee when  hordes of them march."
  :symbol #\a
  :colour #\o
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

(define-monster-kind "monster-324" "grave wight"
  :desc "It is a ghostly form with eyes that haunt you."
  :symbol #\W
  :colour #\b
  :alignment '<evil>
  :type '(<undead>)
  :depth 30
  :rarity 1
  :hitpoints '(12 . 10)
  :armour 50
  :speed 110
  :xp 325
  :abilities '(<bash-door> <open-door> <cold-blood> (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 30
  :vision 20
  :attacks '((<touch> :type <exp_20> :damage nil) (<hit> :type <hurt> :damage (1 . 7))
             (<hit> :type <hurt> :damage (1 . 7)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :special-abilities '((<spell> <darkness>) (<spell> (<cause> 3)) (<spell> <scare>) (<frequency> 1/8))) 

(define-monster-kind "monster-325" "killer slicer beetle"
  :desc "It is a beetle with deadly sharp cutting mandibles and a rock-hard  carapace."
  :symbol #\K
  :colour #\y
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

(define-monster-kind "monster-326" "ghost"
  :desc "You don't believe in them."
  :symbol #\G
  :colour #\w
  :alignment '<evil>
  :type '(<undead>)
  :depth 31
  :rarity 1
  :hitpoints '(13 . 8)
  :armour 30
  :speed 120
  :xp 350
  :abilities '(<pass-wall> <pick-up-item> <cold-blood> <invisible> (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :alertness 10
  :vision 20
  :attacks '((<claw> :type <lose-wis> :damage (1 . 6)) (<claw> :type <lose-int> :damage (1 . 6))
             (<touch> :type <exp_20> :damage nil) (<wail> :type <terrify> :damage nil))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5))
  :special-abilities '((<spell> <drain-mana>) (<spell> <paralysis>) (<spell> <blindness>) (<frequency> 1/15))) 

(define-monster-kind "monster-327" "death watch beetle"
  :desc "It is a giant beetle that produces a chilling sound."
  :symbol #\K
  :colour #\D
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

(define-monster-kind "monster-328" "ogre shaman"
  :desc "It is an ogre wrapped in furs and covered in grotesque body paints."
  :symbol #\O
  :colour #\o
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

(define-monster-kind "monster-329" "nexus quylthulg"
  :desc "It is a very unstable, strange pulsing mound of flesh."
  :symbol #\Q
  :colour #\R
  :depth 32
  :rarity 1
  :hitpoints '(10 . 12)
  :armour 1
  :speed 110
  :xp 300
  :abilities '(<empty-mind> <invisible> <never-attack> <never-move> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 10
  :special-abilities '((<spell> <teleport-away>) (<spell> <blink>) (<frequency> 1))) 


(define-monster-kind "monster-331" "ninja"
  :desc "A humanoid clothed in black who moves with blinding speed."
  :symbol #\p
  :colour #\y
  :alignment '<evil>
  :depth 32
  :rarity 2
  :hitpoints '(13 . 12)
  :armour 60
  :speed 120
  :xp 300
  :abilities '(<bash-door> <open-door>)
  :immunities '(<sleep> <confuse>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <lose-str> :damage (3 . 4)) (<hit> :type <lose-str> :damage (3 . 4))
             (<hit> :type <poison> :damage (3 . 4)))
  :treasures '((<drop> "1d2"))
  :sex '<male>) 

(define-monster-kind "monster-332" "memory moss"
  :desc "A mass of green vegetation.  You don't remember seeing anything like it  before."
  :symbol #\,
  :colour #\r
  :depth 32
  :rarity 3
  :hitpoints '(1 . 2)
  :armour 1
  :speed 110
  :xp 150
  :abilities '(<empty-mind> <stupid> <never-move> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 5
  :vision 30
  :attacks '((<hit> :type <confuse> :damage (1 . 4)) (<hit> :type <confuse> :damage (1 . 4)))
  :special-abilities '((<spell> <forget>) (<frequency> 1/6))) 

(define-monster-kind "monster-333" "storm giant"
  :desc "It is a twenty-five foot tall giant wreathed in lighting."
  :symbol #\P
  :colour #\B
  :alignment '<evil>
  :type '(<giant>)
  :depth 32
  :rarity 1
  :hitpoints '(38 . 10)
  :armour 60
  :speed 110
  :xp 1500
  :abilities '(<bash-door> <open-door> <pick-up-item> <max-hitpoints> <initial-sleeper>)
  :immunities '(<lightning> <cold>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <elec> :damage (3 . 8)) (<hit> :type <elec> :damage (3 . 8))
             (<hit> :type <elec> :damage (3 . 8)))
  :treasures '((<drop> "1d2"))
  :special-abilities '((<spell> (<ball> <lightning>)) (<spell> (<bolt> <lightning>)) (<spell> <scare>)
                       (<spell> <confusion>) (<spell> <teleport-player>) (<spell> <blink>) (<frequency> 1/8))) 

(define-monster-kind "monster-334" "cave troll"
  :desc "He is a vicious monster, feared for his ferocity."
  :symbol #\T
  :colour #\u
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
  :sex '<male>) 

(define-monster-kind "half-troll" "half-troll"
  :desc "A huge, ugly, half-human in search of plunder."
  :symbol #\T
  :colour #\U
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
  :sex '<male>) 

(define-monster-kind "mystic" "mystic"
  :desc "An adept at unarmed combat, the mystic strikes with stunning power.  He  can summon help from nature and is able to focus his power to ease any  pain."
  :symbol #\p
  :colour #\o
  :depth 33
  :rarity 3
  :hitpoints '(35 . 10)
  :armour 50
  :speed 120
  :xp 500
  :abilities '(<bash-door> <open-door> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <acid>)
  :alertness 5
  :vision 30
  :attacks '((<kick> :type <hurt> :damage (10 . 2)) (<kick> :type <hurt> :damage (10 . 2))
             (<kick> :type <hurt> :damage (10 . 2)) (<kick> :type <hurt> :damage (10 . 2)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :sex '<male>
  :special-abilities '((<summon> <spider>) (<spell> <heal>) (<frequency> 1/6))) 

(define-monster-kind "monster-337" "barrow wight"
  :desc "It is a ghostly nightmare of a entity."
  :symbol #\W
  :colour #\v
  :alignment '<evil>
  :type '(<undead>)
  :depth 33
  :rarity 3
  :hitpoints '(15 . 10)
  :armour 40
  :speed 110
  :xp 375
  :abilities '(<bash-door> <open-door> <cold-blood> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<touch> :type <exp_40> :damage nil) (<hit> :type <hurt> :damage (1 . 8))
             (<hit> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop-chance> 3/5))
  :special-abilities '((<spell> <darkness>) (<spell> (<cause> 2)) (<spell> <scare>) (<spell> <paralysis>)
                       (<frequency> 1/8))) 

(define-monster-kind "monster-338" "giant skeleton troll"
  :desc "It is the animated form of a massive troll."
  :symbol #\s
  :colour #\w
  :alignment '<evil>
  :type '(<undead> <troll>)
  :depth 33
  :rarity 1
  :hitpoints '(45 . 10)
  :armour 50
  :speed 110
  :xp 325
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind> <max-hitpoints>)
  :immunities '(<fear> <sleep> <confuse> <poison> <cold>)
  :alertness 20
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 5)) (<bite> :type <hurt> :damage (1 . 5))
             (<hit> :type <hurt> :damage (1 . 9)) (<hit> :type <hurt> :damage (1 . 9)))) 

(define-monster-kind "monster-339" "chaos drake"
  :desc "A dragon twisted by the forces of chaos.  It seems first ugly, then fair,  as its form shimmers and changes in front of your eyes."
  :symbol #\d
  :colour #\v
  :alignment '<evil>
  :type '(<dragon>)
  :depth 33
  :rarity 3
  :hitpoints '(50 . 10)
  :armour 100
  :speed 110
  :xp 700
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confuse> <fire>)
  :alertness 30
  :vision 25
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<claw> :type <hurt> :damage (1 . 8))
             (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <chaos>) (<breath> <disenchant>) (<spell> <scare>) (<spell> <confusion>)
                       (<spell> <slow>) (<frequency> 1/6))) 


(define-monster-kind "monster-340" "law drake"
  :desc "This dragon is clever and cunning.  It laughs at your puny efforts to  disturb it."
  :symbol #\d
  :colour #\B
  :alignment '<evil>
  :type '(<dragon>)
  :depth 33
  :rarity 3
  :hitpoints '(55 . 10)
  :armour 100
  :speed 110
  :xp 700
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <cold>)
  :alertness 30
  :vision 25
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<claw> :type <hurt> :damage (1 . 8))
             (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <shards>) (<breath> <sound>) (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>)
                       (<frequency> 1/6))) 

(define-monster-kind "monster-341" "balance drake"
  :desc "A mighty dragon, the balance drake seeks to maintain the Cosmic Balance,  and despises your feeble efforts to destroy evil."
  :symbol #\d
  :colour #\v
  :alignment '<evil>
  :type '(<dragon>)
  :depth 33
  :rarity 3
  :hitpoints '(60 . 10)
  :armour 100
  :speed 110
  :xp 700
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confuse> <cold> <fire>)
  :alertness 30
  :vision 25
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<claw> :type <hurt> :damage (1 . 8))
             (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <chaos>) (<breath> <disenchant>) (<breath> <shards>) (<breath> <sound>)
                       (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>) (<frequency> 1/6))) 

(define-monster-kind "monster-342" "ethereal drake"
  :desc "A dragon of elemental power, with control over light and dark, the  ethereal drake's eyes glare with white hatred from the shadows."
  :symbol #\d
  :colour #\o
  :alignment '<evil>
  :type '(<dragon>)
  :depth 33
  :rarity 3
  :hitpoints '(45 . 10)
  :armour 100
  :speed 110
  :xp 700
  :abilities '(<pass-wall> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 15
  :vision 25
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<claw> :type <hurt> :damage (1 . 8))
             (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <darkness>) (<breath> <light>) (<spell> <scare>) (<spell> <confusion>)
                       (<spell> <slow>) (<frequency> 1/6))) 


(define-monster-kind "shade" "shade"
  :desc "A shadowy form clutches at you from the darkness.  A powerful undead with  a deadly touch."
  :symbol #\G
  :colour #\D
  :alignment '<evil>
  :type '(<undead>)
  :depth 33
  :rarity 3
  :hitpoints '(14 . 20)
  :armour 30
  :speed 120
  :xp 350
  :abilities '(<pass-wall> <pick-up-item> <cold-blood> <invisible> (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :alertness 10
  :vision 20
  :attacks '((<claw> :type <lose-int> :damage (1 . 10)) (<touch> :type <exp_40> :damage nil)
             (<wail> :type <terrify> :damage nil))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) <only-drop-items>)
  :special-abilities '((<spell> <forget>) (<spell> <drain-mana>) (<spell> <paralysis>) (<spell> <blindness>)
                       (<frequency> 1/15))) 

(define-monster-kind "spectre" "spectre"
  :desc "A phantasmal shrieking spirit.  Its wail drives the intense cold of pure  evil deep within your body."
  :symbol #\G
  :colour #\U
  :alignment '<evil>
  :type '(<undead>)
  :depth 33
  :rarity 3
  :hitpoints '(14 . 20)
  :armour 30
  :speed 120
  :xp 350
  :abilities '(<pass-wall> <pick-up-item> <cold-blood> <invisible> (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :alertness 10
  :vision 20
  :attacks '((<claw> :type <lose-wis> :damage (5 . 5)) (<touch> :type <exp-40> :damage nil)
             (<wail> :type <terrify> :damage nil))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) <only-drop-items>)
  :special-abilities '((<spell> <forget>) (<spell> <drain-mana>) (<spell> <paralysis>) (<spell> <blindness>)
                       (<frequency> 1/15))) 

(define-monster-kind "water-troll" "water troll"
  :desc "He is a troll that reeks of brine."
  :symbol #\T
  :colour #\s
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
  :sex '<male>) 

(define-monster-kind "monster-349" "fire elemental"
  :desc "It is a towering inferno of flames."
  :symbol #\E
  :colour #\r
  :alignment '<evil>
  :depth 33
  :rarity 2
  :hitpoints '(30 . 8)
  :armour 50
  :speed 110
  :xp 350
  :abilities '(<powerful-breath> <bash-door> <overrun-others> <overrun-items> <empty-mind> (<random-mover> 1/4)
               <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <fire>)
  :alertness 50
  :vision 12
  :attacks '((<hit> :type <fire> :damage (4 . 6)) (<hit> :type <fire> :damage (4 . 6)))
  :special-abilities '((<spell> (<bolt> <fire>)) (<frequency> 1/6))) 


(define-monster-kind "astral-deva" "astral deva"
  :desc "It is an angel moving very quickly, wielding a holy war hammer and casting  a volley of powerful spells in your direction."
  :symbol #\A
  :colour #\G
  :depth 33
  :rarity 6
  :hitpoints '(45 . 10)
  :armour 68
  :speed 120
  :xp 400
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <pick-up-item> <smart> <max-hitpoints>
               <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 255
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (4 . 3))
             (<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (4 . 3)))
  :treasures '((<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :special-abilities '((<summon> <monsters>) (<spell> (<bolt> <fire>)) (<spell> <mind-blast>) (<spell> <scare>)
                       (<spell> <blindness>) (<spell> <haste>) (<spell> <heal>) (<frequency> 1/3))) 

(define-monster-kind "monster-351" "water elemental"
  :desc "It is a towering tempest of water."
  :symbol #\E
  :colour #\s
  :alignment '<evil>
  :depth 33
  :rarity 2
  :hitpoints '(25 . 8)
  :armour 40
  :speed 110
  :xp 325
  :abilities '(<powerful-breath> <bash-door> <overrun-items> <overrun-others> <cold-blood> <empty-mind>
               (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison>)
  :alertness 50
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (1 . 10)) (<hit> :type <hurt> :damage (1 . 10))
             (<hit> :type <hurt> :damage (1 . 10)))
  :special-abilities '((<spell> (<bolt> <cold>)) (<frequency> 1/6))) 

(define-monster-kind "monster-352" "invisible stalker"
  :desc "It is impossible to define its form but its violence is legendary."
  :symbol #\E
  :colour #\y
  :alignment '<evil>
  :depth 34
  :rarity 3
  :hitpoints '(19 . 12)
  :armour 46
  :speed 130
  :xp 300
  :abilities '(<powerful-breath> <bash-door> <open-door> <cold-blood> <invisible> <empty-mind> (<random-mover> 1/2))
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 6))
             (<hit> :type <hurt> :damage (1 . 6)))) 

(define-monster-kind "monster-353" "carrion crawler"
  :desc "A hideous centipede covered in slime and with glowing tentacles around its  head."
  :symbol #\c
  :colour #\o
  :type '(<animal>)
  :depth 34
  :rarity 2
  :hitpoints '(20 . 12)
  :armour 40
  :speed 110
  :xp 100
  :abilities '(<bash-door> <weird-mind> (<random-mover> 1/4))
  :immunities '(<poison>)
  :alertness 10
  :vision 15
  :attacks '((<sting> :type <paralyze> :damage (2 . 6)) (<sting> :type <paralyze> :damage (2 . 6)))) 

(define-monster-kind "monster-354" "master thief"
  :desc "Cool and confident, fast and lithe; protect your possessions quickly!"
  :symbol #\p
  :colour #\B
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
  :sex '<male>) 


(define-monster-kind "lich" "lich"
  :desc "It is a skeletal form dressed in robes.  It radiates vastly evil power."
  :symbol #\L
  :colour #\o
  :alignment '<evil>
  :type '(<undead>)
  :depth 34
  :rarity 3
  :hitpoints '(30 . 10)
  :armour 60
  :speed 110
  :xp 800
  :abilities '(<bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 60
  :vision 20
  :attacks '((<touch> :type <lose-dex> :damage (2 . 8)) (<touch> :type <lose-dex> :damage (2 . 8))
             (<touch> :type <un_power> :damage nil) (<touch> :type <exp-40> :damage nil))
  :treasures '((<drop> "1d2"))
  :special-abilities '((<spell> <brain-smash>) (<spell> <drain-mana>) (<spell> (<cause> 3)) (<spell> <scare>)
                       (<spell> <slow>) (<spell> <paralysis>) (<spell> <blindness>) (<spell> <teleport-away>)
                       (<spell> <teleport-player>) (<spell> <blink>) (<frequency> 1/4))) 

(define-monster-kind "monster-357" "master vampire"
  :desc "It is a humanoid form dressed in robes.  Power emanates from its chilling  frame."
  :symbol #\V
  :colour #\g
  :alignment '<evil>
  :type '(<undead>)
  :depth 34
  :rarity 3
  :hitpoints '(34 . 10)
  :armour 60
  :speed 110
  :xp 750
  :abilities '(<regenerate> <bash-door> <open-door> <cold-blood> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<bite> :type <exp_40> :damage (1 . 4)) (<bite> :type <exp_40> :damage (1 . 4))
             (<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop> "4d2"))
  :special-abilities '((<spell> (<bolt> <nether>)) (<spell> <darkness>) (<spell> <forget>) (<spell> <mind-blast>)
                       (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <confusion>) (<spell> <paralysis>)
                       (<spell> <teleport-player>) (<frequency> 1/6))) 

(define-monster-kind "monster-358" "giant grey scorpion"
  :desc "It is a giant grey scorpion.  It looks poisonous."
  :symbol #\S
  :colour #\s
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

(define-monster-kind "monster-359" "earth elemental"
  :desc "It is a towering form composed of rock with fists of awesome power."
  :symbol #\E
  :colour #\u
  :alignment '<evil>
  :depth 34
  :rarity 2
  :hitpoints '(30 . 10)
  :armour 60
  :speed 100
  :xp 375
  :abilities '(<powerful-breath> <pass-wall> <overrun-others> <overrun-items> <cold-blood> <empty-mind>
               <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 90
  :vision 10
  :attacks '((<hit> :type <hurt> :damage (4 . 6)) (<hit> :type <hurt> :damage (4 . 6))
             (<hit> :type <hurt> :damage (4 . 6)))
  :special-abilities '((<spell> (<bolt> <acid>)) (<frequency> 1/8))) 


(define-monster-kind "air-elemental" "air elemental"
  :desc "It is a towering tornado of winds."
  :symbol #\E
  :colour #\B
  :alignment '<evil>
  :depth 34
  :rarity 2
  :hitpoints '(30 . 5)
  :armour 50
  :speed 120
  :xp 390
  :abilities '(<powerful-breath> <bash-door> <overrun-items> <overrun-others> <cold-blood> <empty-mind>
               (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 50
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (1 . 10)) (<hit> :type <confuse> :damage (1 . 4))
             (<hit> :type <hurt> :damage (1 . 10)))
  :special-abilities '((<spell> (<bolt> <lightning>)) (<frequency> 1/8))) 

(define-monster-kind "monster-361" "hell hound"
  :desc "It is a giant dog that glows with heat.  Flames pour from its nostrils."
  :symbol #\C
  :colour #\r
  :alignment '<evil>
  :type '(<animal>)
  :depth 35
  :rarity 3
  :hitpoints '(40 . 10)
  :armour 80
  :speed 120
  :xp 600
  :abilities '(<push-others> <bash-door> (<random-mover> 1/4) <max-hitpoints> <initial-sleeper>)
  :immunities '(<fire>)
  :alertness 0
  :vision 25
  :attacks '((<bite> :type <fire> :damage (3 . 12)) (<bite> :type <fire> :damage (3 . 12))
             (<bite> :type <fire> :damage (3 . 12)))
  :special-abilities '((<breath> <fire>) (<frequency> 1/5))) 

(define-monster-kind "monster-362" "eog golem"
  :desc "It is a massive deep brown statue, striding towards you with an  all-too-familiar purpose.  Your magic surprisingly feels much less  powerful now."
  :symbol #\g
  :colour #\u
  :depth 35
  :rarity 4
  :hitpoints '(100 . 20)
  :armour 125
  :speed 100
  :xp 1200
  :abilities '(<bash-door> <cold-blood> <empty-mind>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (6 . 6)) (<hit> :type <hurt> :damage (6 . 6))
             (<hit> :type <hurt> :damage (8 . 6)) (<hit> :type <hurt> :damage (8 . 6)))
  :treasures '((<drop> "2d2") <only-drop-gold>)) 

(define-monster-kind "monster-363" "olog"
  :desc "It is a massive intelligent troll with needle sharp fangs."
  :symbol #\T
  :colour #\y
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

(define-monster-kind "monster-364" "dagashi"
  :desc "A human warrior, moving with lightning speed."
  :symbol #\p
  :colour #\y
  :alignment '<evil>
  :depth 35
  :rarity 4
  :hitpoints '(13 . 25)
  :armour 70
  :speed 120
  :xp 500
  :abilities '(<bash-door> <open-door>)
  :immunities '(<sleep> <confuse>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <poison> :damage (3 . 4)) (<hit> :type <lose-str> :damage (3 . 4))
             (<hit> :type <lose-str> :damage (3 . 4)) (<hit> :type <poison> :damage (3 . 4)))
  :treasures '((<drop> "1d2"))
  :sex '<male>) 

(define-monster-kind "monster-365" "gravity hound"
  :desc "Unfettered by the usual constraints of gravity, these unnatural creatures  are walking on the walls and even the ceiling!  The earth suddenly feels  rather less solid as you see gravity warp all round the monsters."
  :symbol #\Z
  :colour #\W
  :type '(<animal>)
  :depth 35
  :rarity 2
  :hitpoints '(35 . 10)
  :armour 30
  :speed 110
  :xp 500
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<bite> :type <hurt> :damage (2 . 12))
             (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <gravity>) (<frequency> 1/5))) 

(define-monster-kind "monster-366" "acidic cytoplasm"
  :desc "A disgusting animated blob of destruction.  Flee its gruesome hunger!"
  :symbol #\j
  :colour #\s
  :depth 35
  :rarity 5
  :hitpoints '(40 . 10)
  :armour 18
  :speed 120
  :xp 36
  :abilities '(<bash-door> <open-door> <empty-mind> <stupid> <cold-blood> <pick-up-item> <max-hitpoints>)
  :immunities '(<sleep> <confuse> <fear> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 1
  :vision 12
  :attacks '((<touch> :type <acid> :damage (1 . 10)) (<touch> :type <acid> :damage (1 . 10))
             (<touch> :type <acid> :damage (1 . 10)) (<touch> :type <acid> :damage (1 . 10)))
  :treasures '((<drop> "4d2") (<drop> "1d2"))) 

(define-monster-kind "monster-367" "inertia hound"
  :desc "Bizarrely, this hound seems to be hardly moving at all, yet it approaches  you with deadly menace.  It makes you tired just to look at it."
  :symbol #\Z
  :colour #\W
  :type '(<animal>)
  :depth 35
  :rarity 2
  :hitpoints '(35 . 10)
  :armour 30
  :speed 110
  :xp 500
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<bite> :type <hurt> :damage (2 . 12))
             (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <inertia>) (<frequency> 1/5))) 

(define-monster-kind "monster-368" "impact hound"
  :desc "A deep brown shape is visible before you, its canine form strikes you with  an almost physical force.  The dungeon floor buckles as if struck by a  powerful blow as it stalks towards you."
  :symbol #\Z
  :colour #\u
  :type '(<animal>)
  :depth 35
  :rarity 2
  :hitpoints '(35 . 10)
  :armour 30
  :speed 110
  :xp 500
  :abilities '(<bash-door> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<bite> :type <hurt> :damage (2 . 12))
             (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <force>) (<frequency> 1/8))) 

(define-monster-kind "dread" "dread"
  :desc "It is a form that screams its presence against the eye.  Death incarnate,  its hideous black body seems to struggle against reality as the universe  itself struggles to banish it."
  :symbol #\G
  :colour #\o
  :alignment '<evil>
  :type '(<undead>)
  :depth 35
  :rarity 2
  :hitpoints '(25 . 20)
  :armour 30
  :speed 120
  :xp 600
  :abilities '(<pass-wall> <cold-blood> <invisible> <pick-up-item> (<random-mover> 1/4) <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <lose-str> :damage (3 . 4)) (<hit> :type <hurt> :damage (6 . 6))
             (<hit> :type <hurt> :damage (6 . 6)))
  :treasures '((<drop> "2d2") (<drop-chance> 3/5) <only-drop-items>)
  :special-abilities '((<spell> (<bolt> <nether>)) (<spell> <drain-mana>) (<spell> <confusion>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<frequency> 1/15))
  :appear-in-group? #'van-novice-appears-in-group?)


(define-monster-kind "monster-370" "ooze elemental"
  :desc "It is a towering mass of filth, an eyesore of ooze."
  :symbol #\E
  :colour #\g
  :alignment '<evil>
  :depth 35
  :rarity 3
  :hitpoints '(13 . 10)
  :armour 80
  :speed 110
  :xp 300
  :abilities '(<powerful-breath> <bash-door> <overrun-others> <overrun-items> <cold-blood> <empty-mind>
               <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 90
  :vision 10
  :attacks '((<touch> :type <acid> :damage (1 . 10)) (<touch> :type <acid> :damage (1 . 10))
             (<touch> :type <acid> :damage (1 . 10)))
  :special-abilities '((<spell> (<ball> <acid>)) (<spell> (<bolt> <acid>)) (<frequency> 1/5))) 

(define-monster-kind "monster-371" "smoke elemental"
  :desc "It is a towering blackened form, crackling with heat."
  :symbol #\E
  :colour #\R
  :alignment '<evil>
  :depth 35
  :rarity 3
  :hitpoints '(15 . 10)
  :armour 80
  :speed 120
  :xp 375
  :abilities '(<powerful-breath> <bash-door> <overrun-others> <overrun-items> <empty-mind> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :alertness 90
  :vision 10
  :attacks '((<bite> :type <hurt> :damage (2 . 6)) (<bite> :type <hurt> :damage (2 . 6)))
  :special-abilities '((<spell> (<bolt> <fire>)) (<spell> <darkness>) (<frequency> 1/5))) 

(define-monster-kind "monster-372" "young black dragon"
  :desc "It has a form that legends are made of.  Its still-tender scales are a  darkest black hue.  Acid drips from its body."
  :symbol #\d
  :colour #\s
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
  :desc "A massive elephantine form with eyes twisted by madness."
  :symbol #\q
  :colour #\s
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


(define-monster-kind "red-ant" "giant red ant"
  :desc "A giant ant covered in shaggy fur.  Its powerful jaws glow with heat."
  :symbol #\a
  :colour #\r
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

(define-monster-kind "mature-white-dragon" "mature white dragon"
  :desc "A large dragon, scales gleaming bright white."
  :symbol #\d
  :colour #\w
  :alignment '<evil>
  :type '(<dragon>)
  :depth 35
  :rarity 1
  :hitpoints '(40 . 10)
  :armour 65
  :speed 110
  :xp 1000
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <cold>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 8)) (<claw> :type <hurt> :damage (1 . 8))
             (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "2d2"))
  :special-abilities '((<breath> <cold>) (<spell> <scare>) (<frequency> 1/10))) 

(define-monster-kind "monster-376" "xorn"
  :desc "A huge creature of the element Earth.  Able to merge with its element, it  has four huge arms protruding from its enormous torso."
  :symbol #\X
  :colour #\u
  :depth 36
  :rarity 2
  :hitpoints '(16 . 10)
  :armour 80
  :speed 110
  :xp 650
  :abilities '(<pass-wall> <overrun-items> <cold-blood> <empty-mind> <max-hitpoints>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 6))
             (<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 6)))) 

(define-monster-kind "monster-377" "shadow"
  :desc "A mighty spirit of darkness of vaguely humanoid form.  Razor-edged claws  reach out to end your life as it glides towards you, seeking to suck the  energy from your soul to feed its power."
  :symbol #\G
  :colour #\D
  :alignment '<evil>
  :type '(<undead>)
  :depth 36
  :rarity 3
  :hitpoints '(10 . 20)
  :armour 30
  :speed 120
  :xp 400
  :abilities '(<pass-wall> <cold-blood> <invisible> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :alertness 20
  :vision 30
  :attacks '((<claw> :type <lose_wis> :damage (1 . 10)) (<claw> :type <lose_int> :damage (1 . 10))
             (<touch> :type <exp_40> :damage nil) (<touch> :type <exp_80> :damage nil))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :special-abilities '((<spell> <slow>) (<spell> <teleport-player>) (<frequency> 1/8))) 

(define-monster-kind "monster-378" "phantom"
  :desc "An unholy creature of darkness, the aura emanating from this evil being  saps your very soul."
  :symbol #\G
  :colour #\v
  :alignment '<evil>
  :type '(<undead>)
  :depth 36
  :rarity 3
  :hitpoints '(20 . 25)
  :armour 30
  :speed 120
  :xp 400
  :abilities '(<pass-wall> <cold-blood> <invisible> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :alertness 20
  :vision 30
  :attacks '((<claw> :type <lose_wis> :damage (1 . 10)) (<claw> :type <lose_int> :damage (1 . 10))
             (<touch> :type <exp_40> :damage nil) (<touch> :type <exp_80> :damage nil))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :special-abilities '((<spell> <forget>) (<frequency> 1/5))) 

(define-monster-kind "monster-379" "grey wraith"
  :desc "A tangible but ghostly form, made of grey fog.  The air around it feels  deathly cold."
  :symbol #\W
  :colour #\s
  :alignment '<evil>
  :type '(<undead>)
  :depth 36
  :rarity 1
  :hitpoints '(19 . 10)
  :armour 50
  :speed 110
  :xp 700
  :abilities '(<bash-door> <open-door> <cold-blood> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<touch> :type <exp_40> :damage nil) (<hit> :type <hurt> :damage (1 . 10))
             (<hit> :type <hurt> :damage (1 . 10)))
  :treasures '((<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<spell> <darkness>) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <paralysis>)
                       (<frequency> 1/7))) 

(define-monster-kind "monster-380" "young multi-hued dragon"
  :desc "It has a form that legends are made of.  Beautiful scales of shimmering  and magical colours cover it."
  :symbol #\d
  :colour #\v
  :alignment '<evil>
  :type '(<dragon>)
  :depth 36
  :rarity 1
  :hitpoints '(32 . 10)
  :armour 60
  :speed 110
  :xp 1320
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 50
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 10)) (<claw> :type <hurt> :damage (1 . 9))
             (<claw> :type <hurt> :damage (1 . 9)))
  :treasures '((<drop> "4d2") (<drop> "1d2"))
  :special-abilities '((<breath> <poison>) (<breath> <lightning>) (<breath> <cold>) (<breath> <fire>) (<breath> <acid>)
                       (<spell> <scare>) (<frequency> 1/5))) 

(define-monster-kind "monster-381" "colossus"
  :desc "An enormous construct resembling a titan made from stone.  It strides  purposefully towards you, swinging its slow fists with earth-shattering  power."
  :symbol #\g
  :colour #\G
  :depth 36
  :rarity 4
  :hitpoints '(30 . 100)
  :armour 150
  :speed 100
  :xp 850
  :abilities '(<bash-door> <cold-blood> <empty-mind> <max-hitpoints>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :alertness 10
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (6 . 6)) (<hit> :type <hurt> :damage (6 . 6))
             (<hit> :type <hurt> :damage (10 . 10)) (<hit> :type <hurt> :damage (10 . 10)))) 

(define-monster-kind "monster-382" "young gold dragon"
  :desc "It has a form that legends are made of.  Its still-tender scales are a  tarnished gold hue, and light is reflected from its form."
  :symbol #\d
  :colour #\y
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


(define-monster-kind "monster-384" "mature blue dragon"
  :desc "A large dragon, scales tinted deep blue."
  :symbol #\d
  :colour #\b
  :alignment '<evil>
  :type '(<dragon>)
  :depth 36
  :rarity 1
  :hitpoints '(40 . 10)
  :armour 75
  :speed 110
  :xp 1200
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <lightning>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 10)) (<claw> :type <hurt> :damage (1 . 8))
             (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<breath> <lightning>) (<spell> <scare>) (<frequency> 1/9))) 

(define-monster-kind "monster-385" "mature green dragon"
  :desc "A large dragon, scales tinted deep green."
  :symbol #\d
  :colour #\g
  :alignment '<evil>
  :type '(<dragon>)
  :depth 36
  :rarity 1
  :hitpoints '(40 . 10)
  :armour 70
  :speed 110
  :xp 1100
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 6)) (<claw> :type <hurt> :damage (1 . 4))
             (<claw> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<breath> <poison>) (<spell> <scare>) (<frequency> 1/9))) 

(define-monster-kind "monster-386" "mature bronze dragon"
  :desc "A large dragon with scales of rich bronze."
  :symbol #\d
  :colour #\U
  :type '(<dragon>)
  :depth 36
  :rarity 2
  :hitpoints '(44 . 10)
  :armour 70
  :speed 110
  :xp 1300
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 150
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 10)) (<claw> :type <hurt> :damage (1 . 8))
             (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "4d2") (<drop> "1d2"))
  :special-abilities '((<breath> <confusion>) (<spell> <scare>) (<spell> <confusion>) (<frequency> 1/9))) 

(define-monster-kind "monster-387" "young red dragon"
  :desc "It has a form that legends are made of.  Its still-tender scales are a  deepest red hue.  Heat radiates from its form."
  :symbol #\d
  :colour #\r
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
  :desc "A larger cousin of the lurker, this creature traps unsuspecting victims  and paralyzes them, to be slowly digested later."
  :symbol #\.
  :colour #\w
  :depth 36
  :rarity 3
  :hitpoints '(60 . 10)
  :armour 75
  :speed 120
  :xp 580
  :abilities '(<cold-blood> <empty-mind> <invisible> <max-hitpoints> <never-move> <see-through> <special-symbol>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 10
  :vision 30
  :attacks '((<hit> :type <paralyze> :damage (15 . 1)) (<hit> :type <paralyze> :damage (15 . 1))
             (<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (3 . 8)))) 

(define-monster-kind "monster-389" "bodak"
  :desc "It is a humanoid form composed of flames and hatred."
  :symbol #\u
  :colour #\r
  :alignment '<evil>
  :type '(<demon>)
  :depth 36
  :rarity 2
  :hitpoints '(35 . 10)
  :armour 68
  :speed 110
  :xp 750
  :abilities '(<bash-door> <open-door> <pick-up-item> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <fire>)
  :alertness 90
  :vision 10
  :attacks '((<gaze> :type <exp_20> :damage nil) (<hit> :type <fire> :damage (4 . 6))
             (<hit> :type <fire> :damage (4 . 6)))
  :special-abilities '((<summon> <demon>) (<spell> (<ball> <fire>)) (<spell> (<bolt> <fire>)) (<frequency> 1/4))) 


(define-monster-kind "monster-390" "ice elemental"
  :desc "It is a towering glacier of ice."
  :symbol #\E
  :colour #\w
  :alignment '<evil>
  :depth 36
  :rarity 2
  :hitpoints '(35 . 10)
  :armour 60
  :speed 110
  :xp 650
  :abilities '(<powerful-breath> <bash-door> <overrun-others> <overrun-items> <cold-blood> <empty-mind>
               <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold>)
  :alertness 90
  :vision 10
  :attacks '((<bite> :type <cold> :damage (1 . 3)) (<hit> :type <hurt> :damage (4 . 6))
             (<bite> :type <cold> :damage (1 . 3)))
  :special-abilities '((<spell> (<ball> <cold>)) (<spell> (<bolt> <ice>)) (<frequency> 1/5))) 

(define-monster-kind "monster-391" "necromancer"
  :desc "A gaunt figure, clothed in black robes."
  :symbol #\p
  :colour #\R
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
  :sex '<male>
  :special-abilities '((<summon> <undead>) (<spell> (<bolt> <nether>)) (<spell> (<cause> 3)) (<spell> <scare>)
                       (<spell> <paralysis>) (<spell> <blindness>) (<spell> <teleport-player>) (<spell> <teleport>)
                       (<spell> <haste>) (<frequency> 1/3))) 


(define-monster-kind "monster-393" "demonologist"
  :desc "A figure twisted by evil standing in robes of deepest crimson."
  :symbol #\p
  :colour #\R
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
  :sex '<male>
  :special-abilities '((<summon> <demon>) (<spell> <paralysis>) (<spell> <teleport>) (<frequency> 1/2))) 

(define-monster-kind "monster-394" "mummified troll"
  :desc "It is a massive figure clothed in wrappings.  You are wary of its massive  fists."
  :symbol #\z
  :colour #\w
  :alignment '<evil>
  :type '(<undead> <troll>)
  :depth 37
  :rarity 1
  :hitpoints '(19 . 10)
  :armour 50
  :speed 110
  :xp 420
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind> <max-hitpoints>)
  :immunities '(<fear> <sleep> <confuse> <poison> <cold>)
  :alertness 50
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 6)) (<hit> :type <hurt> :damage (2 . 6)))
  :treasures '((<drop-chance> 3/5))) 


(define-monster-kind "monster-396" "will o' the wisp"
  :desc "A strange ball of glowing light.  It disappears and reappears and seems to  draw you to it.  You seem somehow compelled to stand still and watch its  strange dancing motion."
  :symbol #\E
  :colour #\W
  :depth 37
  :rarity 4
  :hitpoints '(20 . 10)
  :armour 150
  :speed 130
  :xp 500
  :abilities '(<powerful-breath> <pass-wall> <invisible> <empty-mind> <smart> (<random-mover> 1/2) <max-hitpoints>
               <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 0
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (1 . 9)) (<hit> :type <hurt> :damage (1 . 9))
             (<hit> :type <hurt> :damage (1 . 9)) (<hit> :type <hurt> :damage (1 . 9)))
  :special-abilities '((<spell> (<cause> 2)) (<spell> <confusion>) (<spell> <teleport>) (<spell> <blink>)
                       (<frequency> 1/2))) 

(define-monster-kind "monster-397" "magma elemental"
  :desc "It is a towering glowing form of molten hate."
  :symbol #\E
  :colour #\r
  :alignment '<evil>
  :depth 37
  :rarity 2
  :hitpoints '(35 . 10)
  :armour 70
  :speed 110
  :xp 950
  :abilities '(<powerful-breath> <pass-wall> <overrun-others> <overrun-items> <empty-mind> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <fire>)
  :alertness 90
  :vision 10
  :attacks '((<hit> :type <fire> :damage (3 . 7)) (<hit> :type <hurt> :damage (4 . 6))
             (<hit> :type <fire> :damage (3 . 7)))
  :special-abilities '((<spell> (<ball> <fire>)) (<spell> (<bolt> <plasma>)) (<frequency> 1/7))) 

(define-monster-kind "monster-398" "black pudding"
  :desc "A lump of rotting black flesh that slurrrrrrrps across the dungeon floor."
  :symbol #\j
  :colour #\D
  :depth 37
  :rarity 5
  :hitpoints '(40 . 10)
  :armour 18
  :speed 110
  :xp 36
  :abilities '(<bash-door> <open-door> <pick-up-item> <cold-blood> <empty-mind> <stupid> <max-hitpoints>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 1
  :vision 12
  :attacks '((<touch> :type <acid> :damage (1 . 10)) (<touch> :type <acid> :damage (1 . 10))
             (<touch> :type <acid> :damage (1 . 10)) (<touch> :type <acid> :damage (1 . 10)))
  :treasures '((<drop> "1d2") (<drop-chance> 9/10) (<drop-chance> 3/5))) 

(define-monster-kind "monster-399" "killer blue beetle"
  :desc "It is a giant beetle, whose carapace shimmers with vibrant energies."
  :symbol #\K
  :colour #\b
  :type '(<animal>)
  :depth 37
  :rarity 2
  :hitpoints '(25 . 10)
  :armour 60
  :speed 110
  :xp 850
  :abilities '(<bash-door> <weird-mind> <max-hitpoints>)
  :immunities '(<lightning>)
  :alertness 30
  :vision 16
  :attacks '((<gaze> :type <paralyze> :damage nil) (<claw> :type <elec> :damage (1 . 12))
             (<claw> :type <elec> :damage (1 . 12)))) 


(define-monster-kind "monster-400" "nexus vortex"
  :desc "A maelstrom of potent magical energy."
  :symbol #\v
  :colour #\R
  :depth 37
  :rarity 1
  :hitpoints '(32 . 10)
  :armour 40
  :speed 120
  :xp 800
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/4) (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 100
  :attacks '((<engulf> :type <hurt> :damage (5 . 5)))
  :special-abilities '((<breath> <nexus>) (<frequency> 1/6))) 

(define-monster-kind "monster-401" "plasma vortex"
  :desc "A whirlpool of intense flame, charring the stones at your feet."
  :symbol #\v
  :colour #\r
  :depth 37
  :rarity 1
  :hitpoints '(32 . 10)
  :armour 40
  :speed 120
  :xp 800
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/4) (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <fire>)
  :alertness 0
  :vision 100
  :attacks '((<engulf> :type <fire> :damage (8 . 8)))
  :special-abilities '((<breath> <plasma>) (<frequency> 1/6))) 

(define-monster-kind "monster-402" "mature red dragon"
  :desc "A large dragon, scales tinted deep red."
  :symbol #\d
  :colour #\r
  :alignment '<evil>
  :type '(<dragon>)
  :depth 37
  :rarity 1
  :hitpoints '(48 . 10)
  :armour 80
  :speed 110
  :xp 1400
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire>)
  :alertness 30
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 12)) (<claw> :type <hurt> :damage (1 . 10))
             (<claw> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "4d2") (<drop> "1d2"))
  :special-abilities '((<breath> <fire>) (<spell> <scare>) (<spell> <confusion>) (<frequency> 1/9))) 

(define-monster-kind "monster-403" "mature gold dragon"
  :desc "A large dragon with scales of gleaming gold."
  :symbol #\d
  :colour #\y
  :type '(<dragon>)
  :depth 37
  :rarity 2
  :hitpoints '(56 . 10)
  :armour 80
  :speed 110
  :xp 1500
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 150
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 12)) (<claw> :type <hurt> :damage (1 . 10))
             (<claw> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "4d2") (<drop> "1d2"))
  :special-abilities '((<breath> <sound>) (<spell> <scare>) (<spell> <confusion>) (<frequency> 1/9))) 

(define-monster-kind "monster-404" "crystal drake"
  :desc "A dragon of strange crystalline form.  Light shines through it, dazzling  your eyes with spectrums of colour."
  :symbol #\d
  :colour #\v
  :alignment '<evil>
  :type '(<dragon>)
  :depth 37
  :rarity 2
  :hitpoints '(50 . 10)
  :armour 100
  :speed 120
  :xp 1500
  :abilities '(<bash-door> <open-door> <invisible> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confuse> <cold>)
  :alertness 30
  :vision 25
  :attacks '((<bite> :type <hurt> :damage (2 . 5)) (<claw> :type <hurt> :damage (1 . 4))
             (<claw> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "4d2") <only-drop-items>)
  :special-abilities '((<breath> <shards>) (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>) (<frequency> 1/6))) 

(define-monster-kind "monster-405" "mature black dragon"
  :desc "A large dragon, with scales of deepest black."
  :symbol #\d
  :colour #\s
  :alignment '<evil>
  :type '(<dragon>)
  :depth 37
  :rarity 1
  :hitpoints '(46 . 10)
  :armour 55
  :speed 110
  :xp 1350
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <acid>)
  :alertness 30
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 10)) (<claw> :type <hurt> :damage (1 . 8))
             (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) (<drop-chance> 3/5))
  :special-abilities '((<breath> <acid>) (<spell> <scare>) (<frequency> 1/9))) 

(define-monster-kind "monster-406" "mature multi-hued dragon"
  :desc "A large dragon, scales shimmering many colours."
  :symbol #\d
  :colour #\v
  :alignment '<evil>
  :type '(<dragon>)
  :depth 38
  :rarity 2
  :hitpoints '(64 . 10)
  :armour 65
  :speed 110
  :xp 1700
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 50
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 12)) (<claw> :type <hurt> :damage (1 . 10))
             (<claw> :type <hurt> :damage (1 . 10)))
  :treasures '((<drop> "4d2") (<drop> "3d2"))
  :special-abilities '((<breath> <poison>) (<breath> <lightning>) (<breath> <cold>) (<breath> <fire>) (<breath> <acid>)
                       (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/5))) 

(define-monster-kind "monster-407" "death knight"
  :desc "It is a humanoid form dressed in armour of an ancient form.  From beneath  its helmet, eyes glow a baleful red and seem to pierce you like lances of  fire."
  :symbol #\p
  :colour #\D
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


(define-monster-kind "monster-409" "time vortex"
  :desc "You haven't seen it yet."
  :symbol #\v
  :colour #\B
  :depth 38
  :rarity 4
  :hitpoints '(32 . 10)
  :armour 40
  :speed 130
  :xp 900
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/4) (<random-mover> 1/2) <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 100
  :attacks '((<engulf> :type <hurt> :damage (5 . 5)))
  :special-abilities '((<breath> <time>) (<frequency> 1/6))) 


(define-monster-kind "monster-410" "shimmering vortex"
  :desc "A strange pillar of shining light that hurts your eyes.  Its shape changes  constantly as it cuts through the air towards you.  It is like a beacon,  waking monsters from their slumber."
  :symbol #\v
  :colour #\v
  :depth 38
  :rarity 4
  :hitpoints '(6 . 12)
  :armour 30
  :speed 140
  :xp 200
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/4) (<random-mover> 1/2) <never-attack>
               <initial-sleeper> <colour-changing>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 100
  :special-abilities '(<shriek> (<breath> <light>) (<frequency> 1/4))) 

(define-monster-kind "monster-411" "ancient blue dragon"
  :desc "A huge draconic form.  Lightning crackles along its length."
  :symbol #\D
  :colour #\b
  :alignment '<evil>
  :type '(<dragon>)
  :depth 38
  :rarity 1
  :hitpoints '(70 . 10)
  :armour 80
  :speed 120
  :xp 1500
  :abilities '(<push-others> <powerful-breath> <bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <lightning>)
  :alertness 80
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 8)) (<claw> :type <hurt> :damage (1 . 8))
             (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "4d2") (<drop> "1d2"))
  :special-abilities '((<breath> <lightning>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<frequency> 1/9))) 

(define-monster-kind "monster-412" "ancient bronze dragon"
  :desc "A huge draconic form enveloped in a cascade of colour."
  :symbol #\D
  :colour #\U
  :type '(<dragon>)
  :depth 38
  :rarity 2
  :hitpoints '(73 . 10)
  :armour 100
  :speed 120
  :xp 1700
  :abilities '(<push-others> <powerful-breath> <bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 200
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 10)) (<claw> :type <hurt> :damage (1 . 8))
             (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "4d2") (<drop> "3d2"))
  :special-abilities '((<breath> <confusion>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<frequency> 1/6))) 

(define-monster-kind "beholder" "beholder"
  :desc "A disembodied eye, surrounded by twelve smaller eyes on stalks."
  :symbol #\e
  :colour #\U
  :alignment '<evil>
  :depth 38
  :rarity 4
  :hitpoints '(16 . 100)
  :armour 80
  :speed 120
  :xp 6000
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison>)
  :alertness 10
  :vision 30
  :attacks '((<gaze> :type <un_power> :damage (2 . 6)) (<gaze> :type <lose_int> :damage (2 . 6))
             (<gaze> :type <paralyze> :damage (2 . 4)) (<gaze> :type <exp_20> :damage (2 . 4)))
  :special-abilities '((<spell> (<bolt> <cold>)) (<spell> (<bolt> <fire>)) (<spell> (<bolt> <acid>))
                       (<spell> <darkness>) (<spell> <forget>) (<spell> <mind-blast>) (<spell> <drain-mana>)
                       (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>) (<spell> <blindness>)
                       (<frequency> 1/2))) 

(define-monster-kind "monster-414" "emperor wight"
  :desc "Your life force is torn from your body as this powerful unearthly being  approaches."
  :symbol #\W
  :colour #\r
  :alignment '<evil>
  :type '(<undead>)
  :depth 38
  :rarity 2
  :hitpoints '(38 . 10)
  :armour 40
  :speed 120
  :xp 1600
  :abilities '(<bash-door> <open-door> <cold-blood> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<touch> :type <exp_80> :damage nil) (<touch> :type <exp_80> :damage nil)
             (<hit> :type <hurt> :damage (1 . 12)) (<hit> :type <hurt> :damage (1 . 12)))
  :treasures '((<drop> "4d2") (<drop-chance> 9/10) <only-drop-items>)
  :special-abilities '((<spell> (<bolt> <nether>)) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <paralysis>)
                       (<frequency> 1/6))) 

(define-monster-kind "monster-415" "planetar"
  :desc "It is an angel, fast and strong.  You are stunned by its extreme holiness  and try to resist all desires to obey it."
  :symbol #\A
  :colour #\r
  :depth 38
  :rarity 6
  :hitpoints '(50 . 10)
  :armour 68
  :speed 120
  :xp 1800
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <pick-up-item> <smart> <max-hitpoints>
               <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 255
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (4 . 6)) (<hit> :type <hurt> :damage (5 . 5))
             (<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (4 . 6)))
  :treasures '((<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :special-abilities '((<summon> <angel>) (<summon> <monsters>) (<spell> (<bolt> <plasma>)) (<spell> (<bolt> <mana>))
                       (<spell> <confusion>) (<spell> <teleport-away>) (<spell> <haste>) (<spell> <heal>)
                       (<frequency> 1/11))) 


(define-monster-kind "monster-417" "black wraith"
  :desc "A figure that seems made of void, its strangely human shape is cloaked in  shadow.  It reaches out at you."
  :symbol #\W
  :colour #\D
  :alignment '<evil>
  :type '(<undead>)
  :depth 38
  :rarity 2
  :hitpoints '(50 . 10)
  :armour 55
  :speed 120
  :xp 1700
  :abilities '(<bash-door> <open-door> <cold-blood> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<touch> :type <exp_40> :damage nil) (<touch> :type <exp_40> :damage nil)
             (<hit> :type <hurt> :damage (1 . 12)) (<hit> :type <hurt> :damage (1 . 12)))
  :treasures '((<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :special-abilities '((<spell> (<bolt> <nether>)) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<frequency> 1/7))) 

(define-monster-kind "monster-418" "erinyes"
  :desc "It is a lesser demon of female form; however, she takes little time to  show her true colours."
  :symbol #\U
  :colour #\u
  :alignment '<evil>
  :type '(<demon>)
  :depth 38
  :rarity 2
  :hitpoints '(24 . 10)
  :armour 50
  :speed 110
  :xp 1000
  :abilities '(<powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<touch> :type <lose-str> :damage (1 . 5)) (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop-chance> 3/5) <only-drop-items>)
  :sex '<female>
  :special-abilities '((<spell> (<bolt> <fire>)) (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/7))) 

(define-monster-kind "monster-419" "nether wraith"
  :desc "A form that hurts the eye, death permeates the air around it.  As it nears  you, a coldness saps your soul."
  :symbol #\W
  :colour #\G
  :alignment '<evil>
  :type '(<undead>)
  :depth 39
  :rarity 2
  :hitpoints '(48 . 10)
  :armour 55
  :speed 120
  :xp 1700
  :abilities '(<pass-wall> <cold-blood> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<touch> :type <exp_80> :damage nil) (<touch> :type <exp_80> :damage nil)
             (<hit> :type <hurt> :damage (1 . 12)) (<hit> :type <hurt> :damage (1 . 12)))
  :treasures '((<drop> "4d2") (<drop-chance> 9/10) <only-drop-items>)
  :special-abilities '((<spell> (<bolt> <nether>)) (<spell> <darkness>) (<spell> <mind-blast>) (<spell> (<cause> 3))
                       (<spell> <scare>) (<spell> <blindness>) (<frequency> 1/6))) 


(define-monster-kind "monster-420" "eldrak"
  :desc "A massive troll, larger and stronger than many men together."
  :symbol #\T
  :colour #\r
  :alignment '<evil>
  :type '(<troll>)
  :depth 39
  :rarity 3
  :hitpoints '(75 . 10)
  :armour 80
  :speed 110
  :xp 800
  :abilities '(<bash-door> <open-door> <pick-up-item> <max-hitpoints>)
  :immunities '(<sleep> <confuse> <poison>)
  :alertness 50
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4))
             (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop-chance> 3/5))) 

(define-monster-kind "monster-421" "ettin"
  :desc "A massive troll of huge strength.  Ettins are stupid but violent."
  :symbol #\T
  :colour #\b
  :alignment '<evil>
  :type '(<troll>)
  :depth 39
  :rarity 3
  :hitpoints '(15 . 100)
  :armour 100
  :speed 110
  :xp 1000
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<sleep> <confuse> <poison>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 6)) (<hit> :type <hurt> :damage (3 . 6))
             (<hit> :type <hurt> :damage (3 . 6)))
  :treasures '((<drop> "1d2") <only-drop-items>)) 


(define-monster-kind "monster-424" "ancient white dragon"
  :desc "A huge draconic form.  Frost covers it from head to tail."
  :symbol #\D
  :colour #\w
  :alignment '<evil>
  :type '(<dragon>)
  :depth 39
  :rarity 1
  :hitpoints '(70 . 10)
  :armour 90
  :speed 120
  :xp 2500
  :abilities '(<push-others> <powerful-breath> <bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <cold>)
  :alertness 80
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 12)) (<claw> :type <hurt> :damage (1 . 9))
             (<claw> :type <hurt> :damage (1 . 9)))
  :treasures '((<drop> "4d2") (<drop> "3d2"))
  :special-abilities '((<breath> <cold>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<frequency> 1/9))) 

(define-monster-kind "monster-425" "ancient green dragon"
  :desc "A huge draconic form enveloped in clouds of poisonous vapour."
  :symbol #\D
  :colour #\g
  :alignment '<evil>
  :type '(<dragon>)
  :depth 39
  :rarity 1
  :hitpoints '(72 . 10)
  :armour 85
  :speed 120
  :xp 2400
  :abilities '(<push-others> <powerful-breath> <bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison>)
  :alertness 80
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 10)) (<claw> :type <hurt> :damage (1 . 8))
             (<claw> :type <hurt> :damage (1 . 8)))
  :treasures '((<drop> "4d2") (<drop> "3d2"))
  :special-abilities '((<breath> <poison>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<frequency> 1/9))) 

(define-monster-kind "monster-426" "7-headed hydra"
  :desc "A strange reptilian hybrid with seven heads dripping venom."
  :symbol #\M
  :colour #\G
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

(define-monster-kind "monster-427" "night mare"
  :desc "A fearsome skeletal horse with glowing eyes, that watch you with little  more than a hatred of all that lives."
  :symbol #\q
  :colour #\D
  :alignment '<evil>
  :type '(<undead>)
  :depth 39
  :rarity 3
  :hitpoints '(15 . 100)
  :armour 85
  :speed 120
  :xp 2900
  :abilities '(<bash-door> <open-door> <cold-blood> <max-hitpoints>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :alertness 0
  :vision 30
  :attacks '((<hit> :type <confuse> :damage (6 . 6)) (<hit> :type <hurt> :damage (3 . 8))
             (<hit> :type <hurt> :damage (3 . 8)) (<bite> :type <exp_80> :damage (2 . 6)))
  :treasures '((<drop> "2d2") <only-drop-gold>)) 

(define-monster-kind "monster-428" "vampire lord"
  :desc "A foul wind chills your bones as this ghastly figure approaches."
  :symbol #\V
  :colour #\b
  :alignment '<evil>
  :type '(<undead>)
  :depth 39
  :rarity 3
  :hitpoints '(16 . 100)
  :armour 70
  :speed 120
  :xp 1800
  :abilities '(<regenerate> <bash-door> <open-door> <cold-blood> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<bite> :type <exp_80> :damage (1 . 6)) (<bite> :type <exp_80> :damage (1 . 6))
             (<hit> :type <hurt> :damage (1 . 6)) (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop> "4d2") (<drop-chance> 3/5))
  :special-abilities '((<spell> (<bolt> <nether>)) (<spell> <darkness>) (<spell> <brain-smash>) (<spell> <drain-mana>)
                       (<spell> (<cause> 4)) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<frequency> 1/7))) 

(define-monster-kind "monster-429" "ancient black dragon"
  :desc "A huge draconic form.  Pools of acid melt the floor around it."
  :symbol #\D
  :colour #\s
  :alignment '<evil>
  :type '(<dragon>)
  :depth 39
  :rarity 1
  :hitpoints '(72 . 10)
  :armour 90
  :speed 120
  :xp 2500
  :abilities '(<push-others> <powerful-breath> <bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <acid>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 10)) (<claw> :type <hurt> :damage (1 . 9))
             (<claw> :type <hurt> :damage (1 . 9)))
  :treasures '((<drop> "4d2") (<drop> "3d2"))
  :special-abilities '((<breath> <acid>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<frequency> 1/9))) 


(define-monster-kind "monster-430" "disenchanter worm mass"
  :desc "It is a strange mass of squirming worms.  Magical energy crackles  around its disgusting form."
  :symbol #\w
  :colour #\v
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
  :attacks '((<crawl> :type <un_bonus> :damage (1 . 4)))) 

(define-monster-kind "monster-431" "rotting quylthulg"
  :desc "It is a pulsing flesh mound that reeks of death and putrefaction."
  :symbol #\Q
  :colour #\u
  :alignment '<evil>
  :type '(<animal>)
  :depth 40
  :rarity 1
  :hitpoints '(16 . 10)
  :armour 1
  :speed 120
  :xp 1500
  :abilities '(<empty-mind> <invisible> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 20
  :special-abilities '((<summon> <undead>) (<spell> <teleport>) (<spell> <blink>) (<frequency> 1/2))) 

(define-monster-kind "monster-432" "spirit troll"
  :desc "A weird troll from the elemental planes."
  :symbol #\T
  :colour #\B
  :alignment '<evil>
  :type '(<troll>)
  :depth 40
  :rarity 3
  :hitpoints '(10 . 100)
  :armour 90
  :speed 110
  :xp 900
  :abilities '(<pass-wall> <invisible> <max-hitpoints>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold>)
  :alertness 5
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5))
             (<hit> :type <hurt> :damage (3 . 6)))
  :treasures '((<drop-chance> 9/10))) 

(define-monster-kind "monster-433" "lesser titan"
  :desc "It is a humanoid figure thirty feet tall that gives off an aura of power  and hate."
  :symbol #\P
  :colour #\y
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
  :attacks '((<hit> :type <confuse> :damage (6 . 6)) (<hit> :type <confuse> :damage (6 . 6))
             (<hit> :type <confuse> :damage (6 . 6)) (<hit> :type <confuse> :damage (6 . 6)))
  :treasures '((<drop> "4d2") (<drop> "2d2"))
  :special-abilities '((<summon> <monsters>) (<spell> <scare>) (<spell> <teleport-player>) (<spell> <heal>)
                       (<frequency> 1/3))) 

(define-monster-kind "monster-434" "9-headed hydra"
  :desc "A strange reptilian hybrid with nine smouldering heads."
  :symbol #\M
  :colour #\r
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
  :attacks '((<bite> :type <fire> :damage (3 . 6)) (<bite> :type <fire> :damage (3 . 6))
             (<bite> :type <fire> :damage (3 . 6)) (<bite> :type <fire> :damage (3 . 6)))
  :treasures '((<drop> "4d2") (<drop> "2d2") <only-drop-gold>)
  :special-abilities '((<breath> <fire>) (<spell> (<bolt> <fire>)) (<spell> <scare>) (<frequency> 1/4))) 

(define-monster-kind "monster-435" "enchantress"
  :desc "This elusive female spellcaster has a special affinity for dragons, whom  she rarely fights without."
  :symbol #\p
  :colour #\R
  :alignment '<evil>
  :depth 40
  :rarity 4
  :hitpoints '(52 . 10)
  :armour 60
  :speed 130
  :xp 2100
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 8)) (<hit> :type <hurt> :damage (2 . 6))
             (<hit> :type <hurt> :damage (2 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :sex '<female>
  :special-abilities '((<summon> <dragon>) (<spell> <blindness>) (<frequency> 1/2))) 

(define-monster-kind "monster-436" "archpriest"
  :desc "An evil priest, dressed all in black.  Deadly spells hit you at an  alarming rate as his black spiked mace rains down blow after blow on your  pitiful frame."
  :symbol #\p
  :colour #\G
  :alignment '<evil>
  :depth 40
  :rarity 2
  :hitpoints '(52 . 10)
  :armour 60
  :speed 120
  :xp 1800
  :abilities '(<bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 4))
             (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop> "2d2") (<drop-chance> 9/10) <only-drop-items>)
  :sex '<male>
  :special-abilities '((<summon> <undead>) (<summon> <monster>) (<spell> (<cause> 3)) (<spell> <confusion>)
                       (<spell> <paralysis>) (<spell> <blindness>) (<spell> <heal>) (<frequency> 1/2))) 

(define-monster-kind "monster-437" "sorcerer"
  :desc "A human figure in robes, he moves with magically improved speed, and his  hands are ablur with spell casting."
  :symbol #\p
  :colour #\v
  :alignment '<evil>
  :depth 40
  :rarity 2
  :hitpoints '(52 . 10)
  :armour 60
  :speed 130
  :xp 2150
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 8)) (<hit> :type <hurt> :damage (2 . 8))
             (<hit> :type <hurt> :damage (2 . 8)))
  :treasures '((<drop> "4d2") (<drop-chance> 9/10) <only-drop-items>)
  :sex '<male>
  :special-abilities '((<summon> <dragon>) (<summon> <undead>) (<summon> <monster>) (<spell> (<ball> <cold>))
                       (<spell> (<ball> <fire>)) (<spell> (<bolt> <acid>)) (<spell> <traps>) (<spell> (<cause> 3))
                       (<spell> <confusion>) (<spell> <blindness>) (<spell> <teleport-player>) (<spell> <blink>)
                       (<frequency> 1/2))) 

(define-monster-kind "monster-438" "xaren"
  :desc "It is a tougher relative of the Xorn.  Its hide glitters with metal ores."
  :symbol #\X
  :colour #\s
  :depth 40
  :rarity 1
  :hitpoints '(32 . 10)
  :armour 80
  :speed 120
  :xp 1200
  :abilities '(<pass-wall> <overrun-items> <cold-blood> <empty-mind> <max-hitpoints>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4))
             (<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4)))) 

(define-monster-kind "monster-439" "giant roc"
  :desc "A vast legendary bird, its iron talons rake the most impenetrable of  surfaces and its screech echoes through the many winding dungeon corridors."
  :symbol #\B
  :colour #\u
  :type '(<animal>)
  :depth 40
  :rarity 3
  :hitpoints '(80 . 13)
  :armour 70
  :speed 110
  :xp 1000
  :abilities '(<bash-door>)
  :immunities '(<lightning>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <elec> :damage (12 . 12)) (<crush> :type <hurt> :damage (8 . 12))
             (<crush> :type <hurt> :damage (8 . 12)))) 


(define-monster-kind "monster-441" "minotaur"
  :desc "It is a cross between a human and a bull."
  :symbol #\H
  :colour #\s
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
  :attacks '((<butt> :type <hurt> :damage (2 . 6)) (<butt> :type <hurt> :damage (2 . 6))
             (<butt> :type <hurt> :damage (4 . 6)) (<butt> :type <hurt> :damage (4 . 6)))) 


(define-monster-kind "monster-443" "death drake"
  :desc "It is a dragon-like form wrapped in darkness.  You cannot make out its  true form but you sense its evil."
  :symbol #\D
  :colour #\G
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
  :immunities '(<sleep> <confuse> <cold>)
  :alertness 30
  :vision 25
  :attacks '((<bite> :type <exp_80> :damage (1 . 6)) (<bite> :type <exp_80> :damage (1 . 6))
             (<claw> :type <hurt> :damage (1 . 10)) (<claw> :type <hurt> :damage (1 . 10)))
  :treasures '((<drop> "4d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <nether>) (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>) (<frequency> 1/6))) 

(define-monster-kind "monster-444" "ancient red dragon"
  :desc "A huge draconic form.  Wisps of smoke steam from its nostrils and the  extreme heat surrounding it makes you gasp for breath."
  :symbol #\D
  :colour #\r
  :alignment '<evil>
  :type '(<dragon>)
  :depth 40
  :rarity 1
  :hitpoints '(10 . 100)
  :armour 100
  :speed 120
  :xp 2750
  :abilities '(<push-others> <powerful-breath> <bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 14)) (<claw> :type <hurt> :damage (1 . 10))
             (<claw> :type <hurt> :damage (1 . 10)))
  :treasures '((<drop> "4d2") (<drop> "3d2"))
  :special-abilities '((<breath> <fire>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<frequency> 1/6))) 

(define-monster-kind "monster-445" "ancient gold dragon"
  :desc "A huge draconic form wreathed in a nimbus of light."
  :symbol #\D
  :colour #\y
  :type '(<dragon>)
  :depth 40
  :rarity 2
  :hitpoints '(15 . 100)
  :armour 100
  :speed 120
  :xp 4000
  :abilities '(<push-others> <powerful-breath> <bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 200
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 14)) (<claw> :type <hurt> :damage (1 . 10))
             (<claw> :type <hurt> :damage (1 . 10)))
  :treasures '((<drop> "4d2") (<drop> "3d2"))
  :special-abilities '((<breath> <sound>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<frequency> 1/6))) 

(define-monster-kind "monster-446" "great crystal drake"
  :desc "A huge crystalline dragon.  Its claws could cut you to shreds and its  teeth are razor sharp.  Strange colours ripple through it as it moves in  the light."
  :symbol #\D
  :colour #\v
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
  :immunities '(<sleep> <confuse> <cold>)
  :alertness 30
  :vision 25
  :attacks '((<bite> :type <hurt> :damage (2 . 12)) (<claw> :type <hurt> :damage (1 . 9))
             (<claw> :type <hurt> :damage (1 . 9)))
  :treasures '((<drop> "4d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <shards>) (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>) (<frequency> 1/6))) 

(define-monster-kind "monster-447" "vrock"
  :desc "It is a demon with a long neck and raking claws."
  :symbol #\U
  :colour #\s
  :alignment '<evil>
  :type '(<demon>)
  :depth 40
  :rarity 2
  :hitpoints '(40 . 10)
  :armour 50
  :speed 110
  :xp 1000
  :abilities '(<powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<crush> :type <hurt> :damage (8 . 12)) (<crush> :type <hurt> :damage (8 . 12))
             (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop-chance> 3/5) <only-drop-items>)
  :special-abilities '((<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/8))) 

(define-monster-kind "monster-448" "death quasit"
  :desc "It is a demon of small stature, but its armoured frame moves with  lightning speed and its powers make it a tornado of death and destruction."
  :symbol #\u
  :colour #\D
  :alignment '<evil>
  :type '(<demon>)
  :depth 40
  :rarity 3
  :hitpoints '(44 . 10)
  :armour 80
  :speed 130
  :xp 1000
  :abilities '(<pass-wall> <invisible> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <fire>)
  :alertness 0
  :vision 20
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<claw> :type <hurt> :damage (3 . 3))
             (<bite> :type <lose_dex> :damage (3 . 6)))
  :treasures '((<drop> "4d2") (<drop> "2d2") (<drop-chance> 9/10) <only-drop-items>)
  :special-abilities '((<summon> <demon>) (<spell> <forget>) (<spell> (<cause> 3)) (<spell> <scare>)
                       (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/10))) 



(define-monster-kind "monster-450" "dark elven sorceror"
  :desc "A dark elven figure, dressed in deepest black.  Power seems to crackle  from his slender frame."
  :symbol #\h
  :colour #\v
  :alignment '<evil>
  :depth 41
  :rarity 2
  :hitpoints '(80 . 10)
  :armour 70
  :speed 130
  :xp 3000
  :abilities '(<bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (2 . 8)) (<hit> :type <hurt> :damage (2 . 8))
             (<hit> :type <hurt> :damage (2 . 8)))
  :treasures '((<drop> "4d2") (<drop-chance> 9/10) <only-drop-items>)
  :sex '<male>
  :special-abilities '((<summon> <demon>) (<summon> <undead>) (<summon> <monster>) (<spell> (<ball> <cold>))
                       (<spell> (<ball> <fire>)) (<spell> (<bolt> <acid>)) (<spell> <darkness>) (<spell> (<cause> 3))
                       (<spell> <confusion>) (<spell> <blindness>) (<spell> <teleport-player>) (<spell> <blink>)
                       (<spell> <heal>) (<frequency> 1/2))) 

(define-monster-kind "monster-451" "master lich"
  :desc "A skeletal form wrapped in robes.  Powerful magic crackles along its bony  fingers."
  :symbol #\L
  :colour #\r
  :alignment '<evil>
  :type '(<undead>)
  :depth 41
  :rarity 2
  :hitpoints '(18 . 100)
  :armour 80
  :speed 120
  :xp 10000
  :abilities '(<bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :alertness 50
  :vision 20
  :attacks '((<touch> :type <lose_dex> :damage (2 . 12)) (<touch> :type <lose_dex> :damage (2 . 12))
             (<touch> :type <un_power> :damage nil) (<touch> :type <exp_80> :damage nil))
  :treasures '((<drop> "4d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<summon> <undead>) (<spell> <brain-smash>) (<spell> <drain-mana>) (<spell> (<cause> 4))
                       (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <confusion>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<spell> <teleport-player>) (<spell> <blink>) (<frequency> 1/3))) 

(define-monster-kind "monster-452" "hezrou"
  :desc "It is a demon of lizard form with cruel-looking jaws."
  :symbol #\U
  :colour #\v
  :alignment '<evil>
  :type '(<demon>)
  :depth 41
  :rarity 3
  :hitpoints '(52 . 10)
  :armour 40
  :speed 110
  :xp 1500
  :abilities '(<powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop> "2d2") <only-drop-items>)
  :special-abilities '((<summon> <demon>) (<spell> (<bolt> <fire>)) (<frequency> 1/9))) 


(define-monster-kind "solar" "solar"
  :desc "Never a more heavenly being have you seen.  The very holiness of its  presence makes you deeply respect it.  Few creatures can match the powers  of a Solar; fewer still live to tell the tale after attacking one."
  :symbol #\A
  :colour #\y
  :depth 41
  :rarity 6
  :hitpoints '(100 . 35)
  :armour 140
  :speed 130
  :xp 15000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <pick-up-item> <smart> <initial-sleeper>)
  :immunities '(<poison> <lightning> <cold> <fire> <acid>)
  :alertness 255
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (8 . 6)) (<hit> :type <hurt> :damage (8 . 6))
             (<gaze> :type <terrify> :damage (4 . 4)) (<gaze> :type <terrify> :damage (4 . 4)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :special-abilities '((<summon> <angel>) (<spell> (<bolt> <mana>)) (<spell> (<cause> 4)) (<spell> (<cause> 2))
                       (<spell> <scare>) (<spell> <blindness>) (<spell> <teleport-player>) (<frequency> 1/3))) 

(define-monster-kind "monster-456" "glabrezu"
  :desc "It is demon with arms and pincers, its form a true mockery of life."
  :symbol #\U
  :colour #\o
  :alignment '<evil>
  :type '(<demon>)
  :depth 41
  :rarity 2
  :hitpoints '(60 . 10)
  :armour 40
  :speed 110
  :xp 1750
  :abilities '(<powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop-chance> 9/10) <only-drop-items>)
  :special-abilities '((<summon> <demon>) (<spell> (<bolt> <fire>)) (<frequency> 1/9))) 


(define-monster-kind "nalfeshnee" "nalfeshnee"
  :desc "It is a large demon with the head of a giant boar.  Flames run up and down  its length."
  :symbol #\U
  :colour #\r
  :alignment '<evil>
  :type '(<demon>)
  :depth 42
  :rarity 2
  :hitpoints '(67 . 10)
  :armour 50
  :speed 110
  :xp 2000
  :abilities '(<powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4))
             (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :special-abilities '((<summon> <demon>) (<breath> <fire>) (<spell> <confusion>) (<spell> <blindness>)
                       (<frequency> 1/9))) 

(define-monster-kind "undead-beholder" "undead beholder"
  :desc "A disembodied eye, floating in the air.  Black nether storms rage around  its bloodshot pupil and light seems to bend as it sucks its power from the  very air around it.  Your soul chills as it drains your vitality for its  evil enchantments."
  :symbol #\e
  :colour #\u
  :alignment '<evil>
  :type '(<undead>)
  :depth 42
  :rarity 4
  :hitpoints '(27 . 100)
  :armour 100
  :speed 120
  :xp 4000
  :abilities '(<bash-door> <cold-blood> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 10
  :vision 30
  :attacks '((<gaze> :type <un_power> :damage (2 . 6)) (<gaze> :type <lose_int> :damage (2 . 6))
             (<gaze> :type <paralyze> :damage nil) (<gaze> :type <exp_40> :damage nil))
  :special-abilities '((<summon> <undead>) (<spell> (<bolt> <mana>)) (<spell> <forget>) (<spell> <brain-smash>)
                       (<spell> <mind-blast>) (<spell> <drain-mana>) (<spell> (<cause> 4)) (<spell> <slow>)
                       (<frequency> 1/2))) 




(define-monster-kind "anc-mh-dragon" "ancient multi-hued dragon"
  :desc "A huge draconic form.  Many colours ripple down its massive frame.  Few  live to see another."
  :symbol #\D
  :colour #\v
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
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (3 . 12)) (<claw> :type <hurt> :damage (1 . 12))
             (<claw> :type <hurt> :damage (1 . 12)))
  :treasures '((<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <poison>) (<breath> <lightning>) (<breath> <cold>) (<breath> <fire>) (<breath> <acid>)
                       (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/5))) 

(define-monster-kind "eth-dragon" "ethereal dragon"
  :desc "A huge dragon emanating from the elemental plains, the ethereal dragon is  a master of light and dark.  Its form disappears from sight as it cloaks  itself in unearthly shadows."
  :symbol #\D
  :colour #\o
  :type '(<dragon>)
  :depth 43
  :rarity 2
  :hitpoints '(21 . 100)
  :armour 100
  :speed 120
  :xp 11000
  :abilities '(<push-others> <powerful-breath> <pass-wall> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 15
  :vision 25
  :attacks '((<bite> :type <hurt> :damage (3 . 12)) (<claw> :type <hurt> :damage (1 . 12))
             (<claw> :type <hurt> :damage (1 . 12)))
  :treasures '((<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <confusion>) (<breath> <darkness>) (<breath> <light>) (<spell> <confusion>)
                       (<spell> <blindness>) (<frequency> 1/5))) 


(define-monster-kind "marilith" "marilith"
  :desc "She is a demon of female form with many arms, each bearing deadly weapons."
  :symbol #\U
  :colour #\y
  :alignment '<evil>
  :type '(<demon>)
  :depth 43
  :rarity 2
  :hitpoints '(12 . 100)
  :armour 75
  :speed 120
  :xp 5000
  :abilities '(<powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 6)) (<hit> :type <hurt> :damage (3 . 6))
             (<hit> :type <hurt> :damage (3 . 6)) (<hit> :type <hurt> :damage (3 . 6)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :sex '<female>
  :special-abilities '((<summon> <demon>) (<spell> (<cause> 2)) (<spell> <blindness>) (<frequency> 1/9))) 


(define-monster-kind "monster-467" "lesser balrog"
  :desc "It is a massive humanoid demon wreathed in flames."
  :symbol #\U
  :colour #\R
  :alignment '<evil>
  :type '(<demon>)
  :depth 44
  :rarity 3
  :hitpoints '(18 . 100)
  :armour 50
  :speed 120
  :xp 8000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <fire> :damage (2 . 6))
             (<hit> :type <hurt> :damage (4 . 6)) (<hit> :type <fire> :damage (2 . 6)))
  :treasures '((<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :special-abilities '((<summon> <demon>) (<breath> <fire>) (<spell> <confusion>) (<spell> <blindness>)
                       (<frequency> 1/4))) 


(define-monster-kind "hydra-11" "11-headed hydra"
  :desc "A strange reptilian hybrid with eleven smouldering heads."
  :symbol #\M
  :colour #\R
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
  :attacks '((<bite> :type <fire> :damage (3 . 12)) (<bite> :type <fire> :damage (3 . 12))
             (<bite> :type <fire> :damage (3 . 12)) (<bite> :type <fire> :damage (3 . 12)))
  :treasures '((<drop> "4d2") (<drop> "2d2") <only-drop-gold>)
  :special-abilities '((<breath> <fire>) (<spell> (<ball> <fire>)) (<spell> (<bolt> <plasma>))
                       (<spell> (<bolt> <fire>)) (<spell> <scare>) (<frequency> 1/4))) 


(define-monster-kind "monster-470" "patriarch"
  :desc "A dark priest of the highest order.  Powerful and evil, beware his many  spells."
  :symbol #\p
  :colour #\G
  :alignment '<evil>
  :depth 44
  :rarity 2
  :hitpoints '(80 . 10)
  :armour 60
  :speed 120
  :xp 5000
  :abilities '(<bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 4))
             (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '((<drop> "4d2") (<drop-chance> 9/10) <only-drop-items>)
  :sex '<male>
  :special-abilities '((<summon> <undead>) (<summon> <monsters>) (<spell> <brain-smash>) (<spell> (<cause> 4))
                       (<spell> <paralysis>) (<spell> <blindness>) (<spell> <heal>) (<frequency> 1/2))) 

(define-monster-kind "dreadmaster" "dreadmaster"
  :desc "It is an unlife of power almost unequaled.  An affront to existence, its  very touch abuses and disrupts the flow of life, and its unearthly limbs,  of purest black, crush rock and flesh with ease."
  :symbol #\G
  :colour #\y
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
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <lose-str> :damage (3 . 4)) (<hit> :type <lose-str> :damage (3 . 4))
             (<hit> :type <hurt> :damage (6 . 6)) (<hit> :type <hurt> :damage (6 . 6)))
  :treasures '((<drop> "4d2") (<drop> "1d2") <only-drop-items>)
  :special-abilities '((<summon> <undead>) (<spell> (<bolt> <nether>)) (<spell> <drain-mana>) (<spell> (<cause> 4))
                       (<spell> <confusion>) (<spell> <paralysis>) (<spell> <blindness>) (<spell> <teleport-level>)
                       (<frequency> 1/9))) 

(define-monster-kind "monster-472" "drolem"
  :desc "A constructed dragon, the drolem has massive strength.  Powerful spells  weaved during its creation make it a fearsome adversary.  Its eyes show  little intelligence, but it has been instructed to destroy all it meets."
  :symbol #\g
  :colour #\g
  :type '(<dragon>)
  :depth 44
  :rarity 3
  :hitpoints '(30 . 100)
  :armour 130
  :speed 120
  :xp 12000
  :abilities '(<bash-door> <open-door> <cold-blood> <empty-mind> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :alertness 30
  :vision 25
  :attacks '((<claw> :type <poison> :damage (3 . 3)) (<claw> :type <poison> :damage (3 . 3))
             (<bite> :type <hurt> :damage (5 . 8)) (<bite> :type <hurt> :damage (5 . 8)))
  :special-abilities '((<breath> <poison>) (<arrow> 3) (<spell> <confusion>) (<spell> <slow>) (<spell> <blindness>)
                       (<frequency> 1/6))) 



(define-monster-kind "monster-477" "greater titan"
  :desc "A forty foot tall humanoid that shakes the ground as it walks.  The power  radiating from its frame shakes your courage, its hatred inspired by your  defiance."
  :symbol #\P
  :colour #\o
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
  :attacks '((<hit> :type <confuse> :damage (12 . 12)) (<hit> :type <confuse> :damage (12 . 12))
             (<hit> :type <confuse> :damage (12 . 12)) (<hit> :type <confuse> :damage (12 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :special-abilities '((<summon> <monsters>) (<spell> <teleport-player>) (<spell> <heal>) (<frequency> 1/3))) 

(define-monster-kind "monster-478" "dracolisk"
  :desc "A mixture of dragon and basilisk, the dracolisk stares at you with deep  piercing eyes, its evil breath burning the ground where it stands."
  :symbol #\D
  :colour #\G
  :alignment '<evil>
  :type '(<dragon> <animal>)
  :depth 46
  :rarity 2
  :hitpoints '(35 . 100)
  :armour 120
  :speed 120
  :xp 14000
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire> <acid>)
  :alertness 30
  :vision 25
  :attacks '((<gaze> :type <paralyze> :damage nil) (<bite> :type <hurt> :damage (5 . 8))
             (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :special-abilities '((<breath> <nether>) (<breath> <fire>) (<spell> <scare>) (<spell> <paralysis>) (<frequency> 1/6))) 

(define-monster-kind "monster-479" "death mold"
  :desc "It is the epitome of all that is evil, in a mold.  Its lifeless form draws  power from sucking the souls of those that approach it, a nimbus of pure  evil surrounds it.  Luckily for you, it can't move."
  :symbol #\m
  :colour #\D
  :alignment '<evil>
  :depth 47
  :rarity 1
  :hitpoints '(100 . 20)
  :armour 60
  :speed 140
  :xp 1000
  :abilities '(<never-move> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 0
  :vision 200
  :attacks '((<hit> :type <exp_80> :damage (5 . 5)) (<hit> :type <un_bonus> :damage (7 . 7))
             (<hit> :type <un_bonus> :damage (7 . 7)) (<hit> :type <un_bonus> :damage (7 . 7)))) 


(define-monster-kind "master-mystic" "master mystic"
  :desc "A lord of all that is natural, skilled in the mystic ways.  He is a master  of martial arts and is at one with nature, able to summon help from the  wild if need be."
  :symbol #\p
  :colour #\o
  :depth 50
  :rarity 3
  :hitpoints '(11 . 100)
  :armour 60
  :speed 130
  :xp 6000
  :abilities '(<bash-door> <open-door> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 5
  :vision 30
  :attacks '((<hit> :type <paralyze> :damage (15 . 1)) (<hit> :type <poison> :damage (20 . 1))
             (<kick> :type <hurt> :damage (10 . 2)) (<kick> :type <hurt> :damage (10 . 2)))
  :treasures '((<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :sex '<male>
  :special-abilities '((<summon> <spider>) (<spell> <heal>) (<frequency> 1/3))) 


(define-monster-kind "nightwing" "nightwing"
  :desc "Everywhere colours seem paler and the air chiller.  At the centre of the  cold stands a mighty figure.  Its wings envelop you in the chill of death  as the nightwing reaches out to draw you into oblivion.  Your muscles sag  and your mind loses all will to fight as you stand in awe of this mighty  being."
  :symbol #\W
  :colour #\D
  :alignment '<evil>
  :type '(<undead>)
  :depth 50
  :rarity 4
  :hitpoints '(60 . 30)
  :armour 120
  :speed 120
  :xp 6000
  :abilities '(<bash-door> <open-door> <cold-blood> <smart> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <un-bonus> :damage (6 . 8)) (<hit> :type <un_bonus> :damage (6 . 8))
             (<touch> :type <poison> :damage (3 . 5)) (<touch> :type <poison> :damage (3 . 5)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<summon> <undead>) (<spell> (<ball> <nether>)) (<spell> (<bolt> <nether>))
                       (<spell> (<bolt> <mana>)) (<spell> <brain-smash>) (<spell> (<cause> 4)) (<spell> <scare>)
                       (<spell> <blindness>) (<frequency> 1/4))) 

(define-monster-kind "monster-485" "nether hound"
  :desc "You feel a soul-tearing chill upon viewing this beast, a ghostly form of  darkness in the shape of a large dog."
  :symbol #\Z
  :colour #\G
  :type '(<animal>)
  :depth 51
  :rarity 2
  :hitpoints '(60 . 10)
  :armour 100
  :speed 120
  :xp 5000
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<bite> :type <hurt> :damage (2 . 12))
             (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <nether>) (<frequency> 1/5))) 

(define-monster-kind "monster-486" "time hound"
  :desc "You get a terrible sense of deja vu, or is it a premonition?  All at once  you see a little puppy and a toothless old dog.  Perhaps you should give  up and go to bed."
  :symbol #\Z
  :colour #\B
  :type '(<animal>)
  :depth 51
  :rarity 4
  :hitpoints '(60 . 10)
  :armour 100
  :speed 130
  :xp 5000
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<bite> :type <hurt> :damage (2 . 12))
             (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <time>) (<frequency> 1/8))) 

(define-monster-kind "monster-487" "plasma hound"
  :desc "The very air warps as pure elemental energy stalks towards you in the  shape of a giant hound.  Your hair stands on end and your palms itch as  you sense trouble."
  :symbol #\Z
  :colour #\r
  :type '(<animal>)
  :depth 51
  :rarity 2
  :hitpoints '(60 . 10)
  :armour 100
  :speed 120
  :xp 5000
  :abilities '(<bash-door> <open-door> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<bite> :type <hurt> :damage (2 . 12))
             (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <plasma>) (<frequency> 1/5))) 

(define-monster-kind "monster-488" "demonic quylthulg"
  :desc "A pile of pulsing flesh that glows with an inner hellish fire.  The world  itself seems to cry out against it."
  :symbol #\Q
  :colour #\r
  :alignment '<evil>
  :type '(<animal>)
  :depth 51
  :rarity 1
  :hitpoints '(48 . 10)
  :armour 1
  :speed 120
  :xp 3000
  :abilities '(<empty-mind> <invisible> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 20
  :special-abilities '((<summon> <demon>) (<spell> <teleport>) (<spell> <blink>) (<frequency> 1/2))) 

(define-monster-kind "monster-489" "great storm wyrm"
  :desc "A vast dragon of power.  Storms and lightning crash around its titanic  form.  Deep blue scales reflect the flashes and highlight the creature's  great muscles.  It regards you with contempt."
  :symbol #\D
  :colour #\b
  :alignment '<evil>
  :type '(<dragon>)
  :depth 51
  :rarity 2
  :hitpoints '(30 . 100)
  :armour 150
  :speed 120
  :xp 17000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <lightning>)
  :alertness 80
  :vision 30
  :attacks '((<bite> :type <hurt> :damage (4 . 14)) (<claw> :type <hurt> :damage (1 . 12))
             (<claw> :type <hurt> :damage (1 . 12)) (<claw> :type <hurt> :damage (1 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :special-abilities '((<breath> <lightning>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<frequency> 1/6))) 


(define-monster-kind "monster-493" "grand master mystic"
  :desc "He is one of the few true masters of the art, being extremely skillful in  all forms of unarmed combat and controlling the world's natural creatures  with disdainful ease."
  :symbol #\p
  :colour #\o
  :depth 53
  :rarity 3
  :hitpoints '(22 . 100)
  :armour 80
  :speed 130
  :xp 15000
  :abilities '(<bash-door> <open-door> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 5
  :vision 30
  :attacks '((<hit> :type <paralyze> :damage (15 . 1)) (<hit> :type <poison> :damage (20 . 1))
             (<kick> :type <hurt> :damage (10 . 2)) (<kick> :type <hurt> :damage (20 . 2)))
  :treasures '((<drop> "4d2") <only-drop-items>)
  :sex '<male>
  :special-abilities '((<summon> <hound>) (<summon> <spider>) (<spell> <mind-blast>) (<spell> <heal>)
                       (<frequency> 1/2))) 


(define-monster-kind "monster-495" "ethereal hound"
  :desc "A pale green hound.  Pulsing red lines and strange fluorescent light hints at internal organs best left to the imagination."
  :symbol #\Z
  :colour #\G
  :type '(<animal>)
  :depth 54
  :rarity 3
  :hitpoints '(60 . 15)
  :armour 100
  :speed 120
  :xp 5000
  :abilities '(<pass-wall> <invisible> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<bite> :type <hurt> :damage (2 . 12))
             (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <nether>) (<frequency> 1/5))) 

(define-monster-kind "monster-496" "great ice wyrm"
  :desc "An immense dragon capable of awesome destruction.  You have never felt  such extreme cold, or witnessed such an icy stare.  Begone quickly or feel  its wrath!"
  :symbol #\D
  :colour #\w
  :alignment '<evil>
  :type '(<dragon>)
  :depth 54
  :rarity 2
  :hitpoints '(30 . 100)
  :armour 170
  :speed 120
  :xp 20000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <cold>)
  :alertness 80
  :vision 30
  :attacks '((<bite> :type <hurt> :damage (4 . 14)) (<claw> :type <hurt> :damage (3 . 12))
             (<claw> :type <hurt> :damage (1 . 12)) (<claw> :type <hurt> :damage (1 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :special-abilities '((<breath> <cold>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<frequency> 1/6))) 


(define-monster-kind "monster-498" "nightcrawler"
  :desc "This intensely evil creature bears the form of a gargantuan black worm.   Its gaping maw is a void of blackness, acid drips from its steely hide.   It is like nothing you have ever seen before, and a terrible chill runs  down your spine as you face it."
  :symbol #\W
  :colour #\D
  :alignment '<evil>
  :type '(<undead>)
  :depth 54
  :rarity 4
  :hitpoints '(80 . 60)
  :armour 160
  :speed 120
  :xp 8000
  :abilities '(<bash-door> <open-door> <cold-blood> <smart> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold> <fire>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<bite> :type <acid> :damage (10 . 10)) (<bite> :type <acid> :damage (10 . 10))
             (<sting> :type <lose_con> :damage (8 . 8)) (<sting> :type <lose_con> :damage (8 . 8)))
  :treasures '(<drop-good> (<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :special-abilities '((<summon> <undead>) (<breath> <nether>) (<spell> (<ball> <nether>)) (<spell> (<bolt> <nether>))
                       (<spell> (<bolt> <mana>)) (<spell> <brain-smash>) (<spell> <scare>) (<spell> <blindness>)
                       (<frequency> 1/4))) 

(define-monster-kind "monster-499" "hand druj"
  :desc "A skeletal hand floating in the air, motionless except for its flexing  fingers."
  :symbol #\s
  :colour #\y
  :alignment '<evil>
  :type '(<undead>)
  :depth 55
  :rarity 4
  :hitpoints '(60 . 10)
  :armour 110
  :speed 130
  :xp 12000
  :abilities '(<cold-blood> <smart> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <cold>)
  :alertness 10
  :vision 20
  :special-abilities '((<spell> <darkness>) (<spell> <forget>) (<spell> (<cause> 3)) (<spell> <scare>)
                       (<spell> <confusion>) (<spell> <blindness>) (<spell> <teleport-away>) (<frequency> 1))) 


(define-monster-kind "monster-500" "eye druj"
  :desc "A bloodshot eyeball floating in the air, you'd be forgiven for assuming it  harmless."
  :symbol #\s
  :colour #\r
  :alignment '<evil>
  :type '(<undead>)
  :depth 55
  :rarity 4
  :hitpoints '(10 . 100)
  :armour 90
  :speed 130
  :xp 24000
  :abilities '(<cold-blood> <smart> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <cold> <fire>)
  :alertness 10
  :vision 20
  :attacks '((<gaze> :type <exp_80> :damage nil) (<gaze> :type <exp_80> :damage nil))
  :special-abilities '((<summon> <undead>) (<spell> (<ball> <nether>)) (<spell> (<bolt> <nether>))
                       (<spell> (<bolt> <mana>)) (<frequency> 1))) 

(define-monster-kind "monster-501" "skull druj"
  :desc "A glowing skull possessed by sorcerous power.  It need not move, but  merely blast you with mighty magic."
  :symbol #\s
  :colour #\o
  :alignment '<evil>
  :type '(<undead>)
  :depth 55
  :rarity 4
  :hitpoints '(14 . 100)
  :armour 120
  :speed 130
  :xp 25000
  :abilities '(<cold-blood> <smart> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <cold> <fire>)
  :alertness 10
  :vision 20
  :attacks '((<bite> :type <lose_wis> :damage (4 . 4)) (<bite> :type <lose_int> :damage (4 . 4))
             (<bite> :type <paralyze> :damage (4 . 4)) (<bite> :type <exp_80> :damage (4 . 4)))
  :special-abilities '((<summon> <undead>) (<spell> (<ball> <water>)) (<spell> (<bolt> <nether>))
                       (<spell> (<bolt> <plasma>)) (<spell> <traps>) (<spell> <brain-smash>) (<spell> <mind-blast>)
                       (<spell> (<cause> 4)) (<spell> <slow>) (<frequency> 1))) 

(define-monster-kind "monster-502" "chaos vortex"
  :desc "Void, nothingness, spinning destructively."
  :symbol #\v
  :colour #\v
  :depth 55
  :rarity 1
  :hitpoints '(32 . 20)
  :armour 80
  :speed 140
  :xp 4000
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/4) (<random-mover> 1/2) <never-attack>
               <initial-sleeper> <colour-changing>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 100
  :special-abilities '((<breath> <chaos>) (<frequency> 1/6))) 

(define-monster-kind "monster-503" "aether vortex"
  :desc "An awesome vortex of pure magic, power radiates from its frame."
  :symbol #\v
  :colour #\v
  :depth 55
  :rarity 2
  :hitpoints '(32 . 20)
  :armour 40
  :speed 130
  :xp 4500
  :abilities '(<powerful-breath> <bash-door> <empty-mind> (<random-mover> 1/4) (<random-mover> 1/2) <initial-sleeper>
               <colour-changing>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 0
  :vision 100
  :attacks '((<engulf> :type <cold> :damage (3 . 3)) (<engulf> :type <acid> :damage (3 . 3))
             (<engulf> :type <fire> :damage (3 . 3)) (<engulf> :type <elec> :damage (5 . 5)))
  :special-abilities '((<breath> <nexus>) (<breath> <plasma>) (<breath> <gravity>) (<breath> <time>)
                       (<breath> <inertia>) (<breath> <force>) (<breath> <nether>) (<breath> <shards>)
                       (<breath> <chaos>) (<breath> <confusion>) (<breath> <sound>) (<breath> <darkness>)
                       (<breath> <light>) (<breath> <poison>) (<breath> <lightning>) (<breath> <cold>)
                       (<breath> <fire>) (<breath> <acid>) (<frequency> 1/6))) 


(define-monster-kind "monster-506" "great hell wyrm"
  :desc "A vast dragon of immense power.  Fire leaps continuously from its huge  form.  The air around it scalds you.  Its slightest glance burns you, and  you truly realize how insignificant you are."
  :symbol #\D
  :colour #\r
  :alignment '<evil>
  :type '(<dragon>)
  :depth 55
  :rarity 2
  :hitpoints '(54 . 100)
  :armour 170
  :speed 120
  :xp 23000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire>)
  :alertness 40
  :vision 40
  :attacks '((<bite> :type <hurt> :damage (4 . 14)) (<claw> :type <hurt> :damage (3 . 12))
             (<claw> :type <hurt> :damage (1 . 12)) (<claw> :type <hurt> :damage (1 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <fire>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<frequency> 1/6))) 

(define-monster-kind "monster-507" "draconic quylthulg"
  :desc "It looks like it was once a dragon corpse, now deeply infected with  magical bacteria that make it pulse in a foul and degrading way."
  :symbol #\Q
  :colour #\g
  :alignment '<evil>
  :type '(<animal>)
  :depth 55
  :rarity 3
  :hitpoints '(72 . 10)
  :armour 1
  :speed 120
  :xp 5500
  :abilities '(<empty-mind> <invisible> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 20
  :special-abilities '((<summon> <dragon>) (<spell> <teleport>) (<spell> <blink>) (<frequency> 1/2))) 


(define-monster-kind "monster-512" "nightwalker"
  :desc "A huge giant garbed in black, more massive than a titan and stronger than  a dragon.  With terrible blows, it breaks your armour from your back,  leaving you defenseless against its evil wrath.  It can smell your fear,  and you in turn smell the awful stench of death as this ghastly figure  strides towards you menacingly."
  :symbol #\W
  :colour #\D
  :alignment '<evil>
  :type '(<undead>)
  :depth 59
  :rarity 4
  :hitpoints '(50 . 65)
  :armour 175
  :speed 130
  :xp 15000
  :abilities '(<bash-door> <open-door> <cold-blood> <smart> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <un_bonus> :damage (7 . 7)) (<hit> :type <un_bonus> :damage (7 . 7))
             (<hit> :type <un_bonus> :damage (10 . 10)) (<hit> :type <un_bonus> :damage (10 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :special-abilities '((<summon> <undead>) (<spell> (<ball> <nether>)) (<spell> (<bolt> <nether>))
                       (<spell> (<bolt> <mana>)) (<spell> <brain-smash>) (<spell> <scare>) (<spell> <blindness>)
                       (<frequency> 1/4))) 


(define-monster-kind "monster-515" "dreadlord"
  :desc "It is a massive form of animated death, its colour deeper than black.  It  drinks in light, and space around it is twisted and torn by the weight of  its evil.  It is unlife and it knows nothing but the stealing of souls and  the stench of death.  Flee its hunger!"
  :symbol #\G
  :colour #\r
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
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <lose-str> :damage (4 . 6)) (<hit> :type <lose-str> :damage (4 . 6))
             (<hit> :type <hurt> :damage (6 . 6)) (<hit> :type <hurt> :damage (6 . 6)))
  :treasures '((<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<summon> <undead>) (<spell> (<ball> <nether>)) (<spell> <drain-mana>) (<spell> <confusion>)
                       (<spell> <paralysis>) (<spell> <blindness>) (<frequency> 1/4))) 


(define-monster-kind "monster-517" "chaos beetle"
  :desc "With biting jaws and catching claws, this immense beetle is like death incarnate chasing behind you!"
  :symbol #\K
  :colour #\v
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

(define-monster-kind "monster-518" "chaos hound"
  :desc "A constantly changing canine form, this hound rushes towards you as if  expecting mayhem and chaos ahead.  It appears to have an almost kamikaze  relish for combat.  You suspect all may not be as it seems."
  :symbol #\Z
  :colour #\v
  :type '(<animal>)
  :depth 65
  :rarity 1
  :hitpoints '(60 . 30)
  :armour 100
  :speed 120
  :xp 10000
  :abilities '(<bash-door> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confuse>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<bite> :type <hurt> :damage (2 . 12))
             (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <chaos>) (<frequency> 1/5))) 

(define-monster-kind "monster-519" "great wyrm of chaos"
  :desc "A massive dragon of changing form.  As you watch, it appears first fair  and then foul.  Its body is twisted by chaotic forces as it strives to  stay real.  Its very existence distorts the universe around it."
  :symbol #\D
  :colour #\v
  :alignment '<evil>
  :type '(<dragon>)
  :depth 67
  :rarity 2
  :hitpoints '(45 . 100)
  :armour 170
  :speed 120
  :xp 29000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confuse>)
  :alertness 20
  :vision 40
  :attacks '((<bite> :type <hurt> :damage (8 . 14)) (<claw> :type <hurt> :damage (6 . 12))
             (<claw> :type <hurt> :damage (5 . 12)) (<claw> :type <hurt> :damage (5 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<summon> <dragon>) (<breath> <disenchant>) (<breath> <chaos>) (<spell> <scare>)
                       (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/3))) 


(define-monster-kind "monster-520" "great wyrm of law"
  :desc "A massive dragon of powerful intellect.  It seeks to dominate the universe  and despises all other life.  It sees all who do not obey it as mere  insects to be crushed underfoot."
  :symbol #\D
  :colour #\B
  :type '(<dragon>)
  :depth 67
  :rarity 2
  :hitpoints '(45 . 100)
  :armour 170
  :speed 120
  :xp 29000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 255
  :vision 40
  :attacks '((<bite> :type <hurt> :damage (8 . 14)) (<claw> :type <hurt> :damage (6 . 12))
             (<claw> :type <hurt> :damage (5 . 12)) (<claw> :type <hurt> :damage (5 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<summon> <dragon>) (<breath> <shards>) (<breath> <sound>) (<spell> <scare>)
                       (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "monster-521" "great wyrm of balance"
  :desc "A massive dragon, one of the mightiest of dragonkind.  It is thousands of  years old and seeks to maintain the Cosmic Balance.  It sees you as an  upstart troublemaker without the wisdom to control your actions.  It will  destroy you."
  :symbol #\D
  :colour #\v
  :type '(<dragon>)
  :depth 67
  :rarity 4
  :hitpoints '(49 . 100)
  :armour 170
  :speed 120
  :xp 31000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confuse>)
  :alertness 255
  :vision 40
  :attacks '((<bite> :type <hurt> :damage (8 . 14)) (<claw> :type <hurt> :damage (6 . 12))
             (<claw> :type <hurt> :damage (5 . 12)) (<claw> :type <hurt> :damage (5 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<summon> <high-dragon>) (<summon> <dragon>) (<breath> <disenchant>) (<breath> <shards>)
                       (<breath> <chaos>) (<breath> <sound>) (<spell> <scare>) (<spell> <confusion>)
                       (<spell> <blindness>) (<frequency> 1/3))) 


(define-monster-kind "black-reaver" "black reaver"
  :desc "A humanoid form,  black as night, advancing steadily and unstoppably.   Flee!"
  :symbol #\L
  :colour #\D
  :alignment '<evil>
  :type '(<undead>)
  :depth 71
  :rarity 3
  :hitpoints '(35 . 100)
  :armour 170
  :speed 120
  :xp 23000
  :abilities '(<destroy-wall> <bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :alertness 50
  :vision 20
  :attacks '((<hit> :type <lose-str> :damage (4 . 6)) (<hit> :type <lose-str> :damage (4 . 6))
             (<hit> :type <un_bonus> :damage (6 . 8)) (<hit> :type <un_bonus> :damage (6 . 8)))
  :treasures '(<drop-good> (<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :special-abilities '((<summon> <undead>) (<spell> (<ball> <nether>)) (<spell> (<ball> <mana>))
                       (<spell> <brain-smash>) (<spell> <drain-mana>) (<spell> (<cause> 4)) (<spell> (<cause> 3))
                       (<spell> <confusion>) (<spell> <paralysis>) (<spell> <blindness>) (<spell> <teleport-player>)
                       (<frequency> 1/3))) 

(define-monster-kind "monster-525" "master quylthulg"
  :desc "A pulsating mound of flesh, shining with silver pulses of throbbing light."
  :symbol #\Q
  :colour #\B
  :alignment '<evil>
  :type '(<animal>)
  :depth 71
  :rarity 3
  :hitpoints '(20 . 100)
  :armour 1
  :speed 120
  :xp 12000
  :abilities '(<empty-mind> <invisible> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 20
  :special-abilities '((<summon> <high-demon>) (<summon> <high-dragon>) (<summon> <high-undead>) (<summon> <dragon>)
                       (<summon> <undead>) (<summon> <monsters>) (<summon> <monster>) (<frequency> 1/2))) 

(define-monster-kind "monster-526" "greater draconic quylthulg"
  :desc "A massive mound of scaled flesh, throbbing and pulsating with multi-hued  light."
  :symbol #\Q
  :colour #\G
  :alignment '<evil>
  :type '(<animal>)
  :depth 71
  :rarity 3
  :hitpoints '(15 . 100)
  :armour 1
  :speed 120
  :xp 10500
  :abilities '(<empty-mind> <invisible> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 20
  :special-abilities '((<summon> <high-dragon>) (<spell> <teleport-player>) (<spell> <blink>) (<frequency> 1/2))) 

(define-monster-kind "monster-527" "greater rotting quylthulg"
  :desc "A massive pile of rotting flesh.  A disgusting stench fills the air as it  throbs and writhes."
  :symbol #\Q
  :colour #\U
  :alignment '<evil>
  :type '(<animal>)
  :depth 71
  :rarity 3
  :hitpoints '(15 . 100)
  :armour 1
  :speed 120
  :xp 10500
  :abilities '(<empty-mind> <invisible> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 20
  :special-abilities '((<summon> <high-undead>) (<spell> <teleport-player>) (<spell> <blink>) (<frequency> 1/2))) 




(define-monster-kind "aether-hound" "aether hound"
  :desc "A shifting, swirling form.  It seems to be all colours and sizes and  shapes, though the dominant form is that of a huge dog.  You feel very  uncertain all of a sudden."
  :symbol #\Z
  :colour #\v
  :type '(<animal>)
  :depth 75
  :rarity 2
  :hitpoints '(60 . 30)
  :armour 100
  :speed 120
  :xp 10000
  :abilities '(<bash-door> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 0
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (3 . 3)) (<bite> :type <hurt> :damage (2 . 12))
             (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :special-abilities '((<breath> <nexus>) (<breath> <plasma>) (<breath> <gravity>) (<breath> <time>)
                       (<breath> <inertia>) (<breath> <force>) (<breath> <disenchant>) (<breath> <nether>)
                       (<breath> <shards>) (<breath> <chaos>) (<breath> <confusion>) (<breath> <sound>)
                       (<breath> <darkness>) (<breath> <light>) (<breath> <poison>) (<breath> <lightning>)
                       (<breath> <cold>) (<breath> <fire>) (<breath> <acid>) (<frequency> 1/5))) 


(define-monster-kind "monster-537" "hell hound"
  :desc "It is a giant dog that glows with heat.  Flames pour from its nostrils."
  :symbol #\C
  :colour #\r
  :alignment '<evil>
  :type '(<animal>)
  :depth 83
  :rarity 4
  :hitpoints '(48 . 10)
  :armour 80
  :speed 120
  :xp 600
  :abilities '(<push-others> <bash-door> (<random-mover> 1/4) <max-hitpoints> <initial-sleeper>)
  :immunities '(<fire>)
  :alertness 30
  :vision 25
  :attacks '((<bite> :type <fire> :damage (3 . 12)) (<bite> :type <fire> :damage (3 . 12))
             (<bite> :type <fire> :damage (3 . 12)))
  :special-abilities '((<breath> <fire>) (<frequency> 1/5))) 


(define-monster-kind "dracolich" "dracolich"
  :desc "The skeletal form of a once-great dragon, enchanted by magic most  perilous.  Its animated form strikes with speed and drains life from its  prey to satisfy its hunger."
  :symbol #\D
  :colour #\G
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
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :alertness 30
  :vision 25
  :attacks '((<bite> :type <exp_80> :damage (1 . 6)) (<bite> :type <exp_80> :damage (1 . 6))
             (<claw> :type <hurt> :damage (1 . 12)) (<claw> :type <hurt> :damage (1 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :special-abilities '((<breath> <nether>) (<breath> <cold>) (<spell> <scare>) (<spell> <confusion>) (<frequency> 1/6))) 

(define-monster-kind "quasit" "quasit"
  :desc "The chaotic evil master's favourite pet."
  :symbol #\u
  :colour #\o
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
             (<bite> :type <lose_dex> :damage (1 . 6)))
  :treasures '((<drop> "1d2") <only-drop-items>)
  :special-abilities '((<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>) (<spell> <teleport-level>)
                       (<spell> <teleport-player>) (<spell> <teleport>) (<spell> <blink>) (<frequency> 1/10))) 


(define-monster-kind "black-ooze" "black ooze"
  :desc "It is a strangely moving puddle."
  :symbol #\j
  :colour #\D
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

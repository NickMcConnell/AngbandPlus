;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/uniques.lisp - unique monsters for vanilla variant
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

;;; === Note ===
;;; vanilla-specific treats
;;; :depth is translated to power-lvl slot
;;; for locations slot you get (depth . rarity)

(define-monster-kind "hobbit-maggott" "Farmer Maggot"
  :numeric-id  4
  :desc "He's lost his dogs.  He's had his mushrooms stolen.  He's not a happy hobbit!"
  :text-char #\h
  :text-attr #\w
  :x-attr (tile-file 14)
  :x-char (tile-number 1)
  :type '(<unique>)
  :depth 0
  :rarity 4
  :hitpoints '(35 . 10)
  :armour 10
  :speed 110
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<sleep> <confusion>)
  :alertness 3
  :vision 40
  :attacks '((<moan> :type nil :damage nil)
	     (<moan> :type nil :damage nil))
  :treasures '(<drop-good> (<drop-chance> 9/10) <only-drop-items>)
  :gender '<male>)

(define-monster-kind "dog-fang" "Fang"
  :title "Farmer Maggot's dog"
  :numeric-id  47
  :x-attr (tile-file 18)
  :x-char (tile-number 75)
  :desc "A rather vicious dog belonging to Farmer Maggot.  It thinks you are stealing mushrooms."
  :text-char #\C
  :text-attr #\U
  :type '(<animal> <unique>)
  :depth 2
  :rarity 1
  :hitpoints '(5 . 5)
  :armour 30
  :speed 120
  :xp 30
  :abilities '(<bash-door> (<random-mover> 1/4) <max-hitpoints>)
  :immunities '(<sleep> <confusion>)
  :alertness 0
  :vision 30
  :attacks '((<bite> :type <hurt> :damage (1 . 4))))

(define-monster-kind "dog-grip" "Grip"
  :title "Farmer Maggot's dog"
  :numeric-id  46
  :x-attr (tile-file 18)
  :x-char (tile-number 73)
  :desc "A rather vicious dog belonging to Farmer Maggot.  It thinks you are stealing mushrooms."
  :text-char #\C
  :text-attr #\U
  :type '(<animal> <unique>)
  :depth 2
  :rarity 1
  :hitpoints '(5 . 5)
  :armour 30
  :speed 120
  :xp 30
  :abilities '(<bash-door> (<random-mover> 1/4) <max-hitpoints>)
  :immunities '(<sleep> <confusion>)
  :alertness 0
  :vision 30
  :attacks '((<bite> :type <hurt> :damage (1 . 4)))) 

(define-monster-kind "smeagol" "Smeagol"
  :numeric-id  52
  :x-attr (tile-file 18)
  :x-char (tile-number 60)
  :desc "He's been sneaking, and he wants his 'precious.'"
  :text-char #\h
  :text-attr #\b
  :alignment '<evil>
  :type '(<unique>)
  :depth 3
  :rarity 2
  :hitpoints '(5 . 5)
  :armour 12
  :speed 130
  :xp 16
  :abilities '(<bash-door> <open-door> <pick-up-item> <invisible> (<random-mover> 1/4) (<random-mover> 1/2)
               <max-hitpoints>)
  :alertness 5
  :vision 20
  :attacks '((<touch> :type <eat-gold> :damage nil)
	     (<hit> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5) <only-drop-gold>)
  :gender '<male>) 

(define-monster-kind "hobbit-bullroarer" "Bullroarer"
  :title "the hobbit"
  :numeric-id  76
  :x-attr (tile-file 24)
  :x-char (tile-number 0)
  :desc "He is a sturdy hobbit who is renowned for his unusual strength and vigour.  He can prove a troublesome opponent."
  :text-char #\h
  :text-attr #\b
  :type '(<unique>)
  :depth 5
  :rarity 3
  :hitpoints '(6 . 10)
  :armour 8
  :speed 120
  :xp 90
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :alertness 10
  :vision 16
  :attacks '((<touch> :type <eat-gold> :damage nil)
	     (<touch> :type <eat-item> :damage nil)
             (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>)

(define-monster-kind "kobold-mughash" "Mughash"
  :title "the kobold lord"
  :numeric-id  110
  :x-attr (tile-file 24)
  :x-char (tile-number 1)
  :desc "Strong and powerful, for a kobold."
  :text-char #\k
  :text-attr #\b
  :alignment '<evil>
  :type '(<unique>)
  :depth 7
  :rarity 3
  :hitpoints '(15 . 10)
  :armour 20
  :speed 110
  :xp 100
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<poison>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 10)) (<hit> :type <hurt> :damage (1 . 10))
             (<hit> :type <hurt> :damage (1 . 10)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>)

(define-monster-kind "wormtongue" "Wormtongue"
  :title "Agent of Saruman"
  :numeric-id  111
  :x-attr (tile-file 24)
  :x-char (tile-number 2)
  :desc "He's been spying for Saruman.  He is a snivelling wretch with no morals and disgusting habits."
  :text-char #\p
  :text-attr #\b
  :alignment '<evil>
  :type '(<unique>)
  :depth 8
  :rarity 1
  :hitpoints '(25 . 10)
  :armour 30
  :speed 110
  :xp 150
  :abilities '(<bash-door> <open-door> <pick-up-item> <max-hitpoints> <initial-sleeper>)
  :alertness 20
  :vision 20
  :attacks '((<touch> :type <eat-gold> :damage nil) (<hit> :type <hurt> :damage (1 . 5))
             (<hit> :type <hurt> :damage (1 . 5)))
  :treasures '(<drop-great> <drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<ball-spell> <poison>) (<bolt-spell> <cold>) (<spell> <traps>) (<spell> <slow>)
                       (<spell> <heal>) (<frequency> 1/5))) 

(define-monster-kind "orc-lagduf" "Lagduf"
  :title "the Snaga"
  :numeric-id  112
  :x-attr (tile-file 24)
  :x-char (tile-number 3)
  :desc "A captain of a regiment of weaker orcs, Lagduf keeps his troop in order with displays of excessive violence."
  :text-char #\o
  :text-attr #\o
  :alignment '<evil>
  :type '(<orc> <unique>)
  :depth 8
  :rarity 2
  :hitpoints '(19 . 10)
  :armour 32
  :speed 110
  :xp 80
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :alertness 30
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 9)) (<hit> :type <hurt> :damage (1 . 9))
             (<hit> :type <hurt> :damage (1 . 10)) (<hit> :type <hurt> :damage (1 . 10)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "brodda" "Brodda"
  :title "the easterling"
  :numeric-id  133
  :x-attr (tile-file 24)
  :x-char (tile-number 4)
  :desc "A nasty piece of work, Brodda picks on defenseless women and children."
  :text-char #\p
  :text-attr #\u
  :type '(<unique>)
  :depth 9
  :rarity 2
  :hitpoints '(21 . 10)
  :armour 25
  :speed 110
  :xp 100
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 12)) (<hit> :type <hurt> :damage (1 . 12))
             (<hit> :type <hurt> :damage (1 . 12)) (<hit> :type <hurt> :damage (1 . 12)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>)

(define-monster-kind "yeek-orfax" "Orfax"
  :title "son of Boldor"
  :numeric-id  137
  :x-attr (tile-file 24)
  :x-char (tile-number 5)
  :desc "He's just like daddy!  He knows mighty spells, but fortunately he is a  yeek."
  :text-char #\y
  :text-attr #\b
  :alignment '<evil>
  :type '(<animal> <unique>)
  :depth 10
  :rarity 3
  :hitpoints '(12 . 10)
  :armour 20
  :speed 120
  :xp 80
  :abilities '(<bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper>)
  :alertness 10
  :vision 18
  :attacks '((<insult> :type nil :damage nil) (<insult> :type nil :damage nil) (<hit> :type <hurt> :damage (1 . 8))
             (<hit> :type <hurt> :damage (1 . 9)))
  :treasures '(<drop-good> (<drop-chance> 9/10) <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <monster>) (<spell> <confusion>) (<spell> <slow>) (<spell> <teleport-player>)
                       (<spell> <blink>) (<spell> <heal>) (<frequency> 1/4))) 

(define-monster-kind "orc-grishnakh" "Grishnakh"
  :title "the hill-orc"
  :numeric-id 140
  :x-attr (tile-file 24)
  :x-char (tile-number 6)
  :desc "He is a cunning and devious orc with a chaotic nature."
  :text-char #\o
  :text-attr #\U
  :alignment '<evil>
  :type '(<orc> <unique>)
  :depth 10
  :rarity 3
  :hitpoints '(23 . 10)
  :armour 20
  :speed 110
  :xp 160
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<poison>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 10)) (<hit> :type <hurt> :damage (1 . 12))
             (<hit> :type <hurt> :damage (1 . 10)) (<hit> :type <hurt> :damage (1 . 12)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "castamir" "Castamir"
  :title "the Usurper"
  :numeric-id  408
  :x-attr (tile-file 24)
  :x-char (tile-number 35)
  :desc "A Black Numenorean who usurped the throne of Gondor, he is treacherous and  evil."
  :text-char #\p
  :text-attr #\R
  :alignment '<evil>
  :type '(<unique>)
  :depth 38
  :rarity 5
  :hitpoints '(88 . 10)
  :armour 90
  :speed 120
  :xp 1600
  :abilities '(<bash-door> <pick-up-item> <open-door> <smart> <max-hitpoints> <initial-sleeper>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5))
             (<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<bolt-spell> <cold>) (<bolt-spell> <electricity>) (<bolt-spell> <cold>)
                       (<bolt-spell> <fire>) (<spell> <traps>) (<spell> <heal>) (<frequency> 1/2))) 

(define-monster-kind "elemental-vargo" "Vargo"
  :title "Tyrant of Fire"
  :numeric-id  416
  :x-attr (tile-file 24)
  :x-char (tile-number 36)
  :desc "A towering fire elemental, Vargo burns everything beyond recognition."
  :text-char #\E
  :text-attr #\r
  :alignment '<evil>
  :type '(<unique>)
  :depth 38
  :rarity 3
  :hitpoints '(15 . 100)
  :armour 50
  :speed 120
  :xp 3000
  :abilities '(<powerful-breath> <bash-door> <overrun-others> <overrun-items> <empty-mind> (<random-mover> 1/4)
               <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <fire>)
  :alertness 50
  :vision 12
  :attacks '((<hit> :type <fire> :damage (4 . 6)) (<hit> :type <fire> :damage (4 . 6))
             (<hit> :type <fire> :damage (4 . 6)) (<hit> :type <fire> :damage (4 . 6)))
  :special-abilities '((<ball-spell> <fire>) (<bolt-spell> <plasma>) (<frequency> 1/4)))

(define-monster-kind "elemental-waldern" "Waldern"
  :title "King of Water"
  :numeric-id  422
  :x-attr (tile-file 24)
  :x-char (tile-number 37)
  :desc "A towering water elemental, Waldern is master of all things liquid.  Wave after wave drowns your frail body."
  :text-char #\E
  :text-attr #\s
  :alignment '<evil>
  :type '(<unique>)
  :depth 39
  :rarity 3
  :hitpoints '(20 . 100)
  :armour 40
  :speed 120
  :xp 3250
  :abilities '(<powerful-breath> <bash-door> <overrun-others> <overrun-items> <cold-blood> <empty-mind>
               (<random-mover> 1/4) <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison>)
  :alertness 50
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5))
             (<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5)))
  :special-abilities '((<ball-spell> <water>) (<ball-spell> <cold>) (<bolt-spell> <water>)
                       (<bolt-spell> <cold>) (<frequency> 1/4))) 

(define-monster-kind "dragon-kavlax" "Kavlax"
  :title "the many-headed"
  :numeric-id  423
  :x-attr (tile-file 24)
  :x-char (tile-number 38)
  :desc "A large dragon with a selection of heads, all shouting and arguing as they  look for prey, but each with its own deadly breath weapon."
  :text-char #\d
  :text-attr #\v
  :alignment '<evil>
  :type '(<dragon> <unique>)
  :depth 39
  :rarity 3
  :hitpoints '(13 . 100)
  :armour 85
  :speed 120
  :xp 3000
  :abilities '(<powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confusion> <electricity> <cold> <fire> <acid>)
  :alertness 30
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12))
             (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<breath> <nexus>) (<breath> <gravity>) (<breath> <shards>) (<breath> <confusion>)
                       (<breath> <sound>) (<breath> <electricity>) (<breath> <cold>) (<breath> <fire>) (<breath> <acid>)
                       (<frequency> 1/4)))

(define-monster-kind "uvatha" "Uvatha"
  :title "the Horseman"
  :numeric-id  440
  :x-attr (tile-file 24)
  :x-char (tile-number 39)
  :desc "A tall black cloaked Ringwraith, he is a master of horsemanship.  He longs  to taste your blood."
  :text-char #\W
  :text-attr #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 40
  :rarity 3
  :hitpoints '(12 . 100)
  :armour 60
  :speed 120
  :xp 7000
  :abilities '(<push-others> <bash-door> <open-door> <cold-blood> <max-hitpoints>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<hit> :type <exp-80> :damage (4 . 6)) (<hit> :type <exp-80> :damage (4 . 6))
             (<hit> :type <hurt> :damage (6 . 6)) (<hit> :type <hurt> :damage (6 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>)

(define-monster-kind "medusa" "Medusa"
  :title "the Gorgon"
  :numeric-id  442
  :x-attr (tile-file 24)
  :x-char (tile-number 40)
  :desc "One of the original three ugly sisters.  Her face could sink a thousand  ships.  Her scales rattle as she slithers towards you, venom dripping from  her ghastly mouth."
  :text-char #\n
  :text-attr #\o
  :alignment '<evil>
  :type '(<unique>)
  :depth 40
  :rarity 3
  :hitpoints '(24 . 100)
  :armour 100
  :speed 120
  :xp 9000
  :abilities '(<bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <fire> <acid>)
  :alertness 5
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (8 . 6)) (<hit> :type <hurt> :damage (8 . 6))
             (<gaze> :type <paralyse> :damage nil) (<gaze> :type <exp-80> :damage nil))
  :treasures '(<drop-good> (<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :gender '<female>
  :special-abilities '((<summon> <hydra>) (<ball-spell> <acid>) (<bolt-spell> <plasma>)
                       (<bolt-spell> <fire>) (<dmg-spell> 3) (<spell> <scare>) (<spell> <paralysis>)
                       (<frequency> 1/2)))

(define-monster-kind "orc-golfimbul" "golfimbul"
  :title "the hill orc chief"
  :numeric-id  159
  :x-attr (tile-file 24)
  :x-char (tile-number 7)
  :desc "A leader of a band of raiding orcs, he picks on hobbits."
  :text-char #\o
  :text-attr #\U
  :alignment '<evil>
  :type '(<orc> <unique>)
  :depth 12
  :rarity 3
  :hitpoints '(24 . 10)
  :armour 60
  :speed 110
  :xp 230
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<poison> <electricity> <cold> <fire>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 10)) (<hit> :type <hurt> :damage (1 . 10))
             (<hit> :type <hurt> :damage (1 . 12)) (<hit> :type <hurt> :damage (1 . 12)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "yeek-boldor" "Boldor"
  :title "King of the yeeks"
  :numeric-id  173
  :x-attr (tile-file 24)
  :x-char (tile-number 8)
  :desc "A great yeek, powerful in magic and sorcery, but a yeek all the same."
  :text-char #\y
  :text-attr #\U
  :alignment '<evil>
  :type '(<animal> <unique>)
  :depth 13
  :rarity 3
  :hitpoints '(18 . 10)
  :armour 24
  :speed 120
  :xp 200
  :abilities '(<bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper>)
  :alertness 10
  :vision 18
  :attacks '((<hit> :type <hurt> :damage (1 . 8)) (<hit> :type <hurt> :damage (1 . 9))
             (<hit> :type <hurt> :damage (1 . 9)))
  :treasures '(<drop-good> (<drop> "1d2") (<drop-chance> 9/10) <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <kin>) (<summon> <monster>) (<spell> <slow>) (<spell> <blindness>)
                       (<spell> <teleport>) (<spell> <blink>) (<spell> <heal>) (<frequency> 1/3))) 

(define-monster-kind "ufthak" "Ufthak"
  :title "of Cirith Ungol"
  :numeric-id  181
  :x-attr (tile-file 24)
  :x-char (tile-number 9)
  :desc "A strong orc guarding the pass of Cirith Ungol.  He is mortally afraid of  spiders."
  :text-char #\o
  :text-attr #\g
  :alignment '<evil>
  :type '(<orc> <unique>)
  :depth 14
  :rarity 3
  :hitpoints '(32 . 10)
  :armour 50
  :speed 110
  :xp 200
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<poison> <cold>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4))
             (<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <hurt> :damage (3 . 4)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>)

(define-monster-kind "ren" "Ren"
  :title "the Unclean"
  :numeric-id  457
  :x-attr (tile-file 24)
  :x-char (tile-number 44)
  :desc "Ren was an insane eastern king who believed himself to be the son of a  volcano god.  At an early age his sanity was destroyed by a plague that  wiped out his family, and he never recovered."
  :text-char #\W
  :text-attr #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 41
  :rarity 3
  :hitpoints '(18 . 100)
  :armour 70
  :speed 120
  :xp 13000
  :abilities '(<push-others> <bash-door> <open-door> <cold-blood> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold> <fire>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<wail> :type <terrify> :damage nil) (<touch> :type <exp-80> :damage nil)
             (<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <monster>) (<ball-spell> <fire>) (<bolt-spell> <nether>)
                       (<bolt-spell> <fire>) (<dmg-spell> 3) (<spell> <scare>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "dawndeath" "Ji Indur Dawndeath"
  :numeric-id  464
  :x-attr (tile-file 24)
  :x-char (tile-number 45)
  :desc "This Ringwraith was a weak-minded sorcerer-king who fell easily under  Sauron's power."
  :text-char #\W
  :text-attr #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 43
  :rarity 4
  :hitpoints '(18 . 100)
  :armour 70
  :speed 120
  :xp 12000
  :abilities '(<push-others> <bash-door> <open-door> <cold-blood> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold> <fire>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<touch> :type <exp-40> :damage nil) (<touch> :type <exp-40> :damage nil)
             (<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <undead>) (<ball-spell> <nether>) (<ball-spell> <fire>) (<dmg-spell> 3)
                       (<spell> <scare>) (<spell> <paralysis>) (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "elemental-quaker" "Quaker"
  :title "Master of Earth"
  :numeric-id  466
  :x-attr (tile-file 24)
  :x-char (tile-number 46)
  :desc "A towering stone elemental stands before you.  The walls and ceiling are  reduced to rubble as Quaker advances."
  :text-char #\E
  :text-attr #\u
  :alignment '<evil>
  :type '(<unique>)
  :depth 43
  :rarity 4
  :hitpoints '(18 . 100)
  :armour 97
  :speed 110
  :xp 6000
  :abilities '(<powerful-breath> <pass-wall> <overrun-others> <overrun-items> <cold-blood> <empty-mind> <max-hitpoints>
               <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 90
  :vision 10
  :attacks '((<hit> :type <shatter> :damage (10 . 10)) (<hit> :type <hurt> :damage (6 . 6))
             (<hit> :type <hurt> :damage (6 . 6)) (<hit> :type <hurt> :damage (6 . 6)))
  :gender '<male>
  :special-abilities '((<ball-spell> <acid>) (<bolt-spell> <acid>) (<frequency> 1/6)))

(define-monster-kind "elemental-ariel" "Ariel"
  :title "Queen of Air"
  :numeric-id  468
  :x-attr (tile-file 24)
  :x-char (tile-number 47)
  :desc "A towering air elemental, Ariel, the sorceress, avoids your blows with her extreme speed."
  :text-char #\E
  :text-attr #\B
  :alignment '<evil>
  :type '(<unique>)
  :depth 44
  :rarity 4
  :hitpoints '(27 . 100)
  :armour 50
  :speed 130
  :xp 8000
  :abilities '(<powerful-breath> <bash-door> <overrun-others> <overrun-items> <cold-blood> <empty-mind>
               (<random-mover> 1/4) <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 50
  :vision 12
  :attacks '((<hit> :type <confusion> :damage (1 . 4)) (<hit> :type <hurt> :damage (4 . 6))
             (<hit> :type <confusion> :damage (1 . 4)) (<hit> :type <hurt> :damage (4 . 6)))
  :gender '<female>
  :special-abilities '((<ball-spell> <electricity>) (<ball-spell> <cold>) (<bolt-spell> <electricity>)
                       (<frequency> 1/5))) 

(define-monster-kind "scatha" "Scatha"
  :title "the Worm"
  :numeric-id  473
  :x-attr (tile-file 24)
  :x-char (tile-number 48)
  :desc "An ancient and wise Dragon.  Scatha has grown clever over the long years.  His scales are covered with frost, and his breath sends a shower of ice into the air."
  :text-char #\D
  :text-attr #\w
  :alignment '<evil>
  :type '(<dragon> <unique>)
  :depth 44
  :rarity 2
  :hitpoints '(20 . 100)
  :armour 130
  :speed 120
  :xp 17000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <cold>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (3 . 14))
	     (<claw> :type <hurt> :damage (1 . 10))
             (<claw> :type <hurt> :damage (1 . 10))
	     (<claw> :type <hurt> :damage (1 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<breath> <cold>) (<dmg-spell> 3) (<spell> <confusion>) (<frequency> 1/3))) 

(define-monster-kind "dwar" "Dwar"
  :title "Dog-lord of Waw"
  :numeric-id 474
  :x-attr (tile-file 24)
  :x-char (tile-number 49)
  :desc "Dwar had a special affinity for dogs in life, and can still command them  at will.  He howls manically as he reaches out to destroy you."
  :text-char #\W
  :text-attr #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 44
  :rarity 3
  :hitpoints '(20 . 100)
  :armour 90
  :speed 120
  :xp 13000
  :abilities '(<push-others> <bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold> <fire>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<wail> :type <terrify> :damage nil)
	     (<bite> :type <exp-40> :damage (2 . 4))
	     (<hit> :type <hurt> :damage (5 . 5))
	     (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <hound>) (<summon> <undead>) (<summon> <monsters>) (<ball-spell> <nether>)
                       (<ball-spell> <fire>) (<dmg-spell> 3) (<spell> <scare>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "smaug" "Smaug"
  :title "the Golden"
  :numeric-id  475
  :x-attr (tile-file 24)
  :x-char (tile-number 50)
  :desc "Smaug is one of the Uruloki that still survive, a fire-drake of immense  cunning and intelligence.  His speed through air is matched by few other  dragons and his dragonfire is what legends are made of."
  :text-char #\D
  :text-attr #\r
  :alignment '<evil>
  :type '(<dragon> <unique>)
  :depth 45
  :rarity 2
  :hitpoints '(20 . 100)
  :armour 100
  :speed 120
  :xp 19000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (3 . 14))
	     (<claw> :type <hurt> :damage (1 . 10))
             (<claw> :type <hurt> :damage (1 . 10))
	     (<claw> :type <hurt> :damage (1 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<breath> <fire>) (<dmg-spell> 3) (<spell> <confusion>) (<frequency> 1/3))) 

(define-monster-kind "ulfast" "Ulfast"
  :title "son of Ulfang"
  :numeric-id  211
  :x-attr (tile-file 24)
  :x-char (tile-number 10)
  :desc "A short and swarthy Easterling."
  :text-char #\p
  :text-attr #\U
  :alignment '<evil>
  :type '(<unique>)
  :depth 16
  :rarity 3
  :hitpoints '(34 . 10)
  :armour 40
  :speed 110
  :xp 200
  :abilities '(<bash-door> <open-door> <pick-up-item> <max-hitpoints>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 5))
	     (<hit> :type <hurt> :damage (3 . 5))
             (<hit> :type <hurt> :damage (3 . 5))
	     (<hit> :type <hurt> :damage (3 . 5)))
  :treasures '(<drop-good> (<drop-chance> 9/10) <only-drop-items>)
  :gender '<male>)

(define-monster-kind "dwarf-nar" "Nar"
  :title "the dwarf"
  :numeric-id  215
  :x-attr (tile-file 24)
  :x-char (tile-number 11)
  :desc "This dwarf became so obsessed by gold that Morgoth tricked him into  betraying his friends."
  :text-char #\h
  :text-attr #\y
  :type '(<unique>)
  :depth 17
  :rarity 2
  :hitpoints '(45 . 10)
  :armour 70
  :speed 110
  :xp 250
  :abilities '(<bash-door> <open-door> <initial-sleeper> <max-hitpoints>)
  :immunities '(<sleep> <confusion> <poison> <cold> <fire>)
  :alertness 25
  :vision 25
  :attacks '((<hit> :type <hurt> :damage (3 . 5))
	     (<hit> :type <hurt> :damage (3 . 5))
             (<hit> :type <hurt> :damage (3 . 5))
	     (<hit> :type <hurt> :damage (3 . 5)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> <mind-blast>) (<dmg-spell> 2) (<spell> <confusion>) (<spell> <blindness>)
                       (<spell> <heal>) (<frequency> 1/6)))

(define-monster-kind "orc-shagrat" "shagrat"
  :title "the orc captain"
  :numeric-id  227
  :x-attr (tile-file 24)
  :x-char (tile-number 12)
  :desc "He is an Uruk of power and great cunning."
  :text-char #\o
  :text-attr #\g
  :alignment '<evil>
  :type '(<orc> <unique>)
  :depth 18
  :rarity 2
  :hitpoints '(40 . 10)
  :armour 60
  :speed 110
  :xp 400
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<poison>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 5))
	     (<hit> :type <hurt> :damage (3 . 5))
             (<hit> :type <hurt> :damage (3 . 8))
	     (<hit> :type <hurt> :damage (3 . 8)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>)

(define-monster-kind "orc-gorbag" "Gorbag"
  :title "the orc captain"
  :numeric-id  228
  :x-attr (tile-file 24)
  :x-char (tile-number 13)
  :desc "A gruesomely ugly but cunning orc, his eyes regard you with hatred.  His  powerful arms flex menacingly as he advances."
  :text-char #\o
  :text-attr #\g
  :alignment '<evil>
  :type '(<orc> <unique>)
  :depth 18
  :rarity 3
  :hitpoints '(40 . 10)
  :armour 60
  :speed 110
  :xp 400
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<poison>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 5))
	     (<hit> :type <hurt> :damage (3 . 5))
             (<hit> :type <hurt> :damage (3 . 8))
	     (<hit> :type <hurt> :damage (3 . 8)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>)

(define-monster-kind "orc-bolg" "Bolg"
  :title "son of Azog"
  :numeric-id  235
  :x-attr (tile-file 24)
  :x-char (tile-number 14)
  :desc "A large and powerful orc.  He looks just like his daddy.  He is tall and  fast, but fortunately blessed with orcish brains."
  :text-char #\o
  :text-attr #\r
  :alignment '<evil>
  :type '(<orc> <unique>)
  :depth 20
  :rarity 4
  :hitpoints '(50 . 10)
  :armour 50
  :speed 120
  :xp 800
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<poison>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 6))
	     (<hit> :type <hurt> :damage (3 . 6))
             (<hit> :type <hurt> :damage (3 . 6))
	     (<hit> :type <hurt> :damage (3 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>)

(define-monster-kind "orc-ugluk" "Ugluk"
  :title "the uruk"
  :numeric-id  248
  :x-attr (tile-file 24)
  :x-char (tile-number 15)
  :desc "Another of Morgoth's servants, this orc is strong and cunning.  He is ugly  and scarred from many power struggles."
  :text-char #\o
  :text-attr #\b
  :alignment '<evil>
  :type '(<orc> <unique>)
  :depth 20
  :rarity 4
  :hitpoints '(64 . 10)
  :armour 90
  :speed 110
  :xp 550
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<poison>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 5))
	     (<hit> :type <hurt> :damage (3 . 5))
             (<hit> :type <hurt> :damage (3 . 5))
	     (<hit> :type <hurt> :damage (3 . 5)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>)

(define-monster-kind "orc-lugdush" "Lugdush"
  :title "the uruk"
  :numeric-id  249
  :x-attr (tile-file 24)
  :x-char (tile-number 16)
  :desc "A strong and cunning orc warrior, Lugdush sneers as he insults your mother."
  :text-char #\o
  :text-attr #\b
  :alignment '<evil>
  :type '(<orc> <unique>)
  :depth 21
  :rarity 3
  :hitpoints '(72 . 10)
  :armour 95
  :speed 110
  :xp 550
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<sleep> <confusion> <poison> <cold> <fire>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 5))
	     (<hit> :type <hurt> :damage (3 . 5))
             (<hit> :type <hurt> :damage (3 . 8))
	     (<hit> :type <hurt> :damage (3 . 8)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>)

(define-monster-kind "orc-azog" "Azog"
  :title "King of the uruk-hai"
  :numeric-id  262
  :x-attr (tile-file 24)
  :x-char (tile-number 17)
  :desc "He is also known as the King of Khazad-dum.  His ego is renowned to be  bigger than his head."
  :text-char #\o
  :text-attr #\r
  :alignment '<evil>
  :type '(<orc> <unique>)
  :depth 23
  :rarity 5
  :hitpoints '(90 . 10)
  :armour 80
  :speed 120
  :xp 1111
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<poison>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (5 . 5))
	     (<hit> :type <hurt> :damage (5 . 5))
             (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>)

(define-monster-kind "dwarf-ibun" "Ibun"
  :title "son of Mim"
  :numeric-id  268
  :x-attr (tile-file 24)
  :x-char (tile-number 18)
  :desc "One of the last of the petty dwarves.  Ibun is a tricky sorcerous little  being, full of mischief."
  :text-char #\h
  :text-attr #\o
  :type '(<unique>)
  :depth 24
  :rarity 2
  :hitpoints '(82 . 10)
  :armour 80
  :speed 110
  :xp 300
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <cold> <fire>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <un-bonus> :damage nil)
	     (<hit> :type <hurt> :damage (3 . 6))
             (<hit> :type <hurt> :damage (3 . 6))
	     (<hit> :type <hurt> :damage (3 . 6)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<bolt-spell> <fire>) (<spell> <slow>) (<spell> <heal>) (<frequency> 1/8))) 

(define-monster-kind "dwarf-khim" "Khim"
  :title "son of Mim"
  :numeric-id  269
  :x-attr (tile-file 24)
  :x-char (tile-number 19)
  :desc "One of the last of the petty dwarves.  Khim is a tricky sorcerous little  being, full of mischief."
  :text-char #\h
  :text-attr #\o
  :type '(<unique>)
  :depth 24
  :rarity 2
  :hitpoints '(82 . 10)
  :armour 80
  :speed 110
  :xp 300
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <cold> <fire>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <un-bonus> :damage nil)
	     (<hit> :type <hurt> :damage (3 . 6))
             (<hit> :type <hurt> :damage (3 . 6))
	     (<hit> :type <hurt> :damage (3 . 6)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<bolt-spell> <fire>) (<spell> <slow>) (<spell> <heal>) (<frequency> 1/8))) 

(define-monster-kind "sangahyando" "Sangahyando"
  :title "of Umbar"
  :numeric-id  273
  :x-attr (tile-file 24)
  :x-char (tile-number 20)
  :desc "A Black Numenorean with a blacker heart."
  :text-char #\p
  :text-attr #\u
  :alignment '<evil>
  :type '(<unique>)
  :depth 24
  :rarity 2
  :hitpoints '(80 . 10)
  :armour 80
  :speed 110
  :xp 400
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <electricity> <fire>)
  :alertness 25
  :vision 25
  :attacks '((<hit> :type <hurt> :damage (4 . 6))
	     (<hit> :type <hurt> :damage (4 . 6))
             (<hit> :type <hurt> :damage (4 . 6))
	     (<hit> :type <hurt> :damage (4 . 6)))
  :treasures '(<drop-good> (<drop> "1d2") (<drop-chance> 9/10) <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> <forget>) (<spell> <slow>) (<frequency> 1/4)))

(define-monster-kind "angamaite" "Angamaite"
  :title "of Umbar"
  :numeric-id  274
  :x-attr (tile-file 24)
  :x-char (tile-number 21)
  :desc "A Black Numenorean who hates the men of the west."
  :text-char #\p
  :text-attr #\u
  :alignment '<evil>
  :type '(<unique>)
  :depth 24
  :rarity 2
  :hitpoints '(80 . 10)
  :armour 80
  :speed 110
  :xp 400
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <electricity> <fire>)
  :alertness 25
  :vision 25
  :attacks '((<hit> :type <hurt> :damage (4 . 6))
	     (<hit> :type <hurt> :damage (4 . 6))
             (<hit> :type <hurt> :damage (4 . 6))
	     (<hit> :type <hurt> :damage (4 . 6)))
  :treasures '(<drop-good> (<drop> "1d2") (<drop-chance> 9/10) <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> <forget>) (<spell> <slow>) (<frequency> 1/4)))

(define-monster-kind "ulwarth" "Ulwarth"
  :title "son of Ulfang"
  :numeric-id  284
  :x-attr (tile-file 24)
  :x-char (tile-number 22)
  :desc "A short and swarthy Easterling."
  :text-char #\p
  :text-attr #\U
  :alignment '<evil>
  :type '(<unique>)
  :depth 26
  :rarity 4
  :hitpoints '(85 . 10)
  :armour 40
  :speed 110
  :xp 500
  :abilities '(<bash-door> <open-door> <pick-up-item> <max-hitpoints>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (4 . 6))
	     (<hit> :type <hurt> :damage (4 . 6))
             (<hit> :type <hurt> :damage (4 . 6)))
  :treasures '(<drop-good> (<drop-chance> 9/10) <only-drop-items>)
  :gender '<male>)

(define-monster-kind "dwarf-mim" "Mim"
  :title "betrayer of Turin"
  :numeric-id  290
  :x-attr (tile-file 24)
  :x-char (tile-number 23)
  :desc "The last of his race, Mim is a petty dwarf.  Petty dwarves are strange  creatures, powerful in sorcery and originating in the East.  They were  hunted to extinction by high elves."
  :text-char #\h
  :text-attr #\o
  :alignment '<evil>
  :type '(<unique>)
  :depth 27
  :rarity 4
  :hitpoints '(11 . 100)
  :armour 80
  :speed 120
  :xp 1000
  :abilities '(<bash-door> <open-door> <initial-sleeper> <max-hitpoints>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <un-bonus> :damage nil)
	     (<hit> :type <hurt> :damage (3 . 8))
             (<hit> :type <hurt> :damage (3 . 8))
	     (<hit> :type <hurt> :damage (3 . 8)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<ball-spell> <acid>) (<bolt-spell> <acid>) (<spell> <scare>) (<spell> <heal>)
                       (<frequency> 1/6)))

(define-monster-kind "ogre-lokkak" "Lokkak"
  :title "the ogre chieftain"
  :numeric-id  297
  :x-attr (tile-file 24)
  :x-char (tile-number 24)
  :desc "An ogre renowned for acts of surpassing cruelty, Lokkak quickly became the  leader of a large band of violent ogres."
  :text-char #\O
  :text-attr #\g
  :alignment '<evil>
  :type '(<giant> <unique>)
  :depth 27
  :rarity 2
  :hitpoints '(15 . 100)
  :armour 100
  :speed 120
  :xp 1500
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<poison>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (6 . 6)) (<hit> :type <hurt> :damage (6 . 6))
             (<hit> :type <hurt> :damage (6 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "uldor" "Uldor"
  :title "the accursed"
  :numeric-id  304
  :x-attr (tile-file 24)
  :x-char (tile-number 25)
  :desc "An evil and cunning man from the East."
  :text-char #\p
  :text-attr #\U
  :alignment '<evil>
  :type '(<unique>)
  :depth 28
  :rarity 4
  :hitpoints '(10 . 100)
  :armour 70
  :speed 110
  :xp 600
  :abilities '(<bash-door> <open-door> <pick-up-item> <max-hitpoints>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 5))
	     (<hit> :type <hurt> :damage (4 . 6))
             (<hit> :type <hurt> :damage (4 . 6))
	     (<hit> :type <hurt> :damage (4 . 6)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>)

(define-monster-kind "imp-draebor" "Draebor"
  :title "the imp"
  :numeric-id  307
  :x-attr (tile-file 24)
  :x-char (tile-number 26)
  :desc "An intensely irritating git of a monster."
  :text-char #\u
  :text-attr #\g
  :alignment '<evil>
  :type '(<demon> <unique>)
  :depth 28
  :rarity 5
  :hitpoints '(52 . 10)
  :armour 50
  :speed 120
  :xp 750
  :abilities '(<bash-door> <invisible> <smart> (<random-mover> 1/4) <initial-sleeper> <max-hitpoints>)
  :immunities '(<fire>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 4))
	     (<hit> :type <poison> :damage (3 . 4))
             (<hit> :type <poison> :damage (3 . 4)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :special-abilities '((<summon> <kin>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<spell> <teleport-level>) (<spell> <teleport-away>) (<spell> <teleport-player>)
                       (<spell> <teleport>) (<spell> <blink>) (<frequency> 1/5))) 

(define-monster-kind "shelob" "Shelob"
  :title "spider of darkness"
  :numeric-id  330
  :x-attr (tile-file 24)
  :x-char (tile-number 27)
  :desc "Shelob is an enormous bloated spider, rumoured to have been one of the  brood of Ungoliant the Unlight.  Her poison is legendary, as is her ego,  which may be her downfall.  She used to guard the pass through Cirith  Ungol, but has not been seen there for many eons."
  :text-char #\S
  :text-attr #\D
  :alignment '<evil>
  :type '(<animal> <unique>)
  :depth 32
  :rarity 3
  :hitpoints '(12 . 100)
  :armour 80
  :speed 110
  :xp 1200
  :abilities '(<bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :vulnerabilities '(<light>)
  :alertness 80
  :vision 8
  :attacks '((<sting> :type <poison> :damage (2 . 5))
	     (<sting> :type <lose-str> :damage (1 . 4))
             (<sting> :type <poison> :damage (2 . 5))
	     (<bite> :type <hurt> :damage (2 . 10)))
  :treasures '(<drop-good> (<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :gender '<female>
  :special-abilities '((<summon> <spider>) (<spell> <traps>) (<dmg-spell> 4) (<dmg-spell> 3)
                       (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>) (<spell> <blindness>) (<spell> <heal>)
                       (<frequency> 1/2)))

(define-monster-kind "troll-bert" "Bert"
  :title "the stone troll"
  :numeric-id  343
  :x-attr (tile-file 24)
  :x-char (tile-number 28)
  :desc "Big, brawny, powerful and with a taste for hobbit.  He has friends called Bill and Tom."
  :text-char #\T
  :text-attr #\W
  :alignment '<evil>
  :type '(<troll> <unique>)
  :depth 33
  :rarity 7
  :hitpoints '(11 . 100)
  :armour 70
  :speed 110
  :xp 2000
  :abilities '(<bash-door> <open-door> <pick-up-item> <max-hitpoints>)
  :immunities '(<poison> <cold>)
  :vulnerabilities '(<earth-destruction> <light>)
  :alertness 50
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 3))
	     (<bite> :type <hurt> :damage (2 . 10))
             (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>)

(define-monster-kind "troll-bill" "Bill"
  :title "the stone troll"
  :numeric-id  344
  :x-attr (tile-file 24)
  :x-char (tile-number 29)
  :desc "Big, brawny, powerful and with a taste for hobbit.  He has friends called Bert and Tom."
  :text-char #\T
  :text-attr #\W
  :alignment '<evil>
  :type '(<troll> <unique>)
  :depth 33
  :rarity 7
  :hitpoints '(11 . 100)
  :armour 70
  :speed 110
  :xp 2000
  :abilities '(<bash-door> <open-door> <pick-up-item> <max-hitpoints>)
  :immunities '(<poison> <cold>)
  :vulnerabilities '(<earth-destruction> <light>)
  :alertness 50
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 3))
	     (<bite> :type <hurt> :damage (2 . 10))
             (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "troll-tom" "Tom"
  :title "the stone troll"
  :numeric-id  345
  :x-attr (tile-file 24)
  :x-char (tile-number 30)
  :desc "Big, brawny, powerful and with a taste for hobbit.  He has friends called Bert and Bill."
  :text-char #\T
  :text-attr #\W
  :alignment '<evil>
  :type '(<troll> <unique>)
  :depth 33
  :rarity 7
  :hitpoints '(11 . 100)
  :armour 70
  :speed 110
  :xp 2000
  :abilities '(<bash-door> <open-door> <pick-up-item> <max-hitpoints>)
  :immunities '(<poison> <cold>)
  :vulnerabilities '(<earth-destruction> <light>)
  :alertness 50
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 3))
	     (<bite> :type <hurt> :damage (2 . 10))
             (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "ulfang" "Ulfang"
  :title "the black"
  :numeric-id  355
  :x-attr (tile-file 24)
  :x-char (tile-number 31)
  :desc "A short and swarthy Easterling dressed in Black."
  :text-char #\p
  :text-attr #\U
  :alignment '<evil>
  :type '(<unique>)
  :depth 34
  :rarity 5
  :hitpoints '(10 . 100)
  :armour 90
  :speed 120
  :xp 1200
  :abilities '(<bash-door> <open-door> <pick-up-item> <max-hitpoints>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (5 . 5))
	     (<hit> :type <hurt> :damage (5 . 5))
             (<hit> :type <hurt> :damage (5 . 5))
	     (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "troll-rogrog" "Rogrog"
  :title "the black troll"
  :numeric-id  383
  :x-attr (tile-file 24)
  :x-char (tile-number 32)
  :desc "A massive and cruel troll of great power, drool slides caustically down  his muscular frame.  Despite his bulk, he strikes with stunning speed."
  :text-char #\T
  :text-attr #\D
  :alignment '<evil>
  :type '(<troll> <unique>)
  :depth 36
  :rarity 5
  :hitpoints '(15 . 100)
  :armour 70
  :speed 120
  :xp 5000
  :abilities '(<bash-door> <open-door> <pick-up-item> <max-hitpoints>)
  :immunities '(<poison> <acid>)
  :alertness 50
  :vision 20
  :attacks '((<spit> :type <acid> :damage (3 . 8))
	     (<bite> :type <hurt> :damage (2 . 3))
             (<bite> :type <hurt> :damage (2 . 10))
	     (<hit> :type <hurt> :damage (6 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "lorgan" "Lorgan"
  :title "Chief of the easterlings"
  :numeric-id  392
  :x-attr (tile-file 24)
  :x-char (tile-number 33)
  :desc "A mighty warrior from the east, Lorgan hates everything that he cannot  control."
  :text-char #\p
  :text-attr #\R
  :alignment '<evil>
  :type '(<unique>)
  :depth 36
  :rarity 2
  :hitpoints '(18 . 100)
  :armour 100
  :speed 120
  :xp 1200
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 10
  :vision 25
  :attacks '((<hit> :type <hurt> :damage (3 . 8))
	     (<hit> :type <hurt> :damage (3 . 8))
             (<hit> :type <hurt> :damage (6 . 6))
	     (<hit> :type <hurt> :damage (6 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <monsters>) (<spell> <teleport-player>) (<frequency> 1/4))) 

(define-monster-kind "ant-queen" "Formica"
  :title "Queen of the antz"
  :numeric-id  395
  :x-attr (tile-file 24)
  :x-char (tile-number 34)
  :desc "She's upset because you hurt her children."
  :text-char #\a
  :text-attr #\D
  :type '(<animal> <unique>)
  :depth 37
  :rarity 2
  :hitpoints '(15 . 100)
  :armour 100
  :speed 120
  :xp 1000
  :abilities '(<bash-door> <open-door> <weird-mind> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion>)
  :alertness 10
  :vision 30
  :attacks '((<bite> :type <hurt> :damage (2 . 8))
	     (<bite> :type <hurt> :damage (2 . 8))
             (<bite> :type <hurt> :damage (2 . 12))
	     (<bite> :type <hurt> :damage (2 . 12)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<female>
  :special-abilities '((<summon> <ant>) (<frequency> 1/2))) 

(define-monster-kind "adunaphel" "Adunaphel"
  :title "the quiet"
  :numeric-id  449
  :x-attr (tile-file 24)
  :x-char (tile-number 41)
  :desc "A sorceress in life, Adunaphel quickly fell under Sauron's sway and the power of the rings."
  :text-char #\W
  :text-attr #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 41
  :rarity 3
  :hitpoints '(12 . 100)
  :armour 60
  :speed 120
  :xp 8000
  :abilities '(<push-others> <pass-wall> <cold-blood> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<touch> :type <exp-80> :damage nil)
	     (<hit> :type <hurt> :damage (5 . 5))
             (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<female>
  :special-abilities '((<summon> <monster>) (<bolt-spell> <nether>) (<bolt-spell> <cold>)
                       (<bolt-spell> <fire>) (<bolt-spell> <acid>) (<spell> <forget>) (<dmg-spell> 3)
                       (<spell> <scare>) (<spell> <paralysis>) (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "akhorahil" "Akhorahil"
  :title "the blind"
  :numeric-id  453
  :x-attr (tile-file 24)
  :x-char (tile-number 42)
  :desc "A mighty sorcerer King, Akhorahil was blind in life.  With powerful enchantments, he created jewelled eyes that enabled him to see better than any ordinary man ever could."
  :text-char #\W
  :text-attr #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 41
  :rarity 3
  :hitpoints '(18 . 100)
  :armour 70
  :speed 120
  :xp 12000
  :abilities '(<push-others> <bash-door> <open-door> <cold-blood> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<wail> :type <terrify> :damage nil)
	     (<gaze> :type <exp-80> :damage nil)
             (<hit> :type <hurt> :damage (5 . 5))
	     (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <monster>) (<bolt-spell> <nether>) (<bolt-spell> <cold>)
                       (<bolt-spell> <fire>) (<spell> <darkness>) (<dmg-spell> 3) (<spell> <scare>)
                       (<spell> <paralysis>) (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "gorlim" "Gorlim"
  :title "betrayer of Barahir"
  :numeric-id  454
  :x-attr (tile-file 24)
  :x-char (tile-number 43)
  :desc "This once-mighty warrior was so dominated by Morgoth's power that he became little more than a mindless creature of evil."
  :text-char #\p
  :text-attr #\s
  :type '(<unique>)
  :depth 41
  :rarity 3
  :hitpoints '(16 . 100)
  :armour 120
  :speed 120
  :xp 7000
  :abilities '(<bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <acid>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <un-bonus> :damage (6 . 8))
	     (<hit> :type <un-bonus> :damage (6 . 8))
             (<hit> :type <hurt> :damage (8 . 6))
	     (<hit> :type <hurt> :damage (8 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<bolt-spell> <mana>) (<bolt-spell> <water>) (<dmg-spell> 3) (<frequency> 1/2)))

(define-monster-kind "dragon-itangast" "Itangast"
  :title "the fire drake"
  :numeric-id  480
  :x-attr (tile-file 24)
  :x-char (tile-number 51)
  :desc "A mighty ancient dragon, Itangast's form scorches your flesh.  Wisps of  smoke curl up from his nostrils as he regards you with disdain."
  :text-char #\D
  :text-attr #\r
  :alignment '<evil>
  :type '(<dragon> <unique>)
  :depth 47
  :rarity 4
  :hitpoints '(22 . 100)
  :armour 100
  :speed 120
  :xp 20000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (4 . 14))
	     (<bite> :type <hurt> :damage (3 . 14))
             (<claw> :type <hurt> :damage (1 . 10))
	     (<claw> :type <hurt> :damage (1 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<breath> <fire>) (<dmg-spell> 3) (<spell> <confusion>) (<frequency> 1/3)))

(define-monster-kind "dragon-glaurung" "Glaurung"
  :title "Father of the dragons"
  :numeric-id  481
  :x-attr (tile-file 24)
  :x-char (tile-number 52)
  :desc "Glaurung is the father of all dragons, and was for a long time the most  powerful.  Nevertheless, he still has full command over his brood and can  command them to appear whenever he so wishes.  He is the definition of  dragonfire."
  :text-char #\D
  :text-attr #\r
  :alignment '<evil>
  :type '(<dragon> <unique>)
  :depth 48
  :rarity 2
  :hitpoints '(28 . 100)
  :armour 120
  :speed 120
  :xp 25000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (6 . 14))
	     (<bite> :type <hurt> :damage (6 . 14))
             (<claw> :type <hurt> :damage (4 . 12))
	     (<claw> :type <hurt> :damage (4 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <dragon>) (<breath> <fire>) (<dmg-spell> 3) (<spell> <confusion>)
                       (<frequency> 1/5)))

(define-monster-kind "balrog-muar" "Muar"
  :title "the balrog"
  :numeric-id  483
  :x-attr (tile-file 24)
  :x-char (tile-number 53)
  :desc "A huge balrog surrounded by raging pillars of fire, Muar is indeed a  terrible opponent.  Wielding a great whip of fire and a blazing sword, his  fury blisters your skin and melts your flesh!"
  :text-char #\U
  :text-attr #\o
  :alignment '<evil>
  :type '(<demon> <unique>)
  :depth 50
  :rarity 3
  :hitpoints '(30 . 100)
  :armour 100
  :speed 120
  :xp 30000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<touch> :type <un-power> :damage nil)
	     (<crush> :type <hurt> :damage (8 . 12))
             (<hit> :type <fire> :damage (8 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <demon>) (<summon> <undead>) (<breath> <fire>) (<spell> <scare>) (<spell> <confusion>)
                       (<frequency> 1/4)))

(define-monster-kind "minotaur-baphomet" "Baphomet"
  :title "the minotaur lord"
  :numeric-id  490
  :x-attr (tile-file 24)
  :x-char (tile-number 54)
  :desc "A fearsome bull-headed demon, Baphomet swings a mighty axe as he curses  all that defy him."
  :text-char #\H
  :text-attr #\s
  :alignment '<evil>
  :type '(<unique>)
  :depth 51
  :rarity 4
  :hitpoints '(35 . 100)
  :armour 120
  :speed 130
  :xp 18000
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <fire>)
  :alertness 30
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (10 . 10))
	     (<hit> :type <hurt> :damage (10 . 10))
             (<butt> :type <hurt> :damage (12 . 13))
	     (<butt> :type <hurt> :damage (12 . 13)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<breath> <force>) (<ball-spell> <electricity>) (<bolt-spell> <plasma>)
                       (<bolt-spell> <mana>) (<arrow> 4) (<spell> <slow>) (<frequency> 1/6))) 

(define-monster-kind "harowen" "Harowen"
  :title "the black hand"
  :numeric-id  491
  :x-attr (tile-file 24)
  :x-char (tile-number 55)
  :desc "He is a master of disguise, an expert of stealth, a genius at traps, and  moves with blinding speed.  Check your pockets!"
  :text-char #\p
  :text-attr #\B
  :type '(<unique>)
  :depth 52
  :rarity 3
  :hitpoints '(25 . 100)
  :armour 90
  :speed 140
  :xp 20000
  :abilities '(<bash-door> <open-door> <pick-up-item> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison>)
  :alertness 0
  :vision 40
  :attacks '((<hit> :type <poison> :damage (8 . 5))
	     (<hit> :type <blind> :damage (10 . 5))
             (<touch> :type <eat-item> :damage (5 . 5))
	     (<touch> :type <eat-gold> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> <traps>) (<frequency> 1/6)))

(define-monster-kind "hoarmurath" "Hoarmurath"
  :title "of Dir"
  :numeric-id  492
  :x-attr (tile-file 24)
  :x-char (tile-number 56)
  :desc "A Ringwraith powerful in fell sorcery, he yearns for the life he has lost  for a life of everlasting torment."
  :text-char #\W
  :text-attr #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 52
  :rarity 3
  :hitpoints '(25 . 100)
  :armour 100
  :speed 120
  :xp 40000
  :abilities '(<push-others> <bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<wail> :type <terrify> :damage nil)
	     (<touch> :type <exp-80> :damage nil)
             (<hit> :type <hurt> :damage (5 . 5))
	     (<hit> :type <hurt> :damage (10 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <kin>) (<summon> <undead>) (<ball-spell> <nether>) (<ball-spell> <cold>)
                       (<bolt-spell> <cold>) (<spell> <mind-blast>) (<dmg-spell> 4) (<dmg-spell> 3)
                       (<spell> <scare>) (<spell> <paralysis>) (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "khamul" "Khamul"
  :title "the easterling"
  :numeric-id  494
  :x-attr (tile-file 24)
  :x-char (tile-number 57)
  :desc "A warrior-king of the East.  Khamul is a powerful opponent, his skill in combat awesome and his form twisted by evil cunning."
  :text-char #\W
  :text-attr #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 53
  :rarity 3
  :hitpoints '(35 . 100)
  :armour 100
  :speed 120
  :xp 50000
  :abilities '(<push-others> <bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold> <fire> <acid>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<touch> :type <exp-40> :damage nil)
	     (<touch> :type <exp-40> :damage nil)
             (<hit> :type <hurt> :damage (5 . 5))
	     (<hit> :type <hurt> :damage (10 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <kin>) (<summon> <undead>) (<ball-spell> <nether>) (<ball-spell> <cold>)
                       (<ball-spell> <fire>) (<bolt-spell> <mana>) (<dmg-spell> 4) (<dmg-spell> 3)
                       (<spell> <scare>) (<spell> <paralysis>) (<spell> <blindness>) (<spell> <teleport-level>)
                       (<frequency> 1/2)))

(define-monster-kind "phoenix" "Phoenix"
  :numeric-id  497
  :x-attr (tile-file 24)
  :x-char (tile-number 58)
  :desc "A massive glowing eagle bathed in flames.  The searing heat chars your  skin and melts your armour."
  :text-char #\B
  :text-attr #\r
  :type '(<animal> <unique>)
  :depth 54
  :rarity 3
  :hitpoints '(36 . 100)
  :armour 130
  :speed 120
  :xp 40000
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <fire> <acid>)
  :alertness 0
  :vision 60
  :attacks '((<hit> :type <fire> :damage (9 . 12))
	     (<hit> :type <fire> :damage (9 . 12))
             (<bite> :type <fire> :damage (12 . 6))
	     (<bite> :type <fire> :damage (12 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <plasma>) (<breath> <light>) (<breath> <fire>) (<ball-spell> <fire>)
                       (<bolt-spell> <plasma>) (<bolt-spell> <fire>) (<frequency> 1/3))) 

(define-monster-kind "hydra-lernean" "Lernean hydra"
  :numeric-id  504
  :x-attr (tile-file 24)
  :x-char (tile-number 59)
  :desc "A massive legendary hydra.  It has twelve powerful heads.  Its many eyes  stare at you as clouds of smoke and poisonous vapour rise from its  seething form."
  :text-char #\M
  :text-attr #\w
  :type '(<animal> <unique>)
  :depth 55
  :rarity 2
  :hitpoints '(45 . 100)
  :armour 140
  :speed 120
  :xp 20000
  :abilities '(<powerful-breath> <overrun-others> <bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <fire>)
  :alertness 20
  :vision 20
  :attacks '((<bite> :type <fire> :damage (12 . 6)) (<bite> :type <fire> :damage (12 . 6))
             (<bite> :type <poison> :damage (8 . 6)) (<bite> :type <poison> :damage (8 . 6)))
  :treasures '((<drop> "4d2") (<drop> "3d2") <only-drop-gold>)
  :special-abilities '((<summon> <hydra>) (<breath> <poison>) (<breath> <fire>) (<ball-spell> <poison>)
                       (<ball-spell> <fire>) (<bolt-spell> <plasma>) (<bolt-spell> <fire>)
                       (<spell> <scare>) (<frequency> 1/3))) 

(define-monster-kind "vampire-thuringwethil" "Thuringwethil"
  :numeric-id  505
  :x-attr (tile-file 24)
  :x-char (tile-number 60)
  :desc "Chief messenger between Sauron and Morgoth, she is surely the most deadly of her vampire race.  At first she is charming to meet, but her wings and  eyes give away her true form."
  :text-char #\V
  :text-attr #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 55
  :rarity 4
  :hitpoints '(40 . 100)
  :armour 145
  :speed 130
  :xp 23000
  :abilities '(<regenerate> <bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <confusion> :damage (6 . 6)) (<hit> :type <confusion> :damage (6 . 6))
             (<bite> :type <exp-80> :damage (6 . 6)) (<bite> :type <hurt> :damage (5 . 8)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<female>
  :special-abilities '((<summon> <kin>) (<ball-spell> <nether>) (<spell> <brain-smash>) (<spell> <drain-mana>)
                       (<dmg-spell> 4) (<dmg-spell> 3) (<spell> <scare>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "dwarf-fundin" "Fundin Bluecloak"
  :numeric-id  508
  :x-attr (tile-file 24)
  :x-char (tile-number 61)
  :desc "He is one of the greatest dwarven priests to walk the earth.  Fundin has  earned a high position in the church, and his skill with both weapon and  spell only justify his position further.  His combination of both dwarven  strength and priestly wisdom are a true match for any adventurer."
  :text-char #\h
  :text-attr #\G
  :type '(<unique>)
  :depth 56
  :rarity 2
  :hitpoints '(50 . 100)
  :armour 195
  :speed 130
  :xp 20000
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 10
  :vision 25
  :attacks '((<hit> :type <hurt> :damage (8 . 6))
	     (<hit> :type <hurt> :damage (8 . 6))
             (<hit> :type <hurt> :damage (8 . 6))
	     (<hit> :type <hurt> :damage (10 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <monsters>) (<spell> <forget>) (<spell> <brain-smash>) (<dmg-spell> 4)
                       (<dmg-spell> 3) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<spell> <heal>) (<frequency> 1/4))) 

(define-monster-kind "angel-uriel" "Uriel"
  :title "angel of fire"
  :numeric-id  509
  :x-attr (tile-file 24)
  :x-char (tile-number 62)
  :desc "A creature of godly appearance, you dare not challenge Uriel's supremacy.   Those who stood against him before are but a memory, cremated by his  mastery of elemental fire."
  :text-char #\A
  :text-attr #\r
  :type '(<unique>)
  :depth 56
  :rarity 3
  :hitpoints '(55 . 100)
  :armour 160
  :speed 130
  :xp 25000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <pick-up-item> <smart> <max-hitpoints>
               <initial-sleeper>)
  :immunities '(<poison> <electricity> <cold> <fire> <acid>)
  :alertness 10
  :vision 40
  :attacks '((<hit> :type <hurt> :damage (10 . 10))
	     (<hit> :type <hurt> :damage (10 . 10))
             (<hit> :type <fire> :damage (4 . 6))
	     (<hit> :type <fire> :damage (9 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <angel>) (<breath> <fire>) (<ball-spell> <fire>) (<bolt-spell> <mana>)
                       (<bolt-spell> <fire>) (<spell> <blindness>) (<spell> <teleport-player>) (<frequency> 1/2)))

(define-monster-kind "angel-azriel" "Azriel"
  :title "angel of death"
  :numeric-id  510
  :x-attr (tile-file 24)
  :x-char (tile-number 63)
  :desc "Azriel commands awesome power, his visage holy enough to shrivel your  soul.  You shriek with disbelief as his mastery of death draws you to your  grave.  It is truly beyond all but the mightiest of warriors to stand  against him and live."
  :text-char #\A
  :text-attr #\D
  :type '(<unique>)
  :depth 57
  :rarity 3
  :hitpoints '(60 . 100)
  :armour 170
  :speed 130
  :xp 30000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <pick-up-item> <smart> <max-hitpoints>
               <initial-sleeper>)
  :immunities '(<poison> <electricity> <cold> <fire> <acid>)
  :alertness 10
  :vision 40
  :attacks '((<hit> :type <hurt> :damage (10 . 10))
	     (<hit> :type <hurt> :damage (10 . 10))
             (<hit> :type <blind> :damage (10 . 5))
	     (<touch> :type <exp-80> :damage nil))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <angel>) (<breath> <nether>) (<ball-spell> <nether>) (<bolt-spell> <nether>)
                       (<bolt-spell> <mana>) (<spell> <blindness>) (<spell> <teleport-player>) (<frequency> 1/2))) 

(define-monster-kind "dragon-ancalagon" "Ancalagon"
  :title "the black"
  :numeric-id  511
  :x-attr (tile-file 24)
  :x-char (tile-number 64)
  :desc "'Rushing Jaws' is his name, and death is his game.  No dragon of the brood of Glaurung can match him."
  :text-char #\D
  :text-attr #\D
  :alignment '<evil>
  :type '(<dragon> <unique>)
  :depth 58
  :rarity 3
  :hitpoints '(75 . 100)
  :armour 125
  :speed 120
  :xp 30000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire> <acid>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (10 . 14))
	     (<claw> :type <hurt> :damage (8 . 12))
             (<claw> :type <hurt> :damage (6 . 12))
	     (<claw> :type <hurt> :damage (5 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <high-dragon>) (<summon> <dragon>) (<breath> <fire>) (<breath> <acid>)
                       (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/2)))

(define-monster-kind "angel-gabriel" "Gabriel"
  :title "the Messenger"
  :numeric-id  513
  :x-attr (tile-file 24)
  :x-char (tile-number 65)
  :desc "Commanding a legion of angels, Gabriel will destroy you for your sins.  He  will crush you like the pitiful insignificant being he sees you to be.   Your very soul will be taken into judgement by his supreme authority as he  cleanses the world of evil."
  :text-char #\A
  :text-attr #\w
  :type '(<unique>)
  :depth 59
  :rarity 3
  :hitpoints '(75 . 100)
  :armour 180
  :speed 130
  :xp 35000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <pick-up-item> <smart> <max-hitpoints>
               <initial-sleeper>)
  :immunities '(<poison> <electricity> <cold> <fire> <acid>)
  :alertness 10
  :vision 40
  :attacks '((<hit> :type <hurt> :damage (10 . 10))
	     (<hit> :type <hurt> :damage (10 . 10))
             (<hit> :type <fire> :damage (4 . 6))
	     (<hit> :type <un-bonus> :damage (6 . 8)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <kin>) (<summon> <angel>) (<bolt-spell> <mana>) (<spell> <blindness>)
                       (<spell> <teleport-player>) (<frequency> 1/2))) 

(define-monster-kind "saruman" "Saruman"
  :title "of many colours"
  :numeric-id  514
  :x-attr (tile-file 24)
  :x-char (tile-number 66)
  :desc "Originally known as the White, Saruman fell prey to Sauron's wiles.  He seeks to emulate him and breeds orcs and trolls to fight for him.  He searches forever for the One Ring, to become a mighty Sorcerer-King of the world."
  :text-char #\p
  :text-attr #\v
  :alignment '<evil>
  :type '(<unique>)
  :depth 60
  :rarity 1
  :hitpoints '(50 . 100)
  :armour 100
  :speed 120
  :xp 35000
  :abilities '(<bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :alertness 0
  :vision 100
  :attacks '((<hit> :type <hurt> :damage (5 . 5))
	     (<hit> :type <hurt> :damage (5 . 5))
             (<hit> :type <un-bonus> :damage (6 . 8))
	     (<hit> :type <un-bonus> :damage (6 . 8)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <dragon>) (<summon> <demon>) (<summon> <undead>) (<ball-spell> <water>)
                       (<ball-spell> <cold>) (<ball-spell> <fire>) (<ball-spell> <acid>)
                       (<bolt-spell> <cold>) (<spell> <traps>) (<spell> <forget>) (<spell> <mind-blast>)
                       (<dmg-spell> 4) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<spell> <teleport-away>) (<spell> <teleport>) (<spell> <haste>) (<spell> <heal>)
                       (<frequency> 1/2))) 

(define-monster-kind "cat-lord" "Cat Lord"
  :numeric-id  516
  :x-attr (tile-file 24)
  :x-char (tile-number 67)
  :desc "Master of all things feline, the Cat Lord moves with catlike stealth."
  :text-char #\f
  :text-attr #\r
  :type '(<unique>)
  :depth 64
  :rarity 3
  :hitpoints '(48 . 100)
  :armour 200
  :speed 130
  :xp 30000
  :abilities '(<bash-door> <open-door> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold> <fire>)
  :alertness 0
  :vision 100
  :attacks '((<hit> :type <paralyse> :damage (15 . 1))
	     (<hit> :type <blind> :damage (10 . 5))
             (<touch> :type <lose-dex> :damage (2 . 12))
	     (<hit> :type <confusion> :damage (12 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> <teleport-player>) (<frequency> 1/3)))

(define-monster-kind "tselakus" "Tselakus"
  :title "the Dreadlord"
  :numeric-id  522
  :x-attr (tile-file 24)
  :x-char (tile-number 68)
  :desc "This huge affront to existence twists and tears at the fabric of space.  A  master of mighty magic, Tselakus hungers for your tender flesh.  Darkness  itself recoils from the touch of Tselakus as he leaves a trail of death  and destruction.  Tselakus is a being of sneering contempt, laughing at  your pitiful efforts to defy him.  Mighty claws rend reality as he  annihilates all in his path to your soul!"
  :text-char #\G
  :text-attr #\r
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 68
  :rarity 2
  :hitpoints '(65 . 100)
  :armour 150
  :speed 130
  :xp 35000
  :abilities '(<pass-wall> <cold-blood> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <lose-str> :damage (4 . 6))
	     (<hit> :type <lose-str> :damage (4 . 6))
             (<hit> :type <hurt> :damage (10 . 10))
	     (<hit> :type <hurt> :damage (10 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <kin>) (<summon> <high-undead>) (<summon> <wraith>) (<ball-spell> <nether>)
                       (<ball-spell> <darkness>) (<spell> <confusion>) (<spell> <paralysis>) (<spell> <blindness>)
                       (<frequency> 1/3)))

(define-monster-kind "tiamat" "Tiamat"
  :title "Celestial dragon of evil"
  :numeric-id  523
  :x-attr (tile-file 24)
  :x-char (tile-number 69)
  :desc "Usually found guarding the first plane of Hell, Tiamat is a formidable  opponent, her five heads breathing death to all who stand against her."
  :text-char #\D
  :text-attr #\v
  :alignment '<evil>
  :type '(<dragon> <unique>)
  :depth 70
  :rarity 4
  :hitpoints '(100 . 100)
  :armour 125
  :speed 130
  :xp 45000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>
               <colour-changing>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (10 . 14))
	     (<claw> :type <hurt> :damage (8 . 12))
             (<claw> :type <hurt> :damage (8 . 12))
	     (<claw> :type <hurt> :damage (6 . 12)))
  :treasures '(<drop-great> <drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<female>
  :special-abilities '((<summon> <high-dragon>) (<breath> <poison>) (<breath> <electricity>) (<breath> <cold>)
                       (<breath> <fire>) (<breath> <acid>) (<spell> <scare>) (<spell> <confusion>)
                       (<spell> <blindness>) (<frequency> 1/2)))

(define-monster-kind "vecna" "Vecna"
  :title "the Emperor lich"
  :numeric-id  528
  :x-attr (tile-file 24)
  :x-char (tile-number 70)
  :desc "He is a highly cunning, extremely magical being, spoken of in legends.   This ancient shadow of death wilts any living thing it passes."
  :text-char #\L
  :text-attr #\R
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 72
  :rarity 2
  :hitpoints '(50 . 100)
  :armour 85
  :speed 130
  :xp 30000
  :abilities '(<bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :alertness 50
  :vision 20
  :attacks '((<touch> :type <lose-dex> :damage (2 . 12)) (<touch> :type <lose-dex> :damage (2 . 12))
             (<touch> :type <un-power> :damage nil) (<touch> :type <exp-80> :damage nil))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <undead>) (<summon> <monsters>) (<ball-spell> <nether>) (<bolt-spell> <mana>)
                       (<ball-spell> <mana>) (<spell> <traps>) (<spell> <brain-smash>) (<dmg-spell> 4)
                       (<dmg-spell> 3) (<spell> <scare>) (<spell> <confusion>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<spell> <teleport-player>) (<spell> <blink>) (<frequency> 1/2)))

(define-monster-kind "omarax" "Omarax"
  :title "the eye tyrant"
  :numeric-id  529
  :x-attr (tile-file 24)
  :x-char (tile-number 71)
  :desc "A disembodied eye, floating in the air.  His gaze seems to shred your  soul and his spells crush your will.  He is ancient, his history steeped  in forgotten evils, his atrocities numerous and sickening."
  :text-char #\e
  :text-attr #\v
  :alignment '<evil>
  :type '(<unique>)
  :depth 73
  :rarity 4
  :hitpoints '(65 . 100)
  :armour 80
  :speed 130
  :xp 16000
  :abilities '(<bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison>)
  :alertness 10
  :vision 30
  :attacks '((<gaze> :type <lose-int> :damage (2 . 6))
	     (<gaze> :type <un-power> :damage (2 . 6))
             (<gaze> :type <paralyse> :damage (2 . 6))
	     (<gaze> :type <exp-40> :damage (2 . 6)))
  :gender '<male>
  :special-abilities '((<summon> <kin>) (<bolt-spell> <cold>) (<bolt-spell> <fire>) (<bolt-spell> <acid>)
                       (<ball-spell> <darkness>) (<spell> <darkness>) (<spell> <forget>) (<spell> <mind-blast>)
                       (<spell> <drain-mana>) (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>)
                       (<spell> <blindness>) (<frequency> 1/2)))

(define-monster-kind "spider-ungoliant" "Ungoliant"
  :title "the Unlight"
  :numeric-id  530
  :x-attr (tile-file 24)
  :x-char (tile-number 72)
  :desc "This enormous, hideous spirit of void is in the form of a spider of immense proportions.  She is surrounded by a cloud of Unlight as she sucks  in all living light into her bloated body.  She is always ravenously  hungry and would even eat herself to avoid starvation.  She is rumoured to  have a foul and deadly breath."
  :text-char #\S
  :text-attr #\D
  :alignment '<evil>
  :type '(<animal> <unique>)
  :depth 75
  :rarity 1
  :hitpoints '(130 . 100)
  :armour 160
  :speed 120
  :xp 35000
  :abilities '(<bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison>)
  :vulnerabilities '(<light>)
  :alertness 80
  :vision 8
  :attacks '((<sting> :type <poison> :damage (2 . 5))
	     (<sting> :type <poison> :damage (2 . 5))
             (<bite> :type <poison> :damage (3 . 9))
	     (<bite> :type <poison> :damage (3 . 9)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<female>
  :special-abilities '((<summon> <spider>) (<breath> <darkness>) (<breath> <poison>) (<ball-spell> <darkness>)
                       (<spell> <darkness>) (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>)
                       (<spell> <blindness>) (<spell> <heal>) (<frequency> 1/3)))

(define-monster-kind "mouth-sauron" "Mouth of Sauron"
  :numeric-id  532
  :x-attr (tile-file 24)
  :x-char (tile-number 73)
  :desc "The Mouth of Sauron is a mighty spell caster.  So old that even he cannot  remember his own name, his power and evil are undeniable.  He believes  unshakeably that he is unbeatable and laughs as he weaves his awesome  spells."
  :text-char #\p
  :text-attr #\v
  :alignment '<evil>
  :type '(<unique>)
  :depth 78
  :rarity 3
  :hitpoints '(70 . 100)
  :armour 100
  :speed 130
  :xp 38000
  :abilities '(<bash-door> <open-door> <invisible> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <electricity> <cold> <fire>)
  :alertness 10
  :vision 60
  :attacks '((<touch> :type <un-power> :damage nil)
	     (<touch> :type <un-power> :damage nil)
             (<hit> :type <un-bonus> :damage (6 . 8))
	     (<hit> :type <un-bonus> :damage (6 . 8)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<ball-spell> <nether>) (<ball-spell> <water>) (<ball-spell> <fire>)
                       (<ball-spell> <mana>) (<ball-spell> <darkness>) (<bolt-spell> <plasma>)
                       (<spell> <traps>) (<dmg-spell> 3) (<spell> <paralysis>) (<spell> <teleport-player>)
                       (<frequency> 1/2)))

(define-monster-kind "quylthulg-emperor" "Emperor quylthulg"
  :numeric-id  533
  :x-attr (tile-file 24)
  :x-char (tile-number 74)
  :desc "A huge seething mass of flesh with a rudimentary intelligence, the Emperor  Quylthulg changes colours in front of your eyes.  Pulsating first one  colour then the next, it knows only it must bring help to protect itself."
  :text-char #\Q
  :text-attr #\w
  :alignment '<evil>
  :type '(<animal> <unique>)
  :depth 78
  :rarity 3
  :hitpoints '(50 . 100)
  :armour 1
  :speed 130
  :xp 20000
  :abilities '(<invisible> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 30
  :treasures '((<drop> "4d2") <only-drop-items>)
  :special-abilities '((<summon> <high-demon>) (<summon> <high-dragon>) (<summon> <high-undead>)
                       (<spell> <brain-smash>) (<frequency> 1/2)))

(define-monster-kind "qlzqqlzuup" "Qlzqqlzuup"
  :title "the Lord of flesh"
  :numeric-id  534
  :x-attr (tile-file 24)
  :x-char (tile-number 75)
  :desc "This disgusting creature squeals and snorts as it writhes on the floor.   It pulsates with evil.  Its intent is to overwhelm you with monster after monster, until it can greedily dine on your remains."
  :text-char #\Q
  :text-attr #\o
  :alignment '<evil>
  :type '(<animal> <unique>)
  :depth 78
  :rarity 3
  :hitpoints '(50 . 100)
  :armour 1
  :speed 130
  :xp 20000
  :abilities '(<invisible> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion>)
  :alertness 0
  :vision 30
  :treasures '((<drop> "4d2") <only-drop-items>)
  :special-abilities '((<summon> <kin>) (<summon> <high-demon>) (<summon> <high-dragon>) (<summon> <high-undead>)
                       (<summon> <wraith>) (<summon> <unique>) (<summon> <hound>) (<summon> <ant>) (<summon> <spider>)
                       (<summon> <hydra>) (<summon> <angel>) (<summon> <dragon>) (<summon> <demon>) (<summon> <undead>)
                       (<summon> <monsters>) (<summon> <monster>) (<frequency> 1))) 

(define-monster-kind "murazor" "Murazor"
  :title "the Witch-king of Angmar"
  :numeric-id  535
  :x-attr (tile-file 24)
  :x-char (tile-number 76)
  :desc "The Chief of the Ringwraiths.  A fell being of devastating power.  His  spells are lethal and his combat blows crushingly hard.  He moves at speed, and commands legions of evil to do his bidding.  It is said that he is fated never to die by the hand of mortal man."
  :text-char #\W
  :text-attr #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 80
  :rarity 3
  :hitpoints '(60 . 100)
  :armour 120
  :speed 130
  :xp 42000
  :abilities '(<push-others> <bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<hit> :type <exp-80> :damage (5 . 5))
	     (<hit> :type <exp-80> :damage (5 . 5))
             (<hit> :type <hurt> :damage (10 . 10))
	     (<hit> :type <hurt> :damage (10 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <kin>) (<summon> <high-demon>) (<summon> <high-dragon>) (<summon> <high-undead>)
                       (<summon> <wraith>) (<summon> <monsters>) (<ball-spell> <nether>) (<bolt-spell> <mana>)
                       (<spell> <brain-smash>) (<dmg-spell> 3) (<spell> <scare>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<spell> <teleport-away>) (<frequency> 1/2))) 

(define-monster-kind "pazuzu" "Pazuzu"
  :title "Lord of air"
  :numeric-id  536
  :x-attr (tile-file 24)
  :x-char (tile-number 77)
  :desc "A winged humanoid from the Planes of Hell, Pazuzu grins inhumanely at you  as he decides your fate."
  :text-char #\B
  :text-attr #\w
  :alignment '<evil>
  :type '(<demon> <unique>)
  :depth 82
  :rarity 2
  :hitpoints '(55 . 100)
  :armour 125
  :speed 140
  :xp 30000
  :abilities '(<bash-door> <open-door> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 10
  :vision 40
  :attacks '((<hit> :type <electricity> :damage (12 . 12))
	     (<hit> :type <electricity> :damage (12 . 12))
             (<hit> :type <electricity> :damage (12 . 12))
	     (<hit> :type <electricity> :damage (12 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <high-demon>) (<ball-spell> <electricity>) (<bolt-spell> <mana>)
                       (<bolt-spell> <electricity>) (<spell> <mind-blast>) (<frequency> 1/3))) 

(define-monster-kind "cantoras" "Cantoras"
  :title "the skeletal Lord"
  :numeric-id  538
  :x-attr (tile-file 24)
  :x-char (tile-number 78)
  :desc "A legion of evil undead druj animating the skeleton of a once mighty sorcerer.  His power is devastating and his speed unmatched in the underworld.  Flee his wrath!"
  :text-char #\s
  :text-attr #\w
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 84
  :rarity 2
  :hitpoints '(75 . 100)
  :armour 120
  :speed 140
  :xp 45000
  :abilities '(<bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <cold> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<touch> :type <poison> :damage (3 . 5))
	     (<touch> :type <poison> :damage (3 . 5))
             (<gaze> :type <exp-80> :damage nil)
	     (<gaze> :type <exp-80> :damage nil))
  :treasures '(<drop-great> <drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <high-undead>) (<ball-spell> <nether>) (<ball-spell> <water>)
                       (<bolt-spell> <mana>) (<bolt-spell> <cold>) (<spell> <brain-smash>) (<dmg-spell> 4)
                       (<spell> <scare>) (<spell> <slow>) (<spell> <teleport-player>) (<frequency> 1))) 

(define-monster-kind "tarrasque" "Tarrasque"
  :numeric-id  539
  :x-attr (tile-file 24)
  :x-char (tile-number 79)
  :desc "The Tarrasque is a massive reptile of legend, rumoured to be unkillable  and immune to magic.  Fear its anger, for its devastation is unmatched!"
  :text-char #\R
  :text-attr #\v
  :alignment '<evil>
  :type '(<unique>)
  :depth 84
  :rarity 2
  :hitpoints '(85 . 100)
  :armour 185
  :speed 130
  :xp 35000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>
               <colour-changing>)
  :immunities '(<sleep> <confusion> <cold> <fire>)
  :alertness 20
  :vision 50
  :attacks '((<touch> :type <un-power> :damage nil)
	     (<touch> :type <un-power> :damage nil)
             (<hit> :type <hurt> :damage (10 . 10))
	     (<hit> :type <hurt> :damage (10 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <disenchant>) (<breath> <cold>) (<breath> <fire>) (<frequency> 1/2)))

(define-monster-kind "balrog-lungorthin" "Lungorthin"
  :title "the balrog of white fire"
  :numeric-id  540
  :x-attr (tile-file 24)
  :x-char (tile-number 80)
  :desc "A massive form cloaked in flame.  Lungorthin stares balefully at you with eyes that smoulder red.  The dungeon floor where he stands is scorched by the heat of his body."
  :text-char #\U
  :text-attr #\w
  :alignment '<evil>
  :type '(<demon> <unique>)
  :depth 85
  :rarity 2
  :hitpoints '(80 . 100)
  :armour 125
  :speed 130
  :xp 37000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<touch> :type <un-power> :damage nil)
	     (<crush> :type <hurt> :damage (8 . 12))
             (<hit> :type <fire> :damage (8 . 12))
	     (<hit> :type <fire> :damage (8 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <high-demon>) (<summon> <high-undead>) (<summon> <demon>) (<breath> <fire>)
                       (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/4)))

(define-monster-kind "wolf-draugluin" "Draugluin"
  :title "Sire of all werewolves"
  :numeric-id  541
  :x-attr (tile-file 24)
  :x-char (tile-number 81)
  :desc "Draugluin provides Sauron with a fearsome personal guard.  He is an enormous wolf inhabited with a human spirit.  He is chief of all his kind."
  :text-char #\C
  :text-attr #\u
  :alignment '<evil>
  :type '(<animal> <unique>)
  :depth 87
  :rarity 2
  :hitpoints '(70 . 100)
  :armour 90
  :speed 130
  :xp 40000
  :abilities '(<push-others> <bash-door> <open-door> <pick-up-item> (<random-mover> 1/4) <max-hitpoints>
               <initial-sleeper>)
  :immunities '(<poison>)
  :alertness 90
  :vision 80
  :attacks '((<bite> :type <poison> :damage (2 . 6))
	     (<bite> :type <poison> :damage (2 . 6))
             (<claw> :type <hurt> :damage (3 . 3))
	     (<claw> :type <hurt> :damage (3 . 3)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <hound>) (<summon> <monsters>) (<spell> <scare>) (<frequency> 1/3))) 

(define-monster-kind "feagwath" "Feagwath"
  :title "the undead sorceror"
  :numeric-id  542
  :x-attr (tile-file 24)
  :x-char (tile-number 82)
  :desc "A stench of corruption and decay surrounds this sorcerer, who has clearly  risen from the grave to continue his foul plots and schemes."
  :text-char #\L
  :text-attr #\R
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 90
  :rarity 3
  :hitpoints '(60 . 100)
  :armour 100
  :speed 130
  :xp 45000
  :abilities '(<bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :alertness 0
  :vision 100
  :attacks '((<hit> :type <hurt> :damage (5 . 5))
	     (<hit> :type <hurt> :damage (5 . 5))
             (<hit> :type <un-bonus> :damage (6 . 8))
	     (<hit> :type <un-bonus> :damage (6 . 8)))
  :treasures '(<drop-great> <drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <kin>) (<summon> <high-undead>) (<summon> <demon>) (<summon> <monsters>)
                       (<ball-spell> <fire>) (<bolt-spell> <mana>) (<ball-spell> <mana>)
                       (<spell> <brain-smash>) (<dmg-spell> 4) (<spell> <scare>) (<spell> <blindness>)
                       (<spell> <teleport>) (<frequency> 1/3)))

(define-monster-kind "wolf-carcharoth" "Carcharoth"
  :title "the jaws of thirst"
  :numeric-id  543
  :x-attr (tile-file 24)
  :x-char (tile-number 83)
  :desc "The first guard of Angband, Carcharoth, also known as 'The Red Maw', is the largest wolf to ever walk the earth.  He is highly intelligent and a deadly opponent in combat."
  :text-char #\C
  :text-attr #\D
  :alignment '<evil>
  :type '(<animal> <unique>)
  :depth 92
  :rarity 2
  :hitpoints '(75 . 100)
  :armour 110
  :speed 130
  :xp 40000
  :abilities '(<push-others> <bash-door> <open-door> <pick-up-item> <smart> (<random-mover> 1/4) <max-hitpoints>
               <initial-sleeper>)
  :immunities '(<sleep> <confusion> <poison> <fire>)
  :alertness 10
  :vision 80
  :attacks '((<bite> :type <poison> :damage (4 . 4))
	     (<bite> :type <poison> :damage (4 . 4))
             (<claw> :type <hurt> :damage (3 . 3))
	     (<claw> :type <hurt> :damage (3 . 3)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <hound>) (<breath> <fire>) (<spell> <brain-smash>) (<spell> <scare>) (<spell> <heal>)
                       (<frequency> 1/4)))

(define-monster-kind "cerberus" "Cerberus"
  :title "Guardian of Hades"
  :numeric-id  544
  :x-attr (tile-file 24)
  :x-char (tile-number 84)
  :desc "A two-headed hell hound of fearsome aspect.  Flame burns merrily from its hide as it snarls and roars its defiance."
  :text-char #\C
  :text-attr #\r
  :alignment '<evil>
  :type '(<animal> <unique>)
  :depth 94
  :rarity 1
  :hitpoints '(100 . 100)
  :armour 160
  :speed 130
  :xp 40000
  :abilities '(<push-others> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <fire>)
  :alertness 10
  :vision 50
  :attacks '((<hit> :type <fire> :damage (9 . 12))
	     (<hit> :type <fire> :damage (9 . 12))
             (<hit> :type <fire> :damage (9 . 12))
	     (<hit> :type <fire> :damage (9 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :special-abilities '((<summon> <hound>) (<breath> <nether>) (<breath> <fire>) (<ball-spell> <darkness>)
                       (<frequency> 1/3)))

(define-monster-kind "balrog-gothmog" "Gothmog"
  :title "the High Captain of balrogs"
  :numeric-id  545
  :x-attr (tile-file 24)
  :x-char (tile-number 85)
  :desc "Gothmog is the Chief Balrog in Morgoth's personal guard.  He is renowned for slaying Ecthelion the Warder of the Gates and he has never been defeated in combat.  With his whip of flame and awesome fiery breath he saved his master from Ungoliant's rage."
  :text-char #\U
  :text-attr #\R
  :alignment '<evil>
  :type '(<demon> <unique>)
  :depth 95
  :rarity 1
  :hitpoints '(80 . 100)
  :armour 140
  :speed 130
  :xp 43000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confusion> <electricity> <fire>)
  :alertness 0
  :vision 100
  :attacks '((<touch> :type <un-power> :damage nil)
	     (<crush> :type <hurt> :damage (8 . 12))
             (<hit> :type <fire> :damage (9 . 12))
	     (<hit> :type <fire> :damage (9 . 12)))
  :treasures '(<drop-great> <drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <high-undead>) (<summon> <high-demon>) (<breath> <fire>) (<spell> <scare>)
                       (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/3)))

(define-monster-kind "sauron" "Sauron" ;; no title, he's sacry enough
  :numeric-id  546
  :x-attr (tile-file 17)
  :x-char (tile-number 95)
  :desc "He is Morgoth's most powerful servant.  Mighty in spells and enchantments, he created the One Ring.  His eyes glow with power and his gaze seeks to destroy your soul.  He has many servants, and rarely fights without them."
  :text-char #\p
  :text-attr #\v
  :alignment '<evil>
  :type '(<quest-monster> <unique>)
  :depth 99
  :rarity 1
  :hitpoints '(105 . 100)
  :armour 160
  :speed 130
  :xp 50000
  :abilities '(<regenerate> <push-others> <bash-door> <open-door> <smart> <only-on-set-depth> <max-hitpoints>
               <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire>)
  :alertness 0
  :vision 100
  :attacks '((<touch> :type <un-power> :damage (8 . 12))
	     (<touch> :type <un-power> :damage (8 . 12))
             (<hit> :type <un-bonus> :damage (10 . 12))
	     (<hit> :type <un-bonus> :damage (10 . 12)))
  :treasures '(<drop-great> <drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <high-dragon>) (<summon> <high-undead>)
		       (<summon> <high-demon>) (<summon> <monsters>)
                       (<ball-spell> <darkness>) (<ball-spell> <nether>) (<ball-spell> <water>)
                       (<ball-spell> <fire>) (<ball-spell> <mana>) (<bolt-spell> <plasma>)
                       (<bolt-spell> <mana>) (<bolt-spell> <cold>) (<spell> <forget>) (<spell> <brain-smash>)
                       (<dmg-spell> 4) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<spell> <teleport-level>) (<spell> <teleport>) (<frequency> 1/2)))

(define-monster-kind "morgoth" "Morgoth"
  :title "Lord of Darkness"
  :numeric-id  547
  :x-attr (tile-file 18)
  :x-char (tile-number 71)
  :desc "He is the Master of the Pits of Angband.  His figure is like a black mountain crowned with Lightning.  He rages with everlasting anger, his body scarred by Fingolfin's eight mighty wounds.  He can never rest from his pain, but seeks forever to dominate all that is light and good in the world.  He is the origin of man's fear of darkness and created many foul creatures with his evil powers.  Orcs, Dragons, and Trolls are his most foul corruptions, causing much pain and suffering in the world to please him.  His disgusting visage, twisted with evil, is crowned with iron, the two remaining Silmarils forever burning him.  Grond, the mighty Hammer of  the Underworld, cries defiance as he strides towards you to crush you to a pulp!"
  :text-char #\P
  :text-attr #\D
  :alignment '<evil>
  :type '(<quest-monster> <unique>)
  :depth 100
  :rarity 1
  :hitpoints '(200 . 100)
  :armour 150
  :speed 140
  :xp 60000
  :abilities '(<regenerate> <push-others> <destroy-wall> <smart>
	       <only-on-set-depth> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confusion> <poison> <electricity> <cold> <fire> <acid>)
  :alertness 0
  :vision 100
  :attacks '((<touch> :type <un-power> :damage nil)
	     (<hit> :type <lose-all> :damage (10 . 12))
             (<hit> :type <shatter> :damage (20 . 10))
	     (<hit> :type <shatter> :damage (20 . 10)))
  :treasures '(<drop-planned> <drop-great> <drop-good>
	       (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") (<drop> "1d2")
               <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <high-demon>) (<summon> <high-dragon>)
		       (<summon> <high-undead>) (<summon> <wraith>)
                       (<summon> <unique>) (<summon> <monsters>)
		       (<ball-spell> <nether>) (<bolt-spell> <mana>)
                       (<ball-spell> <mana>) (<spell> <brain-smash>) (<frequency> 1/3))) 


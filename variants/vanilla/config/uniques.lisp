;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/uniques.lisp - unique monsters for vanilla variant
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(define-monster-kind "hobbit-maggott" "Farmer Maggot"
  :desc "He's lost his dogs.  He's had his mushrooms stolen.  He's not a happy hobbit!"
  :symbol #\h
  :colour #\w
  :type '(<unique>)
  :depth 0
  :rarity 4
  :hitpoints '(35 . 10)
  :armour 10
  :speed 110
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<sleep> <confuse>)
  :alertness 3
  :vision 40
  :attacks '((<moan> :type nil :damage nil) (<moan> :type nil :damage nil))
  :treasures '(<drop-good> (<drop-chance> 9/10) <only-drop-items>)
  :gender '<male>)

(define-monster-kind "dog-fang" "Fang, Farmer Maggot's dog"
  :desc "A rather vicious dog belonging to Farmer Maggot.  It thinks you are stealing mushrooms."
  :symbol #\C
  :colour #\U
  :type '(<animal> <unique>)
  :depth 2
  :rarity 1
  :hitpoints '(5 . 5)
  :armour 30
  :speed 120
  :xp 30
  :abilities '(<bash-door> (<random-mover> 1/4) <max-hitpoints>)
  :immunities '(<sleep> <confuse>)
  :alertness 0
  :vision 30
  :attacks '((<bite> :type <hurt> :damage (1 . 4)))) 

(define-monster-kind "dog-grip" "Grip, Farmer Maggot's dog"
  :desc "A rather vicious dog belonging to Farmer Maggot.  It thinks you are stealing mushrooms."
  :symbol #\C
  :colour #\U
  :type '(<animal> <unique>)
  :depth 2
  :rarity 1
  :hitpoints '(5 . 5)
  :armour 30
  :speed 120
  :xp 30
  :abilities '(<bash-door> (<random-mover> 1/4) <max-hitpoints>)
  :immunities '(<sleep> <confuse>)
  :alertness 0
  :vision 30
  :attacks '((<bite> :type <hurt> :damage (1 . 4)))) 

(define-monster-kind "smeagol" "Smeagol"
  :desc "He's been sneaking, and he wants his 'precious.'"
  :symbol #\h
  :colour #\b
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
  :attacks '((<touch> :type <eat-gold> :damage nil) (<hit> :type <hurt> :damage (1 . 4)))
  :treasures '((<drop> "1d2") (<drop-chance> 3/5) <only-drop-gold>)
  :gender '<male>) 

(define-monster-kind "hobbit-bullroarer" "Bullroarer the hobbit"
  :desc "He is a sturdy hobbit who is renowned for his unusual strength and vigour.  He can prove a troublesome opponent."
  :symbol #\h
  :colour #\b
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
  :attacks '((<touch> :type <eat-gold> :damage nil) (<touch> :type <eat-item> :damage nil)
             (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "kobold-mughash" "Mughash the kobold lord"
  :desc "Strong and powerful, for a kobold."
  :symbol #\k
  :colour #\b
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

(define-monster-kind "wormtongue" "Wormtongue, Agent of Saruman"
  :desc "He's been spying for Saruman.  He is a snivelling wretch with no morals and disgusting habits."
  :symbol #\p
  :colour #\b
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
  :special-abilities '((<spell> (<ball> <poison>)) (<spell> (<bolt> <cold>)) (<spell> <traps>) (<spell> <slow>)
                       (<spell> <heal>) (<frequency> 1/5))) 

(define-monster-kind "orc-lagduf" "Lagduf, the Snaga"
  :desc "A captain of a regiment of weaker orcs, Lagduf keeps his troop in order with displays of excessive violence."
  :symbol #\o
  :colour #\o
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

(define-monster-kind "brodda" "Brodda, the easterling"
  :desc "A nasty piece of work, Brodda picks on defenseless women and children."
  :symbol #\p
  :colour #\u
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

(define-monster-kind "yeek-orfax" "Orfax, son of Boldor"
  :desc "He's just like daddy!  He knows mighty spells, but fortunately he is a  yeek."
  :symbol #\y
  :colour #\b
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

(define-monster-kind "orc-grishnakh" "Grishnakh, the hill-orc"
  :desc "He is a cunning and devious orc with a chaotic nature."
  :symbol #\o
  :colour #\U
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

(define-monster-kind "castamir" "Castamir the Usurper"
  :desc "A Black Numenorean who usurped the throne of Gondor, he is treacherous and  evil."
  :symbol #\p
  :colour #\R
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
  :special-abilities '((<spell> (<bolt> <ice>)) (<spell> (<bolt> <lightning>)) (<spell> (<bolt> <cold>))
                       (<spell> (<bolt> <fire>)) (<spell> <traps>) (<spell> <heal>) (<frequency> 1/2))) 

(define-monster-kind "elemental-vargo" "Vargo, Tyrant of Fire"
  :desc "A towering fire elemental, Vargo burns everything beyond recognition."
  :symbol #\E
  :colour #\r
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
  :immunities '(<sleep> <confuse> <poison> <fire>)
  :alertness 50
  :vision 12
  :attacks '((<hit> :type <fire> :damage (4 . 6)) (<hit> :type <fire> :damage (4 . 6))
             (<hit> :type <fire> :damage (4 . 6)) (<hit> :type <fire> :damage (4 . 6)))
  :special-abilities '((<spell> (<ball> <fire>)) (<spell> (<bolt> <plasma>)) (<frequency> 1/4))) 

(define-monster-kind "elemental-waldern" "Waldern, King of Water"
  :desc "A towering water elemental, Waldern is master of all things liquid.  Wave after wave drowns your frail body."
  :symbol #\E
  :colour #\s
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
  :immunities '(<fear> <sleep> <confuse> <poison>)
  :alertness 50
  :vision 12
  :attacks '((<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5))
             (<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5)))
  :special-abilities '((<spell> (<ball> <water>)) (<spell> (<ball> <cold>)) (<spell> (<bolt> <water>))
                       (<spell> (<bolt> <ice>)) (<frequency> 1/4))) 

(define-monster-kind "dragon-kavlax" "Kavlax the Many-headed"
  :desc "A large dragon with a selection of heads, all shouting and arguing as they  look for prey, but each with its own deadly breath weapon."
  :symbol #\d
  :colour #\v
  :alignment '<evil>
  :type '(<dragon> <unique>)
  :depth 39
  :rarity 3
  :hitpoints '(13 . 100)
  :armour 85
  :speed 120
  :xp 3000
  :abilities '(<powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confuse> <lightning> <cold> <fire> <acid>)
  :alertness 30
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12))
             (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<breath> <nexus>) (<breath> <gravity>) (<breath> <shards>) (<breath> <confusion>)
                       (<breath> <sound>) (<breath> <lightning>) (<breath> <cold>) (<breath> <fire>) (<breath> <acid>)
                       (<frequency> 1/4))) 

(define-monster-kind "uvatha" "Uvatha the Horseman"
  :desc "A tall black cloaked Ringwraith, he is a master of horsemanship.  He longs  to taste your blood."
  :symbol #\W
  :colour #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 40
  :rarity 3
  :hitpoints '(12 . 100)
  :armour 60
  :speed 120
  :xp 7000
  :abilities '(<push-others> <bash-door> <open-door> <cold-blood> <max-hitpoints>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<hit> :type <exp-80> :damage (4 . 6)) (<hit> :type <exp-80> :damage (4 . 6))
             (<hit> :type <hurt> :damage (6 . 6)) (<hit> :type <hurt> :damage (6 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "medusa" "Medusa, the Gorgon"
  :desc "One of the original three ugly sisters.  Her face could sink a thousand  ships.  Her scales rattle as she slithers towards you, venom dripping from  her ghastly mouth."
  :symbol #\n
  :colour #\o
  :alignment '<evil>
  :type '(<unique>)
  :depth 40
  :rarity 3
  :hitpoints '(24 . 100)
  :armour 100
  :speed 120
  :xp 9000
  :abilities '(<bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <fire> <acid>)
  :alertness 5
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (8 . 6)) (<hit> :type <hurt> :damage (8 . 6))
             (<gaze> :type <paralyze> :damage nil) (<gaze> :type <exp-80> :damage nil))
  :treasures '(<drop-good> (<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :gender '<female>
  :special-abilities '((<summon> <hydra>) (<spell> (<ball> <acid>)) (<spell> (<bolt> <plasma>))
                       (<spell> (<bolt> <fire>)) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <paralysis>)
                       (<frequency> 1/2))) 

(define-monster-kind "orc-golfimbul" "golfimbul, the hill orc chief"
  :desc "A leader of a band of raiding orcs, he picks on hobbits."
  :symbol #\o
  :colour #\U
  :alignment '<evil>
  :type '(<orc> <unique>)
  :depth 12
  :rarity 3
  :hitpoints '(24 . 10)
  :armour 60
  :speed 110
  :xp 230
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<poison> <lightning> <cold> <fire>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (1 . 10)) (<hit> :type <hurt> :damage (1 . 10))
             (<hit> :type <hurt> :damage (1 . 12)) (<hit> :type <hurt> :damage (1 . 12)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "yeek-boldor" "Boldor, King of the yeeks"
  :desc "A great yeek, powerful in magic and sorcery, but a yeek all the same."
  :symbol #\y
  :colour #\U
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

(define-monster-kind "ufthak" "Ufthak of Cirith Ungol"
  :desc "A strong orc guarding the pass of Cirith Ungol.  He is mortally afraid of  spiders."
  :symbol #\o
  :colour #\g
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

(define-monster-kind "ren" "Ren the Unclean"
  :desc "Ren was an insane eastern king who believed himself to be the son of a  volcano god.  At an early age his sanity was destroyed by a plague that  wiped out his family, and he never recovered."
  :symbol #\W
  :colour #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 41
  :rarity 3
  :hitpoints '(18 . 100)
  :armour 70
  :speed 120
  :xp 13000
  :abilities '(<push-others> <bash-door> <open-door> <cold-blood> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold> <fire>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<wail> :type <terrify> :damage nil) (<touch> :type <exp-80> :damage nil)
             (<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <monster>) (<spell> (<ball> <fire>)) (<spell> (<bolt> <nether>))
                       (<spell> (<bolt> <fire>)) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "dawndeath" "Ji Indur Dawndeath"
  :desc "This Ringwraith was a weak-minded sorcerer-king who fell easily under  Sauron's power."
  :symbol #\W
  :colour #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 43
  :rarity 4
  :hitpoints '(18 . 100)
  :armour 70
  :speed 120
  :xp 12000
  :abilities '(<push-others> <bash-door> <open-door> <cold-blood> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold> <fire>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<touch> :type <exp-40> :damage nil) (<touch> :type <exp-40> :damage nil)
             (<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <undead>) (<spell> (<ball> <nether>)) (<spell> (<ball> <fire>)) (<spell> (<cause> 3))
                       (<spell> <scare>) (<spell> <paralysis>) (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "elemental-quaker" "Quaker, Master of Earth"
  :desc "A towering stone elemental stands before you.  The walls and ceiling are  reduced to rubble as Quaker advances."
  :symbol #\E
  :colour #\u
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
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :vulnerabilities '(<earth-destruction>)
  :alertness 90
  :vision 10
  :attacks '((<hit> :type <shatter> :damage (10 . 10)) (<hit> :type <hurt> :damage (6 . 6))
             (<hit> :type <hurt> :damage (6 . 6)) (<hit> :type <hurt> :damage (6 . 6)))
  :gender '<male>
  :special-abilities '((<spell> (<ball> <acid>)) (<spell> (<bolt> <acid>)) (<frequency> 1/6))) 

(define-monster-kind "elemental-ariel" "Ariel, Queen of Air"
  :desc "A towering air elemental, Ariel, the sorceress, avoids your blows with her extreme speed."
  :symbol #\E
  :colour #\B
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
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 50
  :vision 12
  :attacks '((<hit> :type <confuse> :damage (1 . 4)) (<hit> :type <hurt> :damage (4 . 6))
             (<hit> :type <confuse> :damage (1 . 4)) (<hit> :type <hurt> :damage (4 . 6)))
  :gender '<female>
  :special-abilities '((<spell> (<ball> <lightning>)) (<spell> (<ball> <cold>)) (<spell> (<bolt> <lightning>))
                       (<frequency> 1/5))) 

(define-monster-kind "scatha" "Scatha the Worm"
  :desc "An ancient and wise Dragon.  Scatha has grown clever over the long years.  His scales are covered with frost, and his breath sends a shower of ice into the air."
  :symbol #\D
  :colour #\w
  :alignment '<evil>
  :type '(<dragon> <unique>)
  :depth 44
  :rarity 2
  :hitpoints '(20 . 100)
  :armour 130
  :speed 120
  :xp 17000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <cold>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (3 . 14)) (<claw> :type <hurt> :damage (1 . 10))
             (<claw> :type <hurt> :damage (1 . 10)) (<claw> :type <hurt> :damage (1 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<breath> <cold>) (<spell> (<cause> 3)) (<spell> <confusion>) (<frequency> 1/3))) 

(define-monster-kind "dwar" "Dwar, Dog-lord of Waw"
  :desc "Dwar had a special affinity for dogs in life, and can still command them  at will.  He howls manically as he reaches out to destroy you."
  :symbol #\W
  :colour #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 44
  :rarity 3
  :hitpoints '(20 . 100)
  :armour 90
  :speed 120
  :xp 13000
  :abilities '(<push-others> <bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold> <fire>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<wail> :type <terrify> :damage nil) (<bite> :type <exp-40> :damage (2 . 4))
             (<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <hound>) (<summon> <undead>) (<summon> <monsters>) (<spell> (<ball> <nether>))
                       (<spell> (<ball> <fire>)) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "smaug" "Smaug the Golden"
  :desc "Smaug is one of the Uruloki that still survive, a fire-drake of immense  cunning and intelligence.  His speed through air is matched by few other  dragons and his dragonfire is what legends are made of."
  :symbol #\D
  :colour #\r
  :alignment '<evil>
  :type '(<dragon> <unique>)
  :depth 45
  :rarity 2
  :hitpoints '(20 . 100)
  :armour 100
  :speed 120
  :xp 19000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (3 . 14)) (<claw> :type <hurt> :damage (1 . 10))
             (<claw> :type <hurt> :damage (1 . 10)) (<claw> :type <hurt> :damage (1 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<breath> <fire>) (<spell> (<cause> 3)) (<spell> <confusion>) (<frequency> 1/3))) 

(define-monster-kind "ulfast" "Ulfast, son of Ulfang"
  :desc "A short and swarthy Easterling."
  :symbol #\p
  :colour #\U
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
  :attacks '((<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5))
             (<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5)))
  :treasures '(<drop-good> (<drop-chance> 9/10) <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "dwarf-nar" "Nar, the dwarf"
  :desc "This dwarf became so obsessed by gold that Morgoth tricked him into  betraying his friends."
  :symbol #\h
  :colour #\y
  :type '(<unique>)
  :depth 17
  :rarity 2
  :hitpoints '(45 . 10)
  :armour 70
  :speed 110
  :xp 250
  :abilities '(<bash-door> <open-door> <initial-sleeper> <max-hitpoints>)
  :immunities '(<sleep> <confuse> <poison> <cold> <fire>)
  :alertness 25
  :vision 25
  :attacks '((<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5))
             (<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> <mind-blast>) (<spell> (<cause> 2)) (<spell> <confusion>) (<spell> <blindness>)
                       (<spell> <heal>) (<frequency> 1/6))) 

(define-monster-kind "orc-shagrat" "shagrat, the orc captain"
  :desc "He is an Uruk of power and great cunning."
  :symbol #\o
  :colour #\g
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
  :attacks '((<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5))
             (<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (3 . 8)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "orc-gorbag" "Gorbag, the orc captain"
  :desc "A gruesomely ugly but cunning orc, his eyes regard you with hatred.  His  powerful arms flex menacingly as he advances."
  :symbol #\o
  :colour #\g
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
  :attacks '((<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5))
             (<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (3 . 8)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "orc-bolg" "Bolg, son of Azog"
  :desc "A large and powerful orc.  He looks just like his daddy.  He is tall and  fast, but fortunately blessed with orcish brains."
  :symbol #\o
  :colour #\r
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
  :attacks '((<hit> :type <hurt> :damage (3 . 6)) (<hit> :type <hurt> :damage (3 . 6))
             (<hit> :type <hurt> :damage (3 . 6)) (<hit> :type <hurt> :damage (3 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "orc-ugluk" "Ugluk, the uruk"
  :desc "Another of Morgoth's servants, this orc is strong and cunning.  He is ugly  and scarred from many power struggles."
  :symbol #\o
  :colour #\b
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
  :attacks '((<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5))
             (<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "orc-lugdush" "Lugdush, the uruk"
  :desc "A strong and cunning orc warrior, Lugdush sneers as he insults your mother."
  :symbol #\o
  :colour #\b
  :alignment '<evil>
  :type '(<orc> <unique>)
  :depth 21
  :rarity 3
  :hitpoints '(72 . 10)
  :armour 95
  :speed 110
  :xp 550
  :abilities '(<bash-door> <open-door> <max-hitpoints>)
  :immunities '(<sleep> <confuse> <poison> <cold> <fire>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (3 . 5))
             (<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (3 . 8)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "orc-azog" "Azog, King of the uruk-hai"
  :desc "He is also known as the King of Khazad-dum.  His ego is renowned to be  bigger than his head."
  :symbol #\o
  :colour #\r
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
  :attacks '((<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5))
             (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "dwarf-ibun" "Ibun, son of Mim"
  :desc "One of the last of the petty dwarves.  Ibun is a tricky sorcerous little  being, full of mischief."
  :symbol #\h
  :colour #\o
  :type '(<unique>)
  :depth 24
  :rarity 2
  :hitpoints '(82 . 10)
  :armour 80
  :speed 110
  :xp 300
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <cold> <fire>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <un-bonus> :damage nil) (<hit> :type <hurt> :damage (3 . 6))
             (<hit> :type <hurt> :damage (3 . 6)) (<hit> :type <hurt> :damage (3 . 6)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> (<bolt> <fire>)) (<spell> <slow>) (<spell> <heal>) (<frequency> 1/8))) 

(define-monster-kind "dwarf-khim" "Khim, son of Mim"
  :desc "One of the last of the petty dwarves.  Khim is a tricky sorcerous little  being, full of mischief."
  :symbol #\h
  :colour #\o
  :type '(<unique>)
  :depth 24
  :rarity 2
  :hitpoints '(82 . 10)
  :armour 80
  :speed 110
  :xp 300
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <cold> <fire>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <un-bonus> :damage nil) (<hit> :type <hurt> :damage (3 . 6))
             (<hit> :type <hurt> :damage (3 . 6)) (<hit> :type <hurt> :damage (3 . 6)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> (<bolt> <fire>)) (<spell> <slow>) (<spell> <heal>) (<frequency> 1/8))) 

(define-monster-kind "sangahyando" "Sangahyando of Umbar"
  :desc "A Black Numenorean with a blacker heart."
  :symbol #\p
  :colour #\u
  :alignment '<evil>
  :type '(<unique>)
  :depth 24
  :rarity 2
  :hitpoints '(80 . 10)
  :armour 80
  :speed 110
  :xp 400
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <lightning> <fire>)
  :alertness 25
  :vision 25
  :attacks '((<hit> :type <hurt> :damage (4 . 6)) (<hit> :type <hurt> :damage (4 . 6))
             (<hit> :type <hurt> :damage (4 . 6)) (<hit> :type <hurt> :damage (4 . 6)))
  :treasures '(<drop-good> (<drop> "1d2") (<drop-chance> 9/10) <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> <forget>) (<spell> <slow>) (<frequency> 1/4))) 

(define-monster-kind "angamaite" "Angamaite of Umbar"
  :desc "A Black Numenorean who hates the men of the west."
  :symbol #\p
  :colour #\u
  :alignment '<evil>
  :type '(<unique>)
  :depth 24
  :rarity 2
  :hitpoints '(80 . 10)
  :armour 80
  :speed 110
  :xp 400
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <lightning> <fire>)
  :alertness 25
  :vision 25
  :attacks '((<hit> :type <hurt> :damage (4 . 6)) (<hit> :type <hurt> :damage (4 . 6))
             (<hit> :type <hurt> :damage (4 . 6)) (<hit> :type <hurt> :damage (4 . 6)))
  :treasures '(<drop-good> (<drop> "1d2") (<drop-chance> 9/10) <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> <forget>) (<spell> <slow>) (<frequency> 1/4))) 

(define-monster-kind "ulwarth" "Ulwarth, son of Ulfang"
  :desc "A short and swarthy Easterling."
  :symbol #\p
  :colour #\U
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
  :attacks '((<hit> :type <hurt> :damage (4 . 6)) (<hit> :type <hurt> :damage (4 . 6))
             (<hit> :type <hurt> :damage (4 . 6)))
  :treasures '(<drop-good> (<drop-chance> 9/10) <only-drop-items>)
  :gender '<male>) 


(define-monster-kind "dwarf-mim" "Mim, betrayer of turin"
  :desc "The last of his race, Mim is a petty dwarf.  Petty dwarves are strange  creatures, powerful in sorcery and originating in the East.  They were  hunted to extinction by high elves."
  :symbol #\h
  :colour #\o
  :alignment '<evil>
  :type '(<unique>)
  :depth 27
  :rarity 4
  :hitpoints '(11 . 100)
  :armour 80
  :speed 120
  :xp 1000
  :abilities '(<bash-door> <open-door> <initial-sleeper> <max-hitpoints>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 20
  :vision 20
  :attacks '((<hit> :type <un-bonus> :damage nil) (<hit> :type <hurt> :damage (3 . 8))
             (<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (3 . 8)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> (<ball> <acid>)) (<spell> (<bolt> <acid>)) (<spell> <scare>) (<spell> <heal>)
                       (<frequency> 1/6))) 

(define-monster-kind "ogre-lokkak" "Lokkak, the ogre chieftain"
  :desc "An ogre renowned for acts of surpassing cruelty, Lokkak quickly became the  leader of a large band of violent ogres."
  :symbol #\O
  :colour #\g
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

(define-monster-kind "uldor" "Uldor the accursed"
  :desc "An evil and cunning man from the East."
  :symbol #\p
  :colour #\U
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
  :attacks '((<hit> :type <hurt> :damage (3 . 5)) (<hit> :type <hurt> :damage (4 . 6))
             (<hit> :type <hurt> :damage (4 . 6)) (<hit> :type <hurt> :damage (4 . 6)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>)

(define-monster-kind "imp-draebor" "Draebor, the imp"
  :desc "An intensely irritating git of a monster."
  :symbol #\u
  :colour #\g
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
  :attacks '((<hit> :type <hurt> :damage (3 . 4)) (<hit> :type <poison> :damage (3 . 4))
             (<hit> :type <poison> :damage (3 . 4)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :special-abilities '((<summon> <kin>) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<spell> <teleport-level>) (<spell> <teleport-away>) (<spell> <teleport-player>)
                       (<spell> <teleport>) (<spell> <blink>) (<frequency> 1/5))) 


(define-monster-kind "shelob" "Shelob, spider of darkness"
  :desc "Shelob is an enormous bloated spider, rumoured to have been one of the  brood of Ungoliant the Unlight.  Her poison is legendary, as is her ego,  which may be her downfall.  She used to guard the pass through Cirith  Ungol, but has not been seen there for many eons."
  :symbol #\S
  :colour #\D
  :alignment '<evil>
  :type '(<animal> <unique>)
  :depth 32
  :rarity 3
  :hitpoints '(12 . 100)
  :armour 80
  :speed 110
  :xp 1200
  :abilities '(<bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :vulnerabilities '(<light>)
  :alertness 80
  :vision 8
  :attacks '((<sting> :type <poison> :damage (2 . 5)) (<sting> :type <lose-str> :damage (1 . 4))
             (<sting> :type <poison> :damage (2 . 5)) (<bite> :type <hurt> :damage (2 . 10)))
  :treasures '(<drop-good> (<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :gender '<female>
  :special-abilities '((<summon> <spider>) (<spell> <traps>) (<spell> (<cause> 4)) (<spell> (<cause> 3))
                       (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>) (<spell> <blindness>) (<spell> <heal>)
                       (<frequency> 1/2))) 

(define-monster-kind "troll-bert" "Bert the stone troll"
  :desc "Big, brawny, powerful and with a taste for hobbit.  He has friends called Bill and Tom."
  :symbol #\T
  :colour #\W
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
  :attacks '((<bite> :type <hurt> :damage (2 . 3)) (<bite> :type <hurt> :damage (2 . 10))
             (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "troll-bill" "Bill the stone troll"
  :desc "Big, brawny, powerful and with a taste for hobbit.  He has friends called Bert and Tom."
  :symbol #\T
  :colour #\W
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
  :attacks '((<bite> :type <hurt> :damage (2 . 3)) (<bite> :type <hurt> :damage (2 . 10))
             (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "troll-tom" "Tom the stone troll"
  :desc "Big, brawny, powerful and with a taste for hobbit.  He has friends called Bert and Bill."
  :symbol #\T
  :colour #\W
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
  :attacks '((<bite> :type <hurt> :damage (2 . 3)) (<bite> :type <hurt> :damage (2 . 10))
             (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "ulfang" "Ulfang the black"
  :desc "A short and swarthy Easterling dressed in Black."
  :symbol #\p
  :colour #\U
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
  :attacks '((<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5))
             (<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "troll-rogrog" "Rogrog the black troll"
  :desc "A massive and cruel troll of great power, drool slides caustically down  his muscular frame.  Despite his bulk, he strikes with stunning speed."
  :symbol #\T
  :colour #\D
  :alignment '<evil>
  :type '(<troll> <unique>)
  :depth 36
  :rarity 5
  :hitpoints '(15 . 100)
  :armour 70
  :speed 120
  :xp 5000
  :abilities '(<bash-door> <open-door> <pick-up-item> <max-hitpoints>)
  :immunities '(<poison> <cold>)
  :alertness 50
  :vision 20
  :attacks '((<spit> :type <acid> :damage (3 . 8)) (<bite> :type <hurt> :damage (2 . 3))
             (<bite> :type <hurt> :damage (2 . 10)) (<hit> :type <hurt> :damage (6 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>) 

(define-monster-kind "lorgan" "Lorgan, Chief of the easterlings"
  :desc "A mighty warrior from the east, Lorgan hates everything that he cannot  control."
  :symbol #\p
  :colour #\R
  :alignment '<evil>
  :type '(<unique>)
  :depth 36
  :rarity 2
  :hitpoints '(18 . 100)
  :armour 100
  :speed 120
  :xp 1200
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 10
  :vision 25
  :attacks '((<hit> :type <hurt> :damage (3 . 8)) (<hit> :type <hurt> :damage (3 . 8))
             (<hit> :type <hurt> :damage (6 . 6)) (<hit> :type <hurt> :damage (6 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <monsters>) (<spell> <teleport-player>) (<frequency> 1/4))) 

(define-monster-kind "ant-queen" "the queen ant"
  :desc "She's upset because you hurt her children."
  :symbol #\a
  :colour #\D
  :type '(<animal> <unique>)
  :depth 37
  :rarity 2
  :hitpoints '(15 . 100)
  :armour 100
  :speed 120
  :xp 1000
  :abilities '(<bash-door> <open-door> <weird-mind> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse>)
  :alertness 10
  :vision 30
  :attacks '((<bite> :type <hurt> :damage (2 . 8)) (<bite> :type <hurt> :damage (2 . 8))
             (<bite> :type <hurt> :damage (2 . 12)) (<bite> :type <hurt> :damage (2 . 12)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<female>
  :special-abilities '((<summon> <ant>) (<frequency> 1/2))) 

(define-monster-kind "adunaphel" "Adunaphel the quiet"
  :desc "A sorceress in life, Adunaphel quickly fell under Sauron's sway and the power of the rings."
  :symbol #\W
  :colour #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 41
  :rarity 3
  :hitpoints '(12 . 100)
  :armour 60
  :speed 120
  :xp 8000
  :abilities '(<push-others> <pass-wall> <cold-blood> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<touch> :type <exp_80> :damage nil) (<hit> :type <hurt> :damage (5 . 5))
             (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<female>
  :special-abilities '((<summon> <monster>) (<spell> (<bolt> <nether>)) (<spell> (<bolt> <cold>))
                       (<spell> (<bolt> <fire>)) (<spell> (<bolt> <acid>)) (<spell> <forget>) (<spell> (<cause> 3))
                       (<spell> <scare>) (<spell> <paralysis>) (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "akhorahil" "Akhorahil the blind"
  :desc "A mighty sorcerer King, Akhorahil was blind in life.  With powerful  enchantments, he created jewelled eyes that enabled him to see better than  any ordinary man ever could."
  :symbol #\W
  :colour #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 41
  :rarity 3
  :hitpoints '(18 . 100)
  :armour 70
  :speed 120
  :xp 12000
  :abilities '(<push-others> <bash-door> <open-door> <cold-blood> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<wail> :type <terrify> :damage nil) (<gaze> :type <exp_80> :damage nil)
             (<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <monster>) (<spell> (<bolt> <nether>)) (<spell> (<bolt> <cold>))
                       (<spell> (<bolt> <fire>)) (<spell> <darkness>) (<spell> (<cause> 3)) (<spell> <scare>)
                       (<spell> <paralysis>) (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "gorlim" "Gorlim, betrayer of Barahir"
  :desc "This once-mighty warrior was so dominated by Morgoth's power that he became little more than a mindless creature of evil."
  :symbol #\p
  :colour #\s
  :type '(<unique>)
  :depth 41
  :rarity 3
  :hitpoints '(16 . 100)
  :armour 120
  :speed 120
  :xp 7000
  :abilities '(<bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <acid>)
  :alertness 40
  :vision 20
  :attacks '((<hit> :type <un-bonus> :damage (6 . 8)) (<hit> :type <un-bonus> :damage (6 . 8))
             (<hit> :type <hurt> :damage (8 . 6)) (<hit> :type <hurt> :damage (8 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> (<bolt> <mana>)) (<spell> (<bolt> <water>)) (<spell> (<cause> 3)) (<frequency> 1/2))) 

(define-monster-kind "dragon-itangast" "Itangast the fire drake"
  :desc "A mighty ancient dragon, Itangast's form scorches your flesh.  Wisps of  smoke curl up from his nostrils as he regards you with disdain."
  :symbol #\D
  :colour #\r
  :alignment '<evil>
  :type '(<dragon> <unique>)
  :depth 47
  :rarity 4
  :hitpoints '(22 . 100)
  :armour 100
  :speed 120
  :xp 20000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (4 . 14)) (<bite> :type <hurt> :damage (3 . 14))
             (<claw> :type <hurt> :damage (1 . 10)) (<claw> :type <hurt> :damage (1 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<breath> <fire>) (<spell> (<cause> 3)) (<spell> <confusion>) (<frequency> 1/3))) 

(define-monster-kind "dragon-glaurung" "Glaurung, father of the dragons"
  :desc "Glaurung is the father of all dragons, and was for a long time the most  powerful.  Nevertheless, he still has full command over his brood and can  command them to appear whenever he so wishes.  He is the definition of  dragonfire."
  :symbol #\D
  :colour #\r
  :alignment '<evil>
  :type '(<dragon> <unique>)
  :depth 48
  :rarity 2
  :hitpoints '(28 . 100)
  :armour 120
  :speed 120
  :xp 25000
  :abilities '(<push-others> <powerful-breath> <bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (6 . 14)) (<bite> :type <hurt> :damage (6 . 14))
             (<claw> :type <hurt> :damage (4 . 12)) (<claw> :type <hurt> :damage (4 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <dragon>) (<breath> <fire>) (<spell> (<cause> 3)) (<spell> <confusion>)
                       (<frequency> 1/5))) 

(define-monster-kind "balrog-muar" "muar, the balrog"
  :desc "A huge balrog surrounded by raging pillars of fire, Muar is indeed a  terrible opponent.  Wielding a great whip of fire and a blazing sword, his  fury blisters your skin and melts your flesh!"
  :symbol #\U
  :colour #\o
  :alignment '<evil>
  :type '(<demon> <unique>)
  :depth 50
  :rarity 3
  :hitpoints '(30 . 100)
  :armour 100
  :speed 120
  :xp 30000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<touch> :type <un-power> :damage nil) (<crush> :type <hurt> :damage (8 . 12))
             (<hit> :type <fire> :damage (8 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <demon>) (<summon> <undead>) (<breath> <fire>) (<spell> <scare>) (<spell> <confusion>)
                       (<frequency> 1/4))) 

(define-monster-kind "minotaur-baphomet" "Baphomet the minotaur lord"
  :desc "A fearsome bull-headed demon, Baphomet swings a mighty axe as he curses  all that defy him."
  :symbol #\H
  :colour #\s
  :alignment '<evil>
  :type '(<unique>)
  :depth 51
  :rarity 4
  :hitpoints '(35 . 100)
  :armour 120
  :speed 130
  :xp 18000
  :abilities '(<bash-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <fire>)
  :alertness 30
  :vision 30
  :attacks '((<hit> :type <hurt> :damage (10 . 10)) (<hit> :type <hurt> :damage (10 . 10))
             (<butt> :type <hurt> :damage (12 . 13)) (<butt> :type <hurt> :damage (12 . 13)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<breath> <force>) (<spell> (<ball> <lightning>)) (<spell> (<bolt> <plasma>))
                       (<spell> (<bolt> <mana>)) (<arrow> 4) (<spell> <slow>) (<frequency> 1/6))) 

(define-monster-kind "harowen" "Harowen the black hand"
  :desc "He is a master of disguise, an expert of stealth, a genius at traps, and  moves with blinding speed.  Check your pockets!"
  :symbol #\p
  :colour #\B
  :type '(<unique>)
  :depth 52
  :rarity 3
  :hitpoints '(25 . 100)
  :armour 90
  :speed 140
  :xp 20000
  :abilities '(<bash-door> <open-door> <pick-up-item> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison>)
  :alertness 0
  :vision 40
  :attacks '((<hit> :type <poison> :damage (8 . 5)) (<hit> :type <blind> :damage (10 . 5))
             (<touch> :type <eat-item> :damage (5 . 5)) (<touch> :type <eat-gold> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> <traps>) (<frequency> 1/6))) 

(define-monster-kind "hoarmurath" "Hoarmurath of Dir"
  :desc "A Ringwraith powerful in fell sorcery, he yearns for the life he has lost  for a life of everlasting torment."
  :symbol #\W
  :colour #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 52
  :rarity 3
  :hitpoints '(25 . 100)
  :armour 100
  :speed 120
  :xp 40000
  :abilities '(<push-others> <bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<wail> :type <terrify> :damage nil) (<touch> :type <exp_80> :damage nil)
             (<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (10 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <kin>) (<summon> <undead>) (<spell> (<ball> <nether>)) (<spell> (<ball> <cold>))
                       (<spell> (<bolt> <cold>)) (<spell> <mind-blast>) (<spell> (<cause> 4)) (<spell> (<cause> 3))
                       (<spell> <scare>) (<spell> <paralysis>) (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "khamul" "Khamul the easterling"
  :desc "A warrior-king of the East.  Khamul is a powerful opponent, his skill in combat awesome and his form twisted by evil cunning."
  :symbol #\W
  :colour #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 53
  :rarity 3
  :hitpoints '(35 . 100)
  :armour 100
  :speed 120
  :xp 50000
  :abilities '(<push-others> <bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold> <fire> <acid>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<touch> :type <exp_40> :damage nil) (<touch> :type <exp_40> :damage nil)
             (<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (10 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <kin>) (<summon> <undead>) (<spell> (<ball> <nether>)) (<spell> (<ball> <cold>))
                       (<spell> (<ball> <fire>)) (<spell> (<bolt> <mana>)) (<spell> (<cause> 4)) (<spell> (<cause> 3))
                       (<spell> <scare>) (<spell> <paralysis>) (<spell> <blindness>) (<spell> <teleport-level>)
                       (<frequency> 1/2))) 

(define-monster-kind "phoenix" "the Phoenix"
  :desc "A massive glowing eagle bathed in flames.  The searing heat chars your  skin and melts your armour."
  :symbol #\B
  :colour #\r
  :type '(<animal> <unique>)
  :depth 54
  :rarity 3
  :hitpoints '(36 . 100)
  :armour 130
  :speed 120
  :xp 40000
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <fire> <acid>)
  :alertness 0
  :vision 60
  :attacks '((<hit> :type <fire> :damage (9 . 12)) (<hit> :type <fire> :damage (9 . 12))
             (<bite> :type <fire> :damage (12 . 6)) (<bite> :type <fire> :damage (12 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <plasma>) (<breath> <light>) (<breath> <fire>) (<spell> (<ball> <fire>))
                       (<spell> (<bolt> <plasma>)) (<spell> (<bolt> <fire>)) (<frequency> 1/3))) 

(define-monster-kind "hydra-lernean" "the Lernean hydra"
  :desc "A massive legendary hydra.  It has twelve powerful heads.  Its many eyes  stare at you as clouds of smoke and poisonous vapour rise from its  seething form."
  :symbol #\M
  :colour #\w
  :type '(<animal> <unique>)
  :depth 55
  :rarity 2
  :hitpoints '(45 . 100)
  :armour 140
  :speed 120
  :xp 20000
  :abilities '(<powerful-breath> <overrun-others> <bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <fire>)
  :alertness 20
  :vision 20
  :attacks '((<bite> :type <fire> :damage (12 . 6)) (<bite> :type <fire> :damage (12 . 6))
             (<bite> :type <poison> :damage (8 . 6)) (<bite> :type <poison> :damage (8 . 6)))
  :treasures '((<drop> "4d2") (<drop> "3d2") <only-drop-gold>)
  :special-abilities '((<summon> <hydra>) (<breath> <poison>) (<breath> <fire>) (<spell> (<ball> <poison>))
                       (<spell> (<ball> <fire>)) (<spell> (<bolt> <plasma>)) (<spell> (<bolt> <fire>))
                       (<spell> <scare>) (<frequency> 1/3))) 

(define-monster-kind "vampire-thuringwethil" "thuringwethil"
  :desc "Chief messenger between Sauron and Morgoth, she is surely the most deadly of her vampire race.  At first she is charming to meet, but her wings and  eyes give away her true form."
  :symbol #\V
  :colour #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 55
  :rarity 4
  :hitpoints '(40 . 100)
  :armour 145
  :speed 130
  :xp 23000
  :abilities '(<regenerate> <bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <confuse> :damage (6 . 6)) (<hit> :type <confuse> :damage (6 . 6))
             (<bite> :type <exp_80> :damage (6 . 6)) (<bite> :type <hurt> :damage (5 . 8)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<female>
  :special-abilities '((<summon> <kin>) (<spell> (<ball> <nether>)) (<spell> <brain-smash>) (<spell> <drain-mana>)
                       (<spell> (<cause> 4)) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "dwarf-fundin" "Fundin Bluecloak"
  :desc "He is one of the greatest dwarven priests to walk the earth.  Fundin has  earned a high position in the church, and his skill with both weapon and  spell only justify his position further.  His combination of both dwarven  strength and priestly wisdom are a true match for any adventurer."
  :symbol #\h
  :colour #\G
  :type '(<unique>)
  :depth 56
  :rarity 2
  :hitpoints '(50 . 100)
  :armour 195
  :speed 130
  :xp 20000
  :abilities '(<bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 10
  :vision 25
  :attacks '((<hit> :type <hurt> :damage (8 . 6)) (<hit> :type <hurt> :damage (8 . 6))
             (<hit> :type <hurt> :damage (8 . 6)) (<hit> :type <hurt> :damage (10 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <monsters>) (<spell> <forget>) (<spell> <brain-smash>) (<spell> (<cause> 4))
                       (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<spell> <heal>) (<frequency> 1/4))) 

(define-monster-kind "angel-uriel" "Uriel, angel of fire"
  :desc "A creature of godly appearance, you dare not challenge Uriel's supremacy.   Those who stood against him before are but a memory, cremated by his  mastery of elemental fire."
  :symbol #\A
  :colour #\r
  :type '(<unique>)
  :depth 56
  :rarity 3
  :hitpoints '(55 . 100)
  :armour 160
  :speed 130
  :xp 25000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <pick-up-item> <smart> <max-hitpoints>
               <initial-sleeper>)
  :immunities '(<poison> <lightning> <cold> <fire> <acid>)
  :alertness 10
  :vision 40
  :attacks '((<hit> :type <hurt> :damage (10 . 10)) (<hit> :type <hurt> :damage (10 . 10))
             (<hit> :type <fire> :damage (4 . 6)) (<hit> :type <fire> :damage (9 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <angel>) (<breath> <fire>) (<spell> (<ball> <fire>)) (<spell> (<bolt> <mana>))
                       (<spell> (<bolt> <fire>)) (<spell> <blindness>) (<spell> <teleport-player>) (<frequency> 1/2))) 

(define-monster-kind "angel-azriel" "Azriel, angel of death"
  :desc "Azriel commands awesome power, his visage holy enough to shrivel your  soul.  You shriek with disbelief as his mastery of death draws you to your  grave.  It is truly beyond all but the mightiest of warriors to stand  against him and live."
  :symbol #\A
  :colour #\D
  :type '(<unique>)
  :depth 57
  :rarity 3
  :hitpoints '(60 . 100)
  :armour 170
  :speed 130
  :xp 30000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <pick-up-item> <smart> <max-hitpoints>
               <initial-sleeper>)
  :immunities '(<poison> <lightning> <cold> <fire> <acid>)
  :alertness 10
  :vision 40
  :attacks '((<hit> :type <hurt> :damage (10 . 10)) (<hit> :type <hurt> :damage (10 . 10))
             (<hit> :type <blind> :damage (10 . 5)) (<touch> :type <exp_80> :damage nil))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <angel>) (<breath> <nether>) (<spell> (<ball> <nether>)) (<spell> (<bolt> <nether>))
                       (<spell> (<bolt> <mana>)) (<spell> <blindness>) (<spell> <teleport-player>) (<frequency> 1/2))) 

(define-monster-kind "dragon-ancalagon" "Ancalagon the black"
  :desc "'Rushing Jaws' is his name, and death is his game.  No dragon of the brood of Glaurung can match him."
  :symbol #\D
  :colour #\D
  :alignment '<evil>
  :type '(<dragon> <unique>)
  :depth 58
  :rarity 3
  :hitpoints '(75 . 100)
  :armour 125
  :speed 120
  :xp 30000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire> <acid>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (10 . 14)) (<claw> :type <hurt> :damage (8 . 12))
             (<claw> :type <hurt> :damage (6 . 12)) (<claw> :type <hurt> :damage (5 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <high-dragon>) (<summon> <dragon>) (<breath> <fire>) (<breath> <acid>)
                       (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/2))) 

(define-monster-kind "angel-gabriel" "Gabriel, the Messenger"
  :desc "Commanding a legion of angels, Gabriel will destroy you for your sins.  He  will crush you like the pitiful insignificant being he sees you to be.   Your very soul will be taken into judgement by his supreme authority as he  cleanses the world of evil."
  :symbol #\A
  :colour #\w
  :type '(<unique>)
  :depth 59
  :rarity 3
  :hitpoints '(75 . 100)
  :armour 180
  :speed 130
  :xp 35000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <pick-up-item> <smart> <max-hitpoints>
               <initial-sleeper>)
  :immunities '(<poison> <lightning> <cold> <fire> <acid>)
  :alertness 10
  :vision 40
  :attacks '((<hit> :type <hurt> :damage (10 . 10)) (<hit> :type <hurt> :damage (10 . 10))
             (<hit> :type <fire> :damage (4 . 6)) (<hit> :type <un-bonus> :damage (6 . 8)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <kin>) (<summon> <angel>) (<spell> (<bolt> <mana>)) (<spell> <blindness>)
                       (<spell> <teleport-player>) (<frequency> 1/2))) 

(define-monster-kind "saruman" "Saruman of many colours"
  :desc "Originally known as the White, Saruman fell prey to Sauron's wiles.  He  seeks to emulate him and breeds orcs and trolls to fight for him.  He  searches forever for the One Ring, to become a mighty Sorcerer-King of the  world."
  :symbol #\p
  :colour #\v
  :alignment '<evil>
  :type '(<unique>)
  :depth 60
  :rarity 1
  :hitpoints '(50 . 100)
  :armour 100
  :speed 120
  :xp 35000
  :abilities '(<bash-door> <open-door> <smart> <max-hitpoints> <initial-sleeper> <colour-changing>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :alertness 0
  :vision 100
  :attacks '((<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5))
             (<hit> :type <un-bonus> :damage (6 . 8)) (<hit> :type <un-bonus> :damage (6 . 8)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <dragon>) (<summon> <demon>) (<summon> <undead>) (<spell> (<ball> <water>))
                       (<spell> (<ball> <cold>)) (<spell> (<ball> <fire>)) (<spell> (<ball> <acid>))
                       (<spell> (<bolt> <ice>)) (<spell> <traps>) (<spell> <forget>) (<spell> <mind-blast>)
                       (<spell> (<cause> 4)) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<spell> <teleport-away>) (<spell> <teleport>) (<spell> <haste>) (<spell> <heal>)
                       (<frequency> 1/2))) 

(define-monster-kind "cat-lord" "the Cat Lord"
  :desc "Master of all things feline, the Cat Lord moves with catlike stealth."
  :symbol #\f
  :colour #\r
  :type '(<unique>)
  :depth 64
  :rarity 3
  :hitpoints '(48 . 100)
  :armour 200
  :speed 130
  :xp 30000
  :abilities '(<bash-door> <open-door> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold> <fire>)
  :alertness 0
  :vision 100
  :attacks '((<hit> :type <paralyze> :damage (15 . 1)) (<hit> :type <blind> :damage (10 . 5))
             (<touch> :type <lose_dex> :damage (2 . 12)) (<hit> :type <confuse> :damage (12 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> <teleport-player>) (<frequency> 1/3))) 

(define-monster-kind "tselakus" "Tselakus, the Dreadlord"
  :desc "This huge affront to existence twists and tears at the fabric of space.  A  master of mighty magic, Tselakus hungers for your tender flesh.  Darkness  itself recoils from the touch of Tselakus as he leaves a trail of death  and destruction.  Tselakus is a being of sneering contempt, laughing at  your pitiful efforts to defy him.  Mighty claws rend reality as he  annihilates all in his path to your soul!"
  :symbol #\G
  :colour #\r
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 68
  :rarity 2
  :hitpoints '(65 . 100)
  :armour 150
  :speed 130
  :xp 35000
  :abilities '(<pass-wall> <cold-blood> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :alertness 10
  :vision 20
  :attacks '((<hit> :type <lose-str> :damage (4 . 6)) (<hit> :type <lose-str> :damage (4 . 6))
             (<hit> :type <hurt> :damage (10 . 10)) (<hit> :type <hurt> :damage (10 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <kin>) (<summon> <high-undead>) (<summon> <wraith>) (<spell> (<ball> <nether>))
                       (<spell> (<ball> <darkness>)) (<spell> <confusion>) (<spell> <paralysis>) (<spell> <blindness>)
                       (<frequency> 1/3))) 

(define-monster-kind "tiamat" "Tiamat, Celestial dragon of evil"
  :desc "Usually found guarding the first plane of Hell, Tiamat is a formidable  opponent, her five heads breathing death to all who stand against her."
  :symbol #\D
  :colour #\v
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
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 70
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (10 . 14)) (<claw> :type <hurt> :damage (8 . 12))
             (<claw> :type <hurt> :damage (8 . 12)) (<claw> :type <hurt> :damage (6 . 12)))
  :treasures '(<drop-great> <drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<female>
  :special-abilities '((<summon> <high-dragon>) (<breath> <poison>) (<breath> <lightning>) (<breath> <cold>)
                       (<breath> <fire>) (<breath> <acid>) (<spell> <scare>) (<spell> <confusion>)
                       (<spell> <blindness>) (<frequency> 1/2))) 

(define-monster-kind "vecna" "Vecna, the Emperor lich"
  :desc "He is a highly cunning, extremely magical being, spoken of in legends.   This ancient shadow of death wilts any living thing it passes."
  :symbol #\L
  :colour #\R
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 72
  :rarity 2
  :hitpoints '(50 . 100)
  :armour 85
  :speed 130
  :xp 30000
  :abilities '(<bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :alertness 50
  :vision 20
  :attacks '((<touch> :type <lose_dex> :damage (2 . 12)) (<touch> :type <lose_dex> :damage (2 . 12))
             (<touch> :type <un-power> :damage nil) (<touch> :type <exp_80> :damage nil))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <undead>) (<summon> <monsters>) (<spell> (<ball> <nether>)) (<spell> (<bolt> <mana>))
                       (<spell> (<ball> <mana>)) (<spell> <traps>) (<spell> <brain-smash>) (<spell> (<cause> 4))
                       (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <confusion>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<spell> <teleport-player>) (<spell> <blink>) (<frequency> 1/2))) 

(define-monster-kind "omarax" "Omarax the eye tyrant"
  :desc "A disembodied eye, floating in the air.  His gaze seems to shred your  soul and his spells crush your will.  He is ancient, his history steeped  in forgotten evils, his atrocities numerous and sickening."
  :symbol #\e
  :colour #\v
  :alignment '<evil>
  :type '(<unique>)
  :depth 73
  :rarity 4
  :hitpoints '(65 . 100)
  :armour 80
  :speed 130
  :xp 16000
  :abilities '(<bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison>)
  :alertness 10
  :vision 30
  :attacks '((<gaze> :type <lose_int> :damage (2 . 6)) (<gaze> :type <un-power> :damage (2 . 6))
             (<gaze> :type <paralyze> :damage (2 . 6)) (<gaze> :type <exp_40> :damage (2 . 6)))
  :gender '<male>
  :special-abilities '((<summon> <kin>) (<spell> (<bolt> <cold>)) (<spell> (<bolt> <fire>)) (<spell> (<bolt> <acid>))
                       (<spell> (<ball> <darkness>)) (<spell> <darkness>) (<spell> <forget>) (<spell> <mind-blast>)
                       (<spell> <drain-mana>) (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>)
                       (<spell> <blindness>) (<frequency> 1/2))) 

(define-monster-kind "spider-ungoliant" "Ungoliant, the Unlight"
  :desc "This enormous, hideous spirit of void is in the form of a spider of immense proportions.  She is surrounded by a cloud of Unlight as she sucks  in all living light into her bloated body.  She is always ravenously  hungry and would even eat herself to avoid starvation.  She is rumoured to  have a foul and deadly breath."
  :symbol #\S
  :colour #\D
  :alignment '<evil>
  :type '(<animal> <unique>)
  :depth 75
  :rarity 1
  :hitpoints '(130 . 100)
  :armour 160
  :speed 120
  :xp 35000
  :abilities '(<bash-door> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison>)
  :vulnerabilities '(<light>)
  :alertness 80
  :vision 8
  :attacks '((<sting> :type <poison> :damage (2 . 5)) (<sting> :type <poison> :damage (2 . 5))
             (<bite> :type <poison> :damage (3 . 9)) (<bite> :type <poison> :damage (3 . 9)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<female>
  :special-abilities '((<summon> <spider>) (<breath> <darkness>) (<breath> <poison>) (<spell> (<ball> <darkness>))
                       (<spell> <darkness>) (<spell> <scare>) (<spell> <confusion>) (<spell> <slow>)
                       (<spell> <blindness>) (<spell> <heal>) (<frequency> 1/3))) 

(define-monster-kind "mouth-sauron" "the Mouth of Sauron"
  :desc "The Mouth of Sauron is a mighty spell caster.  So old that even he cannot  remember his own name, his power and evil are undeniable.  He believes  unshakeably that he is unbeatable and laughs as he weaves his awesome  spells."
  :symbol #\p
  :colour #\v
  :alignment '<evil>
  :type '(<unique>)
  :depth 78
  :rarity 3
  :hitpoints '(70 . 100)
  :armour 100
  :speed 130
  :xp 38000
  :abilities '(<bash-door> <open-door> <invisible> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <lightning> <cold> <fire>)
  :alertness 10
  :vision 60
  :attacks '((<touch> :type <un-power> :damage nil) (<touch> :type <un-power> :damage nil)
             (<hit> :type <un-bonus> :damage (6 . 8)) (<hit> :type <un-bonus> :damage (6 . 8)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<spell> (<ball> <nether>)) (<spell> (<ball> <water>)) (<spell> (<ball> <fire>))
                       (<spell> (<ball> <mana>)) (<spell> (<ball> <darkness>)) (<spell> (<bolt> <plasma>))
                       (<spell> <traps>) (<spell> (<cause> 3)) (<spell> <paralysis>) (<spell> <teleport-player>)
                       (<frequency> 1/2))) 

(define-monster-kind "quylthulg-emperor" "the Emperor quylthulg"
  :desc "A huge seething mass of flesh with a rudimentary intelligence, the Emperor  Quylthulg changes colours in front of your eyes.  Pulsating first one  colour then the next, it knows only it must bring help to protect itself."
  :symbol #\Q
  :colour #\w
  :alignment '<evil>
  :type '(<animal> <unique>)
  :depth 78
  :rarity 3
  :hitpoints '(50 . 100)
  :armour 1
  :speed 130
  :xp 20000
  :abilities '(<invisible> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 30
  :treasures '((<drop> "4d2") <only-drop-items>)
  :special-abilities '((<summon> <high-demon>) (<summon> <high-dragon>) (<summon> <high-undead>)
                       (<spell> <brain-smash>) (<frequency> 1/2))) 

(define-monster-kind "qlzqqlzuup" "Qlzqqlzuup, the Lord of flesh"
  :desc "This disgusting creature squeals and snorts as it writhes on the floor.   It pulsates with evil.  Its intent is to overwhelm you with monster after  monster, until it can greedily dine on your remains."
  :symbol #\Q
  :colour #\o
  :alignment '<evil>
  :type '(<animal> <unique>)
  :depth 78
  :rarity 3
  :hitpoints '(50 . 100)
  :armour 1
  :speed 130
  :xp 20000
  :abilities '(<invisible> <never-attack> <never-move> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse>)
  :alertness 0
  :vision 30
  :treasures '((<drop> "4d2") <only-drop-items>)
  :special-abilities '((<summon> <kin>) (<summon> <high-demon>) (<summon> <high-dragon>) (<summon> <high-undead>)
                       (<summon> <wraith>) (<summon> <unique>) (<summon> <hound>) (<summon> <ant>) (<summon> <spider>)
                       (<summon> <hydra>) (<summon> <angel>) (<summon> <dragon>) (<summon> <demon>) (<summon> <undead>)
                       (<summon> <monsters>) (<summon> <monster>) (<frequency> 1))) 

(define-monster-kind "murazor" "Murazor, the Witch-king of Angmar"
  :desc "The Chief of the Ringwraiths.  A fell being of devastating power.  His  spells are lethal and his combat blows crushingly hard.  He moves at  speed, and commands legions of evil to do his bidding.  It is said that he is fated never to die by the hand of mortal man."
  :symbol #\W
  :colour #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 80
  :rarity 3
  :hitpoints '(60 . 100)
  :armour 120
  :speed 130
  :xp 42000
  :abilities '(<push-others> <bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <cold>)
  :vulnerabilities '(<light>)
  :alertness 10
  :vision 90
  :attacks '((<hit> :type <exp_80> :damage (5 . 5)) (<hit> :type <exp_80> :damage (5 . 5))
             (<hit> :type <hurt> :damage (10 . 10)) (<hit> :type <hurt> :damage (10 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <kin>) (<summon> <high-demon>) (<summon> <high-dragon>) (<summon> <high-undead>)
                       (<summon> <wraith>) (<summon> <monsters>) (<spell> (<ball> <nether>)) (<spell> (<bolt> <mana>))
                       (<spell> <brain-smash>) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<spell> <teleport-away>) (<frequency> 1/2))) 

(define-monster-kind "pazuzu" "Pazuzu, Lord of air"
  :desc "A winged humanoid from the Planes of Hell, Pazuzu grins inhumanely at you  as he decides your fate."
  :symbol #\B
  :colour #\w
  :alignment '<evil>
  :type '(<demon> <unique>)
  :depth 82
  :rarity 2
  :hitpoints '(55 . 100)
  :armour 125
  :speed 140
  :xp 30000
  :abilities '(<bash-door> <open-door> <invisible> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 10
  :vision 40
  :attacks '((<hit> :type <elec> :damage (12 . 12)) (<hit> :type <elec> :damage (12 . 12))
             (<hit> :type <elec> :damage (12 . 12)) (<hit> :type <elec> :damage (12 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <high-demon>) (<spell> (<ball> <lightning>)) (<spell> (<bolt> <mana>))
                       (<spell> (<bolt> <lightning>)) (<spell> <mind-blast>) (<frequency> 1/3))) 

(define-monster-kind "cantoras" "Cantoras, the skeletal Lord"
  :desc "A legion of evil undead druj animating the skeleton of a once mighty sorcerer.  His power is devastating and his speed unmatched in the underworld.  Flee his wrath!"
  :symbol #\s
  :colour #\w
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 84
  :rarity 2
  :hitpoints '(75 . 100)
  :armour 120
  :speed 140
  :xp 45000
  :abilities '(<bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <cold> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<touch> :type <poison> :damage (3 . 5)) (<touch> :type <poison> :damage (3 . 5))
             (<gaze> :type <exp_80> :damage nil) (<gaze> :type <exp_80> :damage nil))
  :treasures '(<drop-great> <drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <high-undead>) (<spell> (<ball> <nether>)) (<spell> (<ball> <water>))
                       (<spell> (<bolt> <mana>)) (<spell> (<bolt> <ice>)) (<spell> <brain-smash>) (<spell> (<cause> 4))
                       (<spell> <scare>) (<spell> <slow>) (<spell> <teleport-player>) (<frequency> 1))) 

(define-monster-kind "tarrasque" "the Tarrasque"
  :desc "The Tarrasque is a massive reptile of legend, rumoured to be unkillable  and immune to magic.  Fear its anger, for its devastation is unmatched!"
  :symbol #\R
  :colour #\v
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
  :immunities '(<sleep> <confuse> <cold> <fire>)
  :alertness 20
  :vision 50
  :attacks '((<touch> :type <un-power> :damage nil) (<touch> :type <un-power> :damage nil)
             (<hit> :type <hurt> :damage (10 . 10)) (<hit> :type <hurt> :damage (10 . 10)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "2d2") <only-drop-items>)
  :special-abilities '((<breath> <disenchant>) (<breath> <cold>) (<breath> <fire>) (<frequency> 1/2))) 

(define-monster-kind "balrog-lungorthin" "Lungorthin, the balrog of white fire"
  :desc "A massive form cloaked in flame.  Lungorthin stares balefully at you with  eyes that smoulder red.  The dungeon floor where he stands is scorched by  the heat of his body."
  :symbol #\U
  :colour #\w
  :alignment '<evil>
  :type '(<demon> <unique>)
  :depth 85
  :rarity 2
  :hitpoints '(80 . 100)
  :armour 125
  :speed 130
  :xp 37000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire>)
  :alertness 80
  :vision 20
  :attacks '((<touch> :type <un-power> :damage nil) (<crush> :type <hurt> :damage (8 . 12))
             (<hit> :type <fire> :damage (8 . 12)) (<hit> :type <fire> :damage (8 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <high-demon>) (<summon> <high-undead>) (<summon> <demon>) (<breath> <fire>)
                       (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/4))) 

(define-monster-kind "wolf-draugluin" "Draugluin, Sire of all werewolves"
  :desc "Draugluin provides Sauron with a fearsome personal guard.  He is an enormous wolf inhabited with a human spirit.  He is chief of all his kind."
  :symbol #\C
  :colour #\u
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
  :attacks '((<bite> :type <poison> :damage (2 . 6)) (<bite> :type <poison> :damage (2 . 6))
             (<claw> :type <hurt> :damage (3 . 3)) (<claw> :type <hurt> :damage (3 . 3)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <hound>) (<summon> <monsters>) (<spell> <scare>) (<frequency> 1/3))) 

(define-monster-kind "feagwath" "Feagwath the undead sorceror"
  :desc "A stench of corruption and decay surrounds this sorcerer, who has clearly  risen from the grave to continue his foul plots and schemes."
  :symbol #\L
  :colour #\R
  :alignment '<evil>
  :type '(<undead> <unique>)
  :depth 90
  :rarity 3
  :hitpoints '(60 . 100)
  :armour 100
  :speed 130
  :xp 45000
  :abilities '(<bash-door> <open-door> <cold-blood> <smart> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :alertness 0
  :vision 100
  :attacks '((<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5))
             (<hit> :type <un-bonus> :damage (6 . 8)) (<hit> :type <un-bonus> :damage (6 . 8)))
  :treasures '(<drop-great> <drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <kin>) (<summon> <high-undead>) (<summon> <demon>) (<summon> <monsters>)
                       (<spell> (<ball> <fire>)) (<spell> (<bolt> <mana>)) (<spell> (<ball> <mana>))
                       (<spell> <brain-smash>) (<spell> (<cause> 4)) (<spell> <scare>) (<spell> <blindness>)
                       (<spell> <teleport>) (<frequency> 1/3))) 

(define-monster-kind "wolf-carcharoth" "Carcharoth, the jaws of thirst"
  :desc "The first guard of Angband, Carcharoth, also known as 'The Red Maw', is  the largest wolf to ever walk the earth.  He is highly intelligent and a  deadly opponent in combat."
  :symbol #\C
  :colour #\D
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
  :immunities '(<sleep> <confuse> <poison> <fire>)
  :alertness 10
  :vision 80
  :attacks '((<bite> :type <poison> :damage (4 . 4)) (<bite> :type <poison> :damage (4 . 4))
             (<claw> :type <hurt> :damage (3 . 3)) (<claw> :type <hurt> :damage (3 . 3)))
  :treasures '(<drop-good> (<drop> "1d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <hound>) (<breath> <fire>) (<spell> <brain-smash>) (<spell> <scare>) (<spell> <heal>)
                       (<frequency> 1/4))) 

(define-monster-kind "cerberus" "Cerberus, Guardian of Hades"
  :desc "A two-headed hell hound of fearsome aspect.  Flame burns merrily from its  hide as it snarls and roars its defiance."
  :symbol #\C
  :colour #\r
  :alignment '<evil>
  :type '(<animal> <unique>)
  :depth 94
  :rarity 1
  :hitpoints '(100 . 100)
  :armour 160
  :speed 130
  :xp 40000
  :abilities '(<push-others> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <fire>)
  :alertness 10
  :vision 50
  :attacks '((<hit> :type <fire> :damage (9 . 12)) (<hit> :type <fire> :damage (9 . 12))
             (<hit> :type <fire> :damage (9 . 12)) (<hit> :type <fire> :damage (9 . 12)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :special-abilities '((<summon> <hound>) (<breath> <nether>) (<breath> <fire>) (<spell> (<ball> <darkness>))
                       (<frequency> 1/3))) 

(define-monster-kind "balrog-gothmog" "Gothmog, the High Captain of balrogs"
  :desc "Gothmog is the Chief Balrog in Morgoth's personal guard.  He is renowned  for slaying Ecthelion the Warder of the Gates and he has never been  defeated in combat.  With his whip of flame and awesome fiery breath he  saved his master from Ungoliant's rage."
  :symbol #\U
  :colour #\R
  :alignment '<evil>
  :type '(<demon> <unique>)
  :depth 95
  :rarity 1
  :hitpoints '(80 . 100)
  :armour 140
  :speed 130
  :xp 43000
  :abilities '(<push-others> <powerful-breath> <bash-door> <open-door> <max-hitpoints> <initial-sleeper>)
  :immunities '(<sleep> <confuse> <lightning> <fire>)
  :alertness 0
  :vision 100
  :attacks '((<touch> :type <un-power> :damage nil) (<crush> :type <hurt> :damage (8 . 12))
             (<hit> :type <fire> :damage (9 . 12)) (<hit> :type <fire> :damage (9 . 12)))
  :treasures '(<drop-great> <drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <high-undead>) (<summon> <high-demon>) (<breath> <fire>) (<spell> <scare>)
                       (<spell> <confusion>) (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "sauron" "Sauron, the Sorcerer"
  :desc "He is Morgoth's most powerful servant.  Mighty in spells and enchantments,  he created the One Ring.  His eyes glow with power and his gaze seeks to  destroy your soul.  He has many servants, and rarely fights without them."
  :symbol #\p
  :colour #\v
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
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire>)
  :alertness 0
  :vision 100
  :attacks '((<touch> :type <un-power> :damage nil) (<touch> :type <un-power> :damage nil)
             (<hit> :type <un-bonus> :damage (10 . 12)) (<hit> :type <un-bonus> :damage (10 . 12)))
  :treasures '(<drop-great> <drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <high-dragon>) (<summon> <high-undead>) (<summon> <high-demon>) (<summon> <monsters>)
                       (<spell> (<ball> <darkness>)) (<spell> (<ball> <nether>)) (<spell> (<ball> <water>))
                       (<spell> (<ball> <fire>)) (<spell> (<ball> <mana>)) (<spell> (<bolt> <plasma>))
                       (<spell> (<bolt> <mana>)) (<spell> (<bolt> <ice>)) (<spell> <forget>) (<spell> <brain-smash>)
                       (<spell> (<cause> 4)) (<spell> <scare>) (<spell> <confusion>) (<spell> <blindness>)
                       (<spell> <teleport-level>) (<spell> <teleport>) (<frequency> 1/2))) 

(define-monster-kind "morgoth" "Morgoth, Lord of Darkness"
  :desc "He is the Master of the Pits of Angband.  His figure is like a black  mountain crowned with Lightning.  He rages with everlasting anger, his  body scarred by Fingolfin's eight mighty wounds.  He can never rest from  his pain, but seeks forever to dominate all that is light and good in the  world.  He is the origin of man's fear of darkness and created many foul  creatures with his evil powers.  Orcs, Dragons, and Trolls are his most  foul corruptions, causing much pain and suffering in the world to please  him.  His disgusting visage, twisted with evil, is crowned with iron, the  two remaining Silmarils forever burning him.  Grond, the mighty Hammer of  the Underworld, cries defiance as he strides towards you to crush you to a  pulp!"
  :symbol #\P
  :colour #\D
  :alignment '<evil>
  :type '(<quest-monster> <unique>)
  :depth 100
  :rarity 1
  :hitpoints '(200 . 100)
  :armour 150
  :speed 140
  :xp 60000
  :abilities '(<regenerate> <push-others> <destroy-wall> <smart> <only-on-set-depth> <max-hitpoints> <initial-sleeper>)
  :immunities '(<fear> <sleep> <confuse> <poison> <lightning> <cold> <fire> <acid>)
  :alertness 0
  :vision 100
  :attacks '((<touch> :type <un-power> :damage nil) (<hit> :type <lose-all> :damage (10 . 12))
             (<hit> :type <shatter> :damage (20 . 10)) (<hit> :type <shatter> :damage (20 . 10)))
  :treasures '(<drop-planned> <drop-great> <drop-good> (<drop> "4d2") (<drop> "3d2") (<drop> "2d2") (<drop> "1d2")
               <only-drop-items>)
  :gender '<male>
  :special-abilities '((<summon> <high-demon>) (<summon> <high-dragon>) (<summon> <high-undead>) (<summon> <wraith>)
                       (<summon> <unique>) (<summon> <monsters>) (<spell> (<ball> <nether>)) (<spell> (<bolt> <mana>))
                       (<spell> (<ball> <mana>)) (<spell> <brain-smash>) (<frequency> 1/3))) 


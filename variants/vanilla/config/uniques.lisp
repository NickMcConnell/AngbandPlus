
(in-package :langband)

(define-monster-kind "maggott" "Farmer Maggot"
  :desc "He's lost his dogs.  He's had his mushrooms stolen.  He's not a happy  hobbit!"
  :symbol #\h
  :colour #\w
  :type '(<unique>)
  :level 0
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
  :sex '<male>)

(define-monster-kind "fang" "Fang, Farmer Maggot's dog"
  :desc "A rather vicious dog belonging to Farmer Maggot.  It thinks you are  stealing mushrooms."
  :symbol #\C
  :colour #\U
  :type '(<animal> <unique>)
  :level 2
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

(define-monster-kind "grip" "Grip, Farmer Maggot's dog"
  :desc "A rather vicious dog belonging to Farmer Maggot.  It thinks you are  stealing mushrooms."
  :symbol #\C
  :colour #\U
  :type '(<animal> <unique>)
  :level 2
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

(define-monster-kind "mughash" "Mughash the kobold lord"
  :desc "Strong and powerful, for a kobold."
  :symbol #\k
  :colour #\b
  :alignment '<evil>
  :type '(<unique>)
  :level 7
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
  :sex '<male>) 

(define-monster-kind 'wormtongue "Wormtongue, Agent of Saruman"
  :desc "He's been spying for Saruman.  He is a snivelling wretch with no morals  and disgusting habits."
  :symbol #\p
  :colour #\b
  :alignment '<evil>
  :type '(<unique>)
  :level 8
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
  :sex '<male>
  :special-abilities '((<spell> (<ball> <poison>)) (<spell> (<bolt> <cold>)) (<spell> <traps>) (<spell> <slow>)
                       (<spell> <heal>) (<frequency> 1/5))) 

(define-monster-kind 'lagduf "Lagduf, the Snaga"
  :desc "A captain of a regiment of weaker orcs, Lagduf keeps his troop in order  with displays of excessive violence."
  :symbol #\o
  :colour #\o
  :alignment '<evil>
  :type '(<orc> <unique>)
  :level 8
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
  :sex '<male>) 

(define-monster-kind 'brodda "Brodda, the easterling"
  :desc "A nasty piece of work, Brodda picks on defenseless women and children."
  :symbol #\p
  :colour #\u
  :type '(<unique>)
  :level 9
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
  :sex '<male>) 

(define-monster-kind 'orfax "Orfax, son of Boldor"
  :desc "He's just like daddy!  He knows mighty spells, but fortunately he is a  yeek."
  :symbol #\y
  :colour #\b
  :alignment '<evil>
  :type '(<animal> <unique>)
  :level 10
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
  :sex '<male>
  :special-abilities '((<summon> <monster>) (<spell> <confusion>) (<spell> <slow>) (<spell> <teleport-player>)
                       (<spell> <blink>) (<spell> <heal>) (<frequency> 1/4))) 

(define-monster-kind 'grishnakh "Grishnakh, the hill-orc"
  :desc "He is a cunning and devious orc with a chaotic nature."
  :symbol #\o
  :colour #\U
  :alignment '<evil>
  :type '(<orc> <unique>)
  :level 10
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
  :sex '<male>) 

(define-monster-kind 'castamir "Castamir the Usurper"
  :desc "A Black Numenorean who usurped the throne of Gondor, he is treacherous and  evil."
  :symbol #\p
  :colour #\R
  :alignment '<evil>
  :type '(<unique>)
  :level 38
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
  :sex '<male>
  :special-abilities '((<spell> (<bolt> <ice>)) (<spell> (<bolt> <lightning>)) (<spell> (<bolt> <cold>))
                       (<spell> (<bolt> <fire>)) (<spell> <traps>) (<spell> <heal>) (<frequency> 1/2))) 

(define-monster-kind 'vargo "Vargo, Tyrant of Fire"
  :desc "A towering fire elemental, Vargo burns everything beyond recognition."
  :symbol #\E
  :colour #\r
  :alignment '<evil>
  :type '(<unique>)
  :level 38
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

(define-monster-kind 'waldern "Waldern, King of Water"
  :desc "A towering water elemental, Waldern is master of all things liquid.  Wave after wave drowns your frail body."
  :symbol #\E
  :colour #\s
  :alignment '<evil>
  :type '(<unique>)
  :level 39
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

(define-monster-kind 'kavlax "Kavlax the Many-headed"
  :desc "A large dragon with a selection of heads, all shouting and arguing as they  look for prey, but each with its own deadly breath weapon."
  :symbol #\d
  :colour #\v
  :alignment '<evil>
  :type '(<dragon> <unique>)
  :level 39
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
  :sex '<male>
  :special-abilities '((<breath> <nexus>) (<breath> <gravity>) (<breath> <shards>) (<breath> <confusion>)
                       (<breath> <sound>) (<breath> <lightning>) (<breath> <cold>) (<breath> <fire>) (<breath> <acid>)
                       (<frequency> 1/4))) 

(define-monster-kind 'uvatha "Uvatha the Horseman"
  :desc "A tall black cloaked Ringwraith, he is a master of horsemanship.  He longs  to taste your blood."
  :symbol #\W
  :colour #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :level 40
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
  :attacks '((<hit> :type <exp_80> :damage (4 . 6)) (<hit> :type <exp_80> :damage (4 . 6))
             (<hit> :type <hurt> :damage (6 . 6)) (<hit> :type <hurt> :damage (6 . 6)))
  :treasures '(<drop-good> (<drop> "2d2") <only-drop-items>)
  :sex '<male>) 

(define-monster-kind "medusa" "Medusa, the Gorgon"
  :desc "One of the original three ugly sisters.  Her face could sink a thousand  ships.  Her scales rattle as she slithers towards you, venom dripping from  her ghastly mouth."
  :symbol #\n
  :colour #\o
  :alignment '<evil>
  :type '(<unique>)
  :level 40
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
             (<gaze> :type <paralyze> :damage nil) (<gaze> :type <exp_80> :damage nil))
  :treasures '(<drop-good> (<drop> "2d2") (<drop> "1d2") <only-drop-items>)
  :sex '<female>
  :special-abilities '((<summon> <hydra>) (<spell> (<ball> <acid>)) (<spell> (<bolt> <plasma>))
                       (<spell> (<bolt> <fire>)) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <paralysis>)
                       (<frequency> 1/2))) 

(define-monster-kind "golfimbul" "golfimbul, the hill orc chief"
  :desc "A leader of a band of raiding orcs, he picks on hobbits."
  :symbol #\o
  :colour #\U
  :alignment '<evil>
  :type '(<orc> <unique>)
  :level 12
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
  :sex '<male>) 

(define-monster-kind "boldor" "Boldor, King of the yeeks"
  :desc "A great yeek, powerful in magic and sorcery, but a yeek all the same."
  :symbol #\y
  :colour #\U
  :alignment '<evil>
  :type '(<animal> <unique>)
  :level 13
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
  :sex '<male>
  :special-abilities '((<summon> <kin>) (<summon> <monster>) (<spell> <slow>) (<spell> <blindness>)
                       (<spell> <teleport>) (<spell> <blink>) (<spell> <heal>) (<frequency> 1/3))) 

(define-monster-kind "ufthak" "Ufthak of Cirith Ungol"
  :desc "A strong orc guarding the pass of Cirith Ungol.  He is mortally afraid of  spiders."
  :symbol #\o
  :colour #\g
  :alignment '<evil>
  :type '(<orc> <unique>)
  :level 14
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
  :sex '<male>) 

(define-monster-kind "ren" "Ren the Unclean"
  :desc "Ren was an insane eastern king who believed himself to be the son of a  volcano god.  At an early age his sanity was destroyed by a plague that  wiped out his family, and he never recovered."
  :symbol #\W
  :colour #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :level 41
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
  :attacks '((<wail> :type <terrify> :damage nil) (<touch> :type <exp_80> :damage nil)
             (<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :sex '<male>
  :special-abilities '((<summon> <monster>) (<spell> (<ball> <fire>)) (<spell> (<bolt> <nether>))
                       (<spell> (<bolt> <fire>)) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "dawndeath" "Ji Indur Dawndeath"
  :desc "This Ringwraith was a weak-minded sorcerer-king who fell easily under  Sauron's power."
  :symbol #\W
  :colour #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :level 43
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
  :attacks '((<touch> :type <exp_40> :damage nil) (<touch> :type <exp_40> :damage nil)
             (<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :sex '<male>
  :special-abilities '((<summon> <undead>) (<spell> (<ball> <nether>)) (<spell> (<ball> <fire>)) (<spell> (<cause> 3))
                       (<spell> <scare>) (<spell> <paralysis>) (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "monster-466" "Quaker, Master of Earth"
  :desc "A towering stone elemental stands before you.  The walls and ceiling are  reduced to rubble as Quaker advances."
  :symbol #\E
  :colour #\u
  :alignment '<evil>
  :type '(<unique>)
  :level 43
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
  :sex '<male>
  :special-abilities '((<spell> (<ball> <acid>)) (<spell> (<bolt> <acid>)) (<frequency> 1/6))) 

(define-monster-kind "monster-468" "Ariel, Queen of Air"
  :desc "A towering air elemental, Ariel, the sorceress, avoids your blows  with her extreme speed."
  :symbol #\E
  :colour #\B
  :alignment '<evil>
  :type '(<unique>)
  :level 44
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
  :sex '<female>
  :special-abilities '((<spell> (<ball> <lightning>)) (<spell> (<ball> <cold>)) (<spell> (<bolt> <lightning>))
                       (<frequency> 1/5))) 

(define-monster-kind "scatha" "Scatha the Worm"
  :desc "An ancient and wise Dragon.  Scatha has grown clever over the long years.   His scales are covered with frost, and his breath sends a shower of ice  into the air."
  :symbol #\D
  :colour #\w
  :alignment '<evil>
  :type '(<dragon> <unique>)
  :level 44
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
  :sex '<male>
  :special-abilities '((<breath> <cold>) (<spell> (<cause> 3)) (<spell> <confusion>) (<frequency> 1/3))) 

(define-monster-kind "dwar" "Dwar, Dog-lord of Waw"
  :desc "Dwar had a special affinity for dogs in life, and can still command them  at will.  He howls manically as he reaches out to destroy you."
  :symbol #\W
  :colour #\D
  :alignment '<evil>
  :type '(<undead> <unique>)
  :level 44
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
  :attacks '((<wail> :type <terrify> :damage nil) (<bite> :type <exp_40> :damage (2 . 4))
             (<hit> :type <hurt> :damage (5 . 5)) (<hit> :type <hurt> :damage (5 . 5)))
  :treasures '(<drop-good> (<drop> "4d2") <only-drop-items>)
  :sex '<male>
  :special-abilities '((<summon> <hound>) (<summon> <undead>) (<summon> <monsters>) (<spell> (<ball> <nether>))
                       (<spell> (<ball> <fire>)) (<spell> (<cause> 3)) (<spell> <scare>) (<spell> <paralysis>)
                       (<spell> <blindness>) (<frequency> 1/3))) 

(define-monster-kind "smaug" "Smaug the Golden"
  :desc "Smaug is one of the Uruloki that still survive, a fire-drake of immense  cunning and intelligence.  His speed through air is matched by few other  dragons and his dragonfire is what legends are made of."
  :symbol #\D
  :colour #\r
  :alignment '<evil>
  :type '(<dragon> <unique>)
  :level 45
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
  :sex '<male>
  :special-abilities '((<breath> <fire>) (<spell> (<cause> 3)) (<spell> <confusion>) (<frequency> 1/3))) 

(define-monster-kind "ulfast" "Ulfast, son of Ulfang"
  :desc "A short and swarthy Easterling."
  :symbol #\p
  :colour #\U
  :alignment '<evil>
  :type '(<unique>)
  :level 16
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
  :sex '<male>) 

(define-monster-kind "nar" "Nar, the dwarf"
  :desc "This dwarf became so obsessed by gold that Morgoth tricked him into  betraying his friends."
  :symbol #\h
  :colour #\y
  :type '(<unique>)
  :level 17
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
  :sex '<male>
  :special-abilities '((<spell> <mind-blast>) (<spell> (<cause> 2)) (<spell> <confusion>) (<spell> <blindness>)
                       (<spell> <heal>) (<frequency> 1/6))) 

(in-package :org.langband.vanilla)

(define-ego-item "id-4" "of Resist Acid"
  :numeric-id 4
  :rating 16
  :rarity 1
  :cost 1000
  :tval 37
  :max-sval 99
  :game-values (make-game-values :ignores 2 :resists 2))
(define-ego-item "id-5" "of Resist Lightning"
  :numeric-id 5
  :rating 10
  :rarity 1
  :cost 400
  :tval 37
  :max-sval 99
  :game-values (make-game-values :ignores 4 :resists 4))
(define-ego-item "id-6" "of Resist Fire"
  :numeric-id 6
  :rating 14
  :rarity 1
  :cost 800
  :tval 37
  :max-sval 99
  :game-values (make-game-values :ignores 1 :resists 1))
(define-ego-item "id-7" "of Resist Cold"
  :numeric-id 7
  :rating 12
  :rarity 1
  :cost 600
  :tval 37
  :max-sval 99
  :game-values (make-game-values :ignores 8 :resists 8))
(define-ego-item "id-8" "of Resistance"
  :numeric-id 8
  :rating 20
  :max-to-ac 10
  :rarity 2
  :cost 12500
  :tval 37
  :max-sval 99
  :game-values (make-game-values :ignores 15 :resists 15))
(define-ego-item "id-9" "of Elvenkind"
  :numeric-id 9
  :rating 25
  :xtra 2
  :max-to-ac 10
  :pval 3
  :rarity 4
  :cost 15000
  :tval 37
  :max-sval 99
  :flags '(<stealth>)
  :game-values (make-game-values :ignores 15 :resists 15))
(define-ego-item "id-11" "of Permanence"
  :numeric-id 11
  :rating 30
  :xtra 2
  :max-to-ac 10
  :rarity 2
  :cost 30000
  :tval 36
  :min-sval 2
  :max-sval 2
  :flags '(<hold-life>)
  :game-values (make-game-values :ignores 15 :resists 15 :sustains '(<str> <dex> <con> <int> <wis> <chr>)))
(define-ego-item "id-16" "of Resist Acid"
  :numeric-id 16
  :rating 16
  :rarity 6
  :cost 1000
  :tval 34
  :max-sval 99
  :game-values (make-game-values :ignores 2 :resists 2))
(define-ego-item "id-17" "of Resist Lightning"
  :numeric-id 17
  :rating 10
  :rarity 2
  :cost 400
  :tval 34
  :max-sval 99
  :game-values (make-game-values :ignores 4 :resists 4))
(define-ego-item "id-18" "of Resist Fire"
  :numeric-id 18
  :rating 14
  :rarity 3
  :cost 800
  :tval 34
  :max-sval 99
  :game-values (make-game-values :ignores 1 :resists 1))
(define-ego-item "id-19" "of Resist Cold"
  :numeric-id 19
  :rating 12
  :rarity 2
  :cost 600
  :tval 34
  :max-sval 99
  :game-values (make-game-values :ignores 8 :resists 8))
(define-ego-item "id-20" "of Resistance"
  :numeric-id 20
  :rating 20
  :max-to-ac 10
  :rarity 6
  :cost 12500
  :tval 34
  :max-sval 99
  :game-values (make-game-values :ignores 15 :resists 15))
(define-ego-item "id-24" "of Intelligence"
  :numeric-id 24
  :rating 13
  :pval 2
  :rarity 2
  :cost 500
  :tval 32
  :max-sval 99
  :flags '(<int>)
  :game-values (make-game-values :sustains '(<int>)))
(define-ego-item "id-25" "of Wisdom"
  :numeric-id 25
  :rating 13
  :pval 2
  :rarity 2
  :cost 500
  :tval 32
  :max-sval 99
  :flags '(<wis>)
  :game-values (make-game-values :sustains '(<wis>)))
(define-ego-item "id-26" "of Beauty"
  :numeric-id 26
  :rating 8
  :pval 4
  :rarity 2
  :cost 1000
  :tval 32
  :max-sval 99
  :flags '(<chr>)
  :game-values (make-game-values :sustains '(<chr>)))
(define-ego-item "id-27" "of the Magi"
  :numeric-id 27
  :rating 15
  :xtra 3
  :pval 3
  :rarity 4
  :cost 7500
  :tval 33
  :max-sval 99
  :flags '(<int>)
  :game-values (make-game-values :ignores 15 :resists 15 :sustains '(<int>)))
(define-ego-item "id-28" "of Might"
  :numeric-id 28
  :rating 19
  :pval 3
  :rarity 4
  :cost 2000
  :tval 33
  :max-sval 99
  :flags '(<free-action> <con> <dex> <str>)
  :game-values (make-game-values :sustains '(<str> <dex> <con>)))
(define-ego-item "id-29" "of Lordliness"
  :numeric-id 29
  :rating 17
  :pval 3
  :rarity 2
  :cost 2000
  :tval 33
  :max-sval 99
  :flags '(<chr> <wis>)
  :game-values (make-game-values :sustains '(<wis> <chr>)))
(define-ego-item "id-30" "of Seeing"
  :numeric-id 30
  :rating 8
  :pval 5
  :rarity 2
  :cost 1000
  :tval 32
  :max-sval 99
  :flags '(<see-invisible> <search>)
  :game-values (make-game-values :resists 128))
(define-ego-item "id-31" "of Infravision"
  :numeric-id 31
  :rating 11
  :pval 5
  :rarity 1
  :cost 500
  :tval 32
  :max-sval 99
  :flags '(<hide-type> <infravision>)
  :game-values (make-game-values))
(define-ego-item "id-32" "of Light"
  :numeric-id 32
  :rating 6
  :rarity 2
  :cost 500
  :tval 32
  :max-sval 99
  :flags '(<light-source>)
  :game-values (make-game-values :resists 64))
(define-ego-item "id-33" "of Telepathy"
  :numeric-id 33
  :rating 20
  :rarity 4
  :cost 50000
  :tval 33
  :max-sval 99
  :flags '(<telepathy>)
  :game-values (make-game-values))
(define-ego-item "id-34" "of Regeneration"
  :numeric-id 34
  :rating 10
  :rarity 4
  :cost 1500
  :tval 33
  :max-sval 99
  :flags '(<regeneration>)
  :game-values (make-game-values))
(define-ego-item "id-35" "of Teleportation"
  :numeric-id 35
  :rarity 2
  :tval 33
  :max-sval 99
  :flags '((<curse> <light>) <random-teleport>)
  :game-values (make-game-values))
(define-ego-item "id-36" "of Stupidity"
  :numeric-id 36
  :pval 5
  :rarity 1
  :tval 33
  :max-sval 99
  :flags '((<curse> <light>) <int>)
  :game-values (make-game-values))
(define-ego-item "id-37" "of Naivety"
  :numeric-id 37
  :pval 5
  :rarity 1
  :tval 33
  :max-sval 99
  :flags '((<curse> <light>) <wis>)
  :game-values (make-game-values))
(define-ego-item "id-38" "of Ugliness"
  :numeric-id 38
  :pval 5
  :rarity 2
  :tval 33
  :max-sval 99
  :flags '((<curse> <light>) <chr>)
  :game-values (make-game-values))
(define-ego-item "id-39" "of Sickliness"
  :numeric-id 39
  :pval 5
  :rarity 2
  :tval 33
  :max-sval 99
  :flags '((<curse> <light>) <con> <dex> <str>)
  :game-values (make-game-values))
(define-ego-item "id-40" "of Protection"
  :numeric-id 40
  :rating 10
  :max-to-ac 10
  :rarity 1
  :cost 500
  :tval 35
  :max-sval 99
  :game-values (make-game-values :ignores 15))
(define-ego-item "id-41" "of Stealth"
  :numeric-id 41
  :rating 10
  :pval 3
  :rarity 1
  :cost 500
  :tval 35
  :max-sval 99
  :flags '(<stealth>)
  :game-values (make-game-values))
(define-ego-item "id-42" "of Aman"
  :numeric-id 42
  :rating 20
  :xtra 2
  :max-to-ac 20
  :pval 3
  :rarity 7
  :cost 4000
  :tval 35
  :max-sval 99
  :flags '(<stealth>)
  :game-values (make-game-values :ignores 15))
(define-ego-item "id-44" "of Enveloping"
  :numeric-id 44
  :max-to-hit 10
  :max-to-dmg 10
  :rarity 1
  :tval 35
  :max-sval 99
  :flags '((<curse> <light>) <show-modifiers>)
  :game-values (make-game-values))
(define-ego-item "id-45" "of Vulnerability"
  :numeric-id 45
  :max-to-ac 50
  :rarity 1
  :tval 35
  :max-sval 99
  :flags '((<curse> <light>) <aggravate>)
  :game-values (make-game-values))
(define-ego-item "id-46" "of Irritation"
  :numeric-id 46
  :max-to-hit 15
  :max-to-dmg 15
  :rarity 1
  :tval 35
  :max-sval 99
  :flags '((<curse> <light>) <show-modifiers> <aggravate>)
  :game-values (make-game-values))
(define-ego-item "id-48" "of Free Action"
  :numeric-id 48
  :rating 11
  :rarity 3
  :cost 1000
  :tval 31
  :max-sval 99
  :flags '(<free-action>)
  :game-values (make-game-values))
(define-ego-item "id-49" "of Slaying"
  :numeric-id 49
  :rating 17
  :max-to-hit 5
  :max-to-dmg 5
  :rarity 4
  :cost 1500
  :tval 31
  :max-sval 99
  :flags '(<show-modifiers>)
  :game-values (make-game-values))
(define-ego-item "id-50" "of Agility"
  :numeric-id 50
  :rating 14
  :pval 5
  :rarity 6
  :cost 1000
  :tval 31
  :max-sval 99
  :flags '(<hide-type> <dex>)
  :game-values (make-game-values))
(define-ego-item "id-51" "of Power"
  :numeric-id 51
  :rating 22
  :max-to-hit 5
  :max-to-dmg 5
  :pval 5
  :rarity 12
  :cost 2500
  :tval 31
  :max-sval 99
  :flags '(<hide-type> <show-modifiers> <str>)
  :game-values (make-game-values))
(define-ego-item "id-54" "of Weakness"
  :numeric-id 54
  :pval 10
  :rarity 1
  :tval 31
  :max-sval 99
  :flags '((<curse> <light>) <str>)
  :game-values (make-game-values))
(define-ego-item "id-55" "of Clumsiness"
  :numeric-id 55
  :pval 10
  :rarity 1
  :tval 31
  :max-sval 99
  :flags '((<curse> <light>) <dex>)
  :game-values (make-game-values))
(define-ego-item "id-56" "of Slow Descent"
  :numeric-id 56
  :rating 7
  :rarity 9
  :cost 250
  :tval 30
  :max-sval 99
  :flags '(<feather-fall>)
  :game-values (make-game-values))
(define-ego-item "id-57" "of Stealth"
  :numeric-id 57
  :rating 16
  :pval 3
  :rarity 12
  :cost 500
  :tval 30
  :max-sval 99
  :flags '(<stealth>)
  :game-values (make-game-values))
(define-ego-item "id-58" "of Free Action"
  :numeric-id 58
  :rating 15
  :rarity 25
  :cost 1000
  :tval 30
  :max-sval 99
  :flags '(<free-action>)
  :game-values (make-game-values))
(define-ego-item "id-59" "of Speed"
  :numeric-id 59
  :rating 25
  :pval 10
  :rarity 100
  :cost 200000
  :tval 30
  :max-sval 99
  :flags '(<hide-type> <speed>)
  :game-values (make-game-values))
(define-ego-item "id-61" "of Noise"
  :numeric-id 61
  :rarity 1
  :tval 30
  :max-sval 99
  :flags '((<curse> <light>) <aggravate>)
  :game-values (make-game-values))
(define-ego-item "id-62" "of Slowness"
  :numeric-id 62
  :pval 5
  :rarity 1
  :tval 30
  :max-sval 99
  :flags '((<curse> <light>) <speed>)
  :game-values (make-game-values))
(define-ego-item "id-63" "of Annoyance"
  :numeric-id 63
  :pval 10
  :rarity 1
  :tval 30
  :max-sval 99
  :flags '((<curse> <light>) <aggravate> <speed>)
  :game-values (make-game-values))
(define-ego-item "id-64" "(Holy Avenger)"
  :numeric-id 64
  :rating 30
  :xtra 1
  :max-to-hit 6
  :max-to-dmg 6
  :max-to-ac 4
  :pval 4
  :rarity 10
  :cost 20000
  :tval 23
  :max-sval 99
  :flags '(<blessed-blade> <see-invisible> <wis>)
  :game-values (make-game-values :slays '(<evil> <undead> <demon>)))
(define-ego-item "id-65" "(Defender)"
  :numeric-id 65
  :rating 25
  :xtra 1
  :max-to-hit 4
  :max-to-dmg 4
  :max-to-ac 8
  :pval 4
  :rarity 10
  :cost 15000
  :tval 23
  :max-sval 99
  :flags '(<regeneration> <feather-fall> <see-invisible> <free-action> <stealth>)
  :game-values (make-game-values :ignores 15 :resists 15))
(define-ego-item "id-66" "(Blessed)"
  :numeric-id 66
  :rating 20
  :xtra 3
  :pval 3
  :rarity 10
  :cost 5000
  :tval 23
  :max-sval 99
  :flags '(<blessed-blade> <wis>)
  :game-values (make-game-values))
(define-ego-item "id-68" "of Westernesse"
  :numeric-id 68
  :rating 20
  :max-to-hit 5
  :max-to-dmg 5
  :pval 2
  :rarity 10
  :cost 20000
  :tval 23
  :max-sval 99
  :flags '(<see-invisible> <free-action> <con> <dex> <str>)
  :game-values (make-game-values :slays '(<orc> <troll> <giant>)))
(define-ego-item "id-69" "of Extra Attacks"
  :numeric-id 69
  :rating 20
  :pval 2
  :rarity 10
  :cost 10000
  :tval 23
  :max-sval 99
  :flags '(<extra-blows>)
  :game-values (make-game-values))
(define-ego-item "id-72" "of Melting"
  :numeric-id 72
  :rating 15
  :rarity 10
  :cost 8000
  :tval 23
  :max-sval 99
  :flags '((<brand> <acid>))
  :game-values (make-game-values :ignores 2 :resists 2))
(define-ego-item "id-73" "of Shocking"
  :numeric-id 73
  :rating 20
  :rarity 10
  :cost 4500
  :tval 23
  :max-sval 99
  :flags '((<brand> <electricity>))
  :game-values (make-game-values :ignores 4 :resists 4))
(define-ego-item "id-74" "of Burning"
  :numeric-id 74
  :rating 20
  :rarity 10
  :cost 3000
  :tval 23
  :max-sval 99
  :flags '((<brand> <fire>))
  :game-values (make-game-values :ignores 1 :resists 1))
(define-ego-item "id-75" "of Freezing"
  :numeric-id 75
  :rating 15
  :rarity 10
  :cost 2500
  :tval 23
  :max-sval 99
  :flags '((<brand> <cold>))
  :game-values (make-game-values :ignores 8 :resists 8))
(define-ego-item "id-80" "of Slay Animal"
  :numeric-id 80
  :rating 18
  :rarity 6
  :cost 3500
  :tval 23
  :max-sval 99
  :game-values (make-game-values :slays '(<animal>)))
(define-ego-item "id-81" "of Slay Evil"
  :numeric-id 81
  :rating 18
  :rarity 6
  :cost 3500
  :tval 23
  :max-sval 99
  :game-values (make-game-values :slays '(<evil>)))
(define-ego-item "id-82" "of Slay Undead"
  :numeric-id 82
  :rating 18
  :rarity 6
  :cost 3500
  :tval 23
  :max-sval 99
  :game-values (make-game-values :slays '(<undead>)))
(define-ego-item "id-83" "of Slay Demon"
  :numeric-id 83
  :rating 14
  :rarity 6
  :cost 2500
  :tval 23
  :max-sval 99
  :game-values (make-game-values :slays '(<demon>)))
(define-ego-item "id-84" "of Slay Orc"
  :numeric-id 84
  :rating 10
  :rarity 6
  :cost 2500
  :tval 23
  :max-sval 99
  :game-values (make-game-values :slays '(<orc>)))
(define-ego-item "id-85" "of Slay Troll"
  :numeric-id 85
  :rating 10
  :rarity 6
  :cost 2500
  :tval 23
  :max-sval 99
  :game-values (make-game-values :slays '(<troll>)))
(define-ego-item "id-86" "of Slay Giant"
  :numeric-id 86
  :rating 14
  :rarity 6
  :cost 2500
  :tval 23
  :max-sval 99
  :game-values (make-game-values :slays '(<giant>)))
(define-ego-item "id-87" "of Slay Dragon"
  :numeric-id 87
  :rating 18
  :rarity 6
  :cost 3500
  :tval 23
  :max-sval 99
  :game-values (make-game-values :slays '(<dragon>)))
(define-ego-item "id-88" "of *Slay* Animal"
  :numeric-id 88
  :rating 20
  :pval 2
  :rarity 25
  :cost 6000
  :tval 23
  :max-sval 99
  :flags '(<regeneration> <int>)
  :game-values (make-game-values :slays '(<animal>)))
(define-ego-item "id-89" "of *Slay* Evil"
  :numeric-id 89
  :rating 20
  :pval 2
  :rarity 25
  :cost 5000
  :tval 23
  :max-sval 99
  :flags '(<blessed-blade> <wis>)
  :game-values (make-game-values :slays '(<evil>)))
(define-ego-item "id-90" "of *Slay* Undead"
  :numeric-id 90
  :rating 24
  :pval 2
  :rarity 25
  :cost 8000
  :tval 23
  :max-sval 99
  :flags '(<see-invisible> <wis>)
  :game-values (make-game-values :slays '(<undead>)))
(define-ego-item "id-91" "of *Slay* Demon"
  :numeric-id 91
  :rating 16
  :pval 2
  :rarity 25
  :cost 4000
  :tval 23
  :max-sval 99
  :flags '(<int>)
  :game-values (make-game-values :slays '(<demon>)))
(define-ego-item "id-92" "of *Slay* Orc"
  :numeric-id 92
  :rating 14
  :pval 2
  :rarity 25
  :cost 4000
  :tval 23
  :max-sval 99
  :flags '(<dex>)
  :game-values (make-game-values :slays '(<orc>)))
(define-ego-item "id-93" "of *Slay* Troll"
  :numeric-id 93
  :rating 14
  :pval 2
  :rarity 25
  :cost 4000
  :tval 23
  :max-sval 99
  :flags '(<str>)
  :game-values (make-game-values :slays '(<troll>)))
(define-ego-item "id-94" "of *Slay* Giant"
  :numeric-id 94
  :rating 16
  :pval 2
  :rarity 25
  :cost 4000
  :tval 23
  :max-sval 99
  :flags '(<str>)
  :game-values (make-game-values :slays '(<giant>)))
(define-ego-item "id-95" "of *Slay* Dragon"
  :numeric-id 95
  :rating 24
  :pval 2
  :rarity 25
  :cost 6000
  :tval 23
  :max-sval 99
  :flags '((<execute> <dragon>) <con>)
  :game-values (make-game-values :slays '(<dragon>)))
(define-ego-item "id-100" "of Digging"
  :numeric-id 100
  :rating 4
  :pval 5
  :rarity 1
  :cost 500
  :tval 20
  :max-sval 99
  :flags '((<brand> <acid>) <tunnel>)
  :game-values (make-game-values :ignores 15))
(define-ego-item "id-102" "of Morgul"
  :numeric-id 102
  :max-to-hit 20
  :max-to-dmg 20
  :max-to-ac 10
  :depth 5
  :rarity 20
  :tval 23
  :max-sval 99
  :flags '((<curse> <light>) (<curse> <heavy>) <aggravate> <see-invisible>)
  :game-values (make-game-values))
(define-ego-item "id-104" "of Accuracy"
  :numeric-id 104
  :rating 10
  :max-to-hit 15
  :max-to-dmg 5
  :rarity 1
  :cost 1000
  :tval 19
  :max-sval 99
  :game-values (make-game-values))
(define-ego-item "id-105" "of Velocity"
  :numeric-id 105
  :rating 10
  :max-to-hit 5
  :max-to-dmg 15
  :rarity 1
  :cost 1000
  :tval 19
  :max-sval 99
  :game-values (make-game-values))
(define-ego-item "id-108" "of Extra Might"
  :numeric-id 108
  :rating 20
  :max-to-hit 5
  :max-to-dmg 10
  :pval 2
  :rarity 4
  :cost 10000
  :tval 19
  :max-sval 99
  :flags '(<extra-might>)
  :game-values (make-game-values))
(define-ego-item "id-109" "of Extra Shots"
  :numeric-id 109
  :rating 20
  :max-to-hit 10
  :max-to-dmg 5
  :pval 1
  :rarity 4
  :cost 10000
  :tval 19
  :max-sval 99
  :flags '(<extra-shots>)
  :game-values (make-game-values))
(define-ego-item "id-112" "of Hurt Animal"
  :numeric-id 112
  :rating 10
  :rarity 25
  :cost 25
  :tval 18
  :max-sval 99
  :game-values (make-game-values :slays '(<animal>)))
(define-ego-item "id-113" "of Hurt Evil"
  :numeric-id 113
  :rating 10
  :rarity 25
  :cost 25
  :tval 18
  :max-sval 99
  :game-values (make-game-values :slays '(<evil>)))
(define-ego-item "id-114" "of Hurt Undead"
  :numeric-id 114
  :rating 10
  :rarity 50
  :cost 25
  :tval 18
  :max-sval 99
  :game-values (make-game-values :slays '(<undead>)))
(define-ego-item "id-115" "of Hurt Demon"
  :numeric-id 115
  :rating 10
  :rarity 50
  :cost 15
  :tval 18
  :max-sval 99
  :game-values (make-game-values :slays '(<demon>)))
(define-ego-item "id-116" "of Hurt Orc"
  :numeric-id 116
  :rating 10
  :rarity 50
  :cost 15
  :tval 18
  :max-sval 99
  :game-values (make-game-values :slays '(<orc>)))
(define-ego-item "id-117" "of Hurt Troll"
  :numeric-id 117
  :rating 10
  :rarity 50
  :cost 15
  :tval 18
  :max-sval 99
  :game-values (make-game-values :slays '(<troll>)))
(define-ego-item "id-118" "of Hurt Giant"
  :numeric-id 118
  :rating 10
  :rarity 50
  :cost 15
  :tval 18
  :max-sval 99
  :game-values (make-game-values :slays '(<giant>)))
(define-ego-item "id-119" "of Hurt Dragon"
  :numeric-id 119
  :rating 10
  :rarity 50
  :cost 35
  :tval 18
  :max-sval 99
  :game-values (make-game-values :slays '(<dragon>)))
(define-ego-item "id-122" "of Flame"
  :numeric-id 122
  :rating 10
  :rarity 25
  :cost 30
  :tval 18
  :max-sval 99
  :flags '((<brand> <fire>))
  :game-values (make-game-values :ignores 1))
(define-ego-item "id-123" "of Frost"
  :numeric-id 123
  :rating 10
  :rarity 25
  :cost 25
  :tval 18
  :max-sval 99
  :flags '((<brand> <cold>))
  :game-values (make-game-values :ignores 8))
(define-ego-item "id-124" "of Wounding"
  :numeric-id 124
  :rating 5
  :max-to-hit 5
  :max-to-dmg 5
  :rarity 8
  :cost 20
  :tval 18
  :max-sval 99
  :game-values (make-game-values))
(define-ego-item "id-125" "of Backbiting"
  :numeric-id 125
  :max-to-hit 50
  :max-to-dmg 50
  :depth 5
  :rarity 30
  :tval 18
  :max-sval 99
  :flags '((<curse> <light>))
  :game-values (make-game-values))
(define-ego-item "id-126" "(Shattered)"
  :numeric-id 126
  :max-to-hit 5
  :max-to-dmg 5
  :game-values (make-game-values))
(define-ego-item "id-127" "(Blasted)"
  :numeric-id 127
  :max-to-ac 10
  :game-values (make-game-values))
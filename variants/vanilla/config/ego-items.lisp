(in-package :org.langband.vanilla)

(define-ego-item "ego-item-4" "of Resist Acid"
  :numeric-id 4
  :power-lvl 16
  :locations '((0 . 1))
  :cost 1000
  :obj-types '(<hard-body-armour> <soft-body-armour>)
  :flags '((<ignore> <acid>) (<resist> <acid>)))

(define-ego-item "ego-item-5" "of Resist Lightning"
  :numeric-id 5
  :power-lvl 10
  :locations '((0 . 1))
  :cost 400
  :obj-types '(<hard-body-armour> <soft-body-armour>)
  :flags '((<ignore> <electricity>) (<resist> <electricity>)))

(define-ego-item "ego-item-6" "of Resist Fire"
  :numeric-id 6
  :power-lvl 14
  :locations '((0 . 1))
  :cost 800
  :obj-types '(<hard-body-armour> <soft-body-armour>)
  :flags '((<ignore> <fire>) (<resist> <fire>)))

(define-ego-item "ego-item-7" "of Resist Cold"
  :numeric-id 7
  :power-lvl 12
  :locations '((0 . 1))
  :cost 600
  :obj-types '(<hard-body-armour> <soft-body-armour>)
  :flags '((<ignore> <cold>) (<resist> <cold>)))

(define-ego-item "ego-item-8" "of Resistance"
  :numeric-id 8
  :power-lvl 20
  :max-to-ac 10
  :locations '((0 . 2))
  :cost 12500
  :obj-types '(<hard-body-armour> <soft-body-armour>)
  :flags '((<ignore> <cold>) (<ignore> <fire>) (<ignore> <electricity>) (<ignore> <acid>) (<resist> <cold>) (<resist> <fire>)
           (<resist> <electricity>) (<resist> <acid>)))

(define-ego-item "ego-item-9" "of Elvenkind"
  :numeric-id 9
  :power-lvl 25
  :xtra 2
  :max-to-ac 10
  :pval 3
  :locations '((0 . 4))
  :cost 15000
  :obj-types '(<hard-body-armour> <soft-body-armour>)
  :flags '((<ignore> <cold>) (<ignore> <fire>) (<ignore> <electricity>) (<ignore> <acid>) (<resist> <cold>) (<resist> <fire>)
           (<resist> <electricity>) (<resist> <acid>) <stealth>))

(define-ego-item "ego-item-10" "of Vulnerability"
  :numeric-id 10
  :max-to-ac 50
  :locations '((0 . 2))
  :obj-types '(<hard-body-armour> <soft-body-armour>)
  :flags '((<curse> <light>) <aggravate>))

(define-ego-item "ego-item-11" "of Permanence"
  :numeric-id 11
  :power-lvl 30
  :xtra 2
  :max-to-ac 20
  :locations '((0 . 4))
  :cost 30000
  :obj-types '((:tval 36 :min-sval 2 :max-sval 2))
  :flags '((<ignore> <cold>) (<ignore> <fire>) (<ignore> <electricity>) (<ignore> <acid>) (<resist> <cold>) (<resist> <fire>)
           (<resist> <electricity>) (<resist> <acid>) <hold-life> (<sustain> <chr>) (<sustain> <wis>) (<sustain> <int>) (<sustain> <con>)
           (<sustain> <dex>) (<sustain> <str>)))

(define-ego-item "ego-item-12" "(Dwarven)"
  :numeric-id 12
  :power-lvl 18
  :max-to-ac 15
  :pval 2
  :locations '((0 . 2))
  :cost 5000
  :obj-types '((:tval 37 :min-sval 3 :max-sval 99))
  :flags '(<hide-type> <infravision> <con> <str> <free-action> (<ignore> <fire>) (<ignore> <acid>)))

(define-ego-item "ego-item-16" "of Resist Acid"
  :numeric-id 16
  :power-lvl 16
  :locations '((0 . 1))
  :cost 1000
  :obj-types '(<shield>)
  :flags '((<ignore> <acid>) (<resist> <acid>)))

(define-ego-item "ego-item-17" "of Resist Lightning"
  :numeric-id 17
  :power-lvl 10
  :locations '((0 . 1))
  :cost 400
  :obj-types '(<shield>)
  :flags '((<ignore> <electricity>) (<resist> <electricity>)))

(define-ego-item "ego-item-18" "of Resist Fire"
  :numeric-id 18
  :power-lvl 14
  :locations '((0 . 1))
  :cost 800
  :obj-types '(<shield>)
  :flags '((<ignore> <fire>) (<resist> <fire>)))

(define-ego-item "ego-item-19" "of Resist Cold"
  :numeric-id 19
  :power-lvl 12
  :locations '((0 . 1))
  :cost 600
  :obj-types '(<shield>)
  :flags '((<ignore> <cold>) (<resist> <cold>)))

(define-ego-item "ego-item-20" "of Resistance"
  :numeric-id 20
  :power-lvl 20
  :max-to-ac 10
  :locations '((0 . 2))
  :cost 12500
  :obj-types '(<shield>)
  :flags '((<ignore> <cold>) (<ignore> <fire>) (<ignore> <electricity>) (<ignore> <acid>) (<resist> <cold>) (<resist> <fire>)
           (<resist> <electricity>) (<resist> <acid>)))

(define-ego-item "ego-item-21" "of Elvenkind"
  :numeric-id 21
  :power-lvl 25
  :xtra 2
  :max-to-ac 10
  :pval 3
  :locations '((0 . 4))
  :cost 18000
  :obj-types '(<shield>)
  :flags '((<ignore> <electricity>) (<ignore> <cold>) (<ignore> <fire>) (<ignore> <acid>) (<resist> <electricity>) (<resist> <acid>)
           (<resist> <cold>) (<resist> <fire>) <stealth>))

(define-ego-item "ego-item-22" "of Preservation"
  :numeric-id 22
  :power-lvl 25
  :xtra 2
  :max-to-ac 20
  :locations '((60 . 4))
  :cost 24000
  :obj-types '(<shield>)
  :flags '((<ignore> <cold>) (<ignore> <fire>) (<ignore> <electricity>) (<ignore> <acid>) <hold-life> (<sustain> <dex>) (<sustain> <con>)
           (<sustain> <str>) (<resist> <disenchant>)))

(define-ego-item "ego-item-23" "of Vulnerability"
  :numeric-id 23
  :max-to-ac 50
  :locations '((0 . 2))
  :obj-types '(<shield>)
  :flags '((<curse> <light>) <aggravate>))

(define-ego-item "ego-item-24" "of Intelligence"
  :numeric-id 24
  :power-lvl 13
  :pval 2
  :locations '((0 . 2))
  :cost 500
  :obj-types '(<helmet>)
  :flags '((<sustain> <int>) <int>))

(define-ego-item "ego-item-25" "of Wisdom"
  :numeric-id 25
  :power-lvl 13
  :pval 2
  :locations '((0 . 2))
  :cost 500
  :obj-types '(<helmet>)
  :flags '((<sustain> <wis>) <wis>))

(define-ego-item "ego-item-26" "of Beauty"
  :numeric-id 26
  :power-lvl 8
  :pval 4
  :locations '((0 . 2))
  :cost 1000
  :obj-types '(<helmet>)
  :flags '((<sustain> <chr>) <chr>))

(define-ego-item "ego-item-27" "of the Magi"
  :numeric-id 27
  :power-lvl 15
  :xtra 3
  :pval 3
  :locations '((0 . 4))
  :cost 7500
  :obj-types '(<crown>)
  :flags '((<ignore> <cold>) (<ignore> <fire>) (<ignore> <electricity>) (<ignore> <acid>) (<resist> <cold>) (<resist> <fire>)
           (<resist> <electricity>) (<resist> <acid>) (<sustain> <int>) <int>))

(define-ego-item "ego-item-28" "of Might"
  :numeric-id 28
  :power-lvl 19
  :pval 3
  :locations '((0 . 4))
  :cost 7500
  :obj-types '(<crown>)
  :flags '((<ignore> <cold>) (<ignore> <fire>) (<ignore> <electricity>) (<ignore> <acid>) <free-action> (<sustain> <con>) (<sustain> <dex>)
           (<sustain> <str>) <con> <dex> <str>))

(define-ego-item "ego-item-29" "of Lordliness"
  :numeric-id 29
  :power-lvl 17
  :xtra 3
  :pval 3
  :locations '((0 . 2))
  :cost 7500
  :obj-types '(<crown>)
  :flags '((<ignore> <cold>) (<ignore> <fire>) (<ignore> <electricity>) (<ignore> <acid>) (<resist> <fear>) (<sustain> <chr>)
           (<sustain> <wis>) <chr> <wis>))

(define-ego-item "ego-item-30" "of Seeing"
  :numeric-id 30
  :power-lvl 8
  :pval 5
  :locations '((0 . 2))
  :cost 2000
  :obj-types '(<helmet> <crown>)
  :flags '(<see-invisible> (<resist> <blindness>) <search>))

(define-ego-item "ego-item-31" "of Infravision"
  :numeric-id 31
  :power-lvl 11
  :pval 5
  :locations '((0 . 2))
  :cost 500
  :obj-types '(<helmet>)
  :flags '(<hide-type> <infravision>))

(define-ego-item "ego-item-32" "of Light"
  :numeric-id 32
  :power-lvl 6
  :locations '((0 . 2))
  :cost 1000
  :obj-types '(<helmet>)
  :flags '((<resist> <light>) <light-source>))

(define-ego-item "ego-item-33" "of Telepathy"
  :numeric-id 33
  :power-lvl 20
  :locations '((0 . 6))
  :cost 50000
  :obj-types '(<crown> <helmet>)
  :flags '(<telepathy>))

(define-ego-item "ego-item-34" "of Regeneration"
  :numeric-id 34
  :power-lvl 10
  :locations '((0 . 4))
  :cost 1500
  :obj-types '(<crown> <helmet>)
  :flags '(<regeneration>))

(define-ego-item "ego-item-35" "of Teleportation"
  :numeric-id 35
  :locations '((0 . 2))
  :obj-types '(<helmet>)
  :flags '((<curse> <light>) <random-teleport>))

(define-ego-item "ego-item-36" "of Serenity"
  :numeric-id 36
  :power-lvl 20
  :locations '((0 . 6))
  :cost 4000
  :obj-types '(<crown>)
  :flags '((<resist> <fear>) (<resist> <confusion>) (<resist> <sound>)))

(define-ego-item "ego-item-37" "of Night and Day"
  :numeric-id 37
  :power-lvl 18
  :locations '((35 . 6))
  :cost 4000
  :obj-types '(<crown>)
  :flags '((<ignore> <acid>) (<resist> <blindness>) <see-invisible> <light-source> (<resist> <darkness>) (<resist> <light>)))

(define-ego-item "ego-item-38" "of Dullness"
  :numeric-id 38
  :pval 5
  :locations '((0 . 2))
  :obj-types '(<crown> <helmet>)
  :flags '((<curse> <light>) <chr> <wis> <int>))

(define-ego-item "ego-item-39" "of Sickliness"
  :numeric-id 39
  :pval 5
  :locations '((0 . 2))
  :obj-types '(<crown>)
  :flags '((<curse> <light>) <con> <dex> <str>))

(define-ego-item "ego-item-40" "of Protection"
  :numeric-id 40
  :power-lvl 10
  :max-to-ac 10
  :locations '((0 . 2))
  :cost 1500
  :obj-types '(<cloak>)
  :flags '((<resist> <shards>) (<ignore> <cold>) (<ignore> <fire>) (<ignore> <electricity>) (<ignore> <acid>)))

(define-ego-item "ego-item-41" "of Stealth"
  :numeric-id 41
  :power-lvl 10
  :pval 3
  :locations '((0 . 1))
  :cost 500
  :obj-types '(<cloak>)
  :flags '(<stealth>))

(define-ego-item "ego-item-42" "of Aman"
  :numeric-id 42
  :power-lvl 20
  :xtra 2
  :max-to-ac 20
  :pval 3
  :locations '((0 . 6))
  :cost 4000
  :obj-types '(<cloak>)
  :flags '((<ignore> <cold>) (<ignore> <fire>) (<ignore> <electricity>) (<ignore> <acid>) <stealth>))

(define-ego-item "ego-item-43" "of the Magi"
  :numeric-id 43
  :power-lvl 15
  :xtra 3
  :max-to-ac 4
  :pval 2
  :locations '((30 . 3))
  :cost 2000
  :obj-types '(<cloak>)
  :flags '((<ignore> <acid>) <hide-type> <stealth> (<sustain> <int>) <int>))

(define-ego-item "ego-item-44" "of Enveloping"
  :numeric-id 44
  :max-to-hit 10
  :max-to-dmg 10
  :locations '((0 . 1))
  :obj-types '(<cloak>)
  :flags '((<curse> <light>) <show-modifiers>))

(define-ego-item "ego-item-45" "of Vulnerability"
  :numeric-id 45
  :max-to-ac 50
  :locations '((0 . 1))
  :obj-types '(<cloak>)
  :flags '((<curse> <light>) <aggravate>))

(define-ego-item "ego-item-46" "of Irritation"
  :numeric-id 46
  :max-to-hit 15
  :max-to-dmg 15
  :locations '((0 . 1))
  :obj-types '(<cloak>)
  :flags '((<curse> <light>) <show-modifiers> <aggravate>))

(define-ego-item "ego-item-48" "of Free Action"
  :numeric-id 48
  :power-lvl 11
  :locations '((0 . 4))
  :cost 1000
  :obj-types '(<gloves>)
  :flags '(<free-action>))

(define-ego-item "ego-item-49" "of Slaying"
  :numeric-id 49
  :power-lvl 17
  :max-to-hit 5
  :max-to-dmg 5
  :locations '((0 . 4))
  :cost 1500
  :obj-types '(<gloves>)
  :flags '(<show-modifiers>))

(define-ego-item "ego-item-50" "of Agility"
  :numeric-id 50
  :power-lvl 14
  :pval 5
  :locations '((0 . 6))
  :cost 1000
  :obj-types '(<gloves>)
  :flags '(<hide-type> <dex>))

(define-ego-item "ego-item-51" "of Power"
  :numeric-id 51
  :power-lvl 22
  :max-to-hit 5
  :max-to-dmg 5
  :pval 5
  :locations '((0 . 6))
  :cost 2500
  :obj-types '(<gloves>)
  :flags '(<hide-type> <show-modifiers> <str>))

(define-ego-item "ego-item-52" "of Thievery"
  :numeric-id 52
  :power-lvl 22
  :max-to-hit 8
  :max-to-dmg 3
  :pval 5
  :locations '((40 . 12))
  :cost 5000
  :obj-types '((:tval 31 :min-sval 1 :max-sval 1))
  :flags '(<hide-type> <free-action> <feather-fall> <show-modifiers> <search> <dex>))

(define-ego-item "ego-item-53" "of Combat"
  :numeric-id 53
  :power-lvl 22
  :max-to-hit 3
  :max-to-dmg 8
  :pval 2
  :locations '((50 . 12))
  :cost 7000
  :obj-types '((:tval 31 :min-sval 2 :max-sval 99))
  :flags '(<hide-type> <aggravate> <show-modifiers> <con> <str>))

(define-ego-item "ego-item-54" "of Weakness"
  :numeric-id 54
  :pval 10
  :locations '((0 . 2))
  :obj-types '(<gloves>)
  :flags '(<hide-type> (<curse> <light>) <str>))

(define-ego-item "ego-item-55" "of Clumsiness"
  :numeric-id 55
  :pval 10
  :locations '((0 . 2))
  :obj-types '(<gloves>)
  :flags '(<hide-type> (<curse> <light>) <dex>))

(define-ego-item "ego-item-56" "of Slow Descent"
  :numeric-id 56
  :power-lvl 7
  :locations '((0 . 5))
  :cost 250
  :obj-types '(<boots>)
  :flags '(<feather-fall>))

(define-ego-item "ego-item-57" "of Stealth"
  :numeric-id 57
  :power-lvl 16
  :pval 3
  :locations '((0 . 6))
  :cost 500
  :obj-types '(<boots>)
  :flags '(<hide-type> <stealth>))

(define-ego-item "ego-item-58" "of Free Action"
  :numeric-id 58
  :power-lvl 15
  :locations '((0 . 8))
  :cost 1000
  :obj-types '(<boots>)
  :flags '(<free-action>))

(define-ego-item "ego-item-59" "of Speed"
  :numeric-id 59
  :power-lvl 25
  :pval 10
  :locations '((0 . 24))
  :cost 100000
  :obj-types '(<boots>)
  :flags '(<hide-type> <speed>))

(define-ego-item "ego-item-60" "of Stability"
  :numeric-id 60
  :power-lvl 20
  :locations '((0 . 10))
  :cost 5000
  :obj-types '(<boots>)
  :flags '(<feather-fall> (<resist> <nexus>)))

(define-ego-item "ego-item-61" "of Elvenkind"
  :numeric-id 61
  :power-lvl 30
  :pval 5
  :locations '((60 . 30))
  :cost 200000
  :obj-types '((:tval 30 :min-sval 2 :max-sval 3))
  :flags '((<ignore> <fire>) (<ignore> <acid>) <feather-fall> <hide-type> <speed> <stealth>))

(define-ego-item "ego-item-62" "of Slowness"
  :numeric-id 62
  :pval 5
  :locations '((0 . 5))
  :obj-types '(<boots>)
  :flags '(<hide-type> (<curse> <light>) <speed>))

(define-ego-item "ego-item-63" "of Annoyance"
  :numeric-id 63
  :pval 10
  :locations '((0 . 10))
  :obj-types '(<boots>)
  :flags '(<hide-type> (<curse> <light>) <aggravate> <stealth> <speed>))

(define-ego-item "ego-item-64" "(Holy Avenger)"
  :numeric-id 64
  :power-lvl 30
  :xtra 1
  :max-to-hit 6
  :max-to-dmg 6
  :max-to-ac 4
  :pval 4
  :locations '((0 . 12))
  :cost 20000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<resist> <fear>) <blessed-blade> <see-invisible> (<slay> <demon>) (<slay> <undead>) (<slay> <evil>) <wis>))

(define-ego-item "ego-item-65" "(Defender)"
  :numeric-id 65
  :power-lvl 25
  :xtra 1
  :max-to-hit 4
  :max-to-dmg 4
  :max-to-ac 8
  :pval 4
  :locations '((0 . 12))
  :cost 15000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<ignore> <cold>) (<ignore> <fire>) (<ignore> <electricity>) (<ignore> <acid>) (<resist> <cold>) (<resist> <fire>)
           (<resist> <electricity>) (<resist> <acid>) <regeneration> <feather-fall> <see-invisible> <free-action> <stealth>))

(define-ego-item "ego-item-66" "(Blessed)"
  :numeric-id 66
  :power-lvl 20
  :xtra 3
  :pval 3
  :locations '((0 . 10))
  :cost 5000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '(<blessed-blade> <wis>))

(define-ego-item "ego-item-67" "of Gondolin"
  :numeric-id 67
  :power-lvl 30
  :xtra 3
  :max-to-hit 7
  :max-to-dmg 7
  :locations '((30 . 20))
  :cost 25000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<ignore> <fire>) (<ignore> <acid>) <free-action> <see-invisible> (<resist> <darkness>) <light-source> (<slay> <dragon>)
           (<slay> <troll>) (<slay> <orc>) (<slay> <demon>)))

(define-ego-item "ego-item-68" "of Westernesse"
  :numeric-id 68
  :power-lvl 20
  :max-to-hit 5
  :max-to-dmg 5
  :pval 2
  :locations '((0 . 10))
  :cost 20000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '(<see-invisible> <free-action> (<slay> <giant>) (<slay> <troll>) (<slay> <orc>) <con> <dex> <str>))

(define-ego-item "ego-item-69" "of Extra Attacks"
  :numeric-id 69
  :power-lvl 20
  :pval 2
  :locations '((0 . 10))
  :cost 10000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '(<hide-type> <extra-blows>))

(define-ego-item "ego-item-70" "of Fury"
  :numeric-id 70
  :power-lvl 30
  :max-to-hit 10
  :max-to-dmg 10
  :pval 2
  :locations '((40 . 20))
  :cost 20000
  :obj-types '((:tval 23 :min-sval 16 :max-sval 99) (:tval 22 :min-sval 10 :max-sval 99) (:tval 21 :min-sval 12 :max-sval 99))
  :flags '((<ignore> <fire>) (<ignore> <acid>) <hide-type> (<resist> <fear>) <aggravate> <extra-blows> <str>))

(define-ego-item "ego-item-72" "of Acid"
  :numeric-id 72
  :power-lvl 20
  :locations '((0 . 10))
  :cost 5000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<ignore> <acid>) (<resist> <acid>) (<brand> <acid>)))

(define-ego-item "ego-item-73" "of Lightning"
  :numeric-id 73
  :power-lvl 20
  :locations '((0 . 10))
  :cost 4500
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<ignore> <electricity>) (<resist> <electricity>) (<brand> <electricity>)))

(define-ego-item "ego-item-74" "of Flame"
  :numeric-id 74
  :power-lvl 15
  :locations '((0 . 8))
  :cost 3500
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<ignore> <fire>) (<resist> <fire>) (<brand> <fire>)))

(define-ego-item "ego-item-75" "of Frost"
  :numeric-id 75
  :power-lvl 15
  :locations '((0 . 8))
  :cost 3000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<ignore> <cold>) (<resist> <cold>) (<brand> <cold>)))

(define-ego-item "ego-item-76" "of Venom"
  :numeric-id 76
  :power-lvl 15
  :locations '((0 . 8))
  :cost 4000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<brand> <poison>)))

(define-ego-item "ego-item-80" "of Slay Animal"
  :numeric-id 80
  :power-lvl 18
  :locations '((0 . 6))
  :cost 3000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<slay> <animal>)))

(define-ego-item "ego-item-81" "of Slay Evil"
  :numeric-id 81
  :power-lvl 18
  :locations '((0 . 6))
  :cost 3000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<slay> <evil>)))

(define-ego-item "ego-item-82" "of Slay Undead"
  :numeric-id 82
  :power-lvl 18
  :locations '((0 . 6))
  :cost 3500
  :obj-types '(<sword> <pole-arm> (:tval 21 :min-sval 0 :max-sval 18))
  :flags '((<slay> <undead>)))

(define-ego-item "ego-item-83" "of Slay Demon"
  :numeric-id 83
  :power-lvl 14
  :locations '((0 . 6))
  :cost 3500
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<slay> <demon>)))

(define-ego-item "ego-item-84" "of Slay Orc"
  :numeric-id 84
  :power-lvl 10
  :locations '((0 . 6))
  :cost 2500
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<slay> <orc>)))

(define-ego-item "ego-item-85" "of Slay Troll"
  :numeric-id 85
  :power-lvl 10
  :locations '((0 . 6))
  :cost 2500
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<slay> <troll>)))

(define-ego-item "ego-item-86" "of Slay Giant"
  :numeric-id 86
  :power-lvl 14
  :locations '((0 . 6))
  :cost 2500
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<slay> <giant>)))

(define-ego-item "ego-item-87" "of Slay Dragon"
  :numeric-id 87
  :power-lvl 18
  :locations '((0 . 6))
  :cost 3500
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<slay> <dragon>)))

(define-ego-item "ego-item-88" "of *Slay Animal*"
  :numeric-id 88
  :power-lvl 20
  :pval 2
  :locations '((0 . 20))
  :cost 6000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '(<slow-digest> (<slay> <animal>) <int>))

(define-ego-item "ego-item-89" "of *Slay Evil*"
  :numeric-id 89
  :power-lvl 20
  :xtra 3
  :pval 2
  :locations '((0 . 20))
  :cost 5000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '(<blessed-blade> (<slay> <evil>) <wis>))

(define-ego-item "ego-item-90" "of *Slay Undead*"
  :numeric-id 90
  :power-lvl 24
  :pval 2
  :locations '((0 . 20))
  :cost 8000
  :obj-types '(<sword> <pole-arm> (:tval 21 :min-sval 0 :max-sval 18))
  :flags '(<see-invisible> <wis> (<execute> <undead>)))

(define-ego-item "ego-item-91" "of *Slay Demon*"
  :numeric-id 91
  :power-lvl 16
  :pval 2
  :locations '((0 . 20))
  :cost 8000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<resist> <fire>) <int> (<execute> <demon>)))

(define-ego-item "ego-item-92" "of *Slay Orc*"
  :numeric-id 92
  :power-lvl 14
  :pval 2
  :locations '((0 . 20))
  :cost 4000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<sustain> <dex>) (<slay> <orc>) <dex>))

(define-ego-item "ego-item-93" "of *Slay Troll*"
  :numeric-id 93
  :power-lvl 14
  :pval 2
  :locations '((0 . 20))
  :cost 4000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '(<regeneration> (<slay> <troll>) <str>))

(define-ego-item "ego-item-94" "of *Slay Giant*"
  :numeric-id 94
  :power-lvl 16
  :pval 2
  :locations '((0 . 20))
  :cost 4000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<sustain> <str>) (<slay> <giant>) <str>))

(define-ego-item "ego-item-95" "of *Slay Dragon*"
  :numeric-id 95
  :power-lvl 24
  :pval 2
  :locations '((0 . 20))
  :cost 8000
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<resist> <fear>) (<execute> <dragon>) <con>))

(define-ego-item "ego-item-100" "of Digging"
  :numeric-id 100
  :power-lvl 4
  :pval 4
  :locations '((0 . 1))
  :cost 500
  :obj-types '(<digger>)
  :flags '((<brand> <acid>) <tunnel>))

(define-ego-item "ego-item-101" "of Earthquakes"
  :numeric-id 101
  :power-lvl 8
  :max-to-hit 10
  :max-to-dmg 10
  :pval 6
  :locations '((20 . 4))
  :cost 3000
  :obj-types '(<digger>)
  :flags '(<hide-type> <impact> (<brand> <acid>) <tunnel> <str>))

(define-ego-item "ego-item-102" "of Morgul"
  :numeric-id 102
  :locations '((0 . 5))
  :cost 1
  :obj-types '(<sword> <pole-arm> <hafted>)
  :flags '((<brand> <poison>) (<slay> <undead>) <drain-xp> <hold-life> (<curse> <light>) (<curse> <heavy>) <aggravate> <see-invisible>))

(define-ego-item "ego-item-104" "of Accuracy"
  :numeric-id 104
  :power-lvl 10
  :max-to-hit 15
  :max-to-dmg 5
  :locations '((0 . 1))
  :cost 1000
  :obj-types '(<missile-weapon>))

(define-ego-item "ego-item-105" "of Power"
  :numeric-id 105
  :power-lvl 10
  :max-to-hit 5
  :max-to-dmg 15
  :locations '((0 . 1))
  :cost 1000
  :obj-types '(<missile-weapon>))

(define-ego-item "ego-item-106" "of Lothlorien"
  :numeric-id 106
  :power-lvl 30
  :xtra 3
  :max-to-hit 10
  :max-to-dmg 10
  :pval 2
  :locations '((50 . 4))
  :cost 20000
  :obj-types '((:tval 19 :min-sval 12 :max-sval 13))
  :flags '(<hide-type> (<ignore> <fire>) (<ignore> <acid>) <free-action> <extra-might> <dex>))

(define-ego-item "ego-item-107" "of the Haradrim"
  :numeric-id 107
  :power-lvl 30
  :max-to-hit 5
  :max-to-dmg 15
  :pval 1
  :locations '((50 . 4))
  :cost 20000
  :obj-types '((:tval 19 :min-sval 23 :max-sval 24))
  :flags '(<hide-type> (<ignore> <fire>) (<ignore> <acid>) <extra-shots> <extra-might>))

(define-ego-item "ego-item-108" "of Extra Might"
  :numeric-id 108
  :power-lvl 20
  :max-to-hit 5
  :max-to-dmg 10
  :pval 1
  :locations '((0 . 2))
  :cost 10000
  :obj-types '(<missile-weapon>)
  :flags '(<hide-type> <extra-might>))

(define-ego-item "ego-item-109" "of Extra Shots"
  :numeric-id 109
  :power-lvl 20
  :max-to-hit 10
  :max-to-dmg 5
  :pval 1
  :locations '((0 . 2))
  :cost 10000
  :obj-types '(<missile-weapon>)
  :flags '(<hide-type> <extra-shots>))

(define-ego-item "ego-item-110" "of Buckland"
  :numeric-id 110
  :power-lvl 25
  :max-to-hit 8
  :max-to-dmg 8
  :pval 2
  :locations '((40 . 4))
  :cost 20000
  :obj-types '((:tval 19 :min-sval 2 :max-sval 2))
  :flags '(<hide-type> (<ignore> <fire>) (<ignore> <acid>) <extra-might> <extra-shots> <dex>))

(define-ego-item "ego-item-111" "of the Nazgul"
  :numeric-id 111
  :max-to-hit 10
  :max-to-dmg 10
  :locations '((0 . 2))
  :obj-types '(<missile-weapon>)
  :flags '(<see-invisible> <drain-xp> (<curse> <light>)))

(define-ego-item "ego-item-112" "of Slay Animal"
  :numeric-id 112
  :power-lvl 10
  :locations '((0 . 6))
  :cost 20
  :obj-types '(<ammo>)
  :flags '((<slay> <animal>)))

(define-ego-item "ego-item-113" "of Slay Evil"
  :numeric-id 113
  :power-lvl 10
  :locations '((0 . 6))
  :cost 20
  :obj-types '((:tval 18 :min-sval 0 :max-sval 2) (:tval 17 :min-sval 0 :max-sval 2) (:tval 16 :min-sval 0 :max-sval 2))
  :flags '((<slay> <evil>)))

(define-ego-item "ego-item-114" "of Slay Undead"
  :numeric-id 114
  :power-lvl 10
  :locations '((0 . 8))
  :cost 25
  :obj-types '(<ammo>)
  :flags '((<slay> <undead>)))

(define-ego-item "ego-item-115" "of Slay Demon"
  :numeric-id 115
  :power-lvl 10
  :locations '((0 . 8))
  :cost 25
  :obj-types '(<ammo>)
  :flags '((<slay> <demon>)))

(define-ego-item "ego-item-116" "of Acid"
  :numeric-id 116
  :power-lvl 10
  :locations '((0 . 9))
  :cost 50
  :obj-types '(<ammo>)
  :flags '((<ignore> <acid>) (<brand> <acid>)))

(define-ego-item "ego-item-117" "of Lightning"
  :numeric-id 117
  :power-lvl 10
  :locations '((0 . 9))
  :cost 45
  :obj-types '(<ammo>)
  :flags '((<ignore> <electricity>) (<brand> <electricity>)))

(define-ego-item "ego-item-118" "of Slay Giant"
  :numeric-id 118
  :power-lvl 10
  :locations '((0 . 8))
  :cost 25
  :obj-types '(<ammo>)
  :flags '((<slay> <giant>)))

(define-ego-item "ego-item-119" "of Slay Dragon"
  :numeric-id 119
  :power-lvl 10
  :locations '((0 . 8))
  :cost 40
  :obj-types '(<ammo>)
  :flags '((<slay> <dragon>)))

(define-ego-item "ego-item-120" "of Holy Might"
  :numeric-id 120
  :power-lvl 20
  :max-to-hit 10
  :max-to-dmg 10
  :locations '((40 . 15))
  :cost 60
  :obj-types '((:tval 18 :min-sval 2 :max-sval 99) (:tval 17 :min-sval 2 :max-sval 99) (:tval 16 :min-sval 2 :max-sval 99))
  :flags '((<ignore> <acid>) (<ignore> <fire>) <blessed-blade> (<brand> <fire>) (<slay> <undead>) (<slay> <demon>) (<slay> <evil>)))

(define-ego-item "ego-item-121" "of Venom"
  :numeric-id 121
  :power-lvl 10
  :locations '((0 . 6))
  :cost 40
  :obj-types '((:tval 18 :min-sval 0 :max-sval 2) (:tval 17 :min-sval 0 :max-sval 2) (:tval 16 :min-sval 0 :max-sval 2))
  :flags '((<brand> <poison>)))

(define-ego-item "ego-item-122" "of Flame"
  :numeric-id 122
  :power-lvl 10
  :locations '((0 . 6))
  :cost 35
  :obj-types '(<ammo>)
  :flags '((<ignore> <fire>) (<brand> <fire>)))

(define-ego-item "ego-item-123" "of Frost"
  :numeric-id 123
  :power-lvl 10
  :locations '((0 . 6))
  :cost 30
  :obj-types '(<ammo>)
  :flags '((<ignore> <cold>) (<brand> <cold>)))

(define-ego-item "ego-item-124" "of Wounding"
  :numeric-id 124
  :power-lvl 5
  :max-to-hit 5
  :max-to-dmg 5
  :locations '((0 . 4))
  :cost 20
  :obj-types '(<ammo>))

(define-ego-item "ego-item-125" "of Backbiting"
  :numeric-id 125
  :max-to-hit 50
  :max-to-dmg 50
  :locations '((0 . 1))
  :obj-types '(<ammo>)
  :flags '((<curse> <light>)))

(define-ego-item "ego-item-126" "(Shattered)" :numeric-id 126 :max-to-hit 5 :max-to-dmg 5 :locations '((0 . 0)))

(define-ego-item "ego-item-127" "(Blasted)" :numeric-id 127 :max-to-ac 10 :locations '((0 . 0)))

;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/neckwear.lisp - all sorts of neckwear/amulets
Copyright (c) 2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)


(define-object-kind "amulet-wis" "wisdom"
  :numeric-id 163
  :text-attr #\d
  :text-char #\"
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 3
  :cost 500
  :flags '(<hide-type>)
  :sort-value 4306
  :the-kind '<amulet>
  :game-values (make-game-values :stat-modifiers '(<wis>))) 

(define-object-kind "amulet-chr" "charisma"
  :numeric-id 164
  :text-attr #\d
  :text-char #\"
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 3
  :cost 500
  :flags '(<hide-type>)
  :sort-value 4307
  :the-kind '<amulet>
  :game-values (make-game-values :stat-modifiers '(<chr>))) 

(define-object-kind "amulet-searching" "searching"
  :numeric-id 165
  :text-attr #\d
  :text-char #\"
  :depth 30
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(30 0 0 0)
  :weight 3
  :cost 600
  :flags '(<hide-type>)
  :sort-value 4305
  :the-kind '<amulet>
  :game-values (make-game-values :skill-modifiers '(<search>))) 

(define-object-kind "amulet-teleport" "teleportation"
  :numeric-id 166
  :text-attr #\d
  :text-char #\"
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 3
  :cost 0
  :flags '(<easy-know> <curse>)
  :sort-value 4301
  :the-kind '<amulet>
  :game-values (make-game-values :abilities '(<random-teleport>))) 

(define-object-kind "amulet-slow-digest" "slow digestion"
  :numeric-id 167
  :text-attr #\d
  :text-char #\"
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 3
  :cost 200
  :flags '(<easy-know>)
  :sort-value 4303
  :the-kind '<amulet>
  :game-values (make-game-values :abilities '(<slow-digestion>))) 

(define-object-kind "amulet-resist-acid" "resist acid"
  :numeric-id 168
  :text-attr #\d
  :text-char #\"
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 3
  :cost 300
  :flags '(<easy-know>)
  :sort-value 4304
  :the-kind '<amulet>
  :game-values (make-game-values :ignores '(<acid>) :resists '(<acid>))) 

(define-object-kind "amulet-adornment" "adornment"
  :numeric-id 169
  :text-attr #\d
  :text-char #\"
  :depth 15
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(15 0 0 0)
  :weight 3
  :cost 20
  :flags '(<easy-know>)
  :sort-value 4302
  :the-kind '<amulet>)

(define-object-kind "amulet-magi" "the magi"
  :numeric-id 171
  :text-attr #\d
  :text-char #\"
  :depth 50
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(50 0 0 0)
  :weight 3
  :cost 30000
  :sort-value 4308
  :the-kind '<amulet>
  :game-values (make-game-values :ac-modifier 3 :skill-modifiers '(<search>) :ignores
                              '(<cold> <fire> <electricity> <acid>) :abilities '(<see-invisible> <free-action>))) 

(define-object-kind "amulet-doom" "doom"
  :numeric-id 172
  :text-attr #\d
  :text-char #\"
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 3
  :cost 0
  :flags '(<hide-type> <curse>)
  :sort-value 4300
  :the-kind '<amulet>
  :game-values (make-game-values :stat-modifiers '(<chr> <con> <dex> <wis> <int> <str>))) 

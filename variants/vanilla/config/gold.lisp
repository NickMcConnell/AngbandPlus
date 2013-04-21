;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/gold.lisp - gold-objects for vanilla variant
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(define-object-kind "object-480" "copper"
  :numeric-id 480
  :x-attr (tile-file 10)
  :x-char (tile-number 7)
  :text-attr #\u
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 3
  :sort-value 7101
  :the-kind '<money>) 

(define-object-kind "object-481" "copper"
  :numeric-id 481
  :x-attr (tile-file 10)
  :x-char (tile-number 7)
  :text-attr #\u
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 4
  :sort-value 7102
  :the-kind '<money>) 

(define-object-kind "object-482" "copper"
  :numeric-id 482
  :x-attr (tile-file 10)
  :x-char (tile-number 7)
  :text-attr #\u
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 5
  :sort-value 7103
  :the-kind '<money>) 

(define-object-kind "object-483" "silver"
  :numeric-id 483
  :x-attr (tile-file 10)
  :x-char (tile-number 8)
  :text-attr #\s
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 6
  :sort-value 7104
  :the-kind '<money>) 

(define-object-kind "object-484" "silver"
  :numeric-id 484
  :x-attr (tile-file 10)
  :x-char (tile-number 8)
  :text-attr #\s
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 7
  :sort-value 7105
  :the-kind '<money>) 

(define-object-kind "object-485" "silver"
  :numeric-id 485
  :x-attr (tile-file 10)
  :x-char (tile-number 8)
  :text-attr #\s
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 8
  :sort-value 7106
  :the-kind '<money>) 

(define-object-kind "object-486" "garnets"
  :numeric-id 486
  :x-attr (tile-file 10)
  :x-char (tile-number 12)
  :text-attr #\r
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 9
  :sort-value 7107
  :the-kind '<money>) 

(define-object-kind "object-487" "garnets"
  :numeric-id 487
  :x-attr (tile-file 10)
  :x-char (tile-number 12)
  :text-attr #\r
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 10
  :sort-value 7108
  :the-kind '<money>) 

(define-object-kind "object-488" "gold"
  :numeric-id 488
  :x-attr (tile-file 10)
  :x-char (tile-number 9)
  :text-attr #\y
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 12
  :sort-value 7109
  :the-kind '<money>) 

(define-object-kind "object-489" "gold"
  :numeric-id 489
  :x-attr (tile-file 10)
  :x-char (tile-number 9)
  :text-attr #\y
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 14
  :sort-value 7110
  :the-kind '<money>) 

(define-object-kind "object-490" "gold"
  :numeric-id 490
  :x-attr (tile-file 10)
  :x-char (tile-number 9)
  :text-attr #\y
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 16
  :sort-value 7111
  :the-kind '<money>) 

(define-object-kind "object-491" "opals"
  :numeric-id 491
  :x-attr (tile-file 10)
  :x-char (tile-number 13)
  :text-attr #\W
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 18
  :sort-value 7112
  :the-kind '<money>) 

(define-object-kind "object-492" "sapphires"
  :numeric-id 492
  :x-attr (tile-file 10)
  :x-char (tile-number 14)
  :text-attr #\b
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 20
  :sort-value 7113
  :the-kind '<money>) 

(define-object-kind "object-493" "rubies"
  :numeric-id 493
  :x-attr (tile-file 10)
  :x-char (tile-number 15)
  :text-attr #\r
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 24
  :sort-value 7114
  :the-kind '<money>) 

(define-object-kind "object-494" "diamonds"
  :numeric-id 494
  :x-attr (tile-file 10)
  :x-char (tile-number 16)
  :text-attr #\w
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 28
  :sort-value 7115
  :the-kind '<money>) 

(define-object-kind "object-495" "emeralds"
  :numeric-id 495
  :x-attr (tile-file 10)
  :x-char (tile-number 17)
  :text-attr #\g
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 32
  :sort-value 7116
  :the-kind '<money>) 

(define-object-kind "object-496" "mithril"
  :numeric-id 496
  :x-attr (tile-file 10)
  :x-char (tile-number 10)
  :text-attr #\B
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 40
  :sort-value 7117
  :the-kind '<money>) 

(define-object-kind "object-497" "adamantite"
  :numeric-id 497
  :x-attr (tile-file 10)
  :x-char (tile-number 11)
  :text-attr #\G
  :text-char #\$
  :depth 1
  :rarity 0
  :chance #(0 0 0 0)
  :locale #(0 0 0 0)
  :weight 0
  :cost 80
  :sort-value 7118
  :the-kind '<money>) 

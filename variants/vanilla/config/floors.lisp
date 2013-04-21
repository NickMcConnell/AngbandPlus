;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/floors.lisp - floor-types for vanilla variant
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

;;; === door types

(define-door-type "closed-door" "closed door"
  :num-idx 201
  :text-attr +term-l-umber+
  :text-char #\+
  :cave-flags-on +cave-wall+
  :x-attr (tile-file 25)
  :x-char (tile-number 3))

(define-door-type "open-door" "open door"
  :num-idx 202
  :text-attr +term-l-umber+
  :text-char #\'
  :cave-flags-off +cave-wall+
  :x-attr (tile-file 25)
  :x-char (tile-number 4))

(define-door-type "destroyed-door" "destroyed door"
  :num-idx 203
  :text-attr +term-l-umber+
  :text-char #\'
  :cave-flags-off +cave-wall+
  :x-attr (tile-file 25)
  :x-char (tile-number 5))

;;; === floors

(define-floor-type* "stone-building" "stone building"
  :text-attr +term-white+ :text-char #\#
  :num-idx 65
  :x-attr (tile-file 34) :x-char (tile-number 1)
  :flags #.(logior +floor-flag-wall+
		   +floor-flag-permanent+))

(define-floor-type* "permanent-outer-wall" "permanent outer wall"
  :text-attr +term-white+ :text-char #\#
  :num-idx 66
  :x-attr (tile-file 29) :x-char (tile-number 1)
  :flags #.(logior +floor-flag-wall+
		   +floor-flag-permanent+))


(define-floor-type* "cave-wall" "cave wall"
  :text-attr +term-white+ :text-char #\#
  :num-idx 67
  :x-attr (tile-file 25) :x-char (tile-number 76)
  :flags #.(logior +floor-flag-wall+ +floor-flag-use-light-effect+))

(define-floor-type* "rubble" "pile of rubble"
  :num-idx 70
  :text-attr +term-white+
  :text-char #\:
  :flags +floor-flag-wall+
  :x-attr (tile-file 10) :x-char (tile-number 55))

(define-floor-type* "normal-floor" "normal floor"
  :num-idx 71
  :text-attr +term-white+
  :text-char #\.
  :flags #.(logior +floor-flag-floor+ +floor-flag-use-light-effect+
		   +floor-flag-allow-items+ +floor-flag-allow-creatures+)
  :x-attr (tile-file 26) :x-char (tile-number 14))

(define-floor-type* "room-floor" "room floor"
  :num-idx 72
  :text-attr +term-white+
  :text-char #\.
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-items+
		   +floor-flag-allow-creatures+)
  :x-attr (tile-file 26) :x-char (tile-number 8))

(define-floor-type* "nothing" "nothing"
  :num-idx 73
  :text-attr +term-white+
  :text-char #\Space
  :flags 0
  :x-attr (tile-file 0) :x-char (tile-number 0))

(define-floor-type* "room-wall" "room wall"
  :text-attr +term-white+ :text-char #\#
  :num-idx 74
  :x-attr (tile-file 25) :x-char (tile-number 63)
  :flags #.(logior +floor-flag-wall+ +floor-flag-use-light-effect+))

(define-floor-type* "inside-room-wall" "inside room wall"
  :text-attr +term-white+ :text-char #\#
  :num-idx 76
  :x-attr (tile-file 25) :x-char (tile-number 63)
  :flags #.(logior +floor-flag-wall+ +floor-flag-use-light-effect+))

(define-floor-type* "stair-up" "stair-up"
  :text-attr +term-white+ :text-char #\<
  :num-idx 77
  :x-attr (tile-file 25) :x-char (tile-number 21)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+
		   +floor-flag-exit-upwards+))

(define-floor-type* "stair-down" "stair-down"
  :text-attr +term-white+ :text-char #\>
  :num-idx 78
  :x-attr (tile-file 25) :x-char (tile-number 22)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+
		   +floor-flag-exit-downwards+))

;;; === shop doors (vanilla specific)
(define-floor-type* "shop1" "general store"
  :text-attr +term-l-umber+ :text-char #\1
  :num-idx 701
  :x-attr (tile-file 29) :x-char (tile-number 3)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+))

(define-floor-type* "shop2" "armour"
  :text-attr +term-l-umber+ :text-char #\2
  :num-idx 702
  :x-attr (tile-file 29) :x-char (tile-number 4)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+))

(define-floor-type* "shop3" "weapons"
  :text-attr +term-l-umber+ :text-char #\3
  :num-idx 703
  :x-attr (tile-file 29) :x-char (tile-number 5)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+))

(define-floor-type* "shop4" "temple"
  :text-attr +term-l-umber+ :text-char #\4
  :num-idx 704
  :x-attr (tile-file 29) :x-char (tile-number 6)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+))

(define-floor-type* "shop5" "alchemist"
  :text-attr +term-l-umber+ :text-char #\5
  :num-idx 705
  :x-attr (tile-file 29) :x-char (tile-number 7)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+))

(define-floor-type* "shop6" "magic shop"
  :text-attr +term-l-umber+ :text-char #\6
  :num-idx 706
  :x-attr (tile-file 29) :x-char (tile-number 8)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+))

(define-floor-type* "shop7" "black market"
  :text-attr +term-l-umber+ :text-char #\7
  :num-idx 707
  :x-attr (tile-file 29) :x-char (tile-number 27)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+))

(define-floor-type* "shop8" "home"
  :text-attr +term-l-umber+ :text-char #\8
  :num-idx 708
  :x-attr (tile-file 29) :x-char (tile-number 23)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+))

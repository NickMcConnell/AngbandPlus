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

(define-floor-type 0 "<darkness>" 1 #\Space :mimic nil)
(define-floor-type 1 "open floor" 1 #\. :mimic nil)
(define-floor-type 2 "invisible trap" 1 #\. :mimic 1)
(define-floor-type 3 "glyph of warding" 11 #\; :mimic nil)
(define-floor-type 4 "open door" 15 #\' :mimic nil)
(define-floor-type 5 "broken door" 15 #\' :mimic nil)
(define-floor-type 6 "up staircase" 1 #\< :mimic nil)
(define-floor-type 7 "down staircase" 1 #\> :mimic nil)
(define-floor-type 8 "General Store" 15 #\1 :mimic nil)
(define-floor-type 9 "Armoury" 2 #\2 :mimic nil)
(define-floor-type 10 "Weapon Smiths" 1 #\3 :mimic nil)
(define-floor-type 11 "Temple" 5 #\4 :mimic nil)
(define-floor-type 12 "Alchemy Shop" 6 #\5 :mimic nil)
(define-floor-type 13 "Magic Shop" 4 #\6 :mimic nil)
(define-floor-type 14 "Black Market" 8 #\7 :mimic nil)
(define-floor-type 15 "Home" 11 #\8 :mimic nil)
#||
(define-floor-type 16 "trap door" 1 #\^ :mimic nil)
(define-floor-type 17 "pit" 2 #\^ :mimic nil)
(define-floor-type 18 "pit" 2 #\^ :mimic nil)
(define-floor-type 19 "pit" 2 #\^ :mimic nil)
(define-floor-type 20 "strange rune" 3 #\^ :mimic nil)
(define-floor-type 21 "strange rune" 3 #\^ :mimic nil)
(define-floor-type 22 "discolored spot" 7 #\^ :mimic nil)
(define-floor-type 23 "discolored spot" 7 #\^ :mimic nil)
(define-floor-type 24 "dart trap" 4 #\^ :mimic nil)
(define-floor-type 25 "dart trap" 4 #\^ :mimic nil)
(define-floor-type 26 "dart trap" 4 #\^ :mimic nil)
(define-floor-type 27 "dart trap" 4 #\^ :mimic nil)
(define-floor-type 28 "gas trap" 5 #\^ :mimic nil)
(define-floor-type 29 "gas trap" 5 #\^ :mimic nil)
(define-floor-type 30 "gas trap" 5 #\^ :mimic nil)
(define-floor-type 31 "gas trap" 5 #\^ :mimic nil)

;; hackish
(define-floor-type 16 "trap" +term-dark+ #\^ :mimic nil)
(define-floor-type 17 "trap" +term-white+ #\^ :mimic nil)
(define-floor-type 18 "trap" +term-slate+ #\^ :mimic nil)
(define-floor-type 19 "trap" +term-orange+ #\^ :mimic nil)
(define-floor-type 20 "trap" +term-red+ #\^ :mimic nil)
(define-floor-type 21 "trap" +term-green+ #\^ :mimic nil)
(define-floor-type 22 "trap" +term-blue+ #\^ :mimic nil)
(define-floor-type 23 "trap" +term-umber+ #\^ :mimic nil)
(define-floor-type 24 "trap" +term-l-dark+ #\^ :mimic nil)
(define-floor-type 25 "trap" +term-l-white+ #\^ :mimic nil)
(define-floor-type 26 "trap" +term-violet+ #\^ :mimic nil)
(define-floor-type 27 "trap" +term-yellow+ #\^ :mimic nil)
(define-floor-type 28 "trap" +term-l-red+ #\^ :mimic nil)
(define-floor-type 29 "trap" +term-l-green+ #\^ :mimic nil)
(define-floor-type 30 "trap" +term-l-blue+ #\^ :mimic nil)
(define-floor-type 31 "trap" +term-l-umber+ #\^ :mimic nil)
;; end hack
||#
(define-floor-type 32 "door" 15 #\+ :mimic 32)
(define-floor-type 33 "locked door" 15 #\+ :mimic 32)
(define-floor-type 34 "locked door" 15 #\+ :mimic 32)
(define-floor-type 35 "locked door" 15 #\+ :mimic 32)
(define-floor-type 36 "locked door" 15 #\+ :mimic 32)
(define-floor-type 37 "locked door" 15 #\+ :mimic 32)
(define-floor-type 38 "locked door" 15 #\+ :mimic 32)
(define-floor-type 39 "locked door" 15 #\+ :mimic 32)
(define-floor-type 40 "jammed door" 15 #\+ :mimic 32)
(define-floor-type 41 "jammed door" 15 #\+ :mimic 32)
(define-floor-type 42 "jammed door" 15 #\+ :mimic 32)
(define-floor-type 43 "jammed door" 15 #\+ :mimic 32)
(define-floor-type 44 "jammed door" 15 #\+ :mimic 32)
(define-floor-type 45 "jammed door" 15 #\+ :mimic 32)
(define-floor-type 46 "jammed door" 15 #\+ :mimic 32)
(define-floor-type 47 "jammed door" 15 #\+ :mimic 32)
(define-floor-type 48 "secret door" 1 #\# :mimic 56)
(define-floor-type 49 "pile of rubble" 1 #\: :mimic nil)
(define-floor-type 50 "magma vein" 2 #\% :mimic nil)
(define-floor-type 51 "quartz vein" 1 #\% :mimic nil)
(define-floor-type 52 "magma vein" 2 #\% :mimic 50)
(define-floor-type 53 "quartz vein" 1 #\% :mimic 51)
(define-floor-type 54 "magma vein with treasure" 3 #\* :mimic nil)
(define-floor-type 55 "quartz vein with treasure" 3 #\* :mimic nil)
(define-floor-type 56 "granite wall" 1 #\# :mimic nil)
(define-floor-type 57 "granite wall" 1 #\# :mimic 56)
(define-floor-type 58 "granite wall" 1 #\# :mimic 56)
(define-floor-type 59 "granite wall" 1 #\# :mimic 56)
(define-floor-type 60 "permanent wall" 1 #\# :mimic nil)
(define-floor-type 61 "permanent wall" 1 #\# :mimic 60)
(define-floor-type 62 "permanent wall" 1 #\# :mimic 60)
(define-floor-type 63 "permanent wall" 1 #\# :mimic 60)

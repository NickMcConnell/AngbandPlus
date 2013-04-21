;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: config/settings.lisp - engine-settings
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(define-settings '("birth-settings")
    "instr-x"           23
    "instr-y"           1
    "instr-w"           75
    "instr-attr"        +term-white+
    "info-x"            1
    "info-y"            8
    "info-attr"         +term-l-green+
    "query-x"           2
    "query-y"           21
    "query-attr"        +term-l-red+
    "query-reduced"     nil
    "choice-x"          1
    "choice-y"          2
    "choice-tattr"      +term-white+
    "choice-attr"       +term-l-blue+
    "text-x"            2
    "text-y"            10
    "text-w"            75
    "text-attr"         +term-l-red+
    "altern-cols"       5
    "altern-attr"       +term-white+
    "altern-sattr"      +term-l-blue+
    "note-colour"       +term-white+
    "allow-all-classes" nil
   )

(define-settings '("savefile-selection" "birth-settings")
    "text-x" 8
    "text-attr" +term-yellow+)

(define-settings '("chardisplay-settings")
    "title-x"       1
    "title-y"       2
    "title-attr"    +term-white+
    "value-attr"    +term-l-blue+
    "value-badattr" +term-yellow+
    "picture-x"     23
    "picture-y"     2
    "extra-x"       1
    "extra-y"       18
    "elem-x"        1
    "elem-y"        10
    "combat-x"      28
    "combat-y"      10
    "stats-x"       42
    "stats-y"       3
    "stats-attr"    +term-white+
    "statok-attr"   +term-l-green+
    "statbad-attr"  +term-yellow+
    "skills-x"      50
    "skills-y"      10
    )


(define-settings '("resistdisplay-settings")
    "title-x"    2
    "title-y"    0
    "title-attr" +term-l-blue+
    "list-x"     2
    "list-y"     3
    "res-attr"   +term-l-green+
    "unres-attr" +term-l-red+
    )


(define-settings '("dungeon-settings")
    "max-width"   198 ;; default width
    "max-height"  66 ;; default height
    ;; how many rooms
    "room-number" 50

    "stairs-down" '(3 4) ;; min 3, max 4
    "stairs-up"   '(1 2) ;; min 1, max 2
    )


(define-settings '("basic-frame-locations")
    "name"   "Basic frame locations"
    "race"   1 ;; row
    "class"  2 ;; row

    "level"  4 ;; row to print lvl on
    "xp"     5 ;; row
    "gold"   6 ;; row
    
    "stat"   8  ;; row of first stat
    "ac"     15
    "hp"     16

    "speed"  18
    "hunger" 19
    ;; more slots?
    )


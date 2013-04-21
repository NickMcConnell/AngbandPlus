;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: constants.lisp - constants for the game code
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: This file contains the constants in the game.  should be small.

|#

(in-package :org.langband.engine)

;; for use with c-kode
(defconstant +false+ 0)
(defconstant +true+ 1)

;;; === The colours that the TERM can display.
(defconst +term-dark+    =char-code= (charify-number 0) "a colour")
(defconst +term-white+   =char-code= (charify-number 1) "a colour")
(defconst +term-slate+   =char-code= (charify-number 2) "a colour")
(defconst +term-orange+  =char-code= (charify-number 3) "a colour")
(defconst +term-red+     =char-code= (charify-number 4) "a colour")
(defconst +term-green+   =char-code= (charify-number 5) "a colour")
(defconst +term-blue+    =char-code= (charify-number 6) "a colour")
(defconst +term-umber+   =char-code= (charify-number 7) "a colour")
(defconst +term-l-dark+  =char-code= (charify-number 8) "a colour")
(defconst +term-l-white+ =char-code= (charify-number 9) "a colour")
(defconst +term-violet+  =char-code= (charify-number 10) "a colour")
(defconst +term-yellow+  =char-code= (charify-number 11) "a colour")
(defconst +term-l-red+   =char-code= (charify-number 12) "a colour")
(defconst +term-l-green+ =char-code= (charify-number 13) "a colour")
(defconst +term-l-blue+  =char-code= (charify-number 14) "a colour")
(defconst +term-l-umber+ =char-code= (charify-number 15) "a colour")
;;; === End colour-flags

;;; === The cave flags for coordinates in the dungeon.
(defconst +cave-mark+ u-fixnum #x01 "memorized feature")
(defconst +cave-glow+ u-fixnum #x02 "self-illuminating")
(defconst +cave-icky+ u-fixnum #x04 "part of a vault")
(defconst +cave-room+ u-fixnum #x08 "part of a room")
(defconst +cave-seen+ u-fixnum #x10 "seen flag")
(defconst +cave-view+ u-fixnum #x20 "view flag")
(defconst +cave-temp+ u-fixnum #x40 "temp flag")
(defconst +cave-wall+ u-fixnum #x80 "wall flag")
;;; === end cave-flags


;;; === Flags for features/floors
(defconst +feature-none+ u-fixnum #x00  "no feature")
(defconst +feature-floor+ u-fixnum #x01 "a feature")
(defconst +feature-invisible+ u-fixnum #x02 "a feature")
(defconst +feature-glyph+ u-fixnum #x03 "a feature")
(defconst +feature-open+ u-fixnum #x04 "a feature")
(defconst +feature-broken+ u-fixnum #x05 "a feature")
(defconst +feature-less+ u-fixnum #x06 "a feature")
(defconst +feature-more+ u-fixnum #x07 "a feature")

;; Shops
(defconst +feature-shop-head+ u-fixnum #x08 "a feature")
(defconst +feature-shop-tail+ u-fixnum #x0F "a feature")

;; Traps
(defconst +feature-trap-head+ u-fixnum #x10 "a feature")
(defconst +feature-trap-tail+ u-fixnum #x1F "a feature")

;; doors
(defconstant +feature-door-head+ #x20)
(defconstant +feature-door-tail+ #x2F)

(defconstant +feature-secret+ #x30)
(defconstant +feature-rubble+ #x31)

;; seams
(defconstant +feature-magma+ #x32)
(defconstant +feature-quartz+ #x33)
(defconstant +feature-magma-h+ #x34)
(defconstant +feature-quartz-h+ #x35)
(defconstant +feature-magma-k+ #x36)
(defconstant +feature-quartz-k+ #x37)


(defconstant +feature-wall-extra+ #x38)
(defconstant +feature-wall-inner+ #x39)
(defconstant +feature-wall-outer+ #x3a)
(defconstant +feature-wall-solid+ #x3b)

(defconstant +feature-perm-extra+ #x3c)
(defconstant +feature-perm-inner+ #x3d)
(defconstant +feature-perm-outer+ #x3e)
(defconstant +feature-perm-solid+ #x3f)
;;; === end feature/floor flags


;;; === flags that control print/redraw
;; will probably be altered to let variants have their own extra set
(defconstant +print-misc+   #x00000001)
(defconstant +print-title+  #x00000002)
(defconstant +print-level+  #x00000004)
(defconstant +print-xp+     #x00000008)
(defconstant +print-stats+  #x00000010)
(defconstant +print-armour+ #x00000020)
(defconstant +print-hp+     #x00000040)
(defconstant +print-mana+   #x00000080)
(defconstant +print-gold+   #x00000100)
(defconstant +print-depth+  #x00000200)

(defconstant +print-health+ #x00000800)
(defconstant +print-cut+    #x00001000)
(defconstant +print-stun+   #x00002000)
(defconstant +print-hunger+ #x00004000)

(defconstant +print-blind+    #x00010000)
(defconstant +print-confused+ #x00020000)
(defconstant +print-afraid+   #x00040000)
(defconstant +print-poisoned+ #x00080000)
(defconstant +print-state+    #x00100000)
(defconstant +print-speed+    #x00200000)
(defconstant +print-study+    #x00400000)

(defconstant +print-extra+    #x01000000)
(defconstant +print-basic+    #x02000000 "The panel on the left.")

(defconstant +print-map+      #x08000000)

;;; === end redraw/print flags

;;; === flags for updating the player, the values differ from angband!!
(defconstant +pl-upd-bonuses+        #x00000001)
(defconstant +pl-upd-torch+          #x00000002)
(defconstant +pl-upd-hp+             #x00000010)
(defconstant +pl-upd-mana+           #x00000020)
(defconstant +pl-upd-spells+         #x00000040)
(defconstant +pl-upd-forget-view+    #x00000100)
(defconstant +pl-upd-update-view+    #x00000200)
(defconstant +pl-upd-forget-flow+    #x00001000)
(defconstant +pl-upd-update-flow+    #x00002000)
(defconstant +pl-upd-monsters+       #x00010000)
(defconstant +pl-upd-distance+       #x00020000)
(defconstant +pl-upd-panel+          #x00080000)

;;; === end flags for updating the player

(defconstant +ident-sense+  #x01 "Item has been 'sensed'")
(defconstant +ident-fixed+  #x02 "Item has been 'haggled'")
(defconstant +ident-empty+  #x04 "Item charges are known")
(defconstant +ident-known+  #x08 "Item abilities are known")
(defconstant +ident-rumour+ #x10 "Item background is known")
(defconstant +ident-mental+ #x20 "Item information is known")
(defconstant +ident-cursed+ #x40 "Item is temporarily cursed")
(defconstant +ident-broken+ #x80 "Item is permanently worthless")

;;; === Various monster-flags

(defconstant +monster-flag-view+  #x01 "Monster is in line of sight")
;; ...
(defconstant +monster-flag-born+  #x10 "Monster is being born")
(defconstant +monster-flag-nice+  #x20 "Monster is being nice")
(defconstant +monster-flag-show+  #x40 "Monster is recently memorised")
(defconstant +monster-flag-mark+  #x80 "Monster is currently memorised")
  
(defconstant +block-height+ 11)
(defconstant +block-width+ 11)


;;(defconst +escape+ =char-code= (charify-number 27) "escape-key")
(defconstant +escape+ #\Escape)

(defconstant +store-item-limit+ 24)
(defconstant +store-maximum-items+ 18)
(defconstant +store-minimum-items+ 6)
(defconstant +store-turnover+ 9)

;; make these into variables later.. 

(defconst +screen-height+ u-fixnum 22 "height of screen")
(defconst +screen-width+ u-fixnum 66 "width of screen")

(defconstant +panel-height+ 11)
(defconstant +panel-width+ 33)


(defconstant +start-row-of-map+ 1)
(defconstant +start-column-of-map+ 13)



(defconst +max-sight+ u-fixnum 20 "maximum distance seen")

(defconstant +dungeon-align+ t)


(defconst +tunnel-random+   u-fixnum 10 "chance of random direction")
(defconst +tunnel-change+   u-fixnum 30 "chance of changing direction")
(defconst +tunnel-extra+    u-fixnum 15 "chance of extra tunneling")
(defconst +tunnel-door+     u-fixnum 25 "chance of doors at room entrances")
(defconst +tunnel-junction+ u-fixnum 90 "chance of doors at tunnel junctions")

;; maximum constants
(defconst +tunnel-max+ u-fixnum 900 "maximum tunnel-spaces.")

(defvar *ddd* #1A(2 8 6 4 3 1 9 7 5)
	"Global array for looping through the 'keypad directions'.")

(defvar *ddx* #1A(0 -1 0 1 -1 0 1 -1 0 1)
	"Global array for converting 'keypad direction' into 'offsets'.")

(defvar *ddy* #1A(0 1 1 1 0 0 0 -1 -1 -1)
	     "Global array for converting 'keypad direction' into 'offsets'.")

(defvar *ddx-ddd* #1A(0 0 1 -1 1 -1 1 -1 0)
	     "Global arrays for optimizing 'ddx[ddd[i]]'")
(defvar *ddy-ddd* #1A(1 -1 0 0 1 1 -1 -1 0)
	"Global arrays for optimizing 'ddx[ddd[i]]'")


(defconstant +project-jump+ #x01)
(defconstant +project-beam+ #x02)
(defconstant +project-through+ #x04)
(defconstant +project-stop+ #x08)
(defconstant +project-grid+ #x10)
(defconstant +project-item+ #x20)
(defconstant +project-kill+ #x40)
(defconstant +project-hide+ #x80)

(defconstant +energy-normal-action+ 100)

(defvar *energy-table*  #200(
    1  1  1  1  1  1  1  1  1  1 ;; Slow
    1  1  1  1  1  1  1  1  1  1 ;; Slow     
    1  1  1  1  1  1  1  1  1  1 ;; Slow     
    1  1  1  1  1  1  1  1  1  1 ;; Slow     
    1  1  1  1  1  1  1  1  1  1 ;; Slow     
    1  1  1  1  1  1  1  1  1  1 ;; Slow
    1  1  1  1  1  1  1  1  1  1 ;; S -50
    2  2  2  2  2  2  2  2  2  2 ;; S -40  
    2  2  2  2  2  2  2  3  3  3 ;; S -30
    3  3  3  3  3  4  4  4  4  4 ;; S -20
    5  5  5  5  6  6  7  7  8  9 ;; S -10
   10 11 12 13 14 15 16 17 18 19 ;; Normal
   20 21 22 23 24 25 26 27 28 29 ;; F +10
   30 31 32 33 34 35 36 36 37 37 ;; F +20
   38 38 39 39 40 40 40 41 41 41 ;; F +30
   42 42 42 43 43 43 44 44 44 44 ;; F +40
   45 45 45 45 45 46 46 46 46 46 ;; F +50
   47 47 47 47 47 48 48 48 48 48 ;; F +60
   49 49 49 49 49 49 49 49 49 49 ;; F +70
   49 49 49 49 49 49 49 49 49 49 ;; Fast
   ))


(defconstant +speed-base+ 110)

(defconstant +food-max+      15000 "Bloated")
(defconstant +food-full+     10000 "Normal")
(defconstant +food-hungry+    2000 "Hungry")
(defconstant +food-weak+      1000 "Weak")
(defconstant +food-fainting+   500 "Fainting")
(defconstant +food-starving+   100 "Starving")

(defconstant +illegal-loc-x+ 7777)
(defconstant +illegal-loc-y+ 7777)
(defconstant +room-size-arg-len+ 5)

(defvar *readable-save-file* "_save-game.lisp")
(defvar *binary-save-file* "_save-game.bin")

(defvar *dumps-directory* "doc/dumps/" "Where should various debug-dumps go?")

(defconstant +saved-cave-flags+ (logior +cave-mark+ +cave-glow+ +cave-icky+ +cave-room+))

;; stuff for view.lisp

(defconstant +view-max+ 1536)
(defconstant +vinfo-max-grids+ 161)
(defconstant +vinfo-max-slopes+ 126)


(defvar *vinfo-bit-fields* #8(#xFFFF #xFFFF  ;; 0
				     #xFFFF #xFFFF  ;; 1
				     #xFFFF #xFFFF  ;; 2
				     #x3FFF #xFFFF  ;; 3
				     ))

(defconstant +vinfo-bit-field-len+ 8)

(defconstant +vinfo-grid-field-len+ 8)

(defconstant +scale+ 100000)


(defconstant +calculated-effect+ #x01)
(defconstant +temporary-effect+  #x02)

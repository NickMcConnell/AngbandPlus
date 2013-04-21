;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: constants.lisp - constants for the game code
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: This file contains the constants in the game.  should be small.

|#

(in-package :org.langband.engine)

;; for use with c-code
(defconstant +false+ 0)
(defconstant +true+ 1)

(defconstant +text-end+ #xff "The last legal text-value for an attr or char.")  
(defconstant +graphics-start+ #x100 "The first graphics value.")

;;; === The colours that the TERM can display.
(def-exportconst +term-dark+     (charify-number 0) "a colour")
(def-exportconst +term-white+    (charify-number 1) "a colour")
(def-exportconst +term-slate+    (charify-number 2) "a colour")
(def-exportconst +term-orange+   (charify-number 3) "a colour")
(def-exportconst +term-red+      (charify-number 4) "a colour")
(def-exportconst +term-green+    (charify-number 5) "a colour")
(def-exportconst +term-blue+     (charify-number 6) "a colour")
(def-exportconst +term-umber+    (charify-number 7) "a colour")
(def-exportconst +term-l-dark+   (charify-number 8) "a colour")
(def-exportconst +term-l-white+  (charify-number 9) "a colour")
(def-exportconst +term-violet+   (charify-number 10) "a colour")
(def-exportconst +term-yellow+   (charify-number 11) "a colour")
(def-exportconst +term-l-red+    (charify-number 12) "a colour")
(def-exportconst +term-l-green+  (charify-number 13) "a colour")
(def-exportconst +term-l-blue+   (charify-number 14) "a colour")
(def-exportconst +term-l-umber+  (charify-number 15) "a colour")
;;; === End colour-flags

;;; === The cave flags for coordinates in the dungeon.
(def-exportconst +cave-mark+  #x01 "memorized feature")
(def-exportconst +cave-glow+  #x02 "self-illuminating")
(def-exportconst +cave-icky+  #x04 "part of a vault")
(def-exportconst +cave-room+  #x08 "part of a room")
(def-exportconst +cave-seen+  #x10 "seen flag")
(def-exportconst +cave-view+  #x20 "view flag")
(def-exportconst +cave-temp+  #x40 "temp flag")
(def-exportconst +cave-wall+  #x80 "wall flag")
(def-exportconst +cave-no-tunnel+  #x100 "never tunnel here")
;;; === end cave-flags


;;; === Flags for floors

;; these flags are used
(def-exportconst +floor-flag-wall+             #x01 "The floortype is some kind of a wall, can't see/move through.")
(def-exportconst +floor-flag-permanent+        #x02 "The floortype is permanent and can never be changed.")
(def-exportconst +floor-flag-floor+            #x04 "The floortype is some kind of floor.")
(def-exportconst +floor-flag-allow-items+      #x08 "This floor-type allows items to be dropped on it.")
(def-exportconst +floor-flag-allow-creatures+  #x10 "This floor-type allows creatures to move atop it
and be constructed atop it.")
(def-exportconst +floor-flag-exit-upwards+     #x20 "Can we go up here?")
(def-exportconst +floor-flag-exit-downwards+   #x40 "Can we go down here?")
(def-exportconst +floor-flag-use-light-effect+ #x80 "Should we use a light-effect on this floor?")
;;; === end floor flags


;;; === flags that control print/redraw
;; several ones have been moved to variant!!
;; will probably be altered to let variants have their own extra set
(def-exportconst +print-misc+   #x00000001)
(def-exportconst +print-level+  #x00000002)
(def-exportconst +print-xp+     #x00000004)
(def-exportconst +print-stats+  #x00000008)

(def-exportconst +print-armour+ #x00000010)
(def-exportconst +print-hp+     #x00000020)
(def-exportconst +print-gold+   #x00000040)
(def-exportconst +print-depth+  #x00000080)

(def-exportconst +print-health+ #x00000100)
(def-exportconst +print-speed+  #x00000200)
(def-exportconst +print-hunger+ #x00000400)
(def-exportconst +print-blind+  #x00000800)

(def-exportconst +print-map+    #x00001000)
(def-exportconst +print-extra+  #x00002000)
(def-exportconst +print-basic+  #x00004000 "The panel on the left.")
(def-exportconst +print-equip+  #x00008000 "Print inventory/equip row (if there).")


;;; === end redraw/print flags

;;; === flags for updating the player, the values differ from angband!!
(def-exportconst +pl-upd-bonuses+        #x00000001)
(def-exportconst +pl-upd-torch+          #x00000002)
(def-exportconst +pl-upd-hp+             #x00000010)
;; spell/mana see variants
(def-exportconst +pl-upd-forget-view+    #x00000100)
(def-exportconst +pl-upd-update-view+    #x00000200)
(def-exportconst +pl-upd-forget-flow+    #x00001000)
(def-exportconst +pl-upd-update-flow+    #x00002000)
(def-exportconst +pl-upd-monsters+       #x00010000)
(def-exportconst +pl-upd-distance+       #x00020000)
(def-exportconst +pl-upd-panel+          #x00080000)

;;; === end flags for updating the player

(def-exportconst +ident-sense+  #x01 "Item has been 'sensed'")
(def-exportconst +ident-fixed+  #x02 "Item has been 'haggled'")
(def-exportconst +ident-empty+  #x04 "Item charges are known")
(def-exportconst +ident-known+  #x08 "Item abilities are known")
(def-exportconst +ident-rumour+ #x10 "Item background is known")
(def-exportconst +ident-mental+ #x20 "Item information is known")
(def-exportconst +ident-cursed+ #x40 "Item is temporarily cursed")
(def-exportconst +ident-broken+ #x80 "Item is permanently worthless")

;;; === Various monster-flags

(def-exportconst +monster-flag-view+  #x01 "Monster is in line of sight")
;; ...
(def-exportconst +monster-flag-born+  #x10 "Monster is being born")
(def-exportconst +monster-flag-nice+  #x20 "Monster is being nice")
(def-exportconst +monster-flag-show+  #x40 "Monster is recently memorised")
(def-exportconst +monster-flag-mark+  #x80 "Monster is currently memorised")
  
(defconstant +block-height+ 11)
(defconstant +block-width+ 11)


;;(defconst +escape+ =char-code= (charify-number 27) "escape-key")
(def-exportconst +escape+ #\Escape)

(def-exportconst +store-item-limit+ 24)
(def-exportconst +store-maximum-items+ 18)
(def-exportconst +store-minimum-items+ 6)
(def-exportconst +store-turnover+ 9)
(def-exportconst +max-itemstack-size+ 99)

;; make these into variables later.. 


(def-exportconst +max-sight+ 20 "maximum distance seen")

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

(defconstant +simple-direction-number+ 4 "basic nswe directions in ddd arrays")
(defconstant +normal-direction-number+ 8 "basic nswe directions in ddd arrays plus diagonals")


(def-exportconst +project-jump+ #x01)
(def-exportconst +project-beam+ #x02)
(def-exportconst +project-through+ #x04)
(def-exportconst +project-stop+ #x08)
(def-exportconst +project-grid+ #x10)
(def-exportconst +project-item+ #x20)
(def-exportconst +project-kill+ #x40)
(def-exportconst +project-hide+ #x80)

(def-exportconst +energy-normal-action+ 100)

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


(def-exportconst +speed-base+ 110)

(def-exportconst +food-max+      15000 "Bloated")
(def-exportconst +food-full+     10000 "Normal")
(def-exportconst +food-hungry+    2000 "Hungry")
(def-exportconst +food-weak+      1000 "Weak")
(def-exportconst +food-fainting+   500 "Fainting")
(def-exportconst +food-starving+   100 "Starving")

(defconstant +illegal-loc-x+ 7777)
(defconstant +illegal-loc-y+ 7777)
(defconstant +room-size-arg-len+ 5)

(def-exportconst +saved-cave-flags+ (logior +cave-mark+ +cave-glow+ +cave-icky+ +cave-room+))

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

;; for visual effects
(defconstant +draw-delay+ 25) ;; hackish, remove later


(def-exportconst +calculated-effect+ #x01)
(def-exportconst +temporary-effect+  #x02)

(def-exportconst +max-range+ 18)


(def-exportconst +max-frames+ 9)
(def-exportconst +predefined-frames+ 9)

;; the above need not be the same, but typically is the same

(def-exportconst +full-frame+ 0)
(def-exportconst +message-frame+ 1)
(def-exportconst +charinfo-frame+ 2)
(def-exportconst +misc-frame+ 3)
(def-exportconst +gfxmap-frame+ 4)
(def-exportconst +asciimap-frame+ 5)
(def-exportconst +inv-frame+ 6)
(def-exportconst +dialogue-frame+ 7)
(def-exportconst +infodisp-frame+ 8)

(def-exportconst +frametype-active+ 0)
(def-exportconst +frametype-predefined+ 1)

;; allowed to change between ascii and gfx
(defvar *map-frame* +gfxmap-frame+)
;;(defvar *map-frame* +asciimap-frame+)

(defvar *windows* (make-array +predefined-frames+ :initial-element nil)
  "A vector of the available windows.")

(def-exportconst +winflag-clear-bg+ #x01)
(def-exportconst +winflag-delay-paint+ #x02)

;; alias!
(def-exportconst +query-frame+ +message-frame+)

(def-exportconst +tilefile-armour+ 3)
(def-exportconst +tilefile-effects+ 4)
(def-exportconst +tilefile-food+ 5)
(def-exportconst +tilefile-classes+ 6)
(def-exportconst +tilefile-humans+ 7)
(def-exportconst +tilefile-magic+ 9)
(def-exportconst +tilefile-misc+ 10)
(def-exportconst +tilefile-weapons+ 13)
(def-exportconst +tilefile-people+ 14)
(def-exportconst +tilefile-undeads+ 28)
(def-exportconst +tilefile-buttons+ 38)
(def-exportconst +tilefile-crosshairs+ 40)

;;; these are for the gfxtile system:
(def-exportconst +num-gfx-layers+ 4)
(def-exportconst +background+ 0)
(def-exportconst +decor+ 1)
(def-exportconst +foreground+ 2)
(def-exportconst +effect+ 3)

(def-exportconst +coord-updated+ 1)

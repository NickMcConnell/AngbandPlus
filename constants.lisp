;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: constants.lisp - constants for the game code
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: This file contains the constants in the game.  should be small.

|#

(in-package :langband)

(defconstant +shared-zterm-lib+ "./lib/zterm/liblang_ui.so")
#+allegro
(defconstant +c-null-value+ 0)
#+cmu
(defconstant +c-null-value+ nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype =char-code= ()
    #+cmu
    'u-fixnum
    #+allegro
    'character))

;; for use with c-kode
(defconstant +false+ 0)
(defconstant +true+ 1)

(defmacro charify-number (num)
  #+cmu
  num
  #+allegro
  `(code-char ,num))

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

;; cave flags
(defconst +cave-mark+ u-fixnum #x01 "memorized feature")
(defconst +cave-glow+ u-fixnum #x02 "self-illuminating")
(defconst +cave-icky+ u-fixnum #x04 "part of a vault")
(defconst +cave-room+ u-fixnum #x08 "part of a room")
(defconst +cave-seen+ u-fixnum #x10 "seen flag")
(defconst +cave-view+ u-fixnum #x20 "view flag")
(defconst +cave-temp+ u-fixnum #x40 "temp flag")
(defconst +cave-wall+ u-fixnum #x80 "wall flag")

;; mapping flags, unused
;;(defconst +mapped-seen+ u-fixnum #x01 "seen flag")


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
(defconstant +print-basic+    #x02000000)

(defconstant +print-map+      #x08000000)


(defconstant +forget-view+    #x00010000)
(defconstant +update-view+    #x00020000)

  
(defconstant +block-height+ 11)
(defconstant +block-width+ 11)


(defconst +escape+ =char-code= (charify-number 27) "escape-key")

;;(defconstant +max-depth+ 128)
#||
;; stores.lisp mostly
(defconst +max-stores+ u-fixnum 8 "max stores..")
(defconst +the-home+ u-fixnum 7 "house number.") ;; the num is displayed as 8
||#

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

(defconstant +sexes+ '((<male> . ("Male" "King"))
		       (<female . ("Female" "Queen"))))


(defconst +max-sight+ u-fixnum 20 "maximum distance seen")

(defconstant +dungeon-align+ t)


(defconst +tunnel-random+   u-fixnum 10 "chance of random direction")
(defconst +tunnel-change+   u-fixnum 30 "chance of changing direction")
(defconst +tunnel-extra+    u-fixnum 15 "chance of extra tunneling")
(defconst +tunnel-door+     u-fixnum 25 "chance of doors at room entrances")
(defconst +tunnel-junction+ u-fixnum 90 "chance of doors at tunnel junctions")

;; maximum constants
(defconst +tunnel-max+ u-fixnum 900 "maximum tunnel-spaces.")

(defconstant +ddd+ #1A(2 8 6 4 3 1 9 7 5)
	     "Global array for looping through the 'keypad directions'.")

(defconstant +ddx+ #1A(0 -1 0 1 -1 0 1 -1 0 1)
	     "Global array for converting 'keypad direction' into 'offsets'.")

(defconstant +ddy+ #1A(0 -1 0 1 -1 0 1 -1 0 1)
	     "Global array for converting 'keypad direction' into 'offsets'.")

(defconstant +ddx-ddd+ #1A(0 0 1 -1 1 -1 1 -1 0)
	     "Global arrays for optimizing 'ddx[ddd[i]]'")
(defconstant +ddy-ddd+ #1A(1 -1 0 0 1 1 -1 -1 0)
	     "Global arrays for optimizing 'ddx[ddd[i]]'")


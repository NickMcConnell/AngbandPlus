;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/defines.lisp - various defines that should be loaded as data
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)
#||
(define-normal-event ()
    :backpack-creation
  :on-create
  #'common-creating-backpack)
||#

(defconstant +common-backpack-size+ 23)

(define-object-kind 
    "backpack" "backpack" :numeric-id 750
    :text-attr #\w :text-char #\&
    :depth 0 :rarity nil :chance #(0 0 0 0)
    :locale #(0 0 0 0) :weight nil
    :cost 1200 :the-kind '<container>
    :on-create #'(lambda (item)
		   (let ((container (make-container +common-backpack-size+)))
		     (setf (aobj.contains item) container)
		     t))
    )

(define-room "simple-room" #'common-make-simple-room)
(define-room "overlapping-room" #'common-make-overlapping-room)

(register-information& "status-roll" 100 ;; what's the roll of status
		       "status-cap" 100) ;; what's max status

(register-information& "which-town" "vanilla")
;;(register-information& "which-town" "bartertown")

;;; the various elements you can face in Vanilla
(define-element '<fire>        "fire"        :bit-flag #x00000001 :number 0)
(define-element '<acid>        "acid"        :bit-flag #x00000002 :number 1)
(define-element '<electricity> "electricity" :bit-flag #x00000004 :number 2)
(define-element '<cold>        "cold"        :bit-flag #x00000008 :number 3)
(define-element '<poison>      "poison"      :bit-flag #x00000010 :number 4)
(define-element '<darkness>    "darkness"    :bit-flag #x00000020 :number 5)
(define-element '<light>       "light"       :bit-flag #x00000040 :number 6)
(define-element '<blindness>   "blindness"   :bit-flag #x00000080 :number 7)
(define-element '<disenchant>  "disenchant"  :bit-flag #x00000100 :number 8)
(define-element '<shards>      "shards"      :bit-flag #x00000200 :number 9)
(define-element '<confusion>   "confusion"   :bit-flag #x00000400 :number 10)
(define-element '<nexus>       "nexus"       :bit-flag #x00000800 :number 11)
(define-element '<sound>       "sound"       :bit-flag #x00001000 :number 12)
(define-element '<nether>      "nether"      :bit-flag #x00002000 :number 13)
(define-element '<gravity>     "gravity"     :bit-flag #x00004000 :number 14)
(define-element '<chaos>       "chaos"       :bit-flag #x00008000 :number 15)
(define-element '<fear>        "fear"        :bit-flag #x00010000 :number 16)
(define-element '<sleep>       "sleep"       :bit-flag #x00020000 :number 17)

;; various effects for the players and the monsters in vanilla

(define-effect '<telepathy>       "telepathy"           :number 0  :bit-flag #x000000001)
(define-effect '<hold-life>       "hold life"           :number 1  :bit-flag #x000000002)
(define-effect '<see-invisible>   "see invisible"       :number 2  :bit-flag #x000000004)
(define-effect '<blessed>         "blessed"             :number 3  :bit-flag #x000000008)
(define-effect '<heroic>          "heroic"              :number 4  :bit-flag #x000000010)
(define-effect '<free-action>     "free-action"         :number 5  :bit-flag #x000000020) 
(define-effect '<berserk>         "berserk"             :number 6  :bit-flag #x000000040)
(define-effect '<slow-digest>     "slow digestion"      :number 7  :bit-flag #x000000080)
(define-effect '<regenerate>      "regenerate"          :number 8  :bit-flag #x000000100)
(define-effect '<prot-from-evil>  "protected from evil" :number 9  :bit-flag #x000000200)
(define-effect '<shielded>        "shielded"            :number 10 :bit-flag #x000000400)
(define-effect '<hasted>          "hasted"              :number 11 :bit-flag #x000000800)
(define-effect '<invulnerable>    "invulnerable"        :number 12 :bit-flag #x000001000)
(define-effect '<infravision>     "infravision"         :number 13 :bit-flag #x000002000)
(define-effect '<feather-fall>    "feather-fall"        :number 14 :bit-flag #x000004000)
(define-effect '<glowing>         "glowing"             :number 15 :bit-flag #x000008000)
(define-effect '<earthquake>      "earthquake"          :number 16 :bit-flag #x000010000)
(define-effect '<blessed-blade>   "blessed blade"       :number 17 :bit-flag #x000020000)
(define-effect '<aggravates>      "aggravates"          :number 18 :bit-flag #x000040000)
(define-effect '<random-teleport> "random teleport"     :number 19 :bit-flag #x000080000)
(define-effect '<drain-xp>        "drain xp"            :number 20 :bit-flag #x000100000)
(define-effect '<slowed>          "slowed"              :number 21 :bit-flag #x000200000)
(define-effect '<blindness>       "blindness"           :number 22 :bit-flag #x000400000)
(define-effect '<paralysed>       "paralysed"           :number 23 :bit-flag #x000800000)
(define-effect '<confusion>       "confusion"           :number 24 :bit-flag #x001000000)
(define-effect '<fear>            "fear/afraid"         :number 25 :bit-flag #x002000000)
(define-effect '<hallucinate>     "hallucinate"         :number 26 :bit-flag #x004000000)
(define-effect '<poisoned>        "poisoned"            :number 27 :bit-flag #x008000000)
(define-effect '<cut>             "cut"                 :number 28 :bit-flag #x010000000)
(define-effect '<stun>            "stun"                :number 29 :bit-flag #x020000000)
(define-effect '<recalling>       "recalling"           :number 30 :bit-flag #x040000000)
(define-effect '<resist-acid>     "resist acid"         :number 31 :bit-flag #x080000000)
(define-effect '<resist-elec>     "resist electricity"  :number 32 :bit-flag #x100000000)
(define-effect '<resist-fire>     "resist fire"         :number 33 :bit-flag #x200000000)
(define-effect '<resist-cold>     "resist cold"         :number 34 :bit-flag #x400000000)
(define-effect '<resist-poison>   "resist poison"       :number 35 :bit-flag #x800000000)
(define-effect '<sleeping>        "sleeping"            :number 36 :bit-flag #x1000000000)

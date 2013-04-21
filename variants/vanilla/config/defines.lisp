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

(define-normal-event ()
    :backpack-creation
  :on-create
  #'common-creating-backpack)


(define-object-kind 
    "backpack" "backpack" :numeric-id 750
    :x-attr #\w :x-char #\&
    :depth 0 :rarity nil :chance #(0 0 0 0)
    :locale #(0 0 0 0) :weight nil
    :cost 1200 :obj-type '(<container> <backpack>)
    :the-kind '<container>
    :events (list :backpack-creation))

(define-room "simple-room" #'common-make-simple-room)
(define-room "overlapping-room" #'common-make-overlapping-room)

;;; the various elements you can face in Vanilla
(define-element '<fire>        "fire"        #x00000001)
(define-element '<acid>        "acid"        #x00000002)
(define-element '<electricity> "electricity" #x00000004)
(define-element '<cold>        "cold"        #x00000008)
(define-element '<poison>      "poison"      #x00000010)
(define-element '<darkness>    "darkness"    #x00000020)
(define-element '<light>       "light"       #x00000040)
(define-element '<blindness>   "blindness"   #x00000080)
(define-element '<disenchant>  "disenchant"  #x00000100)
(define-element '<shards>      "shards"      #x00000200)
(define-element '<confusion>   "confusion"   #x00000400)
(define-element '<nexus>       "nexus"       #x00000800)
(define-element '<sound>       "sound"       #x00001000)
(define-element '<nether>      "nether"      #x00002000)
(define-element '<gravity>     "gravity"     #x00004000)
(define-element '<chaos>       "chaos"       #x00008000)

;;; The various special effects in Vanilla
(define-effect '<telepathy>       "telepathy"           #x00000001)
(define-effect '<hold-life>       "hold life"           #x00000002)
(define-effect '<see-invisible>   "see invisible"       #x00000004)
(define-effect '<blessed>         "blessed"             #x00000008)
(define-effect '<heroic>          "heroic"              #x00000010)
(define-effect '<super-heroic>    "super heroic"        #x00000020)
(define-effect '<berserk>         "berserk"             #x00000040)
(define-effect '<slow-digest>     "slow digestion"      #x00000080)
(define-effect '<regenerate>      "regenerate"          #x00000100)
(define-effect '<prot-evil>       "protected from evil" #x00000200)
(define-effect '<shielded>        "shielded"            #x00000400)
(define-effect '<hasted>          "hasted"              #x00000800)
(define-effect '<invulnerable>    "invulnerable"        #x00001000)
(define-effect '<invisible>       "invisible"           #x00002000)
;; infravision??
;; recalling?
;; feather fall??
;; glowing??
;; free action??
;; earthquake?
;; blessed-blade??

;; ill effects on player/monsters
(define-effect '<slowed>          "slowed"          #x00004000)
(define-effect '<blinded>         "blinded"         #x00008000)
(define-effect '<afraid>          "afraid"          #x00010000)
(define-effect '<confused>        "confused"        #x00020000)
(define-effect '<aggravates>      "aggravates"      #x00040000)
(define-effect '<paralysed>       "paralysed"       #x00080000)
(define-effect '<cut>             "cut"             #x00100000)
(define-effect '<stun>            "stun"            #x00200000)
(define-effect '<poisoned>        "poisoned"        #x00400000)
(define-effect '<hallucinate>     "hallucinate"     #x00800000)
(define-effect '<random-teleport> "random teleport" #x01000000)

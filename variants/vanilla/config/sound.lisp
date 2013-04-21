;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/sound.lisp - sound-settings

|#

(in-package :org.langband.vanilla)


(define-sound-effect "hit-someone"
    "hit1.ogg" "hit3.ogg")

(define-sound-effect "miss-someone"
    "miss1.ogg" "miss2.ogg")

(define-sound-effect "kill-someone"
    "TMaDth00.ogg" "TMaDth01.ogg")

(define-sound-effect "shut-door"
    "Doorshut.ogg")

(define-sound-effect "eat-something"
    "eat1.ogg")

(define-sound-effect "zap-something"
    "magksnd2.ogg" "magksnd8.ogg")

(define-sound-effect "trapdoor"
    "chain01.ogg")

(define-sound-effect "acid-splash"
    "drips.ogg")

(define-sound-effect "burning-fire"
    "fire01.ogg")

(define-sound-effect "fire"
    "magksnd8.ogg")

(define-sound-effect "lightning"
    "magksnd2.ogg")

(define-sound-effect "teleport"
    "magksnd1.ogg")

(define-sound-effect "dart-trap"
    "locksound.ogg")

(define-sound-effect "walking"
    ;;"walk_stone01.ogg" "walk_stone02.ogg" "walk_stone03.ogg"
    "walk_stone04.ogg" ;;"walk_stone05.ogg" "walk_stone06.ogg"
  )

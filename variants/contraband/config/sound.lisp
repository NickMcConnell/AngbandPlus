;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/config/sound.lisp - sound-settings

|#

(in-package :org.langband.contraband)


(define-sound-effect "hit-someone"
    "hit1.wav" "hit3.wav")

(define-sound-effect "miss-someone"
    "miss1.wav" "miss2.wav")

(define-sound-effect "kill-someone"
    "TMaDth00.wav" "TMaDth01.wav")

(define-sound-effect "shut-door"
    "Doorshut.wav")

(define-sound-effect "eat-something"
    "eat1.wav")

(define-sound-effect "zap-something"
    "magksnd2.wav" "magksnd8.wav")

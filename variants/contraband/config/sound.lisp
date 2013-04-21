;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/config/sound.lisp - sound-settings

|#

(in-package :org.langband.contraband)


(define-sound-effect +sound-hit+
    "hit1.wav" "hit3.wav")

(define-sound-effect +sound-miss+
    "miss1.wav" "miss2.wav")

(define-sound-effect +sound-kill+
    "TMaDth00.wav" "TMaDth01.wav")

(define-sound-effect +sound-shutdoor+
    "Doorshut.wav")

(define-sound-effect +sound-eat+
    "eat1.wav")

(define-sound-effect +sound-zap+
    "magksnd2.wav" "magksnd8.wav")

;;(define-sound-effect +sound-intro+
;;    "langband_opening01.ogg")

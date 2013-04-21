;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: config/sound.lisp - sound-settings

|#

(in-package :org.langband.engine)

(define-sound +sound-hit+
    "1unch" "hit" "hit1" "drop")

(define-sound +sound-miss+
    "miss" "miss1")

(define-sound +sound-kill+
    "kill" "kill1" "d34" "destroy" "destroy2")

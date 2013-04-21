;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/keys.lisp - addignment of keys
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(define-keypress *ang-keys* :global #\a 'zap-item)
(define-keypress *ang-keys* :global #\b 'browse-spells)
(define-keypress *ang-keys* :global #\c 'close-door)
(define-keypress *ang-keys* :global #\d 'drop-item)
(define-keypress *ang-keys* :global #\e 'show-equipment)
(define-keypress *ang-keys* :global #\f 'fire-missile)
(define-keypress *ang-keys* :global #\g 'get-item)
(define-keypress *ang-keys* :global #\i 'show-inventory)
(define-keypress *ang-keys* :global #\j 'jam-door)
(define-keypress *ang-keys* :global #\m 'invoke-spell)
(define-keypress *ang-keys* :global #\o 'open-door)
(define-keypress *ang-keys* :global #\p 'invoke-spell)
(define-keypress *ang-keys* :global #\q 'quaff-potion)
(define-keypress *ang-keys* :global #\r 'read-text)
(define-keypress *ang-keys* :global #\s 'search-area)
(define-keypress *ang-keys* :global #\t 'take-off-item)
(define-keypress *ang-keys* :global #\u 'use-item)
(define-keypress *ang-keys* :global #\w 'wear-item)
(define-keypress *ang-keys* :global #\z 'zap-item)

(define-keypress *ang-keys* :global #\B 'bash-door)
(define-keypress *ang-keys* :global #\C 'show-character)
(define-keypress *ang-keys* :global #\D 'disarm-trap)
(define-keypress *ang-keys* :global #\E 'eat-item)
(define-keypress *ang-keys* :global #\L 'learn-spell)
(define-keypress *ang-keys* :global #\P 'play-music)
(define-keypress *ang-keys* :global #\Q 'quit-game)
(define-keypress *ang-keys* :global #\R 'rest)
(define-keypress *ang-keys* :global #\S 'save-game)
(define-keypress *ang-keys* :global #\? 'show-help)

(define-keypress *ang-keys* :global #\> 'go-downstairs)
(define-keypress *ang-keys* :global #\< 'go-upstairs)
(define-keypress *ang-keys* :global #\* 'select-target)

;; these can die later..
;;(define-keypress *ang-keys* :global #\A 'print-mapper)

;; Ctrl-P
(define-keypress *ang-keys* :global (code-char 16) 'previous-messages)
;; Ctrl-R
(define-keypress *ang-keys* :global (code-char 18) 'redraw-all)
(define-keypress *ang-keys* :global (code-char 20) 'swap-map)
(define-keypress *ang-keys* :global (code-char 24) 'save-and-exit)



(define-keypress *ang-keys* :global #\, 'stand-still)
(define-keypress *ang-keys* :global #\1 'move-down-left)
(define-keypress *ang-keys* :global #\2 'move-down)
(define-keypress *ang-keys* :global #\3 'move-down-right)
(define-keypress *ang-keys* :global #\4 'move-left)
(define-keypress *ang-keys* :global #\5 'stand-still)
(define-keypress *ang-keys* :global #\6 'move-right)
(define-keypress *ang-keys* :global #\7 'move-up-left)
(define-keypress *ang-keys* :global #\8 'move-up)
(define-keypress *ang-keys* :global #\9 'move-up-right)

(define-keypress *ang-keys* :global #\. 'toggle-run-mode)

(define-keypress *ang-keys* :global '(shift #\1) 'run-down-left)
(define-keypress *ang-keys* :global '(shift #\2) 'run-down)
(define-keypress *ang-keys* :global '(shift #\3) 'run-down-right)
(define-keypress *ang-keys* :global '(shift #\4) 'run-left)
(define-keypress *ang-keys* :global '(shift #\5) 'stand-still)
(define-keypress *ang-keys* :global '(shift #\6) 'run-right)
(define-keypress *ang-keys* :global '(shift #\7) 'run-up-left)
(define-keypress *ang-keys* :global '(shift #\8) 'run-up)
(define-keypress *ang-keys* :global '(shift #\9) 'run-up-right)


;; then those keys used for display
(define-keypress *ang-keys* :display #\C 'print-attack-table)
(define-keypress *ang-keys* :display #\M 'print-misc)
(define-keypress *ang-keys* :display #\R 'print-resists)


#||
(define-keypress *ang-keys* :global #\k 'move-up)
(define-keypress *ang-keys* :global #\l 'move-right)
(define-keypress *ang-keys* :global #\j 'move-down)
(define-keypress *ang-keys* :global #\h 'move-left)
||#

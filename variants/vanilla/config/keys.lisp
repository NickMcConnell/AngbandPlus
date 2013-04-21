;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/keys.lisp - assignment of keys
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

;;; roguelike keys are at the bottom

(define-keypress *angband-keys* :global #\a 'zap-item)
(define-keypress *angband-keys* :global #\b 'browse-spells)
(define-keypress *angband-keys* :global #\c 'close-door)
(define-keypress *angband-keys* :global #\d 'drop-item)
(define-keypress *angband-keys* :global #\e 'show-equipment)
(define-keypress *angband-keys* :global #\f 'fire-missile)
(define-keypress *angband-keys* :global #\g 'get-item)
(define-keypress *angband-keys* :global #\i 'show-inventory)
;;(define-keypress *angband-keys* :global #\j 'jam-door)
(define-keypress *angband-keys* :global #\k 'destroy-item)
(define-keypress *angband-keys* :global #\m 'invoke-spell)
(define-keypress *angband-keys* :global #\o 'open-door)
(define-keypress *angband-keys* :global #\p 'invoke-spell)
(define-keypress *angband-keys* :global #\q 'quaff-potion)
(define-keypress *angband-keys* :global #\r 'read-text)
(define-keypress *angband-keys* :global #\s 'search-area)
(define-keypress *angband-keys* :global #\t 'take-off-item)
(define-keypress *angband-keys* :global #\u 'use-item)
(define-keypress *angband-keys* :global #\v 'throw-item)
(define-keypress *angband-keys* :global #\w 'wear-item)
(define-keypress *angband-keys* :global #\z 'zap-item)

(define-keypress *angband-keys* :global #\B 'bash-door)
(define-keypress *angband-keys* :global #\C 'show-character)
(define-keypress *angband-keys* :global #\D 'disarm-trap)
(define-keypress *angband-keys* :global #\E 'eat-item)
(define-keypress *angband-keys* :global #\F 'refill-item)
(define-keypress *angband-keys* :global #\L 'learn-spell)
;;(define-keypress *angband-keys* :global #\P 'play-music)
(define-keypress *angband-keys* :global #\Q 'quit-game)
(define-keypress *angband-keys* :global #\R 'rest)
(define-keypress *angband-keys* :global #\S 'save-game)
(define-keypress *angband-keys* :global #\? 'show-help)

(define-keypress *angband-keys* :global #\> 'go-downstairs)
(define-keypress *angband-keys* :global #\< 'go-upstairs)
(define-keypress *angband-keys* :global #\* 'select-target)
(define-keypress *angband-keys* :global #\/ 'identify-symbol)

;; these can die later..
;;(define-keypress *angband-keys* :global #\A 'print-mapper)

;; Ctrl-P
(define-keypress *angband-keys* :global (code-char 16) 'previous-messages)
;; Ctrl-R
(define-keypress *angband-keys* :global (code-char 18) 'redraw-all)
(define-keypress *angband-keys* :global (code-char 20) 'swap-map)
(define-keypress *angband-keys* :global (code-char 24) 'save-and-exit)



(define-keypress *angband-keys* :global #\, 'stand-still)
(define-keypress *angband-keys* :global #\1 'move-down-left)
(define-keypress *angband-keys* :global #\2 'move-down)
(define-keypress *angband-keys* :global #\3 'move-down-right)
(define-keypress *angband-keys* :global #\4 'move-left)
(define-keypress *angband-keys* :global #\5 'stand-still)
(define-keypress *angband-keys* :global #\6 'move-right)
(define-keypress *angband-keys* :global #\7 'move-up-left)
(define-keypress *angband-keys* :global #\8 'move-up)
(define-keypress *angband-keys* :global #\9 'move-up-right)

(define-keypress *angband-keys* :global #\. 'toggle-run-mode)

(define-keypress *angband-keys* :global '(shift #\1) 'run-down-left)
(define-keypress *angband-keys* :global '(shift #\2) 'run-down)
(define-keypress *angband-keys* :global '(shift #\3) 'run-down-right)
(define-keypress *angband-keys* :global '(shift #\4) 'run-left)
(define-keypress *angband-keys* :global '(shift #\5) 'stand-still)
(define-keypress *angband-keys* :global '(shift #\6) 'run-right)
(define-keypress *angband-keys* :global '(shift #\7) 'run-up-left)
(define-keypress *angband-keys* :global '(shift #\8) 'run-up)
(define-keypress *angband-keys* :global '(shift #\9) 'run-up-right)


;; then those keys used for display
(define-keypress *angband-keys* :display #\C 'print-attack-table)
(define-keypress *angband-keys* :display #\M 'print-misc)
(define-keypress *angband-keys* :display #\R 'print-resists)


;;; ===================================================
;;; Start roguelike keys
;;; 

(define-keypress *roguelike-keys* :global #\a 'zap-item)
(define-keypress *roguelike-keys* :global #\b 'move-down-left)
(define-keypress *roguelike-keys* :global #\c 'close-door)
(define-keypress *roguelike-keys* :global #\d 'drop-item)
(define-keypress *roguelike-keys* :global #\e 'show-equipment)
(define-keypress *roguelike-keys* :global #\f 'bash-door)
(define-keypress *roguelike-keys* :global #\g 'get-item)
(define-keypress *roguelike-keys* :global #\h 'move-left)
(define-keypress *roguelike-keys* :global #\i 'show-inventory)
(define-keypress *roguelike-keys* :global #\j 'move-down)
(define-keypress *roguelike-keys* :global #\k 'move-up)
(define-keypress *roguelike-keys* :global #\l 'move-right)
(define-keypress *roguelike-keys* :global #\m 'invoke-spell)
(define-keypress *roguelike-keys* :global #\n 'move-down-right)
(define-keypress *roguelike-keys* :global #\o 'open-door)
(define-keypress *roguelike-keys* :global #\p 'invoke-spell)
(define-keypress *roguelike-keys* :global #\q 'quaff-potion)
(define-keypress *roguelike-keys* :global #\r 'read-text)
(define-keypress *roguelike-keys* :global #\s 'search-area)
(define-keypress *roguelike-keys* :global #\t 'fire-missile)
(define-keypress *roguelike-keys* :global #\u 'move-up-right)
(define-keypress *roguelike-keys* :global #\v 'throw-item)
(define-keypress *roguelike-keys* :global #\w 'wear-item)
(define-keypress *roguelike-keys* :global #\y 'move-up-left)
(define-keypress *roguelike-keys* :global #\z 'zap-item)

(define-keypress *roguelike-keys* :global #\B 'run-down-left)
(define-keypress *roguelike-keys* :global #\C 'show-character)
(define-keypress *roguelike-keys* :global #\D 'disarm-trap)
(define-keypress *roguelike-keys* :global #\E 'eat-item)
(define-keypress *roguelike-keys* :global #\F 'refill-item)
(define-keypress *roguelike-keys* :global #\G 'learn-spell)
(define-keypress *roguelike-keys* :global #\H 'run-left)
(define-keypress *roguelike-keys* :global #\J 'run-down)
(define-keypress *roguelike-keys* :global #\K 'run-up)
(define-keypress *roguelike-keys* :global #\L 'run-right)
(define-keypress *roguelike-keys* :global #\N 'run-down-right)
(define-keypress *roguelike-keys* :global #\P 'play-music)
(define-keypress *roguelike-keys* :global #\Q 'quit-game)
(define-keypress *roguelike-keys* :global #\R 'rest)
(define-keypress *roguelike-keys* :global #\S 'jam-door)
(define-keypress *roguelike-keys* :global #\T 'take-off-item)
(define-keypress *roguelike-keys* :global #\U 'run-up-right)
(define-keypress *roguelike-keys* :global #\Y 'run-up-left)
(define-keypress *roguelike-keys* :global #\X 'save-game)
(define-keypress *roguelike-keys* :global #\? 'show-help)

(define-keypress *roguelike-keys* :global #\> 'go-downstairs)
(define-keypress *roguelike-keys* :global #\< 'go-upstairs)
(define-keypress *roguelike-keys* :global #\* 'select-target)
(define-keypress *roguelike-keys* :global #\/ 'identify-symbol)

;; these can die later..
;;(define-keypress *roguelike-keys* :global #\A 'print-mapper)

;; Ctrl-P
(define-keypress *roguelike-keys* :global (code-char 16)'previous-messages)
;; Ctrl-R
(define-keypress *roguelike-keys* :global (code-char 18) 'redraw-all)
(define-keypress *roguelike-keys* :global (code-char 20) 'swap-map)
(define-keypress *roguelike-keys* :global (code-char 24) 'save-and-exit)


;; we keep these; can't hurt to be able to use the keypad. 
(define-keypress *roguelike-keys* :global #\, 'stand-still)
(define-keypress *roguelike-keys* :global #\1 'move-down-left)
(define-keypress *roguelike-keys* :global #\2 'move-down) 
(define-keypress *roguelike-keys* :global #\3 'move-down-right)
(define-keypress *roguelike-keys* :global #\4 'move-left)
(define-keypress *roguelike-keys* :global #\5 'stand-still)
(define-keypress *roguelike-keys* :global #\6 'move-right)
(define-keypress *roguelike-keys* :global #\7 'move-up-left)
(define-keypress *roguelike-keys* :global #\8 'move-up)
(define-keypress *roguelike-keys* :global #\9 'move-up-right)
(define-keypress *roguelike-keys* :global #\. 'toggle-run-mode)

(define-keypress *roguelike-keys* :global '(shift #\1) 'run-down-left)
(define-keypress *roguelike-keys* :global '(shift #\2) 'run-down)
(define-keypress *roguelike-keys* :global '(shift #\3) 'run-down-right)
(define-keypress *roguelike-keys* :global '(shift #\4) 'run-left)
(define-keypress *roguelike-keys* :global '(shift #\5) 'stand-still)
(define-keypress *roguelike-keys* :global '(shift #\6) 'run-right)
(define-keypress *roguelike-keys* :global '(shift #\7) 'run-up-left)
(define-keypress *roguelike-keys* :global '(shift #\8) 'run-up)
(define-keypress *roguelike-keys* :global '(shift #\9) 'run-up-right)


;; then those keys used for display
(define-keypress *roguelike-keys* :display #\C 'print-attack-table)
(define-keypress *roguelike-keys* :display #\M 'print-misc)
(define-keypress *roguelike-keys* :display #\R 'print-resists)

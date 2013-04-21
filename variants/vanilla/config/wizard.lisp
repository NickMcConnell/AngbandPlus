;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/wizard.lisp - wizard keys
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

;; Ctrl-A
(define-keypress *angband-keys* :global (code-char 1) 'wizard-menu)

;; wizard stuff

(define-keypress *angband-keys* :wizard #\B 'break-game)
(define-keypress *angband-keys* :wizard #\D 'go-to-depth)
(define-keypress *angband-keys* :wizard #\G 'set-gold)
(define-keypress *angband-keys* :wizard #\H 'heal-player)
(define-keypress *angband-keys* :wizard #\I 'inspect-coord)
(define-keypress *angband-keys* :wizard #\J 'jump-to-test-level)
(define-keypress *angband-keys* :wizard #\K 'print-keys)
(define-keypress *angband-keys* :wizard #\L 'gain-level) 
(define-keypress *angband-keys* :wizard #\O 'object-create)
;;(define-keypress *angband-keys* :wizard #\P 'print-map-as-ppm)
(define-keypress *angband-keys* :wizard #\S 'send-spell)
;;(define-keypress *angband-keys* :wizard #\T 'print-map)
(define-keypress *angband-keys* :wizard #\U 'summon)
(define-keypress *angband-keys* :wizard #\W 'print-odd-info)
(define-keypress *angband-keys* :wizard #\Z 'in-game-test)

(define-keypress *angband-keys* :wizard #\d 'deliver-damage)
(define-keypress *angband-keys* :wizard #\f 'show-format)
(define-keypress *angband-keys* :wizard #\l 'load-vanilla)
(define-keypress *angband-keys* :wizard #\m 'dump-monsters)
(define-keypress *angband-keys* :wizard #\n 'show-monsters)
;;(define-keypress *angband-keys* :wizard #\o 'dump-objects)
(define-keypress *angband-keys* :wizard #\s 'show-objects)

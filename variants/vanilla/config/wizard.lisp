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
(define-keypress *ang-keys* :global (code-char 1) 'wizard-menu)

;; wizard stuff

(define-keypress *ang-keys* :wizard #\B 'break-game)
(define-keypress *ang-keys* :wizard #\D 'go-to-depth)
(define-keypress *ang-keys* :wizard #\G 'set-gold)
(define-keypress *ang-keys* :wizard #\H 'heal-player)
(define-keypress *ang-keys* :wizard #\I 'inspect-coord)
(define-keypress *ang-keys* :wizard #\J 'jump-to-test-level)
(define-keypress *ang-keys* :wizard #\K 'print-keys)
(define-keypress *ang-keys* :wizard #\L 'gain-level) 
(define-keypress *ang-keys* :wizard #\O 'object-create)
;;(define-keypress *ang-keys* :wizard #\P 'print-map-as-ppm)
(define-keypress *ang-keys* :wizard #\S 'send-spell)
;;(define-keypress *ang-keys* :wizard #\T 'print-map)
(define-keypress *ang-keys* :wizard #\U 'summon)
(define-keypress *ang-keys* :wizard #\W 'print-odd-info)
(define-keypress *ang-keys* :wizard #\Z 'in-game-test)

(define-keypress *ang-keys* :wizard #\d 'deliver-damage)
(define-keypress *ang-keys* :wizard #\l 'load-vanilla)
(define-keypress *ang-keys* :wizard #\m 'dump-monsters)
(define-keypress *ang-keys* :wizard #\n 'show-monsters)
;;(define-keypress *ang-keys* :wizard #\o 'dump-objects)
(define-keypress *ang-keys* :wizard #\s 'show-objects)
(define-keypress *ang-keys* :wizard #\t 'dummy-conversation)

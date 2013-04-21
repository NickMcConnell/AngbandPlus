;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/books.lisp - spellbooks for vanilla variant
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

;;; ===  mage-books

(define-object-kind "magic-beginner" "[magic for beginners]"
  :numeric-id 330
  :x-attr #\R
  :x-char #\?
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 30
  :cost 25
  :obj-type '(<beginner> <spellbook> <mage>)
  :sort-value 6900
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "conjurings-and-tricks" "[conjurings and tricks]"
  :numeric-id 331
  :x-attr #\R
  :x-char #\?
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 30
  :cost 100
  :obj-type '(<conjuring> <spellbook> <mage>)
  :sort-value 6901
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "incantations" "[incantations and illusions]"
  :numeric-id 332
  :x-attr #\R
  :x-char #\?
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 30
  :cost 400
  :obj-type '(<illusions> <spellbook> <mage>)
  :sort-value 6902
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "sorcery-evocations" "[sorcery and evocations]"
  :numeric-id 333
  :x-attr #\R
  :x-char #\?
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 30
  :cost 800
  :obj-type '(<sorcery> <spellbook> <mage>)
  :sort-value 6903
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 


(define-object-kind "resistance-scarab" "[resistance of scarabtarices]"
  :numeric-id 379
  :x-attr #\r
  :x-char #\?
  :depth 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 30
  :cost 5000
  :obj-type '(<resistance> <spellbook> <mage>)
  :sort-value 6904
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "mordenkainen-escapes" "[Mordenkainen's escapes]"
  :numeric-id 380
  :x-attr #\r
  :x-char #\?
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 30
  :cost 10000
  :obj-type '(<escapes> <spellbook> <mage>)
  :sort-value 6905
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "keleks-grimoire" "[Kelek's grimoire of power]"
  :numeric-id 381
  :x-attr #\r
  :x-char #\?
  :depth 60
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(60 0 0 0)
  :weight 30
  :cost 30000
  :obj-type '(<grimoire> <spellbook> <mage>)
  :sort-value 6906
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "tensers-transformations" "[Tenser's transformations]"
  :numeric-id 382
  :x-attr #\r
  :x-char #\?
  :depth 80
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(80 0 0 0)
  :weight 30
  :cost 50000
  :obj-type '(<transformations> <spellbook> <mage>)
  :sort-value 6907
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "raals-tome" "[Raal's tome of destruction]"
  :numeric-id 383
  :x-attr #\r
  :x-char #\?
  :depth 100
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(100 0 0 0)
  :weight 30
  :cost 100000
  :obj-type '(<destruction> <spellbook> <mage>)
  :sort-value 6908
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 


;;; ===  priest-books

(define-object-kind "beginner-handbook" "[beginners handbook]"
  :numeric-id 334
  :x-attr #\G
  :x-char #\?
  :depth 5
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(5 0 0 0)
  :weight 30
  :cost 25
  :obj-type '(<beginner> <spellbook> <priest>)
  :sort-value 7000
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "words-wisdom" "[words of wisdom]"
  :numeric-id 335
  :x-attr #\G
  :x-char #\?
  :depth 10
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(10 0 0 0)
  :weight 30
  :cost 100
  :obj-type '(<words> <spellbook> <priest>)
  :sort-value 7001
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "chants-blessings" "[chants and blessings]"
  :numeric-id 336
  :x-attr #\G
  :x-char #\?
  :depth 20
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(20 0 0 0)
  :weight 30
  :cost 300
  :obj-type '(<chants> <spellbook> <priest>)
  :sort-value 7002
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 

(define-object-kind "exorcism-dispelling" "[exorcism and dispelling]"
  :numeric-id 337
  :x-attr #\G
  :x-char #\?
  :depth 30
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(30 0 0 0)
  :weight 30
  :cost 900
  :obj-type '(<exorcism> <spellbook> <priest>)
  :sort-value 7003
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1)) 


(define-object-kind "ethereal-openings" "[ethereal openings]"
  :numeric-id 384
  :x-attr #\g
  :x-char #\?
  :depth 40
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(40 0 0 0)
  :weight 30
  :cost 5000
  :obj-type '(<openings> <spellbook> <priest>)
  :sort-value 7004
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "godly-insights" "[godly insights]"
  :numeric-id 385
  :x-attr #\g
  :x-char #\?
  :depth 50
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(50 0 0 0)
  :weight 30
  :cost 10000
  :obj-type '(<insights> <spellbook> <priest>)
  :sort-value 7005
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "purifications" "[purifications and healing]"
  :numeric-id 386
  :x-attr #\g
  :x-char #\?
  :depth 60
  :rarity 0
  :chance #(1 0 0 0)
  :locale #(60 0 0 0)
  :weight 30
  :cost 30000
  :obj-type '(<healing> <spellbook> <priest>)
  :sort-value 7006
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "holy-infusions" "[holy infusions]"
  :numeric-id 387
  :x-attr #\g
  :x-char #\?
  :depth 80
  :rarity 0
  :chance #(2 0 0 0)
  :locale #(80 0 0 0)
  :weight 30
  :cost 50000
  :obj-type '(<infusions> <spellbook> <priest>)
  :sort-value 7007
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

(define-object-kind "wrath-of-god" "[wrath of god]"
  :numeric-id 388
  :x-attr #\g
  :x-char #\?
  :depth 100
  :rarity 0
  :chance #(4 0 0 0)
  :locale #(100 0 0 0)
  :weight 30
  :cost 100000
  :obj-type '(<wrath> <spellbook> <priest>)
  :sort-value 7008
  :the-kind '<book>
  :game-values (make-game-values :base-dice 1 :num-dice 1 :ignores '(<cold> <fire> <electricity> <acid>))) 

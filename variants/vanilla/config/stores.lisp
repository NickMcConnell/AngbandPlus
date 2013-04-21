;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: variants/vanilla/config/stores.lisp - stores and owners for vanilla variant
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

Contains definition of stores and store-owners in sexp.

|#

(in-package :langband)

(define-store '<general-store>
    :name "General Store"
    :number 1)

(define-store '<armoury>
    :name "Armoury"
    :number 2)

(define-store '<weapon-smith>
    :name "Weapon Smith"
    :number 3)

(define-store '<temple>
    :name "Temple"
    :number 4)

(define-store '<alchemist>
    :name "Alchemist"
    :number 5)

(define-store '<magic-shop>
    :name "Magic Shop"
    :number 6)

(define-store '<black-market>
    :name "Black Market"
    :type 'black-market
    :number 7)

(define-house '<home>
    :name "Player's home"
    :number 8
    :owner :player)

;; remove
(define-store-owner
    :store-type '<general-store>
    :id 'owner-kondar1
    :name "Kon-Dar the Ugly1"
    :purse 5000
    :max-greed 210
    :min-greed 115
    :haggle-num 5
    :tolerance 7
    :race '<half-orc>)

(define-store-owner
    :store-type '<armoury>
    :id 'owner-kondar2
    :name "Kon-Dar the Ugly"
    :purse 5000
    :max-greed 210
    :min-greed 115
    :haggle-num 5
    :tolerance 7
    :race '<half-orc>)

(define-store-owner
    :store-type '<weapon-smith>
    :id 'owner-kondar3
    :name "Kon-Dar the Ugly"
    :purse 5000
    :max-greed 210
    :min-greed 115
    :haggle-num 5
    :tolerance 7
    :race '<half-orc>)

(define-store-owner
    :store-type '<temple>
    :id 'owner-kondar4
    :name "Kon-Dar the Ugly"
    :purse 5000
    :max-greed 210
    :min-greed 115
    :haggle-num 5
    :tolerance 7
    :race '<half-orc>)

(define-store-owner
    :store-type '<alchemist>
    :id 'owner-kondar5
    :name "Kon-Dar the Ugly"
    :purse 5000
    :max-greed 210
    :min-greed 115
    :haggle-num 5
    :tolerance 7
    :race '<half-orc>)

(define-store-owner
    :store-type '<magic-shop>
    :id 'owner-kondar6
    :name "Kon-Dar the Ugly"
    :purse 5000
    :max-greed 210
    :min-greed 115
    :haggle-num 5
    :tolerance 7
    :race '<half-orc>)

(define-store-owner
    :store-type '<black-market>
    :id 'owner-kondar7
    :name "Kon-Dar the Ugly"
    :purse 5000
    :max-greed 210
    :min-greed 115
    :haggle-num 5
    :tolerance 7
    :race '<half-orc>)


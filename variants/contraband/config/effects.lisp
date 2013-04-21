;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/effects.lisp - definition of magic effects
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.contraband)

(defconstant +effect-file+ 4)

(defun gfx-bolt-array (offset)
  (let ((arr (make-array 10 :initial-element 0)))
    (setf (aref arr 1) (tile-paint-value +effect-file+ (+ 2 offset))
	  (aref arr 2) (tile-paint-value +effect-file+ offset)
	  (aref arr 3) (tile-paint-value +effect-file+ (+ 3 offset))
	  (aref arr 4) (tile-paint-value +effect-file+ (+ 1 offset))
	  (aref arr 6) (tile-paint-value +effect-file+ (+ 1 offset))
	  (aref arr 7) (tile-paint-value +effect-file+ (+ 3 offset))
	  (aref arr 8) (tile-paint-value +effect-file+ offset)
	  (aref arr 9) (tile-paint-value +effect-file+ (+ 2 offset)))
    arr))

(defun text-bolt-array (colour)
  (let ((arr (make-array 10 :initial-element 0)))
    (setf (aref arr 1) (text-paint-value colour #\/)
	  (aref arr 2) (text-paint-value colour #\|)
	  (aref arr 3) (text-paint-value colour #\\)
	  (aref arr 4) (text-paint-value colour #\-)
	  (aref arr 6) (text-paint-value colour #\-)
	  (aref arr 7) (text-paint-value colour #\\)
	  (aref arr 8) (text-paint-value colour #\|)
	  (aref arr 9) (text-paint-value colour #\/))
    arr))

(defun gfx-missile-array (offset)
  (let ((arr (make-array 10 :initial-element 0)))
    (setf (aref arr 1) (tile-paint-value +effect-file+ (+ 0 offset))
	  (aref arr 2) (tile-paint-value +effect-file+ (+ 1 offset))
	  (aref arr 3) (tile-paint-value +effect-file+ (+ 2 offset))
	  (aref arr 4) (tile-paint-value +effect-file+ (+ 3 offset))
	  (aref arr 6) (tile-paint-value +effect-file+ (+ 4 offset))
	  (aref arr 7) (tile-paint-value +effect-file+ (+ 5 offset))
	  (aref arr 8) (tile-paint-value +effect-file+ (+ 6 offset))
	  (aref arr 9) (tile-paint-value +effect-file+ (+ 7 offset)))
    arr))


;;; colours aren't necessarily the same as in vanilla angband.. these colours
;;; are more tweaked for graphics.

;; light blue
(define-spell-effect "magic-missile"
    :gfx-beam (tile-paint-value +effect-file+ 51)
    :text-beam (text-paint-value +term-l-blue+ #\*)
    :gfx-ball (tile-paint-value +effect-file+ 63)
    :text-ball (text-paint-value +term-l-blue+ #\*)
    :gfx-orb (tile-paint-value +effect-file+ 101)
    :text-orb (text-paint-value +term-l-blue+ #\*)
    :gfx-bolts (gfx-bolt-array 24)
    :text-bolts (text-bolt-array +term-l-blue+))

;; yellow
(define-spell-effect "electricity"
    :gfx-beam (tile-paint-value +effect-file+ 54)
    :text-beam (text-paint-value +term-yellow+ #\*)
    :gfx-ball (tile-paint-value +effect-file+ 66)
    :text-ball (text-paint-value +term-yellow+ #\*)
    :gfx-orb (tile-paint-value +effect-file+ 99)
    :text-orb (text-paint-value +term-yellow+ #\*)
    :gfx-bolts (gfx-bolt-array 36)
    :text-bolts (text-bolt-array +term-yellow+))

;; red
(define-spell-effect "fire"
    :gfx-beam (tile-paint-value +effect-file+ 48)
    :text-beam (text-paint-value +term-red+ #\*)
    :gfx-ball (tile-paint-value +effect-file+ 60)
    :text-ball (text-paint-value +term-red+ #\*)
    :gfx-orb (tile-paint-value +effect-file+ 96)
    :text-orb (text-paint-value +term-yellow+ #\*)
    :gfx-bolts (gfx-bolt-array 12)
    :text-bolts (text-bolt-array +term-yellow+))

;; green
(define-spell-effect "acid"
    :gfx-beam (tile-paint-value +effect-file+ 49)
    :text-beam (text-paint-value +term-red+ #\*)
    :gfx-ball (tile-paint-value +effect-file+ 61)
    :text-ball (text-paint-value +term-red+ #\*)
    :gfx-orb (tile-paint-value +effect-file+ 97)
    :text-orb (text-paint-value +term-yellow+ #\*)
    :gfx-bolts (gfx-bolt-array 16)
    :text-bolts (text-bolt-array +term-yellow+))

;; brown
(define-spell-effect "poison"
    :gfx-beam (tile-paint-value +effect-file+ 52)
    :text-beam (text-paint-value +term-umber+ #\*)
    :gfx-ball (tile-paint-value +effect-file+ 64)
    :text-ball (text-paint-value +term-umber+ #\*)
    :gfx-orb (tile-paint-value +effect-file+ 97)
    :text-orb (text-paint-value +term-umber+ #\*)
    :gfx-bolts (gfx-bolt-array 28)
    :text-bolts (text-bolt-array +term-umber+))

(define-visual-projectile "arrow"
    :gfx-path (gfx-missile-array 108)
    :text-path (text-bolt-array +term-umber+))

(define-visual-projectile "bolt"
    :gfx-path (gfx-missile-array 116)
    :text-path (text-bolt-array +term-umber+))

(define-visual-projectile "stone"
    :gfx-path (make-array 10 :initial-element (tile-paint-value 13 54))
    :text-path (text-bolt-array +term-slate+))

;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: variants/vanilla/scrolls.lisp - scroll-effects
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)

(define-object-effect (<scroll> <phase-door>)
    (:effect (:read :use))
  (dun pl item)
  (teleport-creature! dun pl pl 10)
  (possible-identify! pl item)
  :used)

(define-object-effect (<scroll> <illuminate>)
    (:effect (:read :use))
  (dun pl item)
;;  (declare (ignore dun pl item))
  (warn "light.")
  (when (light-area! dun pl (roll-dice 2 8) 2 :type '<light>) ;; 2d8 dmg, radius 2
    (possible-identify! pl item))
  :used)

(define-object-effect (<scroll> <darkness>)
    (:effect (:read :use))
  (dun pl item)
;;  (declare (ignore dun pl item))
  (when (light-area! dun pl (roll-dice 2 8) 2 :type '<darkness>) ;; 2d8 dmg, radius 2
    (possible-identify! pl item))
  :used)


(define-object-effect (<scroll> <teleportation>)
    (:effect (:read :use))
  (dun pl item)
;;  (warn "phase door.")
  (teleport-creature! dun pl pl 100)
  (possible-identify! pl item)
  :used)

(define-object-effect (<scroll> <enchant> <weapon> <to-hit>)
    (:effect (:read :use))
  (dun pl item)
  (let ((retval (enchant-item! dun pl :type '<weapon> :bonus 1 :restrict '<to-hit>)))
    (possible-identify! pl item)
    retval))

(define-object-effect (<scroll> <enchant> <weapon> <to-dmg>)
    (:effect (:read :use))
  (dun pl item)
  (let ((retval (enchant-item! dun pl :type '<weapon> :bonus 1 :restrict '<to-dmg>)))
    (possible-identify! pl item)
    retval))

(define-object-effect (<scroll> <enchant> <weapon> <powerful>)
    (:effect (:read :use))
  (dun pl item)
  (let ((retval (enchant-item! dun pl :type '<weapon> :bonus (+ (randint 3) (randint 3)))))
    (possible-identify! pl item)
    retval))


(define-object-effect (<scroll> <enchant> <armour> <normal>)
    (:effect (:read :use))
  (dun pl item)
  (let ((retval (enchant-item! dun pl :type '<armour> :bonus 1)))
    (possible-identify! pl item)
    retval))

(define-object-effect (<scroll> <enchant> <armour> <powerful>)
    (:effect (:read :use))
  (dun pl item)
  (let ((retval (enchant-item! dun pl :type '<armour> :bonus (+ 2 (randint 3)))))
    (possible-identify! pl item)
    retval))

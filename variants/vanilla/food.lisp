;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/food.lisp - effects of food
Copyright (c) 2001-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(define-object-effect (<food> <mushroom> <poison>)
    (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  (when (set-creature-state! pl :poison (+ 10 (random 10)))
    (possible-identify! pl item))
  :used)

(define-object-effect (<food> <mushroom> <blindness>) (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  (when (set-creature-state! pl :blindness (+ 200 (random 200)))
    (possible-identify! pl item))
  :used)

(define-object-effect (<food> <mushroom> <paranoia>) (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  (when (set-creature-state! pl :fear (+ 10 (random 10)))
    (possible-identify! pl item))
  :used)

(define-object-effect (<food> <mushroom> <confusion>) (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  (when (set-creature-state! pl :confusion (+ 10 (random 10)))
    (possible-identify! pl item))
  :used)

(define-object-effect (<food> <mushroom> <hallucination>) (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  (when (set-creature-state! pl :hallucination (+ 250 (random 250)))
    (possible-identify! pl item))
  :used)

(define-object-effect (<food> <mushroom> <paralysis>) (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  (when (set-creature-state! pl :paralysis (+ 10 (random 10)))
    (possible-identify! pl item))
  :used)

;; weakness
(define-object-effect (<food> <mushroom> <reduce> <str>) (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  ;; add damage and desc
  (update-player-stat! pl '<str> '<reduce>)
  (possible-identify! pl item)
  :used)

;; sickness
(define-object-effect (<food> <mushroom> <reduce> <con>) (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  ;; add damage and desc
  (update-player-stat! pl '<con> '<reduce>)
  (possible-identify! pl item)
  :used)

;; stupidity
(define-object-effect (<food> <mushroom> <reduce> <int>) (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  ;; add damage and desc
  (update-player-stat! pl '<int> '<reduce>)
  (possible-identify! pl item)
  :used)

;; naivety
(define-object-effect (<food> <mushroom> <reduce> <wis>) (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  ;; add damage and desc
  (update-player-stat! pl '<wis> '<reduce>)
  (possible-identify! pl item)
  :used)

(define-object-effect (<food> <mushroom> <cure> <poison>) (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  (when (set-creature-state! pl :poison nil)
    (possible-identify! pl item))
  :used)

(define-object-effect (<food> <mushroom> <cure> <blindness>) (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  (when (set-creature-state! pl :blindness nil)
    (possible-identify! pl item))
  :used)

(define-object-effect (<food> <mushroom> <cure> <paranoia>) (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  (when (set-creature-state! pl :fear nil)
    (possible-identify! pl item))
  :used)

(define-object-effect (<food> <mushroom> <cure> <confusion>) (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  (when (set-creature-state! pl :confusion nil)
    (possible-identify! pl item))
  :used)

(define-object-effect (<food> <mushroom> <cure> <serious>) (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  (when (heal-creature! pl (roll-dice 4 8))
    (possible-identify! pl item))
  :used)

(define-object-effect (<food> <mushroom> <restore> <str>) (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<str> '<restore>)
    (possible-identify! pl item))
  :used)

(define-object-effect (<food> <mushroom> <restore> <con>) (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<con> '<restore>)
    (possible-identify! pl item))
  :used)

(define-object-effect (<food> <mushroom> <restoring>) (:effect (:eat :use)) (dun pl item)
  (declare (ignore dun))
  (update-player-stat! pl '<str> '<restore>)
  (update-player-stat! pl '<dex> '<restore>)
  (update-player-stat! pl '<con> '<restore>)
  (update-player-stat! pl '<int> '<restore>)
  (update-player-stat! pl '<wis> '<restore>)
  (update-player-stat! pl '<chr> '<restore>)
  (possible-identify! pl item)
  :used)

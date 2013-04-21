;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: variants/vanilla/potions.lisp - potion-effects
Copyright (c) 2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)

;; this file and code will change!


;;; ===================
;;; cure potions
;;; ===================

(define-object-effect (<potion> <cure> <light>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (let ((okind (aobj.kind item))
	(amount (roll-dice 2 8)))
    (when (heal-creature! pl amount)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :blindness nil)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :heal-cut '<light>)
      (possible-identify! pl okind))
    :used))

(define-object-effect (<potion> <cure> <serious>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (let ((okind (aobj.kind item))
	(amount (roll-dice 4 8)))
    (when (heal-creature! pl amount)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :blindness nil)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :confusion nil)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :heal-cut '<serious>)
      (possible-identify! pl okind))
    :used))

(define-object-effect (<potion> <cure> <critical>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (let ((okind (aobj.kind item))
	(amount (roll-dice 6 8)))
    (when (heal-creature! pl amount)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :blindness nil)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :confusion nil)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :poison nil)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :cut nil)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :stun nil)
      (possible-identify! pl okind))

    :used))


(define-object-effect (<potion> <cure> <poison>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (let ((okind (aobj.kind item)))
    (when  (set-creature-state! pl :poison nil)
      (possible-identify! pl okind))
    :used))

(define-object-effect (<potion> <cure> <healing> <normal>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (let ((okind (aobj.kind item))
	(amount 300))
    (when (heal-creature! pl amount)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :blindness nil)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :confusion nil)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :poison nil)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :cut nil)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :stun nil)
      (possible-identify! pl okind))
    :used))

(define-object-effect (<potion> <cure> <healing> <powerful>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (let ((okind (aobj.kind item))
	(amount 1200))
    (when (heal-creature! pl amount)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :blindness nil)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :confusion nil)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :poison nil)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :cut nil)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :stun nil)
      (possible-identify! pl okind))
    :used))


(define-object-effect (<potion> <life>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (let ((okind (aobj.kind item))
	(amount 5000))
    (c-print-message! "You feel life flow through your body.")
    (heal-creature! pl amount)
    (set-creature-state! pl :blindness nil)
    (set-creature-state! pl :confusion nil)
    (set-creature-state! pl :poison nil)
    (set-creature-state! pl :cut nil)
    (set-creature-state! pl :stun nil)
    (set-creature-state! pl :hallucination nil)

    (update-player-stat! pl '<str> '<restore>)
    (update-player-stat! pl '<dex> '<restore>)
    (update-player-stat! pl '<con> '<restore>)
    (update-player-stat! pl '<int> '<restore>)
    (update-player-stat! pl '<wis> '<restore>)
    (update-player-stat! pl '<chr> '<restore>)
    (possible-identify! pl okind)
    :used))

;;; =====================
;;; restore-stat potions
;;; =====================

(define-object-effect (<potion> <restore> <str>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<str> '<restore>)
    (possible-identify! pl (aobj.kind item)))
  :used)

(define-object-effect (<potion> <restore> <dex>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<dex> '<restore>)
    (possible-identify! pl (aobj.kind item)))
  :used)

(define-object-effect (<potion> <restore> <con>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<con> '<restore>)
    (possible-identify! pl (aobj.kind item)))
  :used)

(define-object-effect (<potion> <restore> <int>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<int> '<restore>)
    (possible-identify! pl (aobj.kind item)))
  :used)

(define-object-effect (<potion> <restore> <wis>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<wis> '<restore>)
    (possible-identify! pl (aobj.kind item)))
  :used)

(define-object-effect (<potion> <restore> <chr>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<chr> '<restore>)
    (possible-identify! pl (aobj.kind item)))
  :used)

;;; ===================
;;; stat-gain potions
;;; ===================

(define-object-effect (<potion> <increase> <str>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<str> '<increase>)
    (possible-identify! pl (aobj.kind item)))
  :used)

(define-object-effect (<potion> <increase> <dex>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<dex> '<increase>)
    (possible-identify! pl (aobj.kind item)))
  :used)

(define-object-effect (<potion> <increase> <con>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<con> '<increase>)
    (possible-identify! pl (aobj.kind item)))
  :used)

(define-object-effect (<potion> <increase> <int>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<int> '<increase>)
    (possible-identify! pl (aobj.kind item)))
  :used)

(define-object-effect (<potion> <increase> <wis>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<wis> '<increase>)
    (possible-identify! pl (aobj.kind item)))
  :used)

(define-object-effect (<potion> <increase> <chr>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<chr> '<increase>)
    (possible-identify! pl (aobj.kind item)))
  :used)

(define-object-effect (<potion> <augmentation>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<str> '<increase>)
    (possible-identify! pl (aobj.kind item)))
  (when (update-player-stat! pl '<dex> '<increase>)
    (possible-identify! pl (aobj.kind item)))
  (when (update-player-stat! pl '<con> '<increase>)
    (possible-identify! pl (aobj.kind item)))
  (when (update-player-stat! pl '<int> '<increase>)
    (possible-identify! pl (aobj.kind item)))
  (when (update-player-stat! pl '<wis> '<increase>)
    (possible-identify! pl (aobj.kind item)))
  (when (update-player-stat! pl '<chr> '<increase>)
    (possible-identify! pl (aobj.kind item)))
  :used)

;;; ===================
;;; lower-stat potions
;;; ===================

(define-object-effect (<potion> <reduce> <str>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<str> '<reduce>)
    (possible-identify! pl (aobj.kind item)))
  :used)

(define-object-effect (<potion> <reduce> <dex>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<dex> '<reduce>)
    (possible-identify! pl (aobj.kind item)))
  :used)

(define-object-effect (<potion> <reduce> <con>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<con> '<reduce>)
    (possible-identify! pl (aobj.kind item)))
  :used)

(define-object-effect (<potion> <reduce> <int>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<int> '<reduce>)
    (possible-identify! pl (aobj.kind item)))
  :used)

(define-object-effect (<potion> <reduce> <wis>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<wis> '<reduce>)
    (possible-identify! pl (aobj.kind item)))
  :used)

(define-object-effect (<potion> <reduce> <chr>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when (update-player-stat! pl '<chr> '<reduce>)
    (possible-identify! pl (aobj.kind item)))
  :used)

;;; ===================
;;; nutrition
;;; ===================

(define-object-effect (<potion> <slime-mold>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (c-print-message! "You feel less thirsty!")
  (possible-identify! pl (aobj.kind item))
  :used)

(define-object-effect (<potion> <apple-juice>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (c-print-message! "You feel less thirsty!")
  (possible-identify! pl (aobj.kind item))
  :used)

(define-object-effect (<potion> <water>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (c-print-message! "You feel less thirsty!")
  (possible-identify! pl (aobj.kind item))
  :used)


;;; ===================
;;; assorted potions
;;; ===================

(define-object-effect (<potion> <berserk-strength>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (let ((okind (aobj.kind item)))
    (when (heal-creature! pl 12)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :fear nil)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :hero t)
      (possible-identify! pl okind)))
  :used)

(define-object-effect (<potion> <heroism>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (let ((okind (aobj.kind item)))
    (when (heal-creature! pl 30)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :fear nil)
      (possible-identify! pl okind))
    (when  (set-creature-state! pl :berserk t)
      (possible-identify! pl okind)))
  :used)

(define-object-effect (<potion> <boldness>) (:effect (:quaff :use))
    (dun pl item)
  (declare (ignore dun))
  (when  (set-creature-state! pl :fear nil)
    (possible-identify! pl (aobj.kind item)))
  :used)

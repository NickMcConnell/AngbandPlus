;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/combat.lisp - combat-related code for vanilla
Copyright (c) 2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(defmethod melee-inflict-damage! ((attacker player) target the-attack)
  (declare (ignore the-attack))
  (let* ((weapon (get-melee-weapon attacker)))

    (cond ((not weapon)
	   (deduct-hp! target 1)
	   1)
	  (t
	   ;; we have a weapon..
	   (let ((gval (object.game-values weapon)))
	     (assert gval)
	     (let ((dmg (roll-dice (gval.num-dice gval) (gval.base-dice gval))))
	       (incf dmg (gval.dmg-modifier gval))
	       (when (< dmg 1) (setf dmg 1)) ;; minimum damage
;;	       (warn "~ad~a gave ~a dmg to attacker (~a -> ~a hps)"
;;		     (gval.num-dice gval) (gval.base-dice gval) dmg
;;		     (current-hp target) (- (current-hp target) dmg))
	       (deduct-hp! target dmg)
	       dmg))))

    ))

;; this is a bit generic until it gets specialised
(defmethod deliver-damage! ((variant variant) source (target player) damage &key note dying-note)

  ;; to avoid warnings
  (declare (ignore note dying-note))
  
  (let ((did-target-die? nil))
  
    ;; wake it up
    ;; deliver damage
    (decf (current-hp target) damage)

    (bit-flag-add! *redraw* +print-hp+)
    
    (when (minusp (current-hp target))
      (setf did-target-die? t)

      ;; this might fail badly, but at least it will be visible!
      (format-message! "You were killed by ~a" source)

      (kill-target! *dungeon* source target (location-x target) (location-y target))
      )

    did-target-die?))

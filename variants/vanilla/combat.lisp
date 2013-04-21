;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/combat.lisp - combat-related code for vanilla
Copyright (c) 2002-2003 - Stig Erik Sandø

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

;; move somewhere else later
(defmethod shoot-a-missile ((dungeon dungeon) (player player)
			    (missile-weapon active-object/bow)
			    (arrow active-object/ammo))
  
;;  (declare (ignore missile-weapon))
  (block missile-shooting
    (when-bind (dir (get-aim-direction))
      (assert (and (numberp dir) (< dir 10)))
;;      (check-type arrow active-object)
      
      ;;(warn "dir is ~s with ~s + ~s" dir missile-weapon arrow)

      (multiple-value-bind (tx ty)
	  (%get-dest-coords player dir 99)
      
      (let* ((pvx (location-x player))
	     (pvy (location-y player))
	     ;;(ddx *ddx*)
	     ;;(ddy *ddy*)
	     ;;(tx (+ pvx (* 99 (aref ddx dir))))
	     ;;(ty (+ pvy (* 99 (aref ddy dir))))
	     (max-range (+ 10 (* 5 (object.multiplier (aobj.kind missile-weapon)))))
	     (path-arr (make-array (1+ max-range) :fill-pointer 0))
	     (path-len (project-path dungeon max-range path-arr pvx pvy tx ty 0))
	     (cur-x pvx)
	     (cur-y pvy)
	     )

	(declare (ignore path-len))

	
	(loop named follow-path
	      for g across path-arr
	      do
	      (let ((x (grid-x g))
		    (y (grid-y g))
		    (old-x cur-x)
		    (old-y cur-y))
		
		(setq cur-x x
		      cur-y y)
		(unless (cave-floor-bold? dungeon x y)
		  (return-from follow-path nil))

		(let ((gfx-sym (tile-paint-value 4 112)) ;; hack
		      (text-sym (text-paint-value +term-red+ #\-)) ;; hack
		      (diff-x (- cur-x old-x))
		      (diff-y (- cur-y old-y)))
		  (when-bind (vis (get-visual-projectile arrow))
		    ;;(warn "Got ~s for arrow ~s" vis arrow)
		    (setf gfx-sym (aref (projectile.gfx-path vis)
					(get-direction-from-diff diff-x diff-y)))
		    (setf text-sym (aref (projectile.text-path vis)
					 (get-direction-from-diff diff-x diff-y))))
		  ;; do better check on getting this.. 
		  (display-moving-object dungeon x y text-sym gfx-sym))
	      
		(when-bind (monsters (cave-monsters dungeon x y))
		  (let* ((fmon (if (consp monsters) (car monsters) monsters))
			 (mon-name (get-creature-name fmon)))
		    (when (missile-hit-creature? player fmon missile-weapon arrow)
		      
		      (format-message! "The ~a was hit." mon-name)
		      (missile-inflict-damage! player fmon missile-weapon arrow)
		      (when (< (current-hp fmon) 0)
			(format-message! "The ~a died." mon-name)
			(let ((target-xp (get-xp-value fmon)))
			  (alter-xp! player (if target-xp target-xp 0)))
			(kill-target! dungeon player fmon x y)
			;; repaint spot
			(light-spot! dungeon x y))

		      (return-from follow-path nil))))
		))

	;; if it crashes in a wall, your arrow is gone.
	(when (cave-floor-bold? dungeon cur-x cur-y)
	  (item-table-add! (get-item-table dungeon player :floor :x cur-x :y cur-y)
			   arrow))

	))))
  )

;; doesn't check for special arrows, or critical hits, etc. 
(defmethod missile-inflict-damage! ((attacker player) (target active-monster)
				    (miss-wpn active-object/bow) (missile active-object/ammo))

  (let ((bow-gval (object.game-values miss-wpn))
	(miss-gval (object.game-values missile)))
    
    (assert miss-gval)
    
    (let ((dmg (roll-dice (gval.num-dice miss-gval) (gval.base-dice miss-gval))))
      
      (incf dmg (gval.dmg-modifier miss-gval))
      
      (when bow-gval
	(incf dmg (gval.dmg-modifier bow-gval)))

      (when-bind (bowkind (aobj.kind miss-wpn))
	(setf dmg (* dmg (object.multiplier bowkind))))

      (when (< dmg 1) (setf dmg 1)) ;; minimum damage
      
      (deduct-hp! target dmg)

      dmg)))

;; very hackish code..
(defun interactive-fire-a-missile (dungeon player)
  "Hackish shoot-code."

  (block missile-shooting
    (let ((the-bow (get-missile-weapon player))
	  (the-missile nil))
      (flet ((sel-fun (x)
	       (when (typep x 'active-object/ammo)
		 (let ((bowid (object.id (aobj.kind the-bow)))
		       (ammoid (object.id (aobj.kind x))))
		   ;; very very hackish
		   (cond ((or (equal bowid "long-bow")
			      (equal bowid "short-bow"))
			  (or (equal ammoid "arrow")
			      (equal ammoid "seeker-arrow")))
			 ((or (equal bowid "light-xbow")
			      (equal bowid "heavy-xbow"))
			  (or (equal ammoid "bolt")
			      (equal ammoid "seeker-bolt")))
			 ((or (equal bowid "sling")
			      (equal bowid "staff-sling"))
			  (or (equal ammoid "round-pebble")
			      (equal ammoid "iron-shot")))
			 (t nil))
		   ))))
	(unless (and the-bow (typep the-bow 'active-object/bow))
	  (print-message! "You have no missile weapon!")
	  (return-from missile-shooting nil))
	
	(with-dialogue ()
	  (setq the-missile (grab-a-selection-item dungeon player '(:backpack :floor)
						   :prompt "Select missile:"
						   :selection-function #'sel-fun
						   :where :backpack)))
	
	
	(cond ((and the-missile (typep the-missile 'active-object/ammo))
	       (shoot-a-missile dungeon player the-bow the-missile))
	      (t
	       (print-message! "No missile selected!")))
	))
    ))

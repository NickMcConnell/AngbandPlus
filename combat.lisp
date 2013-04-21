;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: combat.lisp - the combat-system
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(defun check-for-hits ()

  )

(defmethod kill-target! (dungeon attacker (target active-monster) x y)
  "Tries to remove the monster at the location."

  (let ((var-obj *variant*)
	(lvl-obj *level*)
	(the-kind (amon.kind target)))
    
    (setf (creature-alive? target) nil)
    
    ;; let us generate drop.. 
    (when (can-creature-drop? var-obj target)
      (creature-drop! var-obj target lvl-obj))
    
    (setf (dungeon.monsters dungeon) (delete target (dungeon.monsters dungeon))
	  (cave-monsters dungeon x y) nil)

    ;; hack
    (when (typep the-kind 'unique-monster)
      (setf (monster.already-dead the-kind) t))
    
    (when (is-player? attacker)
      (let ((mon-know (get-monster-knowledge attacker target)))
	(incf (monster.num-killed mon-know))))
    
    t))
      


  
(defmethod kill-target! (dungeon attacker (target player) x y)
  "Tries to remove the monster at the location."
  (declare (ignore dungeon x y))

  (let ((killer (cond ((stringp attacker)
		       attacker)
		      ;; unsafe, but will give errors
		      (t
		       (get-creature-name attacker)))))

  
    (setf (creature-alive? target) nil)
    (setf (player.dead-from target) killer)
  
  nil))

(defmethod cmb-describe-miss ((attacker active-monster) (target player))

  (format-message! "~@(~a~) misses you." (get-creature-desc attacker #x00))
  nil)

(defmethod cmb-describe-miss ((attacker player) (target active-monster))

  ;; update with uniques later
  (format-message! "You miss ~a." (get-creature-desc target #x00))
  nil)


(defun %calc-perchance (chance the-ac)
  (let ((mid-res (int-/ (* 90 (- chance (int-/ (* 3 the-ac) 4))) chance)))
;;    (warn "mid-res is ~a" mid-res)
    (+ 5 mid-res)))

;;(trace %calc-perchance)

(defun melee-hit-ac? (the-target combat-skill the-ac visible-p)
  "Helper-function that checks if something was a hit."
  (declare (ignore the-target))
  
  (let ((k (random 100)))
    ;; instant miss or hit (5% chance each)
    (when (< k 10) (return-from melee-hit-ac? (< k 5))))
  
  (unless visible-p
    (setq combat-skill (int-/ combat-skill 2)))

  #+chance-warning
  (warn "Chance to hit '~a' with skill ~s vs AC ~a is ~a%"
	(get-creature-name the-target) combat-skill the-ac
	(%calc-perchance combat-skill the-ac))
  
  ;; this check is not 100% correct, but it does the job fairly accurately
  (if (and (plusp combat-skill)
	   (>= (random combat-skill)
	       (int-/ (* 3 the-ac)
		      4)))
      t
      nil))

	    
(defmethod melee-hit-creature? ((attacker player) (target active-monster) the-attack)
  (declare (ignore the-attack))
 
    (let* ((bonus 0) ;; (* (+ to-hit for weapon and dex/str) multiplier)
	   (chance (+ (get-melee-attack-skill *variant* attacker) bonus))
	   (monster-ac (get-creature-ac target))
	   (visible-p t))
      
      (disturbance *variant* attacker target :max)
      (melee-hit-ac? target chance monster-ac visible-p)))

(defmethod get-power-of-attack (variant attack-kind)
  (declare (ignore variant attack-kind))
  (error "fell through get-power-of-attack"))

(defmethod melee-hit-creature? ((attacker active-monster) (target player) the-attack)

  (check-type the-attack attack)
  (disturbance *variant* target attacker :max)
  
  (let ((atype (attack.dmg-type the-attack)))
    (unless atype
      (return-from melee-hit-creature? t)) ;; right?
  
    
    (let* ((power (cond ((typep atype 'attack-type)
			 (attack-type.power atype))
			(t
			 (get-power-of-attack *variant* atype))))
	   (mlvl (get-power-lvl attacker))
	   (rlev (if (plusp mlvl) mlvl 1))
	   (skill (+ power (* 3 rlev))))
      
;;      (warn "Monster (~s ~s): ~s" (get-creature-name attacker) (attack.dmg-type the-attack) skill)
      
      (melee-hit-ac? target skill
		     (get-creature-ac target)
		     t))
    ))


(defun deduct-hp! (target amount)
  (when (plusp amount)
    (decf (current-hp target) amount)))

(defmethod melee-inflict-damage! ((attacker active-monster) target (the-attack attack))
  
  (let* ((dmg-dice (attack.damage the-attack))
	 (damage 0))
    
    (when (consp dmg-dice)
      (setf damage (roll-dice (car dmg-dice) (cdr dmg-dice))))
    
    (let ((atype (attack.dmg-type the-attack)))
      (when (and (typep atype 'attack-type)
		 (functionp (attack-type.hit-effect atype)))
	(return-from melee-inflict-damage! (funcall (attack-type.hit-effect atype)
						    attacker target the-attack damage))))

    ;; simple fallback
    (when (plusp damage)
      (deduct-hp! target damage)
      (when (is-player? target)
	(bit-flag-add! *redraw* +print-hp+)))

    damage))


(defun define-attack-description (symbol desc)
  "Adds a legit attack-description."
  
  (unless (nonboolsym? symbol)
    (error-condition 'illegal-attack-data :id symbol :desc "Attack-desc symbol not legal."))
  (unless (stringp desc)
    (error-condition 'illegal-attack-data :id symbol :desc "Description of an attack not a string."))
  
  (setf (get-attack-description *variant* symbol) desc))
  

(defmethod get-attack-description (variant the-attack)
  "Checks the attack and tries to return an appropriate attack description."
  (let ((descs (variant.attack-descriptions variant))
	(desc nil))

    (etypecase the-attack
      (attack (setf desc (gethash (attack.kind the-attack) descs)))
      (symbol (setf desc (gethash the-attack descs))))
    
    (if desc
	desc
	"hits you.")))

(defmethod (setf get-attack-description) (desc variant attack)
  (assert (stringp desc))
  (setf (gethash attack (variant.attack-descriptions variant)) desc))


(defmethod cmb-describe-hit ((attacker active-monster) (target player) the-attack)
  (let ((desc (get-attack-description *variant* the-attack)))
    (format-message! "~@(~a~) ~A." (get-creature-desc attacker #x00) desc)
    ))

(defmethod cmb-describe-hit ((attacker player) (target active-monster) the-attack)
  (declare (ignore the-attack))
  (format-message! "You hit ~A." (get-creature-desc target #x00)))
        

(defmethod cmb-describe-hit (attacker target the-attack)
  (declare (ignore the-attack))
  (format-message! "~a hits ~A." (get-creature-name attacker) (get-creature-name target))
  nil)


(defmethod cmb-describe-death (attacker target)
  (declare (ignore attacker))
  (typecase target
    (player
     (format-message! "~a dies." (get-creature-name target)))
    (t
     (play-sound "kill-someone")
     (format-message! "~@(~A~) dies." (get-creature-desc target 0))))
  nil)


(defun attack-target! (dungeon attacker target x y the-attack)

  ;;	(describe the-monster)
  (unless (melee-hit-creature? attacker target the-attack)
    (cmb-describe-miss attacker target)
    (play-sound "miss-someone")
    (return-from attack-target! nil))

  (play-sound "hit-someone")
  
  (let ((cur-hp (current-hp target)))
    (cmb-describe-hit attacker target the-attack)
    (melee-inflict-damage! attacker target the-attack)

    ;; target is still alive!
    (when (>= (current-hp target) 0)

      ;; if we got any dmg, let's repaint some
      (when (and (is-player? target)
		 (/= (current-hp target) cur-hp))
	(bit-flag-add! *redraw* +print-hp+))

      (return-from attack-target! t))

    
    ;; target actually died!
    (cmb-describe-death attacker target)
    (let ((target-xp (get-xp-value target)))
      (modify-xp! attacker (if target-xp target-xp 0)))
    
    (kill-target! dungeon attacker target x y)
    ;; repaint
    (light-spot! dungeon x y)
    ))


(defun attack-location! (dungeon player x y)
  "attacks a given location.."

  (when-bind (monsters (cave-monsters dungeon x y))
    (let ((the-monster (car monsters)))
      ;; hack
      ;;(play-sound "hit-someone")
      (attack-target! dungeon player the-monster x y nil)
      )))


(defun cmb-monster-attack! (dungeon player mon the-x the-y)
  "The monster attacks the player  at (the-x,the-y)."
  (dolist (the-attack (monster.attacks mon))
    (when (and (creature-alive? mon) (creature-alive? player))
      (attack-target! dungeon mon player the-x the-y the-attack))))


(defmethod missile-hit-creature? ((attacker player) (target active-monster) missile-weapon missile)
  ;;  (declare (ignore missile-weapon missile))
  
  (let ((num (random 100)))
    (when (< num 10)
      ;; instant hit and miss 5%
      (return-from missile-hit-creature? (< num 5)))

    (let ((bonus 0))
      (when-bind (gvals (aobj.game-values missile-weapon))
	(incf bonus (gval.tohit-modifier gvals)))
      (when-bind (gvals (aobj.game-values missile))
	(incf bonus (gval.tohit-modifier gvals)))
      
    
      ;; add bow modifiers back in
      (let* ((chance (+ (get-ranged-attack-skill *variant* attacker) (* 3 bonus))) ;; hack
	     (dist (distance (location-x attacker) (location-y attacker)
			     (location-x target) (location-y target)))
	     (red-chance (- chance dist))
	     (target-ac (get-creature-ac target))
	     )

	;; fix invisible later
	#+never
	(warn "chance to hit is ~s on ac ~s" red-chance target-ac)
      
	(when (and (plusp red-chance)
		   (>= (random red-chance) (int-/ (* 3 target-ac) 4)))
	  (return-from missile-hit-creature? t))
      
	nil))))

;; override this in the variant code
(defmethod missile-inflict-damage! ((attacker player) (target active-monster) missile-weapon missile)
  (declare (ignore missile-weapon missile))
  (deduct-hp! target (roll-dice 2 4)))

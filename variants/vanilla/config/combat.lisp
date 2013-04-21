;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/combat.lisp - combat factors
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(defun %get-power-level (attacker)
  (let ((mon-lvl (monster.power-lvl (amon.kind attacker))))
    (if (> mon-lvl 1)
	mon-lvl
	1)))

(define-monster-attack '<hurt>
    :power 60
    :hit-effect (attack-effect (attacker target the-attack damage)
		    ;; alter for ac
		    (deduct-hp! target damage)
		    damage))

(define-monster-attack '<shatter>
    :power 60
    :hit-effect (attack-effect (attacker target attack dmg)
		  ;; alter for ac
		  (deduct-hp! target dmg)
		  ;; earthquake?
		  dmg))

(define-monster-attack '<paralyse>
    :power 2
    :hit-effect (attack-effect (attacker target attack dmg)
		  ;; add hack for always dmg
		  (deduct-hp! target dmg)
		  (cond ((eq t (get-creature-state *player* '<free-action>))
			 (print-message! "You are unaffected!"))
			;; dummy save
			((< (random 100) 50)
			 (print-message! "You resist the effects!"))
			(t
			 (modify-creature-state! target '<paralysed>
						 :add (+ 3 (randint (%get-power-level attacker))))))

		    ;; add paralyse knowledge to attacker
		    dmg))

(define-monster-attack '<terrify>
    :power 10
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  (cond ((or (resists-element? target '<fear>)
			     ;; dummy save
			     (< (random 100) 50))
			 (print-message! "You stand your ground!"))
			(t
			 (modify-creature-state! target '<fear>
						 :add (+ 3 (randint (%get-power-level attacker))))))

		    ;; add fear knowledge to attacker
		    dmg))

(define-monster-attack '<blind>
    :power 2
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  (unless (resists-element? target '<blindness>)
		    (modify-creature-state! target '<blindness>
					    :add (+ 10 (randint (%get-power-level attacker)))))
		  ;; add blind knowledge to attacker
		  dmg))


(define-monster-attack '<poison>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  (unless (resists-element? target '<poison>)
		    (modify-creature-state! target '<poisoned>
					    :add (+ 5 (randint (%get-power-level attacker)))))
		  ;; add poison knowledge to attacker
		  dmg))

(define-monster-attack '<confusion>
    :power 10
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  (unless (resists-element? target '<confusion>)
		    (modify-creature-state! target '<confusion>
					    :add (+ 3 (randint (%get-power-level attacker)))))
		  ;; add poison knowledge to attacker
		  dmg))

(define-monster-attack '<exp-40>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  ;; check for hold-life
		  (alter-xp! target 400) ;; hack
		  dmg))

(define-monster-attack '<eat-gold>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (when (plusp dmg)
		    (deduct-hp! target dmg))
		  ;; expand to monsters too later
		  (when (typep target 'player)
		    ;; saving throw
		    (cond ((< (random 100) 50) ;; base on dex and lvl later
			   (print-message! "You quickly protect your money pouch!")
			   ;; possible blink
			   )
			  (t
			   ;; fix this to also apply to monsters
			   (let* ((cur-gold (player.gold target))
				  (amount (+ 25 (int-/ cur-gold 10))))
			     (when (< amount 2)
			       (setf amount 2))
			     ;; skip 5000 limit
			     (when (> amount cur-gold)
			       (setf amount cur-gold))

			     (decf (player.gold target) amount)

			     (cond ((<= amount 0)
				    (print-message! "Nothing was stolen!"))
				   ((plusp (player.gold target))
				    (print-message! "Your purse feels lighter.")
				    (format-message! "~d coins were stolen!" amount))
				   (t
				    (print-message! "Your purse feels lighter.")
				    (print-message! "All your coins were stolen.")))

			     ;; repaint
			     (bit-flag-add! *redraw* +print-gold+)
			     ;; add blink-away
			     ))))

		  dmg))

(define-monster-attack '<lose-str>
    :power 0
    :hit-effect (attack-effect (attacker target the-attack damage)
		  (when damage
		    (deduct-hp! target damage))
		  (when (typep target 'player)
		    (update-player-stat! target '<str> '<reduce>))
		  damage))

(define-monster-attack '<lose-dex>
    :power 0
    :hit-effect (attack-effect (attacker target the-attack damage)
		  (when damage
		    (deduct-hp! target damage))
		  (when (typep target 'player)
		    (update-player-stat! target '<dex> '<reduce>))
		  damage))

(define-monster-attack '<lose-con>
    :power 0
    :hit-effect (attack-effect (attacker target the-attack damage)
		  (when damage
		    (deduct-hp! target damage))
		  (when (typep target 'player)
		    (update-player-stat! target '<con> '<reduce>))
		  damage))

(define-monster-attack '<lose-int>
    :power 0
    :hit-effect (attack-effect (attacker target the-attack damage)
		  (when damage
		    (deduct-hp! target damage))
		  (when (typep target 'player)
		    (update-player-stat! target '<int> '<reduce>))
		  damage))

(define-monster-attack '<lose-wis>
    :power 0
    :hit-effect (attack-effect (attacker target the-attack damage)
		  (when damage
		    (deduct-hp! target damage))
		  (when (typep target 'player)
		    (update-player-stat! target '<wis> '<reduce>))
		  damage))

(define-monster-attack '<lose-chr>
    :power 0
    :hit-effect (attack-effect (attacker target the-attack damage)
		  (when damage
		    (deduct-hp! target damage))
		  (when (typep target 'player)
		    (update-player-stat! target '<chr> '<reduce>))
		  damage))

(define-monster-attack '<lose-all>
    :power 0
    :hit-effect (attack-effect (attacker target the-attack damage)
		  (when damage
		    (deduct-hp! target damage))
		  (when (typep target 'player)
		    (update-player-stat! target '<str> '<reduce>)
		    (update-player-stat! target '<dex> '<reduce>)
		    (update-player-stat! target '<con> '<reduce>)
		    (update-player-stat! target '<int> '<reduce>)
		    (update-player-stat! target '<wis> '<reduce>)
		    (update-player-stat! target '<chr> '<reduce>)
		    )
		  damage))

(define-monster-attack '<acid>
    :power 0
    :hit-effect (attack-effect (attacker target attack dmg)
		  (print-message! "You are covered in acid!")
		  (when (plusp dmg)
		    (deliver-elemental-damage! *variant* attacker target '<acid> dmg))
		  ;; add acid knowledge to attacker
		  dmg))

(define-monster-attack '<cold>
    :power 10
    :hit-effect (attack-effect (attacker target attack dmg)
		  (print-message! "You are covered with frost!")
		  (when (plusp dmg)
		    (deliver-elemental-damage! *variant* attacker target '<cold> dmg))
		  ;; add cold knowledge to attacker
		  dmg))

(define-monster-attack '<fire>
    :power 10
    :hit-effect (attack-effect (attacker target attack dmg)
		  (print-message! "You are enveloped in flames!")
		  (when (plusp dmg)
		    (deliver-elemental-damage! *variant* attacker target '<fire> dmg))
		  ;; add fire knowledge to attacker
		  dmg))

(define-monster-attack '<electricity>
    :power 10
    :hit-effect (attack-effect (attacker target attack dmg)
		  (print-message! "You are struck by electricity!")
		  (when (plusp dmg)
		    (deliver-elemental-damage! *variant* attacker target '<electricity> dmg))
		  ;; add elec knowledge to attacker
		  dmg))

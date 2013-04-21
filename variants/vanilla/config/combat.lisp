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

(define-attack-description '<hit> "hits you")
(define-attack-description '<beg> "begs you for money")
(define-attack-description '<touch> "touches you")
(define-attack-description '<claw> "claws you")
(define-attack-description '<bite> "bites you")
(define-attack-description '<wail> "wails at you")
(define-attack-description '<gaze> "gazes at you")
(define-attack-description '<butt> "butts you")
(define-attack-description '<kick> "kicks you")
(define-attack-description '<spore> "releases spores at you")
(define-attack-description '<engulf> "engulfs you")
(define-attack-description '<insult> "insults you") ;; randomise
(define-attack-description '<moan> "moans at you") ;; randomise
(define-attack-description '<spit> "spits on you")
(define-attack-description '<crush> "crushes you")
(define-attack-description '<crawl> "crawls on you")
(define-attack-description '<sting> "stings you")
(define-attack-description '<drool> "drools on you")

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

(define-monster-attack '<exp-10>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  ;; NOT IMPLEMENTED
		  ;; check for hold-life
		  (modify-xp! target -400) ;; hack
		  dmg))

(define-monster-attack '<exp-20>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  ;; NOT IMPLEMENTED
		  ;; check for hold-life
		  (modify-xp! target -400) ;; hack
		  dmg))


(define-monster-attack '<exp-40>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  ;; check for hold-life
		  (modify-xp! target -400) ;; hack
		  dmg))

(define-monster-attack '<exp-80>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  ;; NOT IMPLEMENTED
		  ;; check for hold-life
		  (modify-xp! target -400) ;; hack
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

(define-monster-attack '<eat-light>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (when (plusp dmg)
		    (deduct-hp! target dmg))
		  ;; NOT IMPLEMENTED
		  dmg))

(define-monster-attack '<eat-item>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (when (plusp dmg)
		    (deduct-hp! target dmg))
		  ;; NOT IMPLEMENTED
		  dmg))

(define-monster-attack '<eat-food>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (when (plusp dmg)
		    (deduct-hp! target dmg))
		  ;; NOT IMPLEMENTED
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

(define-monster-attack '<water>
    :power 10
    :hit-effect (attack-effect (attacker target attack dmg)
		  (print-message! "You are splashed by water!")
		  (when (plusp dmg)
		    (deliver-elemental-damage! *variant* attacker target '<water> dmg))
		  ;; add water knowledge to attacker
		  dmg))



(define-monster-attack '<un-power>
    :power 15
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  ;; NOT IMPLEMENTED
		  dmg))

(define-monster-attack '<un-bonus>
    :power 20
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  ;; NOT IMPLEMENTED
		  dmg))

;;; Monster special abilities

(define-monster-spabs 'breath-spab '<breath>
  `((<time> :breath-type <time> :desc "time"
     :damage ,#'(lambda (chp)
		 (max-cap 150 (int-/ chp 3))))
    (<fire> :breath-type <fire> :desc "fire" :visual "fire"
     :damage ,#'(lambda (chp)
		 (max-cap 1600 (int-/ chp 3))))
    (<cold> :breath-type <cold> :desc "frost" :visual "cold"
     :damage ,#'(lambda (chp)
		 (max-cap 1600 (int-/ chp 3))))
    (<electricity> :breath-type <electricity> :desc "lightning" :visual "electricity"
     :damage ,#'(lambda (chp)
		 (max-cap 1600 (int-/ chp 3))))
    (<acid> :breath-type <acid> :desc "acid" :visual "acid"
     :damage ,#'(lambda (chp)
		 (max-cap 1600 (int-/ chp 3))))
    (<poison> :breath-type <poison> :desc "gas" :visual "poison"
     :damage ,#'(lambda (chp)
		 (max-cap 1600 (int-/ chp 3))))
    (<plasma> :breath-type <plasma> :desc "plasma" :visual "fire"
     :damage ,#'(lambda (chp)
		 (max-cap 150 (int-/ chp 6))))
    (<light> :breath-type <light> :desc "light" :visual "light"
     :damage ,#'(lambda (chp)
		 (max-cap 400 (int-/ chp 6))))
    (<darkness> :breath-type <darkness> :desc "darkness" :visual "darkness"
     :damage ,#'(lambda (chp)
		 (max-cap 400 (int-/ chp 6))))
    (<nether> :breath-type <nether> :desc "nether" :visual "darkness"
     :damage ,#'(lambda (chp)
		 (max-cap 550 (int-/ chp 6))))
    (<nexus> :breath-type <nexus> :desc "nexus"
     :damage ,#'(lambda (chp)
		 (max-cap 400 (int-/ chp 6))))
    (<shards> :breath-type <shards> :desc "shards"
     :damage ,#'(lambda (chp)
		 (max-cap 500 (int-/ chp 6))))
    (<inertia> :breath-type <inertia> :desc "inertia"
     :damage ,#'(lambda (chp)
		 (max-cap 200 (int-/ chp 6))))
    (<force> :breath-type <force> :desc "force"
     :damage ,#'(lambda (chp)
		 (max-cap 200 (int-/ chp 6))))
    (<chaos> :breath-type <chaos> :desc "chaos"
     :damage ,#'(lambda (chp)
		 (max-cap 500 (int-/ chp 6))))
    (<water> :breath-type <water> :desc "water"
     :damage ,#'(lambda (chp)
		 (max-cap 250 (int-/ chp 3))))
    (<gravity> :breath-type <gravity> :desc "gravity"
     :damage ,#'(lambda (chp)
		 (max-cap 200 (int-/ chp 3))))
    (<sound> :breath-type <sound> :desc "sound" :visual "electricity"
     :damage ,#'(lambda (chp)
		 (max-cap 500 (int-/ chp 6))))
    (<disenchant> :breath-type <disenchant> :desc "disenchantment"
     :damage ,#'(lambda (chp)
		 (max-cap 150 (int-/ chp 3))))
    (<confusion> :breath-type <confusion> :desc "confusion"
     :damage ,#'(lambda (chp)
		 (max-cap 400 (int-/ chp 6))))
    ))

(define-monster-spabs 'ranged-spab '<arrow>
  '((1 :power 1 :range 6 :desc "an arrow")
    (2 :power 3 :range 8 :desc "an arrow")
    (3 :power 5 :range 10 :desc "a missile")
    (4 :power 7 :range 12 :desc "a missile")
    ))

(define-monster-spabs 'summon-spab '<summon>
  '((<angel> :what <angel>)
    (<demon> :what <demon>)
    (<wraith> :what <wraith>)
    (<dragon> :what <dragon>)
    (<kin> :what <kin>)
    (<ant> :what <ant>)
    (<hound> :what <hound>)
    (<unique> :what <unique>)
    (<monsters> :what <monsters>)
    (<monster> :what <monster>)
    (<undead> :what <undead>)
    (<hydra> :what <hydra>)
    (<spider> :what <spider>)
    (<high-undead> :what <high-undead>)
    (<high-dragon> :what <high-dragon>)
    (<high-demon> :what <high-demon>)
    ))

(define-monster-spabs 'spell-spab '<spell>
  `((<confusion>
     :effect ,(spab-effect (creature ability target dungeon)
		  (unless (amon.seen-by-player? creature)
		    (return-from spab-effect nil)) ;; need los
		  (disturbance *variant* *player* creature :major)
		  ;; check blindness
		  (if (is-blind? target)
		      (format-message! "~@(~A~) mumbles, you hear puzzling noises."
				       (get-creature-desc creature #x00))
		      (format-message! "~@(~A~) creates mesmerising illusions."
				       (get-creature-desc creature #x00)))
		  
		  (cond ((or (resists-element? target '<confusion>)
			     (roll-saving-throw target (get-power-lvl creature)))
			 (print-message! "You disbelieve the feeble spell."))
			(t
			 (modify-creature-state! target '<confusion> :add (+ 4 (randint 4)))))
		  t))
    (<blindness>
     :effect ,(spab-effect (creature ability target dungeon)
		  (unless (amon.seen-by-player? creature)
		    (return-from spab-effect nil)) ;; need los
		  (disturbance *variant* *player* creature :major)
		  ;; check blindness
		  (if (is-blind? target)
		      (format-message! "~@(~A~) mumbles."
				       (get-creature-desc creature #x00))
		      (format-message! "~@(~A~) casts a spell, burning your eyes."
				       (get-creature-desc creature #x00)))
		  
		  (cond ((resists-element? target '<blindness>)
			 (print-message! "You are unaffected."))
			((roll-saving-throw target (get-power-lvl creature))
			 (print-message! "You resist the effects!"))
			(t
			 (modify-creature-state! target '<blindness> :add (+ 4 (randint 4)))))
		  t))
    (<paralysis>
     :effect ,(spab-effect (creature ability target dungeon)
		  (unless (amon.seen-by-player? creature)
     		    (return-from spab-effect nil)) ;; need los
		  (disturbance *variant* *player* creature :major)
		  ;; check blindness
		  (if (is-blind? target)
		      (format-message! "~@(~A~) mumbles."
				       (get-creature-desc creature #x00))
		      (format-message! "~@(~A~) stares deep into your eyes."
				       (get-creature-desc creature #x00)))
		  
		  (cond ((eq t (get-creature-state target '<free-action>))
			 (print-message! "You are unaffected."))
			((roll-saving-throw target (get-power-lvl creature))
			 (print-message! "You resist the effects!"))
			(t
			 (modify-creature-state! target '<paralysed> :add (+ 4 (randint 4)))))
		  t))
    (<scare>
     :effect ,(spab-effect (creature ability target dungeon)
	          (unless (amon.seen-by-player? creature)
		    (return-from spab-effect nil)) ;; need los
		  (disturbance *variant* *player* creature :major)
		  ;; check blindness
		  (if (is-blind? target)
		      (format-message! "~@(~A~) mumbles, and you hear scary noises."
				       (get-creature-desc creature #x00))
		      (format-message! "~@(~A~) casts a fearful illusion."
				       (get-creature-desc creature #x00)))
		  
		  (cond ((or (resists-element? target '<fear>)
			     (roll-saving-throw target (get-power-lvl creature)))
			 (print-message! "You refuse to be frightened."))
			(t
			 (modify-creature-state! target '<fear> :add (+ 4 (randint 4)))))
		  t))
     
    (<haste>
     :effect ,(spab-effect (creature ability target dungeon)
 		  (disturbance *variant* *player* creature :major)
		  (setf target creature) ;; hack, fix 
		  (if (is-blind? *player*)
		      (format-message! "~@(~A~) mumbles."
				       (get-creature-desc creature #x00))
		      (format-message! "~@(~A~) concentrates on ~A body!"
				       (get-creature-desc creature #x00)
				       (get-creature-desc creature #x22)))
		     
		  (format-message! "~@(~A~) starts moving faster."
				   (get-creature-desc target #x00))
		  (haste-creature! target +10 20)
		  t))
				  
     
    (<slow>
     :effect ,(spab-effect (creature ability target dungeon)
	          (unless (amon.seen-by-player? creature)
		    (return-from spab-effect nil)) ;; need los
		  (disturbance *variant* *player* creature :major)
		  ;; check blindness
		  (format-message! "~@(~A~) drains power from your muscles."
				       (get-creature-desc creature #x00))

		  (cond ((eq t (get-creature-state target '<free-action>))
			 (print-message! "You are unaffected."))
			((roll-saving-throw target (get-power-lvl creature))
			 (print-message! "You resist the effects!"))
			(t
			 (modify-creature-state! target '<slowed> :add (+ 4 (randint 4)))))
		  
		  t))
    (<blink>
     :effect ,(spab-effect (creature ability target dungeon)
		  (disturbance *variant* *player* creature :major)
		  (format-message! "~@(~A~) blinks away."
				   (get-creature-desc creature #x00))
		  (teleport-creature! dungeon *player* creature 10)
		  t))

    (<teleport>
     :effect ,(spab-effect (creature ability target dungeon)
		  (disturbance *variant* *player* creature :major)
		  (format-message! "~@(~A~) teleports away."
				   (get-creature-desc creature #x00))
		  (teleport-creature! dungeon *player* creature (+ 5 (* +max-sight+ 2)))
		  t))

    (<teleport-away>
     :effect ,(spab-effect (creature ability target dungeon)
		  (disturbance *variant* *player* creature :major)
		  (format-message! "~@(~A~) teleports ~A away."
				   (get-creature-desc creature #x00)
				   (get-creature-desc target #x20))
		  (teleport-creature! dungeon *player* target 100)
		  t))


    (<teleport-level>)
    (<teleport-player>)
    (<missile> :visual "magic-missile"
     :effect ,(spab-effect (creature ability target dungeon)
		  (disturbance *variant* *player* creature :major)
		  ;; check blindness
		  (if (is-blind? target)
		      (format-message! "~@(~A~) mumbles."
				       (get-creature-desc creature #x00))
		      (format-message! "~@(~A~) casts a magic missile."
				       (get-creature-desc creature #x00)
				       ;;(get-creature-desc target #x20)
				       ))
		  (van-fire-bolt! creature target (get-spell-effect '<magic-missile>)
				  (+ (roll-dice 2 6) (int-/ (get-power-lvl creature) 3))
				  :projected-object ability)
		  t))

    (<darkness> :visual "darkness"
     :effect ,(spab-effect (creature ability target dungeon)
		  (unless (amon.seen-by-player? creature)
		    (return-from spab-effect nil)) ;; need los
		  (disturbance *variant* *player* creature :major)
		  ;; check blindness
		  (if (is-blind? *player*)
		      (format-message! "~@(~A~) mumbles."
				       (get-creature-desc creature #x00))
		      (format-message! "~@(~A~) conjures up shadows."
				       (get-creature-desc creature #x00)))
		  (light-area! dungeon creature (location-x target)
			       (location-y target) 0 3 :type '<darkness>
			       :projected-object ability)
		  ))
    
    (<brain-smash>)
    (<mind-blast>)
    (<drain-mana>)
    (<forget>)
    (<heal>)
    (<traps>)
    (<shriek>)
    ))

(define-monster-spabs 'bolt-spell-spab '<bolt-spell>
  `((<fire> :type <fire> :desc "fire bolt" :visual "fire"
     :damage ,#'(lambda (lvl)
		 (+ (roll-dice 9 8) (int-/ lvl 3))))
    (<cold> :type <cold> :desc "frost bolt" :visual "cold"
     :damage ,#'(lambda (lvl)
		 (+ (roll-dice 6 8) (int-/ lvl 3))))
    (<acid> :type <acid> :desc "acid bolt" :visual "acid" 
     :damage ,#'(lambda (lvl)
		 (+ (roll-dice 7 8) (int-/ lvl 3))))
    (<electricity> :type <electricity> :desc "lightning bolt" :visual "electricity"
     :damage ,#'(lambda (lvl)
		 (+ (roll-dice 4 8) (int-/ lvl 3))))
    (<poison> :type <poison> :desc "poison bolt" :visual "poison"
     :damage ,#'(lambda (lvl)
		 (+ (roll-dice 9 8) (int-/ lvl 3))))
    (<mana> :type <mana> :desc "mana bolt" :visual "magic-missile"
     :damage ,#'(lambda (lvl)
		 (+ 50 (randint (int-/ (* lvl 7) 2)))))
    (<plasma> :type <plasma> :desc "plasma bolt" :visual "fire"
     :damage ,#'(lambda (lvl)
		 (+ 10 (roll-dice 8 7) lvl)))
    (<nether> :type <nether> :desc "nether bolt" :visual "darkness"
     :damage ,#'(lambda (lvl)
		 (+ 30 (roll-dice 5 5) (int-/ (* 3 lvl) 2))))
    (<water> :type <water> :desc "water bolt"
     :damage ,#'(lambda (lvl)
		 (+ (roll-dice 10 10) lvl)))
    ))

(define-monster-spabs 'ball-spell-spab '<ball-spell>
  `((<fire> :type <fire> :desc "fire ball" :visual "fire"
     :damage ,#'(lambda (lvl)
		 (+ 10 (randint (int-/ (* lvl 7) 2)))))
    
    (<cold> :type <cold> :desc "cold ball" :visual "cold"
     :damage ,#'(lambda (lvl)
		 (+ 10 (randint (int-/ (* lvl 3) 2)))))

    (<acid> :type <acid> :desc "acid ball" :visual "acid"
     :damage ,#'(lambda (lvl)
		      (+ 15 (randint (* lvl 3)))))

    (<electricity> :type <electricity> :desc "lightning ball" :visual "electricity"
     :damage ,#'(lambda (lvl)
		 (+ 8 (randint (int-/ (* lvl 3) 2)))))

    (<poison> :type <poison> :desc "stinking cloud" :visual "poison" ;; not right, but ok
     :damage ,#'(lambda (lvl)
		  (declare (ignore lvl))
		  (roll-dice 12 2)))

    (<mana> :type <mana> :desc "mana storm" :visual "magic-missile"
     :damage ,#'(lambda (lvl)
		 (+ (* 5 lvl) (roll-dice 10 10))))

    (<darkness> :type <darkness> :desc "darkness storm" :visual "darkness"
     :damage ,#'(lambda (lvl)
		 (+ (* 5 lvl) (roll-dice 10 10))))

    (<water> :type <water> :desc "whirlpool"
     :damage ,#'(lambda (lvl)
		 (+ 50 (randint (int-/ (* lvl 5) 2)))))

    (<nether> :type <nether> :desc "nether" :visual "darkness"
     :damage ,#'(lambda (lvl)
		 (+ 50 lvl (roll-dice 10 10))))

    ))

(define-monster-spabs 'dmg-spell-spab '<dmg-spell>
  '((1 :power 1)
    (2 :power 2)
    (3 :power 3)
    (4 :power 4)
    ))

